CREATE OR REPLACE TABLE multivariate_episode AS
WITH events AS (
    SELECT person_id, int_var_id, start_episode AS event_date, 1 AS delta
    FROM new_variables_ids
    UNION ALL
    SELECT person_id, int_var_id, end_episode + 1 AS event_date, -1 AS delta
    FROM new_variables_ids
),
events_agg AS (
    SELECT person_id, int_var_id, event_date, SUM(delta) AS delta
    FROM events
    GROUP BY person_id, int_var_id, event_date
),
-- running on/off state for each variable, evaluated only at ITS OWN event dates
own_running AS (
    SELECT person_id, int_var_id, event_date,
        SUM(delta) OVER (
            PARTITION BY person_id, int_var_id ORDER BY event_date
            ROWS UNBOUNDED PRECEDING
        ) AS active
    FROM events_agg
),
person_breakpoints AS (SELECT DISTINCT person_id, event_date FROM events_agg),
person_vars        AS (SELECT DISTINCT person_id, int_var_id FROM events_agg),
-- align every variable onto every breakpoint for that person
aligned AS (
    SELECT pb.person_id, pb.event_date, pv.int_var_id, orun.active
    FROM person_breakpoints pb
    JOIN person_vars pv ON pb.person_id = pv.person_id
    LEFT JOIN own_running orun
        ON orun.person_id = pb.person_id
       AND orun.int_var_id = pv.int_var_id
       AND orun.event_date = pb.event_date
),
-- forward-fill each variable's last known state across breakpoints where it had no event
carried AS (
    SELECT person_id, event_date, int_var_id,
        COALESCE(
            LAST_VALUE(active IGNORE NULLS) OVER (
                PARTITION BY person_id, int_var_id ORDER BY event_date
                ROWS UNBOUNDED PRECEDING
            ), 0
        ) AS active
    FROM aligned
),
-- combos AS (
--     SELECT person_id, event_date,
--         list_sort(list(int_var_id) FILTER (active = 1)) AS combination
--     FROM carried
--     GROUP BY person_id, event_date
-- ),
combos AS (
    SELECT person_id, event_date,
        list_sort(list(int_var_id) FILTER (active > 0)) AS combination
    FROM carried
    GROUP BY person_id, event_date
),
-- next_date computed BEFORE dropping empty combos, so end_episode math stays correct
with_next AS (
    SELECT person_id, event_date AS start_episode, combination,
        LEAD(event_date) OVER (PARTITION BY person_id ORDER BY event_date) AS next_date,
        CASE WHEN LAG(combination) OVER (PARTITION BY person_id ORDER BY event_date) = combination
             THEN 0 ELSE 1 END AS value_changed
    FROM combos
),
nonempty AS (
    SELECT * FROM with_next WHERE len(combination) > 0
),
grouped AS (
    SELECT person_id, start_episode, combination, next_date,
        SUM(value_changed) OVER (PARTITION BY person_id ORDER BY start_episode ROWS UNBOUNDED PRECEDING) AS grp
    FROM nonempty
),
person_bounds AS (
    SELECT person_id, MAX(end_episode) AS max_real_day
    FROM new_variables_ids GROUP BY person_id
)
SELECT g.person_id, g.combination,
       MIN(g.start_episode) AS start_episode,
       LEAST(COALESCE(MAX(g.next_date), pb.max_real_day + 1) - 1, pb.max_real_day) AS end_episode
FROM grouped g
JOIN person_bounds pb ON g.person_id = pb.person_id
GROUP BY g.person_id, g.combination, g.grp, pb.max_real_day;

FROM multivariate_episode;

CREATE OR REPLACE TABLE dim_combination AS
SELECT 
    combination,
    ROW_NUMBER() OVER (ORDER BY combination) AS dic_index
FROM (
    SELECT DISTINCT combination 
    FROM multivariate_episode
);


--NEW 14
-- Step 2b: Pivot multivariate_episode (list-of-int_var_id combination
-- column) into the wide person x episode x variable_id format that the
-- original pipeline produced via R's dcast(). This replaces the R-side
-- unnest + join + dcast block in multivariate_episodes_pipeline.R.
--
-- Pure SQL, single execute_sql_file() call -- no host-language column
-- discovery step needed between the two statements. The PIVOT learns its
-- column set (one per distinct variable_id actually present in some
-- combination -- not every variable_id in dim_var; a variable that's never
-- active anywhere correctly gets no column) at the moment it runs, and the
-- second statement's CASE ... COLUMNS(*) ... END transform expression
-- expands DuckDB-side to one CASE per matched column, so it adapts to
-- whatever columns the PIVOT produced without needing to know their names
-- in advance.
--
-- Semantics preserved from the original dcast(value.var="value", fill=FALSE):
--   - variable NOT in the episode's combination at all  -> 'FALSE' (string)
--   - variable IS in the combination, but its value happens to be NULL
--     (e.g. a univariate gap-fill code)                  -> NULL (real NA)
-- These two cases must not collapse into the same thing, which is why the
-- struct sentinel {'present': true, 'val': ...} is used as the pivot's
-- aggregated value instead of pivoting on value directly: PIVOT's
-- first(value) alone can't distinguish "no row" from "row with NULL value".

-- DROP TABLE IF EXISTS multivariate_episode_wide_raw;
-- DROP TABLE IF EXISTS multivariate_episode_wide;

-- Statement 1: unnest each episode's combination, join to dim_var to
-- recover variable_id/value, then pivot into one struct column per
-- distinct variable_id seen in any combination.
CREATE OR REPLACE TABLE multivariate_episode_wide_raw AS (
    WITH unpacked AS (
        SELECT
            me.person_id,
            me.start_episode,
            me.end_episode,
            dv.variable_id,
            {'present': true, 'val': upper(dv.value)} AS cell
        FROM multivariate_episode me, UNNEST(me.combination) AS u(int_var_id)
        JOIN dim_var dv ON dv.int_var_id = u.int_var_id
    )
    PIVOT unpacked
    ON variable_id USING first(cell)
    GROUP BY person_id, start_episode, end_episode
);

-- Statement 2: unwrap each per-variable struct column into the final
-- value -- 'FALSE' where the struct itself is NULL (variable absent from
-- the combination), otherwise the variable's actual value (which may
-- itself be NULL). COLUMNS(*) applies this CASE expression to every
-- non-key column dynamically, whatever those columns turned out to be.
CREATE OR REPLACE TABLE multivariate_episode_wide AS (
    SELECT
        person_id,
        start_episode,
        end_episode,
        CASE
            WHEN COLUMNS(* EXCLUDE (person_id, start_episode, end_episode)) IS NULL
            THEN 'FALSE'
            ELSE CAST(
                struct_extract(COLUMNS(* EXCLUDE (person_id, start_episode, end_episode)), 'val')
                AS VARCHAR
            )
        END
    FROM multivariate_episode_wide_raw
);
