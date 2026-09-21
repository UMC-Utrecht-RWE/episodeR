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
-- sparse: a row exists only at this variable's own event dates. Gaps at
-- other variables' breakpoints are filled later (see carried).
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
-- cross-joins every variable onto every person's breakpoints (not just its
-- own), so a combination change triggered by one variable is visible to
-- all the others at that date too
aligned AS (
    SELECT pb.person_id, pb.event_date, pv.int_var_id, orun.active
    FROM person_breakpoints pb
    JOIN person_vars pv ON pb.person_id = pv.person_id
    LEFT JOIN own_running orun
        ON orun.person_id = pb.person_id
       AND orun.int_var_id = pv.int_var_id
       AND orun.event_date = pb.event_date
),
-- closes the gaps left by the cross join above: active is NULL at any
-- breakpoint that isn't this variable's own event, so carry the last
-- known value forward (0 before its first event)
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
-- COALESCE covers the last episode, where next_date is NULL because there's
-- no following breakpoint; LEAST then clamps to max_real_day so an episode
-- never ends past the person's last real observation
SELECT g.person_id, g.combination,
       MIN(g.start_episode) AS start_episode,
       LEAST(COALESCE(MAX(g.next_date), pb.max_real_day + 1) - 1, pb.max_real_day) AS end_episode
FROM grouped g
JOIN person_bounds pb ON g.person_id = pb.person_id
GROUP BY g.person_id, g.combination, g.grp, pb.max_real_day;

-- PIVOT's column set is discovered from the data at run time: one column
-- per distinct variable_id that's actually active in some combination, not
-- every variable_id in dim_var. A variable never active for anyone
-- correctly gets no column at all.
--
-- The pivoted cell is a {'present', 'val'} struct rather than the raw
-- value, because first(value) alone can't tell "variable absent from the
-- combination" apart from "variable present with a NULL value" -- both
-- would just be NULL. The struct's 'present' flag keeps those cases
-- distinguishable for the unwrap step below.
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

-- A NULL struct means the variable was absent from the combination, which
-- must render as the string 'FALSE' -- not the same as a present variable
-- whose actual value is NULL, which must stay NULL. COLUMNS(*) is used
-- instead of naming columns because the pivot's column set isn't known
-- until it runs; DuckDB expands this CASE once per matched column.
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
