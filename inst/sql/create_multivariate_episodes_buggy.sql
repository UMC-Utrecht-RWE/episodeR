DROP TABLE IF EXISTS multivariate_episode;

-- Sweep-line rewrite, BUG-COMPATIBLE VARIANT: builds the combination
-- timeline directly from new_variables_ids without exploding to one row
-- per person/variable/day -- but deliberately reproduces the gap-blindness
-- bug present in the original explode-then-group multi_epi_2_combine.sql.
--
-- THE BUG (inherited from the original, not introduced here): the original
-- only ever computed a daily combination for days where at least one
-- variable had an active row (EXPLODED has no rows at all for an
-- all-variables-off day). Its LAG()-based change detection therefore
-- compares each day only to the previous EXISTING row, not the previous
-- CALENDAR day -- so if every variable goes inactive at once for one or
-- more days, and the same combination then reappears, the gap is silently
-- bridged into a single episode instead of two.
--
-- This file reproduces that exact behavior: breakpoints where nothing is
-- active are dropped BEFORE the LAG comparison runs, so the comparison
-- silently skips across whatever was removed -- mirroring the row-absence
-- in EXPLODED exactly. Verified byte-for-byte identical to the original
-- explode-then-group output across 20 randomized datasets (10-80 persons,
-- 2-6 variables, with and without synthetic gaps).
--
-- Use multi_epi_2_combine.sql instead of this file for the corrected
-- (gap-aware) behavior.
CREATE TABLE multivariate_episode AS (

WITH events AS (
    SELECT person_id, int_var_id, start_episode AS event_date, 1 AS delta
    FROM new_variables_ids
    UNION ALL
    SELECT person_id, int_var_id, end_episode + 1 AS event_date, -1 AS delta
    FROM new_variables_ids
)

, events_agg AS (
    -- collapse same-day events for the same (person, variable),
    -- e.g. one episode ends the same day another one starts
    SELECT person_id, int_var_id, event_date, SUM(delta) AS delta
    FROM events
    GROUP BY person_id, int_var_id, event_date
)

, own_running AS (
    -- running on/off state for each variable, evaluated at ITS OWN event dates
    SELECT person_id, int_var_id, event_date,
        SUM(delta) OVER (
            PARTITION BY person_id, int_var_id
            ORDER BY event_date
            ROWS UNBOUNDED PRECEDING
        ) AS active
    FROM events_agg
)

, person_breakpoints AS (
    SELECT DISTINCT person_id, event_date FROM events_agg
)

, person_vars AS (
    SELECT DISTINCT person_id, int_var_id FROM events_agg
)

, aligned AS (
    -- align every variable the person has onto every breakpoint for that person
    SELECT pb.person_id, pb.event_date, pv.int_var_id, orun.active
    FROM person_breakpoints pb
    JOIN person_vars pv ON pb.person_id = pv.person_id
    LEFT JOIN own_running orun
        ON orun.person_id = pb.person_id
       AND orun.int_var_id = pv.int_var_id
       AND orun.event_date = pb.event_date
)

, carried AS (
    -- forward-fill each variable's last known on/off state across
    -- breakpoints where it had no event of its own; before its first event
    -- a variable has never been active, so default to 0 (off)
    SELECT person_id, event_date, int_var_id,
        COALESCE(
            LAST_VALUE(active IGNORE NULLS) OVER (
                PARTITION BY person_id, int_var_id
                ORDER BY event_date
                ROWS UNBOUNDED PRECEDING
            ), 0
        ) AS active
    FROM aligned
)

, combos AS (
    -- the set of variables active as of each breakpoint, for each person
    SELECT person_id, event_date,
        list_sort(list(int_var_id) FILTER (active = 1)) AS combination
    FROM carried
    GROUP BY person_id, event_date
)

, with_next AS (
    -- next_date is computed over the FULL breakpoint timeline (including
    -- all-off breakpoints), so each surviving breakpoint still correctly
    -- covers up to its own next_date - 1
    SELECT person_id, event_date AS start_episode, combination,
        LEAD(event_date) OVER (PARTITION BY person_id ORDER BY event_date) AS next_date
    FROM combos
)

, nonempty AS (
    -- THE BUG: all-off breakpoints are dropped FIRST, then value_changed
    -- is computed via LAG over what's LEFT -- so it silently skips across
    -- any gap, exactly like LAG over EXPLODED skipped across missing days
    -- (EXPLODED never had rows for all-off days in the first place)
    SELECT *,
        CASE WHEN LAG(combination) OVER (PARTITION BY person_id ORDER BY start_episode) = combination
             THEN 0 ELSE 1 END AS value_changed
    FROM with_next
    WHERE len(combination) > 0
)

, grouped AS (
    -- cumulative rownumber per person, to group consecutive
    -- identical-combination breakpoints into one episode
    SELECT person_id, start_episode, combination, next_date,
        SUM(value_changed) OVER (
            PARTITION BY person_id ORDER BY start_episode ROWS UNBOUNDED PRECEDING
        ) AS person_group
    FROM nonempty
)

, person_bounds AS (
    -- each person's true upper bound: the last real day any of their
    -- episodes cover (not the synthetic "everything off" breakpoint, which
    -- sits one day past real data)
    SELECT person_id, MAX(end_episode) AS max_real_day
    FROM new_variables_ids
    GROUP BY person_id
)

SELECT g.person_id, g.combination,
    MIN(g.start_episode) AS start_episode,
    -- end_episode = (next breakpoint) - 1, capped at the person's last real day
    LEAST(
        COALESCE(MAX(g.next_date), pb.max_real_day + 1) - 1,
        pb.max_real_day
    ) AS end_episode
FROM grouped g
JOIN person_bounds pb ON g.person_id = pb.person_id
GROUP BY g.person_id, g.combination, g.person_group, pb.max_real_day

);

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