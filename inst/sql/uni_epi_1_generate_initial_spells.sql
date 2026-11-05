-- Step 1: Generate most-recent-record-resolved, trimmed, chain-merged episodes (replicates v2 firststep_v2.sql)
-- Input: D3_CONCEPTS (view on concepts_db), study_variables, all_persons
-- Output: episodes_raw (person_id, variable_id, value, start_episode, end_episode)
--
-- Pipeline inside this step (mirrors v2 firststep CTEs):
--   concept_dedup     → deduplicate per (person, concept, date)
--   MCE_SV            → initial episodes: date + end_look_back / date + start_look_back
--   ranked_dates      → sort by start_episode per (person, variable)
--   intervals         → LEFT JOIN next row to get new_end
--   adjusted          → most-recent-record resolution: crop end_episode to next_start - 1
--   trimmed           → clamp to [start_study_date, end_study_date] + filter degenerate
--   episodes_raw      → chain-merge same-value adjacent/overlapping trimmed intervals
CREATE OR REPLACE TABLE concept_dedup AS
WITH
    concepts_dated AS (
        SELECT
            c.person_id,
            c.concept_id,
            CAST(c.date AS DATE) AS date,
            CAST(c.value AS VARCHAR) AS value
        FROM
            D3_CONCEPTS c
    )
SELECT
    c.person_id,
    c.concept_id,
    c.date,
    CASE
        WHEN COUNT(DISTINCT c.value) > 1 THEN NULL
        ELSE MAX(c.value)
    END AS value
FROM
    concepts_dated c
    INNER JOIN all_persons p ON c.person_id = p.person_id
GROUP BY
    c.person_id,
    c.concept_id,
    c.date;

CREATE OR REPLACE TABLE trimmed_episodes AS
WITH
    MCE_SV AS (
        SELECT DISTINCT
            c.person_id,
            sv.variable_id,
            c.value,
            c.date + CAST(sv.end_look_back AS INTEGER) AS start_episode,
            c.date + CAST(sv.start_look_back AS INTEGER) AS end_episode
        FROM
            concept_dedup c
            JOIN study_variables sv ON c.concept_id = sv.concept_id
        WHERE
            c.concept_id IN ({concept_id_list})
    ),
    ranked_dates AS (
        SELECT
            *,
            ROW_NUMBER() OVER (
                PARTITION BY
                    person_id,
                    variable_id
                ORDER BY
                    start_episode
            ) AS rn
        FROM
            MCE_SV
    ),
    intervals AS (
        SELECT
            rd1.person_id,
            rd1.variable_id,
            rd1.value,
            rd1.start_episode,
            rd1.end_episode,
            rd2.start_episode AS new_end
        FROM
            ranked_dates rd1
            LEFT JOIN ranked_dates rd2 ON rd1.person_id = rd2.person_id
            AND rd1.variable_id = rd2.variable_id
            AND rd1.rn + 1 = rd2.rn
    ),
    -- Most-recent-record resolution: crop end to next record's start - 1
    adjusted AS (
        SELECT
            person_id,
            variable_id,
            value,
            start_episode,
            CASE
                WHEN new_end IS NOT NULL
                AND new_end <= end_episode
                AND NOT start_episode = end_episode THEN new_end - 1
                ELSE end_episode
            END AS end_episode
        FROM
            intervals
    ),
    -- Clamp to [start_study_date, end_study_date]; filter out episodes that don't overlap study period
    trimmed AS (
        SELECT
            a.person_id,
            a.variable_id,
            a.value,
            CASE
                WHEN a.start_episode < DATE({start_study_date}) THEN DATE({start_study_date})
                ELSE a.start_episode
            END AS start_episode,
            CASE
                WHEN a.end_episode > DATE({end_study_date}) THEN DATE({end_study_date})
                ELSE a.end_episode
            END AS end_episode
        FROM
            adjusted a
        WHERE
            (
                a.start_episode BETWEEN {start_study_date} AND {end_study_date}
            )
            OR (
                a.end_episode BETWEEN {start_study_date} AND {end_study_date}
            )
            OR (
                a.start_episode < {start_study_date}
                AND a.end_episode > {end_study_date}
            )
    )
SELECT
    person_id,
    variable_id,
    value,
    start_episode,
    end_episode
FROM
    trimmed;

-- Materialise trimmed before chain-merge to avoid DuckDB CTE re-evaluation inconsistencies
-- when trimmed is referenced multiple times in correlated subqueries.
CREATE OR REPLACE TABLE episodes_raw AS
SELECT
    s1.person_id,
    s1.variable_id,
    s1.value,
    s1.start_episode,
    MIN(t1.end_episode) AS end_episode
FROM
    trimmed_episodes s1
    INNER JOIN trimmed_episodes t1 ON s1.start_episode <= t1.end_episode
    AND s1.person_id = t1.person_id
    AND s1.variable_id = t1.variable_id
    AND (
        s1.value = t1.value
        OR (
            s1.value IS NULL
            AND t1.value IS NULL
        )
    )
    AND NOT EXISTS (
        SELECT
            1
        FROM
            trimmed_episodes t2
        WHERE
            t1.end_episode >= t2.start_episode - 1
            AND t1.end_episode < t2.end_episode
            AND t1.person_id = t2.person_id
            AND t1.variable_id = t2.variable_id
            AND (
                t1.value = t2.value
                OR (
                    t1.value IS NULL
                    AND t2.value IS NULL
                )
            )
    )
WHERE
    NOT EXISTS (
        SELECT
            1
        FROM
            trimmed_episodes s2
        WHERE
            s1.start_episode > s2.start_episode
            AND s1.start_episode <= s2.end_episode + 1
            AND s1.person_id = s2.person_id
            AND s1.variable_id = s2.variable_id
            AND (
                s1.value = s2.value
                OR (
                    s1.value IS NULL
                    AND s2.value IS NULL
                )
            )
    )
GROUP BY
    s1.person_id,
    s1.variable_id,
    s1.value,
    s1.start_episode;
