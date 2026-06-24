-- Step 1: Generate most-recent-record-resolved, trimmed, chain-merged episodes (replicates v2 firststep_v2.sql)
-- Input: D3_CONCEPTS (view on concepts_db), study_variables, all_persons, pregnancy_episode_windows
-- Output: episodes_raw (person_id, variable_id, value, start_episode, end_episode)
--
-- Pipeline inside this step (mirrors v2 firststep CTEs):
--   concept_dedup     → deduplicate per (person, concept, date)
--   MCE_SV            → two modes controlled by study_variables.excluding_pregnancies:
--                        FALSE (default): concept date inside pregnancy window
--                          (lmp_date → pregnancy_end_date); episode from concept date
--                          to pregnancy_end_date, value preserved.
--                        TRUE: concept date inside look-back window
--                          (lmp_date - start_look_back → lmp_date); episode forced to
--                          TRUE from concept date to pregnancy_end_date; any concept date
--                          that falls within a pregnancy window is excluded.
--   prior_carry       → for study_variables with is_prior = TRUE, create
--                        TRUE episodes spanning later pregnancy windows after
--                        the first TRUE pregnancy window; first TRUE episode
--                        is set to FALSE
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
        -- Standard mode (excluding_pregnancies = FALSE / NULL):
        -- concept date falls inside pregnancy window; episode from concept date
        -- to pregnancy_end_date with the original concept value.
        SELECT DISTINCT
            c.person_id,
            sv.variable_id,
            c.value,
            c.date AS start_episode,
            pw.pregnancy_end_date AS end_episode
        FROM
            concept_dedup c
            JOIN study_variables sv ON c.concept_id = sv.concept_id
            JOIN pregnancy_episode_windows pw ON pw.person_id = c.person_id
            AND COALESCE(UPPER(TRIM(CAST(pw.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
            AND c.date BETWEEN pw.lmp_date AND pw.pregnancy_end_date
        WHERE
            c.concept_id IN ({concept_id_list})
            AND COALESCE(
                UPPER(TRIM(CAST(sv.excluding_pregnancies AS VARCHAR))),
                ''
            ) NOT IN ('TRUE', 'T', '1', 'YES', 'Y')
        UNION ALL
        -- Excluding mode (excluding_pregnancies = TRUE):
        -- concept date falls in the look-back window before lmp_date
        -- (lmp_date - start_look_back <= concept_date <= lmp_date); episode is forced
        -- TRUE from concept date to pregnancy_end_date.
        -- Events whose concept date falls within any pregnancy window are excluded.
        SELECT DISTINCT
            c.person_id,
            sv.variable_id,
            'TRUE' AS value,
            c.date AS start_episode,
            pw.pregnancy_end_date AS end_episode
        FROM
            concept_dedup c
            JOIN study_variables sv ON c.concept_id = sv.concept_id
            JOIN pregnancy_episode_windows pw ON pw.person_id = c.person_id
            AND COALESCE(UPPER(TRIM(CAST(pw.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
            AND c.date BETWEEN pw.lmp_date - CAST(sv.start_look_back AS INTEGER) AND pw.lmp_date
        WHERE
            c.concept_id IN ({concept_id_list})
            AND COALESCE(
                UPPER(TRIM(CAST(sv.excluding_pregnancies AS VARCHAR))),
                ''
            ) IN ('TRUE', 'T', '1', 'YES', 'Y')
            -- Exclude concept dates that fall within any active pregnancy window
            AND NOT EXISTS (
                SELECT
                    1
                FROM
                    pregnancy_episode_windows excl_pw
                WHERE
                    excl_pw.person_id = c.person_id
                    AND COALESCE(UPPER(TRIM(CAST(excl_pw.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
                    AND c.date BETWEEN excl_pw.lmp_date AND excl_pw.pregnancy_end_date
            )
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
WITH
    pregnancy_windows AS (
        SELECT
            pw.person_id,
            pw.lmp_date,
            pw.pregnancy_end_date,
            ROW_NUMBER() OVER (
                PARTITION BY
                    pw.person_id
                ORDER BY
                    pw.lmp_date,
                    pw.pregnancy_end_date
            ) AS pregnancy_window_rank
        FROM
            pregnancy_episode_windows pw
            INNER JOIN all_persons p ON pw.person_id = p.person_id
        WHERE
            COALESCE(UPPER(TRIM(CAST(pw.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
    ),
    first_true_windows AS (
        SELECT DISTINCT
            s1.person_id,
            s1.variable_id,
            pw.pregnancy_window_rank
        FROM
            trimmed_episodes s1
            INNER JOIN pregnancy_windows pw ON s1.person_id = pw.person_id
            AND s1.start_episode BETWEEN pw.lmp_date AND pw.pregnancy_end_date
            INNER JOIN study_variables sv ON s1.variable_id = sv.variable_id
        WHERE
            COALESCE(UPPER(TRIM(CAST(s1.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
            AND COALESCE(CAST(sv.is_prior AS BOOLEAN), FALSE) = TRUE
    ),
    first_true_per_var AS (
        SELECT
            person_id,
            variable_id,
            MIN(pregnancy_window_rank) AS first_true_rank
        FROM
            first_true_windows
        GROUP BY
            person_id,
            variable_id
    ),
    first_true_episode AS (
        SELECT
            person_id,
            variable_id,
            start_episode,
            end_episode
        FROM
            (
                SELECT
                    s.person_id,
                    s.variable_id,
                    s.start_episode,
                    s.end_episode,
                    ROW_NUMBER() OVER (
                        PARTITION BY
                            s.person_id,
                            s.variable_id
                        ORDER BY
                            s.start_episode,
                            s.end_episode
                    ) AS rn
                FROM
                    trimmed_episodes s
                    INNER JOIN study_variables sv ON s.variable_id = sv.variable_id
                WHERE
                    COALESCE(UPPER(TRIM(CAST(s.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
                    AND COALESCE(CAST(sv.is_prior AS BOOLEAN), FALSE) = TRUE
            ) ranked_true_episodes
        WHERE
            rn = 1
    ),
    prior_carry_forward AS (
        SELECT DISTINCT
            pw.person_id,
            sv.variable_id,
            'TRUE' AS value,
            pw.lmp_date AS start_episode,
            pw.pregnancy_end_date AS end_episode
        FROM
            pregnancy_windows pw
            INNER JOIN study_variables sv ON COALESCE(CAST(sv.is_prior AS BOOLEAN), FALSE) = TRUE
            INNER JOIN first_true_per_var f ON pw.person_id = f.person_id
            AND sv.variable_id = f.variable_id
        WHERE
            pw.pregnancy_window_rank > f.first_true_rank
    ),
    episodes_base AS (
        SELECT
            s.person_id,
            s.variable_id,
            CASE
                WHEN f.person_id IS NOT NULL THEN 'FALSE'
                ELSE s.value
            END AS value,
            s.start_episode,
            s.end_episode
        FROM
            trimmed_episodes s
            LEFT JOIN first_true_episode f ON s.person_id = f.person_id
            AND s.variable_id = f.variable_id
            AND s.start_episode = f.start_episode
            AND s.end_episode = f.end_episode
            AND COALESCE(UPPER(TRIM(CAST(s.value AS VARCHAR))), '') IN ('TRUE', 'T', '1', 'YES', 'Y')
    ),
    episodes_input AS (
        SELECT
            person_id,
            variable_id,
            value,
            start_episode,
            end_episode
        FROM
            episodes_base
        UNION ALL
        SELECT
            person_id,
            variable_id,
            value,
            start_episode,
            end_episode
        FROM
            prior_carry_forward
    ),
    chain_merged AS (
        SELECT
            s1.person_id,
            s1.variable_id,
            s1.value,
            s1.start_episode,
            MIN(t1.end_episode) AS end_episode
        FROM
            episodes_input s1
            INNER JOIN episodes_input t1 ON s1.start_episode <= t1.end_episode
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
                    episodes_input t2
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
                    episodes_input s2
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
            s1.start_episode
    )
SELECT
    cm.person_id,
    cm.variable_id,
    cm.value,
    cm.start_episode,
    cm.end_episode
FROM
    chain_merged cm;
