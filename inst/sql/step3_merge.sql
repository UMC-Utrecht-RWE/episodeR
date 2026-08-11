-- =====================================================================
-- STEP 3: Merge touching/overlapping episodes of the same combination
-- Linear-cost window-function rewrite (no self-join).
-- =====================================================================
CREATE OR REPLACE TABLE multivariate_episode_merged AS (
    WITH ordered AS (
        SELECT
            person_id,
            combination,
            start_episode::DATE AS start_episode,
            end_episode::DATE AS end_episode,
            MAX(end_episode::DATE) OVER (
                PARTITION BY person_id, combination
                ORDER BY start_episode, end_episode
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
            ) AS running_max_end
        FROM multivariate_episode
    )
    , flagged AS (
        SELECT
            *,
            CASE
                WHEN running_max_end IS NULL THEN 1
                WHEN start_episode > running_max_end + 1 THEN 1
                ELSE 0
            END AS new_group_flag
        FROM ordered
    )
    , grouped AS (
        SELECT
            *,
            SUM(new_group_flag) OVER (
                PARTITION BY person_id, combination
                ORDER BY start_episode, end_episode
                ROWS UNBOUNDED PRECEDING
            ) AS grp
        FROM flagged
    )
    SELECT
        person_id, combination,
        MIN(start_episode) AS start_episode,
        MAX(end_episode) AS end_episode
    FROM grouped
    GROUP BY person_id, combination, grp
    ORDER BY person_id, combination, start_episode
);
