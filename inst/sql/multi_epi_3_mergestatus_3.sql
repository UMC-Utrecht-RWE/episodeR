-- Per-batch: append this batch's decoded, merged episodes to the running total
INSERT INTO final_episodes
WITH ordered AS (
    SELECT
        person_id,
        dic_index,
        start_episode::DATE AS start_episode,
        end_episode::DATE AS end_episode,
        MAX(end_episode::DATE) OVER (
            PARTITION BY person_id, dic_index
            ORDER BY start_episode, end_episode
            ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
        ) AS running_max_end
    FROM multivariate_episode_coded
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
            PARTITION BY person_id, dic_index
            ORDER BY start_episode, end_episode
            ROWS UNBOUNDED PRECEDING
        ) AS grp
    FROM flagged
)
, merged_coded AS (
    SELECT
        person_id,
        dic_index,
        MIN(start_episode) AS start_episode,
        MAX(end_episode) AS end_episode
    FROM grouped
    GROUP BY person_id, dic_index, grp
)
SELECT
    m.person_id,
    m.start_episode,
    m.end_episode,
    d.* EXCLUDE (dic_index)
FROM merged_coded m
JOIN dictionary d USING (dic_index)
ORDER BY m.person_id, m.start_episode;