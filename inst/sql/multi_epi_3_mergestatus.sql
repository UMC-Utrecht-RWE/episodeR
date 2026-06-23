-- Merge intervals that touch or overlap (within 1 day) per person_id + dic_index,
-- using window functions instead of a self-join for linear-time scaling

-- CREATE TABLE multivariate_episode_merged AS (

    WITH ordered AS (
        -- For each row, find the running max end_episode seen so far in this
        -- partition (looking only at PRIOR rows), so nested/overlapping intervals
        -- are correctly absorbed rather than just compared to the immediate LAG
        SELECT
            person_id,
            dic_index,
            start_episode,
            end_episode,
            MAX(end_episode) OVER (
                PARTITION BY person_id, dic_index
                ORDER BY start_episode, end_episode
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
            ) AS running_max_end
        FROM multivariate_episode_coded
    )

    , flagged AS (
        -- Flag the start of a new merged group: either the first row in the
        -- partition, or a row whose start is more than 1 day past everything
        -- seen so far (a genuine gap, not a touch/overlap)
        SELECT
            *,
            CASE
                WHEN running_max_end IS NULL THEN 1
                WHEN start_episode > running_max_end + INTERVAL '1 day' THEN 1
                ELSE 0
            END AS new_group_flag
        FROM ordered
    )

    , grouped AS (
        -- Turn the flags into a cumulative group id per partition
        SELECT
            *,
            SUM(new_group_flag) OVER (
                PARTITION BY person_id, dic_index
                ORDER BY start_episode, end_episode
                ROWS UNBOUNDED PRECEDING
            ) AS grp
        FROM flagged
    )

    -- Collapse each group into one merged episode
    SELECT
        person_id,
        dic_index,
        MIN(start_episode) AS start_episode,
        MAX(end_episode) AS end_episode
    FROM grouped
    GROUP BY person_id, dic_index, grp
    ORDER BY person_id, dic_index, start_episode
-- );
;