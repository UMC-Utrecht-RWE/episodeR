-- CREATE TABLE multivariate_episode_merged AS (

    WITH ordered AS (
        -- Cast to DATE up front so all downstream arithmetic and output
        -- is clean DATE, regardless of the source column's TIMESTAMP type
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
        -- running_max_end is now DATE, so integer-day arithmetic (+1)
        -- works directly without DuckDB's DATE -> TIMESTAMP promotion
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