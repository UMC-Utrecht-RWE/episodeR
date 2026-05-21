-- Merge intervals that are concatenated
SELECT 
    s1.person_id,
    s1.dic_index,
    s1.start_episode,
    MIN(t1.end_episode) AS end_episode
FROM main.multivariate_episode_coded s1
INNER JOIN main.multivariate_episode_coded t1 ON (
    s1.start_episode <= t1.end_episode AND
    NOT EXISTS (
        SELECT 1 
        FROM main.multivariate_episode_coded t2 
        WHERE t1.end_episode >= t2.start_episode - INTERVAL '1 day'
          AND t1.end_episode < t2.end_episode 
          AND t1.person_id = t2.person_id 
          AND t1.dic_index = t2.dic_index
    ) AND
    s1.person_id = t1.person_id AND
    s1.dic_index = t1.dic_index
)
WHERE NOT EXISTS (
    SELECT 1 
    FROM main.multivariate_episode_coded s2 
    WHERE s1.start_episode > s2.start_episode 
      AND s1.start_episode <= s2.end_episode + INTERVAL '1 day'
      AND s1.person_id = s2.person_id AND
      s1.dic_index = s2.dic_index
)
GROUP BY 
    s1.person_id,
    s1.dic_index,
    s1.start_episode
ORDER BY 
    s1.person_id,
    s1.dic_index,
    s1.start_episode
