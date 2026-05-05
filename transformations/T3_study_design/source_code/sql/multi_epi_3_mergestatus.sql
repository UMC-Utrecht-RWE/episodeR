-- Merge intervals that are concatenated
SELECT 
    s1.person_id,
    s1.dic_index,
    s1.matching_status_start,
    MIN(t1.matching_status_end) AS matching_status_end
FROM main.matching_status_coded s1
INNER JOIN main.matching_status_coded t1 ON (
    s1.matching_status_start <= t1.matching_status_end AND
    NOT EXISTS (
        SELECT 1 
        FROM main.matching_status_coded t2 
        WHERE t1.matching_status_end >= t2.matching_status_start - 1 
          AND t1.matching_status_end < t2.matching_status_end 
          AND t1.person_id = t2.person_id 
          AND t1.dic_index = t2.dic_index
    ) AND
    s1.person_id = t1.person_id AND
    s1.dic_index = t1.dic_index
)
WHERE NOT EXISTS (
    SELECT 1 
    FROM main.matching_status_coded s2 
    WHERE s1.matching_status_start > s2.matching_status_start 
      AND s1.matching_status_start <= s2.matching_status_end + 1 
      AND s1.person_id = s2.person_id AND
      s1.dic_index = s2.dic_index
)
GROUP BY 
    s1.person_id,
    s1.dic_index,
    s1.matching_status_start
ORDER BY 
    s1.person_id,
    s1.dic_index,
    s1.matching_status_start
