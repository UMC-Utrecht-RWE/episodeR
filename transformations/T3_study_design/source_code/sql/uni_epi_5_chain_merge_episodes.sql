-- Step 5: Chain-merge same-value overlapping/adjacent intervals (v2 fifthstep)
-- For each (person, variable), collapses contiguous runs of identical value
-- into a single spell, producing the minimal set of maximal same-value intervals.
-- Input: spells_complete
-- Output: D3_UNIVARIATE_EPISODES

CREATE OR REPLACE TABLE D3_UNIVARIATE_EPISODES AS
SELECT
  s1.person_id,
  s1.variable_id,
  s1.value,
  s1.spell_start,
  MIN(t1.spell_end) AS spell_end
FROM spells_complete s1
INNER JOIN spells_complete t1
  ON  s1.spell_start <= t1.spell_end
  AND s1.person_id   = t1.person_id
  AND s1.variable_id = t1.variable_id
  AND (s1.value = t1.value OR (s1.value IS NULL AND t1.value IS NULL))
  AND NOT EXISTS (
    -- t1 is not the true end of the chain: there is a t2 that extends further
    SELECT 1 FROM spells_complete t2
    WHERE t1.spell_end   >= t2.spell_start - 1
      AND t1.spell_end   <  t2.spell_end
      AND t1.person_id   = t2.person_id
      AND t1.variable_id = t2.variable_id
      AND (t1.value = t2.value OR (t1.value IS NULL AND t2.value IS NULL))
  )
WHERE NOT EXISTS (
  -- s1 is not a chain start: there is an earlier s2 with the same value that overlaps/touches s1
  SELECT 1 FROM spells_complete s2
  WHERE s1.spell_start  > s2.spell_start
    AND s1.spell_start <= s2.spell_end + 1
    AND s1.person_id    = s2.person_id
    AND s1.variable_id  = s2.variable_id
    AND (s1.value = s2.value OR (s1.value IS NULL AND s2.value IS NULL))
)
GROUP BY s1.person_id, s1.variable_id, s1.value, s1.spell_start
ORDER BY s1.person_id, s1.variable_id, s1.spell_start;
