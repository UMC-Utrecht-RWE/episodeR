-- Step 1: Generate most-recent-record-resolved, trimmed, chain-merged spells (replicates v2 firststep_v2.sql)
-- Input: D3_CONCEPTS (view on concepts_db), study_variables, all_persons
-- Output: spells_raw (person_id, variable_id, value, spell_start, spell_end)
--
-- Pipeline inside this step (mirrors v2 firststep CTEs):
--   concept_dedup  → deduplicate per (person, concept, date)
--   MCE_SV         → initial spells: date + end_look_back / date + start_look_back
--   ranked_dates   → sort by spell_start per (person, variable)
--   intervals      → LEFT JOIN next row to get new_variable_end_spell
--   adjusted       → most-recent-record resolution: crop spell_end to next_start - 1
--   trimmed        → clamp to [start_study_date, end_study_date] + filter degenerate
--   spells_raw     → chain-merge same-value adjacent/overlapping trimmed intervals

CREATE OR REPLACE TABLE concept_dedup AS
SELECT
  c.person_id,
  c.concept_id,
  c.date,
  CASE
    WHEN COUNT(DISTINCT c.value) > 1 THEN 'unknown'
    ELSE MAX(c.value)
  END AS value
FROM D3_CONCEPTS c
INNER JOIN all_persons p ON c.person_id = p.person_id
GROUP BY c.person_id, c.concept_id, c.date;

CREATE OR REPLACE TABLE trimmed_spells AS
WITH
MCE_SV AS (
  SELECT DISTINCT
    c.person_id,
    sv.variable_id,
    c.value,
    c.date + CAST(sv.end_look_back   AS INTEGER) AS spell_start,
    c.date + CAST(sv.start_look_back AS INTEGER) AS spell_end
  FROM concept_dedup c
  JOIN study_variables sv ON c.concept_id = sv.concept_id
  WHERE c.concept_id IN ({concept_id_list})
),
ranked_dates AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY spell_start) AS rn
  FROM MCE_SV
),
intervals AS (
  SELECT
    rd1.person_id,
    rd1.variable_id,
    rd1.value,
    rd1.spell_start,
    rd1.spell_end,
    rd2.spell_start AS new_spell_end
  FROM ranked_dates rd1
  LEFT JOIN ranked_dates rd2
    ON rd1.person_id = rd2.person_id
    AND rd1.variable_id = rd2.variable_id
    AND rd1.rn + 1 = rd2.rn
),
-- Most-recent-record resolution: crop spell_end to next record's start - 1
adjusted AS (
  SELECT
    person_id,
    variable_id,
    value,
    spell_start,
    CASE
      WHEN new_spell_end IS NOT NULL
        AND new_spell_end <= spell_end
        AND NOT spell_start = spell_end
      THEN new_spell_end - 1
      ELSE spell_end
    END AS spell_end
  FROM intervals
),
-- Clamp to [start_study_date, end_study_date]; filter out spells that don't overlap study period
trimmed AS (
  SELECT
    a.person_id,
    a.variable_id,
    a.value,
    CASE WHEN a.spell_start < DATE({start_study_date}) THEN DATE({start_study_date}) ELSE a.spell_start END AS spell_start,
    CASE WHEN a.spell_end   > DATE({end_study_date})   THEN DATE({end_study_date})   ELSE a.spell_end   END AS spell_end
  FROM adjusted a
  WHERE
    (a.spell_start BETWEEN {start_study_date} AND {end_study_date})
    OR (a.spell_end   BETWEEN {start_study_date} AND {end_study_date})
    OR (a.spell_start < {start_study_date} AND a.spell_end > {end_study_date})
)
SELECT person_id, variable_id, value, spell_start, spell_end FROM trimmed;

-- Materialise trimmed before chain-merge to avoid DuckDB CTE re-evaluation inconsistencies
-- when trimmed is referenced multiple times in correlated subqueries.
CREATE OR REPLACE TABLE spells_raw AS
SELECT
  s1.person_id,
  s1.variable_id,
  s1.value,
  s1.spell_start,
  MIN(t1.spell_end) AS spell_end
FROM trimmed_spells s1
INNER JOIN trimmed_spells t1
  ON  s1.spell_start   <= t1.spell_end
  AND s1.person_id      = t1.person_id
  AND s1.variable_id    = t1.variable_id
  AND (s1.value = t1.value OR (s1.value IS NULL AND t1.value IS NULL))
  AND NOT EXISTS (
    SELECT 1 FROM trimmed_spells t2
    WHERE t1.spell_end   >= t2.spell_start - 1
      AND t1.spell_end   <  t2.spell_end
      AND t1.person_id    = t2.person_id
      AND t1.variable_id  = t2.variable_id
      AND (t1.value = t2.value OR (t1.value IS NULL AND t2.value IS NULL))
  )
WHERE NOT EXISTS (
  SELECT 1 FROM trimmed_spells s2
  WHERE s1.spell_start  > s2.spell_start
    AND s1.spell_start <= s2.spell_end + 1
    AND s1.person_id    = s2.person_id
    AND s1.variable_id  = s2.variable_id
    AND (s1.value = s2.value OR (s1.value IS NULL AND s2.value IS NULL))
)
GROUP BY s1.person_id, s1.variable_id, s1.value, s1.spell_start;
