-- Step 2: Fill gaps between/before/after known spells with missing_set_to value (v2 secondstep)
-- For each (person, variable):
--   (a) between consecutive non-adjacent spells: [prev_end+1, next_start-1]
--   (b) before the first spell: [start_study_date, first_start-1]
--   (c) after the last spell: [last_end+1, end_study_date]
-- Gap fills use the variable-specific missing_set_to from study_variables.
-- Input: spells_raw, study_variables
-- Output: spells_with_gaps

CREATE OR REPLACE TABLE spells_with_gaps AS
WITH ranked AS (
  SELECT *,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY spell_start, spell_end)      AS rank_asc,
    ROW_NUMBER() OVER (PARTITION BY person_id, variable_id ORDER BY spell_start DESC, spell_end DESC) AS rank_desc,
    LAG(spell_end) OVER (PARTITION BY person_id, variable_id ORDER BY spell_start, spell_end)    AS prev_end
  FROM spells_raw
),
-- (a) Gaps between consecutive non-overlapping non-adjacent spells
--     gap = [prev_spell_end + 1, current_spell_start - 1]
empty_spaces AS (
  SELECT person_id, variable_id,
    prev_end + 1        AS spell_start,
    spell_start - 1     AS spell_end
  FROM ranked
  WHERE prev_end IS NOT NULL
    AND spell_start > prev_end + 1
),
-- (b) Before first spell: [start_study_date, first_spell_start - 1]
--     Only when first spell doesn't already start at study start
before_first AS (
  SELECT person_id, variable_id,
    DATE({start_study_date}) AS spell_start,
    spell_start - 1           AS spell_end
  FROM ranked
  WHERE rank_asc = 1
    AND spell_start > DATE({start_study_date})
),
-- (c) After last spell: [last_spell_end + 1, end_study_date]
after_last AS (
  SELECT person_id, variable_id,
    spell_end + 1              AS spell_start,
    DATE({end_study_date})     AS spell_end
  FROM ranked
  WHERE rank_desc = 1
)
SELECT person_id, variable_id, value, spell_start, spell_end FROM spells_raw
UNION ALL
SELECT es.person_id, es.variable_id, sv.missing_set_to AS value, es.spell_start, es.spell_end
FROM empty_spaces es JOIN study_variables sv ON sv.variable_id = es.variable_id
WHERE es.spell_start <= es.spell_end
UNION ALL
SELECT bf.person_id, bf.variable_id, sv.missing_set_to AS value, bf.spell_start, bf.spell_end
FROM before_first bf JOIN study_variables sv ON sv.variable_id = bf.variable_id
WHERE bf.spell_start <= bf.spell_end
UNION ALL
SELECT al.person_id, al.variable_id, sv.missing_set_to AS value, al.spell_start, al.spell_end
FROM after_last al JOIN study_variables sv ON sv.variable_id = al.variable_id
WHERE al.spell_start <= al.spell_end;
