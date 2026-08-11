-- =====================================================================
-- BRANCH A: Daily explosion + bitmask combination
-- =====================================================================
-- Map int_var_id onto dense bit positions (0, 1, 2, ...). Combination
-- is stored as DuckDB's native BIT (bitstring) type rather than an
-- integer shift -- this removes any ceiling on distinct variable
-- count. (An earlier integer-shift version (1 << bit_pos) overflowed
-- past 63 distinct variables with "Left-shift value N is out of
-- range" -- BIT has no such limit.)
CREATE OR REPLACE TABLE var_rank AS (
    SELECT
        int_var_id,
        ROW_NUMBER() OVER (ORDER BY int_var_id) - 1 AS bit_pos
    FROM (SELECT DISTINCT int_var_id FROM new_variables_ids)
);

-- Explode each interval into one row per covered calendar day.
-- DISTINCT absorbs duplicate dates from overlapping episodes of the
-- same (person_id, int_var_id) -- safe because data is partitioned by
-- person_id, so a person's complete interval set is present here.
CREATE OR REPLACE TABLE EXPLODED AS (
    SELECT DISTINCT
        V.person_id,
        R.bit_pos,
        UNNEST(GENERATE_SERIES(
            V.start_episode,
            V.end_episode,
            INTERVAL '1 day'
        ))::DATE AS dates
    FROM new_variables_ids V
    JOIN var_rank R USING (int_var_id)
);

-- Collapse to one row per person/day. Combination is a BIT_OR of each
-- active variable's bit, set within a bitstring sized to the total
-- number of distinct variables -- order-independent by construction,
-- so the "3;7" vs "7;3" STRING_AGG fragmentation bug class does not
-- exist in this representation, and there's no width ceiling.
--
-- end_date = dates (each row is a 1-day span) so the output shape
-- matches Branch B's (person_id, dates, end_date, combination) span
-- representation, keeping step 2 identical for both branches.
CREATE OR REPLACE TABLE COMBINATIONS AS (
    SELECT
        person_id,
        dates,
        dates AS end_date,
        BIT_OR(
            SET_BIT(
                REPEAT('0', (SELECT COUNT(*) FROM var_rank))::BIT,
                bit_pos::INTEGER,
                1
            )
        ) AS combination
    FROM EXPLODED
    GROUP BY ALL
);