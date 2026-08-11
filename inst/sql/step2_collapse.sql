-- =====================================================================
-- STEP 2: Collapse COMBINATIONS into multivariate_episode
--
-- Works identically regardless of which branch produced COMBINATIONS,
-- because both branches emit the same shape:
--   (person_id, dates AS span_start, end_date AS span_end, combination)
-- Branch A emits 1-day spans (dates = end_date for every row).
-- Branch B emits multi-day spans (end_date computed via LEAD).
-- =====================================================================
CREATE OR REPLACE TABLE multivariate_episode AS (
    -- combination = 0 means no variable active during that span -- a
    -- real gap, not an episode -- so it is dropped before grouping.
    WITH active_only AS (
        SELECT * FROM COMBINATIONS WHERE combination != 0
    )
    -- A new episode starts on either a combination change OR a
    -- calendar gap. Gap detection compares this span's start to the
    -- PREVIOUS span's END (not start), which is what correctly
    -- generalizes to multi-day spans -- comparing starts only would
    -- wrongly treat any multi-day span as containing an internal gap.
    , changes AS (
        SELECT
            person_id,
            dates AS span_start,
            end_date AS span_end,
            combination,
            CASE
                WHEN LAG(combination) OVER (PARTITION BY person_id ORDER BY dates)
                        IS DISTINCT FROM combination
                  OR LAG(end_date) OVER (PARTITION BY person_id ORDER BY dates) IS NULL
                  OR dates - LAG(end_date) OVER (PARTITION BY person_id ORDER BY dates) > 1
                THEN 1
                ELSE 0
            END AS value_changed
        FROM active_only
    )
    , grouped AS (
        SELECT *,
            SUM(value_changed) OVER (
                PARTITION BY person_id ORDER BY span_start ROWS UNBOUNDED PRECEDING
            ) AS grp
        FROM changes
    )
    SELECT
        person_id,
        combination,
        MIN(span_start) AS start_episode,
        MAX(span_end) AS end_episode
    FROM grouped
    GROUP BY person_id, combination, grp
    ORDER BY person_id, start_episode
);

-- Sanity check: detects whether any SINGLE row in multivariate_episode
-- spans a gap that isn't actually covered by source data -- i.e. true
-- false-merge cases, where two genuinely separate periods got fused
-- into one (start_episode, end_episode) row.
--
-- This is checked by re-joining each episode against the underlying
-- COMBINATIONS spans and confirming every calendar day in
-- [start_episode, end_episode] is actually covered by a span with a
-- matching combination -- a real gap inside the claimed episode means
-- some day in that range has no covering span at all.
--
-- NOTE: adjacent separate episodes with the SAME combination and a
-- gap between them (e.g. the classic gap-bug fixture: combination "1"
-- on Jan3, then again on Jan6-8) are NOT a violation -- step 2 is
-- expected to keep those as two separate rows, and this check
-- correctly leaves them alone, since each row individually has no
-- internal gap.
CREATE OR REPLACE TABLE multivariate_episode_gap_check AS (
    SELECT E.*
    FROM multivariate_episode E
    WHERE EXISTS (
        SELECT 1
        FROM (
            SELECT UNNEST(GENERATE_SERIES(
                E.start_episode, E.end_episode, INTERVAL '1 day'
            ))::DATE AS d
        ) days
        WHERE NOT EXISTS (
            SELECT 1 FROM COMBINATIONS C
            WHERE C.person_id = E.person_id
              AND C.combination = E.combination
              AND days.d BETWEEN C.dates AND C.end_date
        )
    )
);
