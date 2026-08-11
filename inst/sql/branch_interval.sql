-- =====================================================================
-- BRANCH B: Interval boundary events, no daily explosion
-- =====================================================================
-- Same dense bit-position mapping as Branch A.
CREATE TABLE var_rank AS (
    SELECT
        int_var_id,
        ROW_NUMBER() OVER (ORDER BY int_var_id) - 1 AS bit_pos
    FROM (SELECT DISTINCT int_var_id FROM new_variables_ids)
);

-- IMPORTANT: pre-merge overlapping intervals of the SAME variable
-- before generating boundary events. Without this, two overlapping
-- intervals of the same int_var_id each contribute a separate +delta
-- "turn on" event, and the running SUM double-counts that variable's
-- bit during the overlap (it briefly adds 2x the bit value instead of
-- behaving like a set union), corrupting the combination for the
-- overlap period and leaving residual incorrect state after only one
-- of the two "turn off" events fires. This is the interval-merge
-- pattern (running-max + cumulative-group), applied per
-- (person_id, int_var_id) instead of per (person_id, dic_index).
CREATE TABLE intervals_merged AS (
    WITH ordered AS (
        SELECT
            person_id,
            int_var_id,
            start_episode,
            end_episode,
            MAX(end_episode) OVER (
                PARTITION BY person_id, int_var_id
                ORDER BY start_episode, end_episode
                ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
            ) AS running_max_end
        FROM new_variables_ids
    )
    , flagged AS (
        SELECT *,
            CASE
                WHEN running_max_end IS NULL THEN 1
                WHEN start_episode > running_max_end + 1 THEN 1
                ELSE 0
            END AS new_group_flag
        FROM ordered
    )
    , grouped AS (
        SELECT *,
            SUM(new_group_flag) OVER (
                PARTITION BY person_id, int_var_id
                ORDER BY start_episode, end_episode
                ROWS UNBOUNDED PRECEDING
            ) AS grp
        FROM flagged
    )
    SELECT
        person_id, int_var_id,
        MIN(start_episode) AS start_episode,
        MAX(end_episode) AS end_episode
    FROM grouped
    GROUP BY person_id, int_var_id, grp
);

-- Every interval contributes two boundary events: a "turn on" at
-- start_episode, and a "turn off" the day after end_episode. Row count
-- scales with 2x the number of (now de-overlapped) intervals, not with
-- calendar span. Built from intervals_merged, not the raw table, so
-- each variable's bit is guaranteed to turn on/off at most once per
-- contiguous active stretch.
CREATE TABLE EVENTS AS (
    SELECT
        V.person_id,
        V.start_episode AS event_date,
        CAST(1 << R.bit_pos AS BIGINT) AS delta
    FROM intervals_merged V
    JOIN var_rank R USING (int_var_id)

    UNION ALL

    SELECT
        V.person_id,
        (V.end_episode + 1) AS event_date,
        CAST(-(1 << R.bit_pos) AS BIGINT) AS delta
    FROM intervals_merged V
    JOIN var_rank R USING (int_var_id)
);

-- Collapse same-day events per person (a day can have multiple
-- variables turning on/off simultaneously), then running-sum the
-- deltas to get the active-variable bitmask as of each boundary date.
-- Each row's combination is valid from `dates` until the day before
-- the NEXT boundary date for that person -- so we compute that span
-- explicitly here rather than leaving it implicit, since step 2
-- expects a (person_id, dates, combination) grain that represents
-- a single calendar day, and a boundary-event row represents a SPAN.
CREATE TABLE COMBINATIONS_RAW AS (
    WITH daily_delta AS (
        SELECT person_id, event_date, SUM(delta) AS delta
        FROM EVENTS
        GROUP BY ALL
    )
    SELECT
        person_id,
        event_date AS dates,
        SUM(delta) OVER (
            PARTITION BY person_id ORDER BY event_date
            ROWS UNBOUNDED PRECEDING
        ) AS combination
    FROM daily_delta
);

-- Expand each boundary row into an explicit (start, end) span by
-- looking ahead to the next boundary date for that person. This is
-- the bridge that makes Branch B's output structurally compatible
-- with what step 2 expects: one row per constant-combination span,
-- not one row per boundary point.
--
-- The LAST segment for each person has no "next boundary" to bound
-- it -- its true end is open-ended in principle (the person's last
-- known active variable just continues with no recorded end event
-- past max(end_episode)+1, which itself becomes a "turn off" event,
-- so in practice the last COMBINATIONS_RAW row per person is always
-- combination = 0, the close-out row -- and it gets filtered in step2
-- anyway. Any row with combination != 0 always has a LEAD() value,
-- because the corresponding "turn off" event guarantees a later
-- boundary date exists. So this COALESCE is a defensive fallback only
-- and should never actually be hit for an active (non-zero) span.
CREATE TABLE COMBINATIONS AS (
    SELECT
        person_id,
        dates,
        combination,
        COALESCE(
            LEAD(dates) OVER (PARTITION BY person_id ORDER BY dates) - 1,
            dates
        ) AS end_date
    FROM COMBINATIONS_RAW
);
