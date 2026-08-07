# Shared helper for the black-box pipeline tests (test-*-pipeline-blackbox.R).
#
# reference_combine() is a small, deliberately naive, pure-R (no SQL)
# reference implementation of "combine per-variable episodes into
# multivariate combination episodes": it intersects interval boundaries
# across all variables for a person, then chain-merges consecutive
# identical combinations. It's simple enough to trust by inspection, and
# it's independent of multi_epi_*.sql, so it can serve as an oracle for
# black-box tests that must survive a rewrite of that SQL.
#
# Validated against the hand-verified 21-row fixture in
# test-multivariate-pipeline-blackbox.R (reproduces it exactly) before
# being trusted for new scenarios.

#' @param uni_episodes data.table/data.frame(person_id, variable_id, value,
#'   start_episode, end_episode). For each (person_id, variable_id), the
#'   intervals must be non-overlapping and gap-free across a shared
#'   [min, max] date range (as univariate_episodes_pipeline() guarantees).
#' @return data.table(person_id, start_episode, end_episode, <one column
#'   per variable_id>), chain-merged, sorted by person_id/start_episode.
reference_combine <- function(uni_episodes) {
  uni_episodes <- data.table::as.data.table(uni_episodes)
  variables <- sort(unique(uni_episodes$variable_id))
  persons <- unique(uni_episodes$person_id)
  out <- vector("list", length(persons))

  for (i in seq_along(persons)) {
    p <- persons[i]
    sub <- uni_episodes[person_id == p]
    breakpoints <- sort(unique(c(sub$start_episode, sub$end_episode + 1)))
    if (length(breakpoints) < 2) next
    seg_start <- breakpoints[-length(breakpoints)]
    seg_end <- breakpoints[-1] - 1
    seg <- data.table::data.table(start_episode = seg_start, end_episode = seg_end)

    for (v in variables) {
      vsub <- sub[variable_id == v]
      seg[[v]] <- vapply(seg_start, function(s) {
        m <- vsub[start_episode <= s & end_episode >= s]
        if (nrow(m) == 0) NA_character_ else as.character(m$value[1])
      }, character(1))
    }

    data.table::setorder(seg, start_episode)
    n <- nrow(seg)
    changed <- rep(TRUE, n)
    if (n > 1) {
      for (j in 2:n) {
        same <- all(vapply(variables, function(v) {
          a <- seg[[v]][j]
          b <- seg[[v]][j - 1]
          identical(a, b) || (is.na(a) && is.na(b))
        }, logical(1)))
        changed[j] <- !same
      }
    }
    grp <- cumsum(changed)
    merged <- seg[,
      c(
        list(start_episode = min(start_episode), end_episode = max(end_episode)),
        lapply(.SD, `[`, 1)
      ),
      by = grp,
      .SDcols = variables
    ]
    merged[, grp := NULL]
    merged[, person_id := p]
    out[[i]] <- merged
  }
  result <- data.table::rbindlist(out, use.names = TRUE)
  data.table::setcolorder(result, c("person_id", "start_episode", "end_episode", variables))
  data.table::setorder(result, person_id, start_episode)
  result[]
}
