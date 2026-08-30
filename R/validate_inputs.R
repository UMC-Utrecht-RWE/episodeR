# Argument validation helpers shared by create_univariate_episodes() and
# create_multivariate_episodes(). Factored out because both functions were
# independently duplicating the same batch-column boolean-parsing logic,
# and neither validated `con` or `batch_size` at all -- failures surfaced
# as DuckDB/DBI errors several calls deep instead of naming the bad
# argument. validate_required_columns() also closes a silent-failure gap:
# without it, a missing `concept_id` column made
# create_univariate_episodes() write nothing with no error at all.

##' @param con Object to check.
##' @noRd
validate_connection <- function(con) {
  if (is.null(con) || !inherits(con, "DBIConnection") || !DBI::dbIsValid(con)) {
    stop("con must be a live DBI connection.", call. = FALSE)
  }
  invisible(TRUE)
}

##' @param x Object to check.
##' @param arg_name Name to use for x in the error message.
##' @noRd
validate_data_frame <- function(x, arg_name) {
  if (!is.data.frame(x)) {
    stop(sprintf("%s must be a data frame.", arg_name), call. = FALSE)
  }
  invisible(TRUE)
}

##' @param batch_size Object to check.
##' @noRd
validate_batch_size <- function(batch_size) {
  if (
    !is.numeric(batch_size) ||
      length(batch_size) != 1 ||
      is.na(batch_size) ||
      batch_size != as.integer(batch_size) ||
      batch_size < 1
  ) {
    stop("batch_size must be a single positive whole number.", call. = FALSE)
  }
  invisible(TRUE)
}

##' @param study_variables Data frame to check.
##' @param required Character vector of column names that must be present.
##' @noRd
validate_required_columns <- function(study_variables, required) {
  missing_cols <- setdiff(required, names(study_variables))
  if (length(missing_cols) > 0) {
    stop(sprintf(
      "study_variables must include column(s): %s.",
      paste(missing_cols, collapse = ", ")
    ), call. = FALSE)
  }
  invisible(TRUE)
}

##' Validates that `batch_column` is present in `study_variables` and
##' Boolean-like, then resolves it to a logical vector (TRUE/FALSE,
##' "true"/"false", "1"/"0", "yes"/"no", case-insensitive; NA treated as
##' FALSE).
##'
##' @param study_variables Data frame expected to contain `batch_column`.
##' @param batch_column Name of the column to validate and resolve.
##' @return Logical vector, one value per row of study_variables.
##' @noRd
validate_batch_column <- function(study_variables, batch_column) {
  if (!(batch_column %in% names(study_variables))) {
    stop(sprintf(
      "study_variables must include a Boolean '%s' column to control batching per variable.",
      batch_column
    ), call. = FALSE)
  }

  batch_values <- study_variables[[batch_column]]
  if (is.logical(batch_values)) {
    use_batch <- batch_values
  } else {
    normalized <- tolower(trimws(as.character(batch_values)))
    use_batch <- normalized %in% c("true", "t", "1", "yes", "y")
    invalid_batch_values <- !(normalized %in%
      c("true", "t", "1", "yes", "y", "false", "f", "0", "no", "n", "", "na"))
    if (any(invalid_batch_values, na.rm = TRUE)) {
      stop(sprintf(
        "Column '%s' must contain only Boolean-like values (TRUE/FALSE, 1/0, yes/no).",
        batch_column
      ), call. = FALSE)
    }
  }
  use_batch[is.na(use_batch)] <- FALSE
  use_batch
}
