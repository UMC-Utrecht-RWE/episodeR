##' Apply Data Type Conversions to a Wide Episode Table
##'
##' Converts variable columns in a wide \code{data.table} of episodes to the
##' R types declared in \code{study_variables[[data_type_col]]}. Only columns
##' whose names match a \code{variable_id} entry in \code{study_variables} are
##' affected; all other columns (e.g. \code{person_id}, \code{start_episode},
##' \code{end_episode}) are left unchanged.
##'
##' Supported type tokens (case-insensitive):
##' \describe{
##'   \item{BOOL, BOOLEAN}{\code{as.logical}}
##'   \item{NUM, NUMERIC, DOUBLE, FLOAT}{\code{as.numeric}}
##'   \item{INT, INTEGER}{\code{as.integer}}
##'   \item{CHAR, CHARACTER, STRING}{\code{as.character}}
##'   \item{DATE}{\code{as.Date}}
##' }
##'
##' @param dt A \code{data.table} in wide format with one column per variable.
##' @param study_variables Data frame containing at minimum a \code{variable_id}
##'   column and the column named by \code{data_type_col}.
##' @param data_type_col Name of the column in \code{study_variables} that
##'   holds the target data type token for each variable. Defaults to
##'   \code{"data_type"}.
##'
##' @return The input \code{dt}, modified in-place (and returned invisibly for
##'   use in pipelines).
##'
##' @import data.table
##' @export
apply_data_types <- function(dt, study_variables, data_type_col = "data_type") {
  type_map <- list(
    BOOL      = as.logical,
    BOOLEAN   = as.logical,
    NUM       = as.numeric,
    NUMERIC   = as.numeric,
    DOUBLE    = as.numeric,
    FLOAT     = as.numeric,
    INT       = as.integer,
    INTEGER   = as.integer,
    CHAR      = as.character,
    CHARACTER = as.character,
    STRING    = as.character,
    DATE      = as.Date
  )
  sv <- as.data.frame(study_variables)
  for (var_id in intersect(sv$variable_id, names(dt))) {
    dtype <- toupper(trimws(sv[[data_type_col]][sv$variable_id == var_id]))
    if (length(dtype) == 1L && !is.na(dtype) && dtype %in% names(type_map)) {
      data.table::set(dt, j = var_id, value = type_map[[dtype]](dt[[var_id]]))
    }
  }
  dt
}
