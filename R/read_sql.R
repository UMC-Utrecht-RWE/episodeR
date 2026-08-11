#' Read a SQL File Bundled with the Package
#'
#' Reads a SQL script from the package's installed `inst/sql` directory as a
#' single string, ready to be executed directly (optionally after templating
#' with glue::glue()).
#'
#' @param filename Name of the SQL file inside `inst/sql`, e.g.
#' "multi_initial.sql".
#'
#' @return A single string with the file's contents.
read_sql <- function(filename) {
  paste(
    readLines(system.file("sql", filename, package = "episodeR"), warn = FALSE),
    collapse = "\n"
  )
}
