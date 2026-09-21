##' Write One Batch of multivariate_episode_wide to Parquet
##'
##' Copies the current \code{multivariate_episode_wide} table out to a
##' single batch parquet file, casting \code{start_episode}/\code{end_episode}
##' to DATE.
##'
##' @param con DBI connection with \code{multivariate_episode_wide} already
##' built.
##' @param output_path Directory to write the batch file into.
##' @param i_batch Batch index, used to name the output file
##' (\code{batch_00001.parquet}, ...).
write_batch_output <- function(con, output_path, i_batch) {
  batch_file <- file.path(output_path, sprintf("batch_%05d.parquet", i_batch))
  DBI::dbExecute(
    con,
    sprintf(
      "COPY (
          SELECT
            person_id,
            TRY_CAST(start_episode AS DATE) AS start_episode,
            TRY_CAST(end_episode AS DATE) AS end_episode,
            * EXCLUDE (person_id, start_episode, end_episode)
         FROM multivariate_episode_wide
         )
       TO '%s' (FORMAT 'parquet')",
      batch_file
    )
  )
}
