# episodeR 0.0.4

- Fixed bug in `multivariate_episodes_pipeline()` where batched runs always wrote to `<dirname(output_path)>/D3_MULTIVARIATE_EPISODES`, ignoring `output_path`. For batched runs `output_path` is now the output directory; a warning is logged when this differs from the previous location.
- Fixed bug in `multivariate_episodes_pipeline()` where batched runs never created the `D3_MULTIVARIATE_EPISODES` table in DuckDB. Batched runs now build it from the batch parquet files (`union_by_name`), matching unbatched runs. The table is also dropped at the start of each run, so repeated unbatched runs on the same connection no longer append onto earlier results.
- Batched runs of `multivariate_episodes_pipeline()` now give every batch the same variable columns as an unbatched run. Variables without episodes in a batch are filled the same way `dcast(fill = FALSE)` fills them in an unbatched run, instead of being absent from that batch's parquet file (and NULL in the combined table).
- `multivariate_episodes_pipeline()` now writes its output to `<output_path>.staging` and replaces the existing output at `output_path` only after the run succeeds. A run that fails (e.g. a missing batch column) no longer deletes the previous result.

# episodeR 0.0.3

- This version is to be used to provide a stable release where the package versions in DESCRIPTION and release will match
- Adjusted the test to match the developments happened in v0.0.2.1
- Updates to renv for using updated package versions
- Other projects that were loading episodeR@v0.0.2.1 should update to v0.0.3

# episodeR 0.0.2.1

- Fixed bug with hardcoded output directory in multivariate episodes.
- However, this is a buggy a release as the package version in the DESCRIPTION file was not changed.

# episodeR 0.0.2

* Enhance runtime in multivariate episodes.

# episodeR 0.0.1

* Initial version for release.
