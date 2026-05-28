# episodeR

**episodeR** encodes time-varying study variables from a clinical concepts table into discrete, non-overlapping *episode* intervals, ready for downstream survival analysis, pharmacoepidemiology, or real-world evidence studies.

It is built on [DuckDB](https://duckdb.org/) (via DBI) and [data.table](https://rdatatable.gitlab.io/data.table/) and is designed to handle large populations through optional person-level batching.

---

## Concepts

| Term                     | Meaning                                                                                       |
| ------------------------ | --------------------------------------------------------------------------------------------- |
| **Spell**                | A raw recorded period for one person × variable (from `D3_CONCEPTS`)                          |
| **Univariate episode**   | Spells for a single variable collapsed into non-overlapping intervals with a consistent value |
| **Multivariate episode** | A combined interval where the status of *all* variables is constant simultaneously            |

---

## Pipeline overview

```
D3_CONCEPTS (parquet/hive)
        │
        ▼
univariate_episodes_pipeline()   →  D3_UNIVARIATE_EPISODES (hive-partitioned parquet)
        │
        ▼
multivariate_episodes_pipeline() →  D3_MULTIVARIATE_EPISODES (parquet)
```

### Univariate pipeline (5 SQL steps)

1. Generate initial spells with most-recent-record resolution
2. Fill gaps between spells using `missing_set_to` per variable
3. Add rows for persons with no recorded spells
4. Trim all intervals to `[start_study_date, end_study_date]`
5. Chain-merge adjacent intervals with identical values

### Multivariate pipeline (3 SQL steps)

1. Explode univariate episodes to one row per person × variable × day
2. Combine daily rows into multivariate status intervals
3. Merge adjacent intervals with identical combined status

---

## Installation

```r
# Install from GitHub
devtools::install_github("UMC-Utrecht-RWE/episodeR")
```

Requires `duckdb`, `DBI`, `data.table`, `logger`, and the internal `picard` helper package.

---

## Usage

```r
library(episodeR)

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = ":memory:")

# 1. Build univariate episodes
univariate_episodes_pipeline(
  study_variables            = sv_meta,   # data frame — see format below
  con                        = con,
  concepts_table             = "D3_CONCEPTS",
  sql_dir                    = system.file("sql", package = "episodeR"),
  start_study_date           = "2020-01-01",
  end_date_missing_inclusion = "2023-12-31",
  output_hive_path           = "output/D3_UNIVARIATE_EPISODES",
  batch_column               = "batch",
  missing_col                = "missing_set_to"
)

# 2. Build multivariate episodes
multivariate_episodes_pipeline(
  study_variables             = sv_meta,
  con                         = con,
  d3_univariate_episodes_path = "output/D3_UNIVARIATE_EPISODES",
  sql_dir                     = system.file("sql", package = "episodeR"),
  output_path                 = "output/D3_MULTIVARIATE_EPISODES.parquet",
  batch_column                = "batch",
  data_type_col               = "data_type"  # converts columns to declared R types
)

DBI::dbDisconnect(con, shutdown = TRUE)
```

### `study_variables` format

| Column                              | Description                                                 |
| ----------------------------------- | ----------------------------------------------------------- |
| `variable_id`                       | Unique variable name (becomes a column in the wide output)  |
| `concept_id`                        | Source concept identifier in `D3_CONCEPTS`                  |
| `start_look_back` / `end_look_back` | Days to extend spell start/end                              |
| `missing_set_to`                    | Value to fill for persons/periods with no recorded spell    |
| `batch`                             | `TRUE`/`FALSE` — process this variable in person-id batches |
| `data_type`                         | Target R type: `BOOL`, `NUM`, `INT`, `CHAR`, `DATE`         |

---

## Functions

| Function                           | Description                               |
| ---------------------------------- | ----------------------------------------- |
| `univariate_episodes_pipeline()`   | Runs the 5-step univariate SQL pipeline   |
| `multivariate_episodes_pipeline()` | Runs the 3-step multivariate SQL pipeline |

---

## License

GPL (>= 3)
