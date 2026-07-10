# Copilot instructions for `climate`

`climate` is a CRAN R package for downloading in-situ meteorological and hydrological data from OGIMET, IMGW-PIB, NOAA, and University of Wyoming sources. The package targets R >= 4.1.0 and uses roxygen2 with markdown enabled.

## Build, test, and lint commands

Run commands from the package root.

- Load the package for interactive work: `R -q -e 'devtools::load_all()'`
- Regenerate `man/` and `NAMESPACE` after roxygen changes: `R -q -e 'devtools::document()'`
- Run the full test suite: `R -q -e 'devtools::test()'`
- Run a single test file: `R -q -e 'testthat::test_file("tests/testthat/test-meteo_imgw.R")'`
- Run package linting: `R -q -e 'lintr::lint_package()'`
- Run a local package check: `R -q -e 'devtools::check()'`
- Run the CI-style check locally when needed: `R -q -e 'rcmdcheck::rcmdcheck(args = c("--no-manual", "--as-cran", "--run-donttest"), error_on = "warning", check_dir = "check")'`
- Run coverage: `R -q -e 'covr::package_coverage()'`

## High-level architecture

- Public download functions are thin wrappers that dispatch by `interval` to interval-specific implementations. Keep wrapper signatures and the underlying `*_hourly()`, `*_daily()`, and `*_monthly()` functions in sync. Examples:
  - `meteo_imgw()` -> `meteo_imgw_hourly()`, `meteo_imgw_daily()`, `meteo_imgw_monthly()`
  - `hydro_imgw()` -> `hydro_imgw_daily()`, `hydro_imgw_monthly()`
  - `meteo_ogimet()` -> `ogimet_hourly()`, `ogimet_daily()`

- The package has separate ingestion paths for each upstream source family:
  - **IMGW archive downloads**: archive ZIP files are downloaded from `danepubliczne.imgw.pl`, unpacked, read through `imgw_read()`, then normalized and optionally joined with built-in station metadata.
  - **IMGW datastore / telemetry downloads**: `meteo_imgw_datastore()` and `hydro_imgw_datastore()` fetch large monthly telemetry archives from the datastore endpoint. These are raw, high-volume datasets and are handled separately from the archive-style IMGW functions.
  - **OGIMET**: HTML is scraped with `XML::readHTMLTable`; station identity is based on WMO IDs. Hourly precipitation post-processing is handled by `precip_split()`.
  - **NOAA / Wyoming**: direct file or page downloads for ISH hourly data, Mauna Loa CO2, and Wyoming soundings.

- IMGW column renaming is a distinct normalization layer. Most IMGW functions accept `col_names = "short" | "full" | "polish"` and pass results through `meteo_shortening_imgw()` or `hydro_shortening_imgw()`. The mapping tables live in built-in datasets backed by `data-raw/`.

- Package data and docs follow standard R package patterns:
  - exported code in `R/`
  - tests in `tests/testthat/`
  - built-in datasets in `data/`, generated from `data-raw/`
  - roxygen-generated docs in `man/`

## Key conventions

- Do not hand-edit `man/` or `NAMESPACE`; update roxygen comments and run `devtools::document()`.

- Do not hand-edit `data/*.rda`; regenerate datasets from the relevant scripts in `data-raw/` and then use `usethis::use_data(...)`.

- Preserve graceful network-failure behavior. User-facing download functions commonly keep `allow_failure = TRUE` and wrap the real worker in a `tryCatch`, while the underlying implementation lives in a `*_bp` helper. Reuse `test_url()` for download gating instead of introducing hard failures for transient network issues.

- Tests that touch the network are written to be offline-safe. Follow the existing pattern at the top of network tests: `if (!curl::has_internet()) return(invisible(NULL))`.

- IMGW station handling is source-specific. Meteorological IMGW archive functions expect station names in uppercase, not numeric IDs; renamed stations may need multiple names such as `c("POZNAŃ", "POZNAŃ-ŁAWICA")`.

- Preserve the encoding fallback logic in `imgw_read()`. IMGW files vary in delimiter and encoding, so the CP1250 / UTF-8 / transliteration branches are intentional.

- If you add a new IMGW column, update both the abbreviation source data in `data-raw/` and the runtime shortening layer in `R/meteo_shortening_imgw.R` or `R/hydro_shortening_imgw.R`.

- If you introduce new data.table non-standard evaluation symbols, add them to `R/globals.R` to avoid `R CMD check` NOTES.

- `R/parser.R` is the exported parser implementation. If `inst/parser.R` exists, treat it as a sandbox/helper script rather than the package API surface.
