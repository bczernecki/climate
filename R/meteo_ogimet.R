#' Download meteorological (Synop) data from the Ogimet service
#'
#' Unified entry point for downloading hourly or daily meteorological data
#' from [Ogimet](https://www.ogimet.com/). Two backends are supported:
#'
#' - **`"synop"`** (default for hourly): Downloads raw SYNOP messages from the
#'   Ogimet `getsynop` endpoint and decodes them with [parser()]. Supports
#'   station mode (one or more WMO IDs) and/or country mode (`country_name`).
#'   A default output columns are described in the **synop output** section below, but 
#'   can be enhanced optionally with `simplified = FALSE` or `return_list = TRUE`
#'   to include more of decoded SYNOP fields.
#'
#' - **`"html"`** (default for daily): Scrapes pre-formatted summary tables
#'   from the Ogimet using HTML parsing. Supports station mode only (one or more WMO IDs).
#'   Output columns are described in the **html output** section below.
#'
#' @param interval `"hourly"` (default) or `"daily"` — time resolution to retrieve.
#' @param date Length-2 character or Date vector giving the start and end of
#'   the requested period, e.g. `c("2018-05-01", "2018-07-01")`. Defaults to
#'   the last 30 days.
#' @param station WMO ID(s) of the station(s) to download. Character or numeric
#'   vector. Not required when `country_name` is provided (SYNOP path only).
#' @param country_name Optional character string. When provided, the SYNOP path
#'   downloads all Ogimet stations for the named country in a single request
#'   (e.g. `"Poland"`, `"Germany"`), and `station` is ignored. Valid only with
#'   `source = "synop"` (or the default hourly path).
#' @param source Character. Backend to use: `"synop"` (raw SYNOP decoding) or
#'   `"html"` (HTML scraping). When `NULL` (default) the backend is chosen
#'   automatically: `"synop"` for `interval = "hourly"`, `"html"` for
#'   `interval = "daily"`.
#' @param ... Optional named arguments:
#'   \describe{
#'     \item{`allow_failure`}{Logical. When `TRUE` (default) network or parsing
#'       errors are caught and a message is emitted; when `FALSE` errors
#'       propagate.}
#'     \item{`simplified`}{Logical. Applies to `source = "synop"` only. When
#'       `TRUE` (default) a compact 20-column `data.frame` is returned (see
#'       **synop output** below). When `FALSE` the full [parser()] output is
#'       returned with 30+ columns.}
#'     \item{`precip_split`}{Logical. Split the precipitation field into
#'       separate `pr6`, `pr12`, and `pr24` columns. Valid only for
#'       `interval = "hourly"` with `source = "html"`; a warning is emitted
#'       otherwise. Default `TRUE`.}
#'     \item{`return_list`}{Logical. Applies to `source = "synop"` only. When
#'       `TRUE` a named list with elements `data` (compact 20-column
#'       `data.frame`) and `full` (30+ column parser output) is returned
#'       instead of a `data.frame`. A warning is emitted when used with
#'       `source = "html"`. Default `FALSE`.}
#'   }
#'
#' @export
#'
#' @return
#' **synop output** (`source = "synop"`, `simplified = TRUE` or `return_list = TRUE` `$data`):
#' A `data.frame` with one row per decoded SYNOP observation and approx. 20 columns:
#' `date` (POSIXct UTC), `station`, `t2m`, `dpt2m`, `rel_hum`, `tmax`,
#' `tmin`, `wd`, `ws`, `gust`, `press`, `slp`, `press_tend`, `precip`,
#' `Nt`, `Nh`, `N_base`, `insol`, `visibility`, `snow`.
#'
#' **synop output** (`source = "synop"`, `simplified = FALSE`):
#' A `data.frame` with 30+ columns from [parser()], prefixed by `station_id`
#' and `Date`.
#'
#' **html output** (`source = "html"`, `interval = "hourly"`):
#' A `data.frame` with columns: `station_ID`, optionally `Lon`/`Lat`,
#' `Date`, `TC`, `TdC`, `TmaxC`, `TminC`, `ddd`, `ffkmh`, `Gustkmh`,
#' `P0hPa`, `PseahPa`, `PTnd`, `Nt`, `Nh`, `HKm`, `InsoD1`, `Viskm`,
#' `Snowcm`, and (when `precip_split = TRUE`) `pr6`, `pr12`, `pr24`.
#'
#' **html output** (`source = "html"`, `interval = "daily"`):
#' A `data.frame` with columns: `station_ID`, optionally `Lon`/`Lat`,
#' `Date`, `TemperatureCAvg`, `TemperatureCMax`, `TemperatureCMin`,
#' `TdAvgC`, `HrAvg`, `WindkmhDir`, `WindkmhInt`, `WindkmhGust`,
#' `PresslevHp`, `PreselevHp`, `Precmm`, `SunD1h`, `SnowDepcm`,
#' `TotClOct`, `lowClOct`, `VisKm`.
#'
#' Returns `NULL` invisibly on failure when `allow_failure = TRUE`.
#'
#' @examples
#' \donttest{
#'   # Hourly SYNOP data for Poznan-Lawica (default source = "synop")
#'   poznan_h = meteo_ogimet(interval = "hourly",
#'                           station  = 12330,
#'                           date     = c("2009-12-01", "2009-12-04"))
#'
#'   # Daily HTML summaries for New York - La Guardia (default source = "html")
#'   new_york = meteo_ogimet(interval = "daily", station  = 72503)
#'
#'   # Hourly with full parser output as a list
#'   poznan_list = meteo_ogimet(interval     = "hourly",
#'                              station      = 12330,
#'                              date         = c("2009-12-01", "2009-12-04"),
#'                              return_list  = TRUE)
#'   head(poznan_list$data)  # simplified
#'   head(poznan_list$full)  # all parser columns
#'
#'   # Country mode: all Polish stations for one day
#'   germany = meteo_ogimet(interval      = "hourly",
#'                          country_name  = "Germany",
#'                          date          = c("2009-12-15", "2009-12-15"))
#'
#'   # Force SYNOP backend for daily data
#'   poznan_d = meteo_ogimet(interval = "daily",
#'                           station  = 12330,
#'                           date     = c("2009-12-01", "2009-12-04"),
#'                           source   = "synop")
#'
#'   # Force HTML backend for hourly data
#'   poznan_h2 = meteo_ogimet(interval = "hourly",
#'                            station  = 12330,
#'                            date     = c("2019-06-01", "2019-06-08"),
#'                            source   = "html")
#' }
#'
meteo_ogimet = function(interval     = "hourly",
                        date         = c(Sys.Date() - 30, Sys.Date()),
                        station      = NULL,
                        country_name = NULL,
                        source       = NULL,
                        ...) {

  dots          = list(...)
  allow_failure = if (!is.null(dots$allow_failure)) dots$allow_failure else TRUE
  simplified    = if (!is.null(dots$simplified))    dots$simplified    else TRUE
  precip_split  = if (!is.null(dots$precip_split))  dots$precip_split  else TRUE
  return_list   = if (!is.null(dots$return_list))   dots$return_list   else FALSE

  if (!interval %in% c("hourly", "daily")) {
    stop("Wrong `interval` value. It should be either 'hourly' or 'daily'")
  }

  if (!is.null(source)) {
    source = match.arg(source, c("synop", "html"))
  }

  effective_source = if (!is.null(source)) {
    source
  } else if (interval == "hourly") {
    "synop"
  } else {
    "html"
  }

  # Warn for HTML-only params used with SYNOP
  if (effective_source == "synop") {
    if (!isTRUE(precip_split)) {
      warning("`precip_split` is not supported for source = 'synop' and will be ignored.", call. = FALSE)
    }
  }

  # Warn for SYNOP-only params used with HTML
  if (effective_source == "html") {
    if (isTRUE(return_list)) {
      warning("`return_list` is only supported for source = 'synop' and will be ignored.", call. = FALSE)
    }
    if (!is.null(country_name)) {
      warning("`country_name` is only supported for source = 'synop' and will be ignored.", call. = FALSE)
    }
  }

  # ── HTML backend ─────────────────────────────────────────────────────────────
  if (effective_source == "html") {
    if (interval == "daily") {
      if (!precip_split) {
        warning("The `precip_split` argument is only valid for hourly time step", call. = FALSE)
      }
      return(ogimet_daily(date = date,
                          station = station, allow_failure = allow_failure))
    } else {
      return(ogimet_hourly(date = date, station = station,
                           precip_split = precip_split, allow_failure = allow_failure))
    }
  }

  # ── SYNOP backend ─────────────────────────────────────────────────────────────
  if (return_list) {
    # Fetch full (unsimplified) output, then build both simplified and full views.
    full_data = if (allow_failure) {
      tryCatch(
        meteo_ogimet_synop_bp(station = station, date = date,
                               country = NULL, country_name = country_name,
                               simplified = FALSE),
        error = function(e) {
          message(paste("Problems with downloading data.",
                        "Run function with argument allow_failure = FALSE",
                        "to see more details"))
          invisible(NULL)
        }
      )
    } else {
      meteo_ogimet_synop_bp(station = station, date = date,
                             country = NULL, country_name = country_name,
                             simplified = FALSE)
    }

    if (is.null(full_data) || nrow(full_data) == 0) return(invisible(NULL))
    return(list(data = .synop_simplify(full_data), full = full_data))
  }

  # Standard SYNOP path — return a data.frame
  if (allow_failure) {
    tryCatch(
      meteo_ogimet_synop_bp(station = station, date = date,
                             country = NULL, country_name = country_name,
                             simplified = simplified),
      error = function(e) {
        message(paste("Problems with downloading data.",
                      "Run function with argument allow_failure = FALSE",
                      "to see more details"))
        invisible(NULL)
      }
    )
  } else {
    meteo_ogimet_synop_bp(station = station, date = date,
                           country = NULL, country_name = country_name,
                           simplified = simplified)
  }
}
