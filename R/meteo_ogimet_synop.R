#' Download and decode raw SYNOP messages from the Ogimet getsynop service
#'
#' Downloads raw SYNOP messages from the Ogimet `getsynop` endpoint and decodes
#' them into a tidy `data.frame` using the [synop_parser()] function. Two retrieval
#' modes are supported:
#'
#' - **Station mode** (`station` provided): fetches messages for one or more
#'   WMO station IDs.
#'   URL form: `http://www.ogimet.com/cgi-bin/getsynop?block=<id>&begin=<YYYYMMDDhhmm>&end=<YYYYMMDDhhmm>`
#'
#' - **Country mode** (`country_name` provided): fetches messages for all
#'   Ogimet stations in a country in a single request.
#'   URL form: `http://www.ogimet.com/cgi-bin/getsynop?begin=<YYYYMMDDhhmm>&end=<YYYYMMDDhhmm>&state=<country_name>`
#'
#' When both `station` and `country_name` are supplied, `country_name` takes
#' precedence and a warning is issued.
#'
#' Each line of the response is a comma-separated record:
#' `station_id,year,month,day,hour,minute,<SYNOP message>`.
#' The SYNOP message is decoded via [synop_parser()] with `as_data_frame = TRUE`.
#'
#' @param station Numeric or character vector of WMO station IDs. Optional when
#'   `country_name` is provided; required otherwise.
#' @param date Character or Date vector of length 2 giving the start and end of
#'   the requested period, e.g. `c("2009-12-01", "2009-12-04")`. Defaults to
#'   the last 30 days.
#' @param country Optional; passed to [synop_parser()] for country-specific
#'   precipitation indicator decoding (e.g. `"RU"`). Single string or `NULL`
#'   (default). This is distinct from `country_name`.
#' @param country_name Optional character string naming the country whose
#'   stations should be downloaded, as recognised by Ogimet (e.g.
#'   `"Poland"`, `"Germany"`, `"France"`). When provided, the `state=` Ogimet
#'   parameter is used and `station` is ignored. The full date range is
#'   fetched in a single request.
#' @param simplified Logical. When `TRUE` (default) returns a compact `data.frame` with
#'   20 standardised columns (see **Value** below). When `FALSE` the
#'   full parser output is returned.
#' @param allow_failure Logical. When `TRUE` (default) network errors are caught
#'   and a message is emitted; when `FALSE` errors propagate to the caller.
#'
#' @return By default (`simplified = TRUE`), a compact `data.frame` with one
#'   row per decoded SYNOP observation. Columns:
#'
#'   * `date` — Observation date-time (`POSIXct`, UTC).
#'   * `station` — WMO station identifier (character).
#'   * `t2m` — Air temperature at 2 m (°C).
#'   * `dpt2m` — Dew-point temperature at 2 m (°C).
#'   * `rel_hum` — Relative humidity (%), derived via [compute_relative_humidity()].
#'   * `tmax` — Daily maximum temperature from Section 3 (°C).
#'   * `tmin` — Daily minimum temperature from Section 3 (°C).
#'   * `wd` — Wind direction (degrees).
#'   * `ws` — Wind speed (m/s or knots, per `wind_unit`).
#'   * `gust` — Highest gust speed from Section 3, same unit as `ws`.
#'   * `press` — Station-level pressure (hPa).
#'   * `slp` — Sea-level pressure (hPa).
#'   * `press_tend` — 3-hour pressure change (hPa).
#'   * `precip` — Precipitation amount (mm).
#'   * `Nt` — Total cloud cover (oktas, 0–8) from the `Nddff` group.
#'   * `Nh` — Cover of low clouds (genera Sc, St, Cu, Cb) in oktas (0–8),
#'     from Section 1 group `8NhCLCMCH`; `NA` when not reported.
#'   * `N_base` — Height of base of lowest observed cloud (m).
#'   * `insol` — Daily sunshine duration (hours).
#'   * `visibility` — Horizontal visibility (m).
#'   * `snow` — Total snow depth (cm); 0 for trace amounts.
#'
#'   When `simplified = FALSE`, a `data.frame` with the first two columns
#'   `station_id` (WMO identifier, character) and `Date` (`POSIXct`, UTC),
#'   followed by all columns produced by [synop_parser()] with `as_data_frame = TRUE`:
#'   `station_type`, `region`, `obs_day`, `obs_hour`, `wind_unit`,
#'   `wind_estimated`, `visibility`, `cloud_cover`, `wind_direction`,
#'   `wind_speed`, `air_temperature`, `dewpoint_temperature`,
#'   `station_pressure`, `sea_level_pressure`, `pressure_tendency`,
#'   `pressure_change`, `precipitation_amount`, `precipitation_time`,
#'   `cloud_base_min`, `cloud_base_max`, `low_cloud_type`, `middle_cloud_type`,
#'   `high_cloud_type`, `low_cloud_amount`, `maximum_temperature`,
#'   `minimum_temperature`, `gust`, `sunshine_duration`,
#'   `snow_depth`, `snow_depth_state`, `source`.
#'
#'   Returns `NULL` invisibly when the download fails and `allow_failure = TRUE`.
#'
#' @export
#'
#' @examples
#' \donttest{
#'   # Station mode: Poznan-Lawica (Poland)
#'   poznan = meteo_ogimet_synop(station = 12330,
#'                               date = c("2009-12-01", "2009-12-04"))
#'   head(poznan)
#'
#'   # Station mode: multiple stations
#'   two_stations = meteo_ogimet_synop(station = c(12330, 12375),
#'                                     date = c("2019-06-01", "2019-06-03"))
#'   head(two_stations)
#'   
#'   # Country mode: all Polish stations for one day
#'   poland = meteo_ogimet_synop(country_name = "Poland",
#'                               date = c("2009-12-15", "2009-12-15"))
#'   head(poland)
#'
#'   # Simplified view
#'   poznan_simple = meteo_ogimet_synop(station = 12330,
#'                                      date = c("2009-12-01", "2009-12-04"),
#'                                      simplified = TRUE)
#'   head(poznan_simple)
#' }
#'
meteo_ogimet_synop = function(station = NULL,
                               date = c(Sys.Date() - 30, Sys.Date()),
                               country = NULL,
                               country_name = NULL,
                               simplified = TRUE,
                               allow_failure = TRUE) {
  .Deprecated("meteo_ogimet",
              msg = paste0("'meteo_ogimet_synop()' is deprecated. ",
                           "Use 'meteo_ogimet(source = \"synop\")' instead."))
  if (allow_failure) {
    tryCatch(
      meteo_ogimet_synop_bp(station = station, date = date,
                             country = country, country_name = country_name,
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
                           country = country, country_name = country_name,
                           simplified = simplified)
  }
}

#' @keywords internal
#' @noRd
meteo_ogimet_synop_bp = function(station, date, country, country_name, simplified) {

  if (is.null(station) && is.null(country_name)) {
    stop("Provide at least one of `station` or `country_name`.")
  }

  if (!is.null(station) && !is.null(country_name)) {
    warning("`station` is ignored when `country_name` is provided.", call. = FALSE)
    station = NULL
  }

  if (!curl::has_internet()) {
    message("No internet connection!")
    return(invisible(NULL))
  }

  all_results = list()

  begin_date = as.Date(min(date))
  end_date   = as.Date(max(date))

  if (!is.null(country_name)) {
    # ── Country mode ─────────────────────────────────────────────────────────
    # Single request (auto-split if response exceeds 200 000 rows).
    message(paste("Downloading country:", country_name))
    url_tmpl = paste0(
      "http://www.ogimet.com/cgi-bin/getsynop?begin=%s",
      "&end=%s",
      "&state=", utils::URLencode(country_name, reserved = TRUE)
    )
    chunk = .ogimet_synop_fetch_decode(url_tmpl, begin_date, end_date,
                                        label = country_name, country = country,
                                        use_csv_station_id = TRUE)
    if (!is.null(chunk)) all_results[[length(all_results) + 1L]] = chunk

  } else {
    # ── Station mode ──────────────────────────────────────────────────────────
    for (station_nr in station) {
      message(paste("station:", station_nr))
      url_tmpl = paste0(
        "http://www.ogimet.com/cgi-bin/getsynop?block=", station_nr,
        "&begin=%s&end=%s"
      )
      chunk = .ogimet_synop_fetch_decode(url_tmpl, begin_date, end_date,
                                          label = station_nr, country = country,
                                          use_csv_station_id = FALSE,
                                          station_id_override = as.character(station_nr))
      if (!is.null(chunk)) all_results[[length(all_results) + 1L]] = chunk

      if (!identical(station_nr, station[length(station)])) Sys.sleep(20)
    }
  }

  if (length(all_results) == 0) return(invisible(NULL))

  out = do.call(rbind, all_results)
  rownames(out) = NULL

  out = out[which(!is.na(out$Date) &
                    as.Date(out$Date) >= as.Date(min(date)) &
                    as.Date(out$Date) <= as.Date(max(date))), ]
  out = unique(out)
  rownames(out) = NULL

  if (simplified) {
    out = .synop_simplify(out)
  }

  out
}

# Internal helper: convert full SYNOP parser output to the compact 20-column data.frame.
# Used both by meteo_ogimet_synop_bp (simplified=TRUE) and by meteo_ogimet (return_list=TRUE).
#' @keywords internal
#' @noRd
.synop_simplify = function(out) {
  data.frame(
    date             = out$Date,
    station          = out$station_id,
    t2m              = out$air_temperature,
    dpt2m            = out$dewpoint_temperature,
    rel_hum          = round(compute_relative_humidity(out$air_temperature,
                                                       out$dewpoint_temperature), 1),
    tmax             = out$maximum_temperature,
    tmin             = out$minimum_temperature,
    wd               = out$wind_direction,
    ws               = out$wind_speed,
    gust             = out$gust,
    press            = out$station_pressure,
    slp              = out$sea_level_pressure,
    press_tend       = out$pressure_change,
    precip           = out$precipitation_amount,
    Nt               = out$cloud_cover,
    Nh               = out$low_cloud_amount,
    N_base           = out$cloud_base_min,
    insol            = out$sunshine_duration,
    visibility       = out$visibility,
    snow             = out$snow_depth,
    stringsAsFactors = FALSE
  )
}

# Recursive raw-line fetcher.
#
# Builds the URL from `url_tmpl` (a sprintf template with two %s slots for
# begin and end timestamps), GETs it, and returns the non-empty trimmed lines.
# Ogimet caps responses at 200 000 rows server-side; receiving exactly that many
# lines signals truncation, so the date range is halved and each half is fetched
# recursively.  When the range can no longer be bisected (begin_date == end_date)
# a warning is issued and the truncated chunk is returned as-is.
.ogimet_synop_raw_lines = function(url_tmpl, begin_date, end_date, label) {
  begin_str = paste0(format(begin_date, "%Y%m%d"), "0000")
  end_str   = paste0(format(end_date,   "%Y%m%d"), "2359")
  url = sprintf(url_tmpl, begin_str, end_str)
  message(url)

  resp = tryCatch(
    httr::GET(
      url,
      httr::add_headers(
        `User-Agent`      = "Mozilla/5.0 (Macintosh; Intel Mac OS X 10.15; rv:143.0) Gecko/20100101 Firefox/143.0",
        `Accept`          = "text/plain,text/html,*/*",
        `Accept-Language` = "en-US,en;q=0.5",
        `Referer`         = "http://www.ogimet.com/getsynop.phtml"
      )
    ),
    error = function(e) NULL
  )

  if (is.null(resp) || httr::http_error(resp)) {
    message(paste("Could not retrieve data for:", label))
    return(NULL)
  }

  body = httr::content(resp, as = "text", encoding = "UTF-8")
  if (is.na(body) || !nzchar(trimws(body))) {
    message(paste("Empty response for:", label))
    return(NULL)
  }

  lines = strsplit(body, "\n")[[1]]
  lines = trimws(lines[nzchar(trimws(lines))])

  # 200 000 rows is the Ogimet server cap: receiving that many means the
  # response was truncated, so split the date range and retry each half.
  if (length(lines) >= 200000L) {
    if (begin_date >= end_date) {
      warning(
        paste0("Response for ", label, " hit the 200 000-row server limit but",
               " the date range cannot be split further. Returning partial results."),
        call. = FALSE
      )
      return(lines)
    }
    mid_date = begin_date + as.integer((end_date - begin_date) / 2L)
    message(paste0(
      "Server limit reached (200 000 rows); splitting date range: [",
      begin_date, ", ", mid_date, "] and [", mid_date + 1L, ", ", end_date, "]"
    ))
    lo = .ogimet_synop_raw_lines(url_tmpl, begin_date, mid_date,    label)
    hi = .ogimet_synop_raw_lines(url_tmpl, mid_date + 1L, end_date, label)
    return(c(lo, hi))
  }

  lines
}

# Internal: fetch, (recursively) split if server limit is hit, parse lines, decode SYNOP.
# `url_tmpl`           - sprintf template; two %s slots for begin/end timestamps
# `begin_date`/`end_date` - Date objects defining the requested range
# `label`              - used in user-facing messages (station ID or country name)
# `use_csv_station_id` - TRUE  -> station_id taken from field 1 of each CSV line
#                        FALSE -> station_id_override is used for every row
.ogimet_synop_fetch_decode = function(url_tmpl, begin_date, end_date, label, country,
                                       use_csv_station_id, station_id_override = NULL) {
  lines = .ogimet_synop_raw_lines(url_tmpl, begin_date, end_date, label)

  if (is.null(lines) || length(lines) == 0) {
    message(paste("No SYNOP data returned for:", label))
    return(NULL)
  }

  message(sprintf("Downloaded %d SYNOP messages for: %s", length(lines), label))

  # Each line: station_id,year,month,day,hour,minute,<SYNOP message>
  # Older records may omit the minute field (6 fields instead of 7).
  parsed_lines = lapply(lines, function(line) {
    parts = strsplit(line, ",", fixed = TRUE)[[1]]
    n = length(parts)
    if (n < 6) return(NULL)

    sid = if (use_csv_station_id) trimws(parts[1]) else station_id_override
    yr  = as.integer(parts[2])
    mo  = as.integer(parts[3])
    dy  = as.integer(parts[4])
    hr  = as.integer(parts[5])

    if (n >= 7) {
      mn        = as.integer(parts[6])
      synop_msg = paste(parts[7:n], collapse = ",")
    } else {
      mn        = 0L
      synop_msg = parts[6]
    }

    dt = tryCatch(
      as.POSIXct(
        sprintf("%04d-%02d-%02d %02d:%02d", yr, mo, dy, hr, mn),
        format = "%Y-%m-%d %H:%M",
        tz = "UTC"
      ),
      error = function(e) NA
    )

    list(station_id = sid, Date = dt, synop = trimws(synop_msg))
  })

  parsed_lines = Filter(Negate(is.null), parsed_lines)
  if (length(parsed_lines) == 0) {
    message(paste("Could not parse any lines for:", label))
    return(NULL)
  }

  synop_msgs  = vapply(parsed_lines, `[[`, character(1), "synop")
  dates       = do.call(c, lapply(parsed_lines, `[[`, "Date"))
  station_ids = vapply(parsed_lines, `[[`, character(1), "station_id")

  n_msgs = length(synop_msgs)
  pb = utils::txtProgressBar(min = 0L, max = n_msgs, style = 3L, file = stderr())
  on.exit(close(pb), add = TRUE)

  country_vec = if (is.null(country)) rep(list(NULL), n_msgs) else as.list(rep(country, length.out = n_msgs))

  decoded_rows = vector("list", n_msgs)
  for (i in seq_len(n_msgs)) {
    decoded_rows[[i]] = tryCatch(
      suppressMessages(synop_parser(synop_msgs[[i]], country = country_vec[[i]], as_data_frame = TRUE)),
      error = function(e) NULL
    )
    utils::setTxtProgressBar(pb, i)
  }
  close(pb)
  on.exit(NULL)     # clear the on.exit so close() isn't called twice

  valid = !vapply(decoded_rows, is.null, logical(1))
  if (!any(valid)) {
    message(paste("SYNOP decoding failed for all messages for:", label))
    return(NULL)
  }

  decoded     = do.call(rbind, decoded_rows[valid])
  dates       = dates[valid]
  station_ids = station_ids[valid]

  decoded$station_id = station_ids

  data.frame(
    station_id = station_ids,
    Date       = dates,
    decoded[, setdiff(names(decoded), "station_id")],
    stringsAsFactors = FALSE
  )
}
