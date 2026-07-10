
test_that(".ogimet_synop_raw_lines splits and recurses when server limit is reached", {
  # Stub the HTTP layer so no network call is made.
  # First call returns exactly 200 000 fake lines (server cap hit -> split).
  # Recursive halves each return 5 lines (within limit).
  call_count = 0L
  line_tmpl  = "12330,2009,12,01,00,00,AAXX 01004 12330 ///// /////"
  fake_full  = paste(rep(line_tmpl, 200000L), collapse = "\n")
  fake_half  = paste(rep(line_tmpl, 5L),      collapse = "\n")

  with_mocked_bindings(
    GET = function(url, ...) {
      call_count <<- call_count + 1L
      structure(
        list(status_code = 200L,
             content = if (call_count == 1L) fake_full else fake_half),
        class = "response"
      )
    },
    http_error = function(resp, ...) FALSE,
    content    = function(resp, ...) resp$content,
    .package   = "httr",
    {
      result = climate:::.ogimet_synop_raw_lines(
        url_tmpl   = "http://example.com/getsynop?begin=%s&end=%s",
        begin_date = as.Date("2009-12-01"),
        end_date   = as.Date("2009-12-04"),
        label      = "test"
      )
      # First call hit the limit -> two recursive calls -> 5 + 5 = 10 lines
      expect_equal(length(result), 10L)
      expect_equal(call_count, 3L)
    }
  )
})

test_that("meteo_ogimet_synop warns when both station and country_name are given", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  # only check the warning; the download itself may or may not succeed
  expect_warning(
    meteo_ogimet(station = 12330, source = "synop",
                       country_name = "Poland",
                       date = c("2009-12-15", "2009-12-15"),
                       allow_failure = TRUE),
    "`station` is ignored"
  )
})

test_that("meteo_ogimet_synop station mode returns a data.frame with expected columns", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(station = 12330, source = "synop",
                               date = c("2009-12-01", "2009-12-04"),
                               simplified = FALSE)

  if (is.null(result)) return(invisible(NULL))

  expect_s3_class(result, "data.frame")
  expect_true("station_id" %in% names(result))
  expect_true("Date" %in% names(result))
  expect_true("air_temperature" %in% names(result))
  expect_true("wind_speed" %in% names(result))
  expect_true(nrow(result) > 0)
})

test_that("meteo_ogimet_synop simplified station mode returns expected columns", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(station = 12330, source = "synop",
                               date = c("2009-12-01", "2009-12-04"))

  if (is.null(result)) return(invisible(NULL))

  expect_s3_class(result, "data.frame")
  expect_true(all(c("date", "station", "t2m", "ws", "Nt") %in% names(result)))
  expect_true(nrow(result) > 0)

  expect_s3_class(result$date, "POSIXct")
  expect_equal(attr(result$date, "tzone"), "UTC")

  expect_true(all(as.Date(result$date) >= as.Date("2009-12-01")))
  expect_true(all(as.Date(result$date) <= as.Date("2009-12-04")))
  
  expect_true("source" %in% names(result))
  expect_true(all(nzchar(result$source[!is.na(result$source)])))
})

test_that("meteo_ogimet_synop station mode handles allow_failure gracefully", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  expect_no_error(
    meteo_ogimet(station = 9999999, date = c("2009-12-01", "2009-12-02"), source = "synop",
                       allow_failure = TRUE)
  )
})

test_that("meteo_ogimet_synop country mode returns a data.frame for one day", {
  if (!curl::has_internet()) return(invisible(NULL))
  skip_on_cran()

  result = meteo_ogimet(country_name = "Poland", source = "synop",
                               date = c("2009-12-15", "2009-12-15"),
                               simplified = FALSE)

  if (is.null(result)) return(invisible(NULL))

  expect_s3_class(result, "data.frame")
  expect_true("station_id" %in% names(result))
  expect_true("Date" %in% names(result))
  expect_true("air_temperature" %in% names(result))
  # Poland has many SYNOP stations; expect multiple rows
  expect_true(nrow(result) > 1)
  
  expect_s3_class(result$date, "POSIXct")
  expect_equal(attr(result$date, "tzone"), "UTC")

  expect_true(all(as.Date(result$date) == as.Date("2009-12-15")))
})


# ── Nt / Nh column mapping ────────────────────────────────────────────────────

test_that("parser Nt=cloud_cover and Nh=low_cloud_amount are decoded correctly", {
  # 71703 -> Nddff: N=7 (total cloud cover 7 oktas)
  # 85232 -> 8NhCLCMCH: Nh=5 (low cloud cover 5 oktas), CL=2 (Sc), CM=3, CH=2
  msg = "AAXX 15151 12120 42461 71703 11013 21016 30184 40192 58006 85232="
  row = synop_parser(msg, as_data_frame = TRUE)
  expect_equal(row$cloud_cover,      7)   # Nt source
  expect_equal(row$low_cloud_amount, 5)   # Nh source
})
