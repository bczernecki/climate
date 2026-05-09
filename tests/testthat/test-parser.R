## Reference SYNOP message used across multiple tests:
## AAXX 01004 88889 12782 61506 10094 20047 30111 40197 53007 60001 81541
## Decoded (reference values verified interactively):
##   station_type  = "AAXX"
##   obs_time      = day 1, hour 0
##   wind_indicator= 4 (KT, anemometer)
##   station_id    = "88889"  (region III)
##   cloud_cover   = 6 okta
##   visibility    = 40000 m
##   wind dir      = 150 deg, speed = 6 kt
##   air_temp      = 9.4 Celsius,  dewpoint = 4.7 Celsius
##   station_pres  = 1011.1 hPa
##   sea_lvl_pres  = 1019.7 hPa
##   precip_s1     = 0 mm

SYNOP_MSG = "AAXX 01004 88889 12782 61506 10094 20047 30111 40197 53007 60001 81541"

# ── input validation ──────────────────────────────────────────────────────────

test_that("parser stops on missing message", {
  expect_error(parser(), "`message` must contain at least one SYNOP string.")
})

test_that("parser stops on zero-length character vector", {
  expect_error(parser(character(0)), "`message` must contain at least one SYNOP string.")
})

test_that("parser stops on non-character input", {
  expect_error(parser(12345), "`message` must be a character vector.")
})

test_that("parser warns and returns NULL for empty string", {
  expect_warning(parser(""), "Empty SYNOP message supplied")
})

test_that("parser stops when country length mismatches message length", {
  expect_error(
    parser(SYNOP_MSG, country = c("RU", "PL")),
    "`country` must be NULL"
  )
})

# ── return-type behaviour ─────────────────────────────────────────────────────

test_that("parser returns a list for a single message (simplify = TRUE)", {
  result = parser(SYNOP_MSG)
  expect_type(result, "list")
})

test_that("parser with simplify = FALSE wraps single message in a list of length 1", {
  result = parser(SYNOP_MSG, simplify = FALSE)
  expect_type(result, "list")
  expect_length(result, 1)
  expect_type(result[[1]], "list")
})

test_that("parser returns a list of n elements for n messages", {
  result = parser(rep(SYNOP_MSG, 3), simplify = FALSE)
  expect_type(result, "list")
  expect_length(result, 3)
})

test_that("parser simplify = FALSE and TRUE are consistent for single message", {
  r_simplified = parser(SYNOP_MSG, simplify = TRUE)
  r_wrapped    = parser(SYNOP_MSG, simplify = FALSE)
  expect_identical(r_simplified, r_wrapped[[1]])
})

# ── top-level field presence ──────────────────────────────────────────────────

test_that("parsed result contains expected top-level fields", {
  result = parser(SYNOP_MSG)
  expected_fields = c(
    "station_type", "obs_time", "wind_indicator", "station_id",
    "precipitation_indicator", "weather_indicator",
    "visibility", "cloud_cover", "surface_wind",
    "air_temperature", "dewpoint_temperature",
    "station_pressure", "sea_level_pressure"
  )
  for (field in expected_fields) {
    expect_true(field %in% names(result), info = paste("missing field:", field))
  }
})

# ── decoded values ────────────────────────────────────────────────────────────

test_that("parser decodes station type correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$station_type$value, "AAXX")
})

test_that("parser decodes station ID correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$station_id$value, "88889")
})

test_that("parser decodes observation time correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$obs_time$day$value,  1)
  expect_equal(result$obs_time$hour$value, 0)
})

test_that("parser decodes wind indicator correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$wind_indicator$unit, "KT")
  expect_false(result$wind_indicator$estimated)
})

test_that("parser decodes cloud cover correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$cloud_cover$value, 6)
  expect_equal(result$cloud_cover$unit,  "okta")
})

test_that("parser decodes visibility correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$visibility$value, 40000)
  expect_equal(result$visibility$unit,  "m")
})

test_that("parser decodes surface wind correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$surface_wind$direction$value, 150)
  expect_equal(result$surface_wind$speed$value,       6)
})

test_that("parser decodes air temperature correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$air_temperature$value, 9.4, tolerance = 1e-6)
  expect_equal(result$air_temperature$unit, "Celsius")
})

test_that("parser decodes dewpoint temperature correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$dewpoint_temperature$value, 4.7, tolerance = 1e-6)
})

test_that("parser decodes station pressure correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$station_pressure$value, 1011.1, tolerance = 0.05)
  expect_equal(result$station_pressure$unit, "hPa")
})

test_that("parser decodes sea-level pressure correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$sea_level_pressure$value, 1019.7, tolerance = 0.05)
  expect_equal(result$sea_level_pressure$unit, "hPa")
})

test_that("parser decodes section-1 precipitation amount correctly", {
  result = parser(SYNOP_MSG)
  expect_equal(result$precipitation_s1$amount$value, 0)
})

# ── country parameter ─────────────────────────────────────────────────────────

test_that("parser accepts a valid single-value country argument", {
  result = parser(SYNOP_MSG, country = "RU")
  expect_type(result, "list")
  expect_true("station_type" %in% names(result))
})

test_that("parser accepts country vector matching message length", {
  result = parser(rep(SYNOP_MSG, 2), country = c("RU", "PL"), simplify = FALSE)
  expect_length(result, 2)
})

# ── whitespace handling ───────────────────────────────────────────────────────

test_that("parser trims leading/trailing whitespace from messages", {
  padded = paste0("  ", SYNOP_MSG, "  ")
  result = parser(padded)
  expect_equal(result$station_id$value, "88889")
})

# ── multiple messages consistency ─────────────────────────────────────────────

test_that("each element of a multi-message result matches the single-message result", {
  single  = parser(SYNOP_MSG)
  multi   = parser(rep(SYNOP_MSG, 2), simplify = FALSE)
  expect_identical(multi[[1]], single)
  expect_identical(multi[[2]], single)
})
