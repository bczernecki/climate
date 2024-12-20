context("stations_meteo_imgw_telemetry")


test_that("test-stations_meteo_imgw_telemetry", {
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    
    df = stations_meteo_imgw_telemetry()
    if (!is.null(df)) {
      if (is.data.frame(df) & nrow(df) > 0) {
        testthat::expect_true(is.data.frame(df))
      }
    }
    
  }
})
