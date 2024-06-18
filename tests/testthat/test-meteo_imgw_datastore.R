context("meteo-imgw-datastore")


test_that("test-meteo_imgw_datastore", {
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    
    imgw_telemetry = meteo_imgw_datastore(year = 2023, 
                                          parameters = "t2m", 
                                          stations = "PSZENNO")
    
    if (is.data.frame(imgw_telemetry) & nrow(imgw_telemetry) > 0) {
      testthat::expect_true(is.data.frame(df))
      testthat::expect_true(nrow(df) > 50000)
    }
  }
})
