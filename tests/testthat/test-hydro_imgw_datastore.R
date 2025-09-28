context("hydro-imgw-datastore")


test_that("hydro-meteo_imgw_datastore", {
  
  skip_on_cran()
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    
    imgw_telemetry = hydro_imgw_datastore(year = 2023, 
                                          parameters = "flow", 
                                          stations = "FORDON")
    if (!is.null(imgw_telemetry)) {
      if (is.data.frame(imgw_telemetry) & nrow(imgw_telemetry) > 0) {
        testthat::expect_true(is.data.frame(imgw_telemetry))
        testthat::expect_true(nrow(imgw_telemetry) > 50000)
      }
    }
  }
})
