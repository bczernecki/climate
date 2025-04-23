
test_that("check_hourly_imgw_climate", {
  
  skip_on_cran()
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    hourly = meteo_imgw_hourly(year = 2024,
                               rank = "climate", 
                               coords = TRUE, 
                               station = "WARSZAWA-FILTRY",
                               allow_failure = FALSE)
    expect_true(nrow(hourly) > 1000)
  }
})
