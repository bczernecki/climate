
test_that("check_hourly_imgw_climate", {
  
  skip_on_cran()
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    clim = suppressMessages(
      meteo_imgw_hourly(year = 2024,
                             rank = "climate", 
                             coords = TRUE, 
                             allow_failure = FALSE, 
                             station = "DOLINA PIĘCIU STAWÓW")
    )
    expect_true(nrow(clim) > 1000)
  }
})
