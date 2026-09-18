context("meteo_imgw_daily")



test_that("meteo_imgw_daily_single_station", {
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    single_station = meteo_imgw_daily(rank = "synop", 
                                      year = 2024, 
                                      station = "LESZNO",
                                      status = FALSE,
                                      coords = TRUE)
    expect_true(nrow(single_station) > 360)
  }
})

test_that("meteo_imgw_daily", {
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    y = 1900 # year not supported
    expect_warning(meteo_imgw_daily(rank = "synop", year = y, status = TRUE,
                                  coords = TRUE, allow_failure = TRUE))
  }
})


test_that("check_column_with_coordinates", {
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    station_with_coordinates = meteo_imgw_daily(rank = "precip",
                                                year = 2002,
                                                coords = TRUE,
                                                station = "IMBRAMOWICE", allow_failure = FALSE)
    if (is.data.frame(station_with_coordinates)) {
      expect_true(any(colnames(station_with_coordinates) %in% c("X", "Y")))
    }
  }
})

test_that("check_message_for_non_existing_station", {
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    expect_true(
      nrow(
        meteo_imgw_daily(rank = "precip",
                                  year = 2002,
                                  coords = TRUE,
                                  station = 9999)
      ) > 160000)
  }
})



test_that("check_encoding_in_non_synop", {
  
  skip_on_cran()
  
  if (!curl::has_internet()) {
    message("No internet connection! \n")
    return(invisible(NULL))
  } else {
    non_synop = expect_message(suppressWarnings(meteo_imgw_daily(year = 2024, 
                                 rank = "precip", 
                                 allow_failure = FALSE)))
    expect_identical(nchar(non_synop$NSP), nchar(trimws(non_synop$NSP)))
    non_synop = suppressWarnings(meteo_imgw_daily(year = 2024, 
                                 rank = "climate", 
                                 allow_failure = FALSE))
    expect_identical(nchar(non_synop$NSP), nchar(trimws(non_synop$NSP)))
  }
})

test_that("current IMGW daily labels use short names", {
  data = data.frame(
    "Maksymalna dobowa temperatura powietrza [°C]" = 1,
    "Minimalna dobowa temperatura powietrza [°C]" = 2,
    "Minimalna dobowa temperatura powietrza przy gruncie [°C]" = 3,
    check.names = FALSE
  )

  expect_named(
    meteo_shortening_imgw(data),
    c("tmax_daily", "tmin_daily", "t5cm_min")
  )
})

test_that("IMGW column labels are retained as attributes", {
  data = data.frame(
    TMAX = 1:2,
    TMIN = 3:4,
    TMNG = 5:6,
    check.names = FALSE
  )
  meta = data.frame(
    parameters = c("TMAX", "TMIN", "TMNG"),
    label = c(
      "Maksymalna dobowa temperatura powietrza [°C]",
      "Minimalna dobowa temperatura powietrza [°C]",
      "Minimalna dobowa temperatura powietrza przy gruncie [°C]"
    ),
    stringsAsFactors = FALSE
  )

  data = imgw_rename_params_to_labels(data, meta)
  data = meteo_shortening_imgw(data)

  expect_equal(
    unname(vapply(data, attr, character(1), which = "label")),
    meta$label
  )
})
