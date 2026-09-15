test_that("hydro_shortening_imgw", {
  
  df = structure(list(`Kod stacji` = c(150210180L, 150210180L),
                 `Nazwa stacji` = c("ANNOPOL", "ANNOPOL"), 
                 `Nazwa rzeki/jeziora` = c("Wisła (2)", "Wisła (2)"), 
                 `Rok hydrologiczny` = c(1969L, 1969L), 
                 `Wskaźnik miesiąca w roku hydrologicznym` = c(1L, 1L), 
                 `Wskaźnik ekstremum` = 1:2, 
                 `Stan wody [cm]` = c(258L, 287L), 
                 `Przepływ [m3/s]` = c(288, 413.266), `Temperatura wody [st. C]` = c(NA_real_, NA_real_), 
                 `Miesiąc kalendarzowy` = c(11L, 11L)), row.names = 7165:7166, class = "data.frame")
  
  eng_short = hydro_shortening_imgw(data = df)
  
  expect_true("riv_or_lake" %in% colnames(eng_short))
  expect_true("Q" %in% colnames(eng_short))

})

test_that("hydro IMGW column labels are retained as attributes", {
  data = structure(
    list(
      `Stan wody [cm]` = c(258L, 287L),
      `Przepływ [m3/s]` = c(288, 413.266)
    ),
    row.names = c(1L, 2L),
    class = "data.frame"
  )
  attr(data[[1]], "label") = "Stan wody [cm]"
  attr(data[[2]], "label") = "Przepływ [m3/s]"

  result = hydro_shortening_imgw(data)

  expect_equal(
    unname(vapply(result, attr, character(1), which = "label")),
    c("Stan wody [cm]", "Przepływ [m3/s]")
  )
})

test_that("generated hydro date column is not renamed", {
  data = data.frame(Data = as.Date("2020-01-01") + 0:1, check.names = FALSE)

  result = hydro_shortening_imgw(data)

  expect_identical(names(result), "Data")
  expect_s3_class(result$Data, "Date")
})