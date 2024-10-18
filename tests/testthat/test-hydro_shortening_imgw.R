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
  
  eng_full = hydro_shortening_imgw(data = df, col_names = "full")
  eng_short = hydro_shortening_imgw(data = df, col_names = "short")
  
  expect_true("River_or_Lake" %in% colnames(eng_full))
  expect_true("riv_or_lake" %in% colnames(eng_short))
  
})