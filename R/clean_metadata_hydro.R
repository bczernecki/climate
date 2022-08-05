#' Hydrological metadata cleaning (IMGW-PIB data only)
#'
#' Internal function for hydrological metadata cleaning
#' @param address URL address of the metadata file
#' @param interval temporal interval
#' @importFrom utils read.fwf
#' @keywords internal
clean_metadata_hydro = function(address, interval) {
  temp = tempfile()

  test_url(link = address, output = temp)
  a = readLines(temp, warn = FALSE)

  a = iconv(a, from = "cp1250", to = "ASCII//TRANSLIT") # usuwamy polskie znaki, bo to robi spore "kuku"
  a = gsub(a, pattern = "\\?", replacement = "") # usuwamy znaki zapytania powstale po konwersji

  # additional workarounds for mac os but not only...
  a = gsub(x = a, pattern = "'", replacement = "")
  a = gsub(x = a, pattern = "\\^", replacement = "")

  if (interval == "monthly") {
    b = list(data.frame(parameters = a[3:12])) # sklad danych jeszcze nie wiem jak ominąć problem kontroli
    # ale on może się zmienić nie wiem czy nie lepiej wykluczyć ostatni rok
  }
  if (interval == "daily") {
    b = data.frame(parameters = a[3:12])
  }
  if (interval == "semiannual_and_annual") {
    godzina = paste0(a[15], ":", a[16]) # nie jestem pewien czy tak bo w dokumentacji jest podzial na dwie kolumny,
    #ale w pliku jest jedna kolumna a pomiaru brak
    data = c(a[12:14], godzina)
    data_od = paste0("wystapienie_od_", data)
    data_do = paste0("wystapienie_od_", data)
    SPT = unlist(strsplit(a[10], "]/")) # stan/przeplyw/temperatura
    SPT[1] = paste0(SPT[1], "]")
    SPT[2] = paste0(SPT[2], "]")
    b = NULL
    for (i in seq_along(SPT)) {
      tmp = c(a[3:9], SPT[i], data_od, data_do)
      b = cbind(b, tmp)
    }
    b = list("H" = data.frame(parameters = b[, 1]),
              "Q" = data.frame(parameters = b[, 2]),
              "T" = data.frame(parameters = b[, 3]))
  }
  b
}
