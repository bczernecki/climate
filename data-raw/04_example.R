library(climate)
profile <- sounding_wyoming(wmo_id = 12120,yy = 2019, mm = 4, dd = 4, hh = 0)
df <- profile[[1]] 
colnames(df)[c(1, 3:4)] = c("press", "temp", "dewpt") # changing column names
RadioSonde::plotsonde(df, winds = FALSE, title = "2019-04-04 00UTC (LEBA, PL)",
                      col = c("red", "blue"), lwd = 3)
