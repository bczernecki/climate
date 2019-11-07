library(climate)
# downloading data
df <- meteo_ogimet(interval = "hourly", date = c("2018-01-01", "2018-12-31"), 
                   station = c("01008"))
library(openair) # external package for plotting wind roses
# converting wind direction from character into degrees 
wdir <- data.frame(ddd = c("CAL", "N", "NNE", "NE", "ENE", "E", "ESE", "SE", "SSE",
                           "S", "SSW", "SW", "WSW", "W", "WNW", "NW", "NNW"),
                   dir = c(NA, 0:15 * 22.5), stringsAsFactors = FALSE)
# changing the date column to the format required by the openair package
df$date <- as.POSIXct(df$Date, tz = "UTC")
df <- merge(df, wdir, by = "ddd", all.x = TRUE) # joining two datasets
df$ws <- df$ffkmh / 3.6 # converting to m/s from km/h
df$gust <- df$Gustkmh / 3.6 # converting to m/s from km/h
windRose(mydata = df, ws = "ws", wd = "dir", type = "season", paddle = FALSE,
         main = "Svalbard Lufthavn (2018)", ws.int = 3, dig.lab = 3, layout = c(4, 1))
