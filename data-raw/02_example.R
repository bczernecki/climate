library(climate)
# downloading data
df <- meteo_ogimet(interval = "hourly", date = c("2018-01-01", "2018-12-31"), 
                   station = c("01008"))
# loading external packages
library(dplyr)
library(openair) # external package for plotting wind roses

# converting wind direction from character into degrees 
wdir <- data.frame(ddd = c("CAL","N","NNE","NE","ENE","E","ESE","SE","SSE",
                           "S","SSW","SW","WSW","W","WNW","NW","NNW"),
                   dir = c(NA, 0:15 * 22.5), stringsAsFactors = FALSE)
# changing date column to the format required by the openair package
df$Date <- as.POSIXct(df$Date, tz = "UTC")
df$date <- df$Date
df <- left_join(df, wdir)
# Joining, by = "ddd" 
df$ws <- df$ffkmh / 3.6 # conversion to m/s from km/h
df$gust <- df$Gustkmh / 3.6 # conversion to m/s from km/h
windRose(mydata = df, ws = "ws", wd = "dir", type = "season", paddle = FALSE,
         main = "Svalbard Lufthavn (2018)", ws.int = 3, dig.lab = 3, layout = c(4, 1))