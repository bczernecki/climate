library(ggplot2)
library(dplyr)
library(tidyr)
library(sf)
library(tmap)
library(rnaturalearth)
library(climate)
ms <- meteo_imgw("monthly", "synop", year = 1978:2017, coords = TRUE)
# calculating annual values
ms %>%
  filter(!(mm > 5 && mm < 9 && t2m_mean_mon == 0)) %>%
  select(station, X, Y, yy, mm, t2m_mean_mon) %>%
  group_by(station, yy, X, Y) %>%
  summarise(annual_mean_t2m = mean(t2m_mean_mon), n = n()) %>%
  filter(n == 12) %>%
  spread(yy, annual_mean_t2m) %>%
  na.omit() -> trend
# extracting trends
regression <- function(x) {
  df <- data.frame(yy = 1978:2017, temp = as.numeric(x))
  coef(lm(temp ~ yy, data = df))[2]
}
trend$coef <- round(apply(trend[, -1:-4], 1, regression) * 100, 1)
trend <- st_as_sf(trend, coords = c("X", "Y"), crs = 4326)
# mapping the results
world <- ne_countries(scale = "medium", returnclass = "sf")
tm <- tm_shape(world) + tm_borders() +
  tm_shape(trend, is.master = TRUE) + tm_dots(col = "coef", size = 4) +
  tm_shape(trend) + tm_text(text = "coef")
tm
