library(imgw)
library(stringr)

m_hs <- meteo_metadata("hourly", "synop")
m_hc <- meteo_metadata("hourly", "climate")
m_ds <- meteo_metadata("daily", "synop")
m_dc <- meteo_metadata("daily", "climate")
m_dp <- meteo_metadata("daily", "precip")
m_ms <- meteo_metadata("monthly", "synop")
m_mc <- meteo_metadata("monthly", "climate")
m_mp <- meteo_metadata("monthly", "precip")

all_meteo_metadata = dplyr::bind_rows(
  m_hs[[1]],
  m_hc[[1]],
  m_ds[[1]],
  m_ds[[2]],
  m_dc[[1]],
  m_dc[[2]],
  m_dp[[1]],
  m_ms[[1]],
  m_ms[[2]],
  m_mc[[1]],
  m_mc[[2]],
  m_mp[[1]]
)

unique_meteo_parameters = str_squish(all_meteo_metadata$parameters) #usuwa podwojne spacje, etc.
unique_meteo_parameters = unique(unique_meteo_parameters)
unique_meteo_parameters = sort(unique_meteo_parameters)

View(unique_meteo_parameters)

# sprawdzenie czy stworzona recznie baza daje sie polaczyc left_joinem:
skroty <- read.csv("data-raw/parametry_skrot.csv", stringsAsFactors = F)
wsio <- data.frame(fullname = unique_meteo_parameters)
laczenie <- dplyr::left_join(wsio,skroty)
head(laczenie)
