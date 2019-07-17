library(imgw)
library(stringr)
synop <- meteo_daily("synop", year=2010)
daily <- synop
head(daily)

abbrev <- read.csv("data-raw/parametry_skrot.csv", stringsAsFactors = F)
saveRDS(abbrev, file="data/abbrev.rda")
abbrev <- read.csv("data-raw/parametry_skrot.csv", stringsAsFactors = F)

orig_columns <- trimws(gsub("\\s+", " ", colnames(daily))) # remove double spaces

# fullname polish, no changes required:
abbrev$fullname[match(orig_columns, abbrev$fullname)]

# abbrev english
colnames(synop) <- abbrev$abbr_ang[match(orig_columns, abbrev$fullname)]
head(synop)

# fullname english
colnames(synop) <- abbrev$fullname_ang[match(orig_columns, abbrev$fullname)]
head(synop)


# zastanowic sie nad usunieciem zduplikowanych kolumn (Np. nazwa stacji)
synop <- synop[,!duplicated(colnames(synop))]
head(synop)
