## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)

mkdir("data")

# read hauls meta-data
eh <- read.taf(taf.data.path("EH", "eh.csv"), colClasses = c("ELVolFlag" = "character")) %>% tibble()
# read herring sample data
em <- read.taf(taf.data.path("EM", "em.csv")) %>% tibble()

# process eh data
eh <-
  eh %>%
  select(
    HaulID, Year, StartLatitude, StartLongitude, statrec, DepthBottom, DepthLower,
    Distance, FlowIntRevs, FlowIntCalibr, NetopeningArea, ELVolFlag
  )

# process em data
em <-
  em %>%
  select(
    HaulID, Length, Number, SubFactor
  )

# clean NAs in data

# all number=NA set to 0
# TODO investigate why there are NAs, either remove or set to zero
em$Number[is.na(em$Number)] <- 0
em$SubFactor[is.na(em$SubFactor)] <- 1

# all missing DepthLower data replaced by DepthBottom - 5
eh$DepthLower[is.na(eh$DepthLower)] <- eh$DepthBottom - 5
# add statrec form lat and long?

# Warn about netopening area if NA

# merging of station data with length data, keep all eh records
ehm <- left_join(eh, em, by = "HaulID")

# here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)
ehm  <-
  subset(
    ehm,
    ehm$StartLongitude >= 9 |
    (ehm$StartLatitude < 54.0 & (ehm$Length > 18 | is.na(ehm$Length))) |
    (ehm$StartLatitude >= 54.0 & ehm$StartLongitude < 6.0) |
    (ehm$StartLatitude >= 57.0 & ehm$StartLongitude < 9.0) |
    (
      ehm$StartLongitude >= 6.0 & ehm$StartLongitude < 9.0 &
      ehm$StartLatitude >= 54.0 & ehm$StartLatitude < 57.0 &
      (ehm$Length > 18 | is.na(ehm$Length))
    )
  )

# write out cleaned, merged and filtered data
write.taf(ehm, dir = "data")




# sub area lookup table
# TODO - move to bootstrap possible
statrec_lookup <-
  unique(eh["statrec"]) %>%
  filter(statrec != "") %>%
  mutate(
    RecLat = substring(statrec, 1, 2),
    RecLong = substring(statrec, 3, 4),
    area = case_when(
      RecLong < "F2" & RecLat > 39 & RecLat < 46 ~ "cw",
      RecLong > "F1" & RecLat > 39 & RecLat < 46 ~ "ce",
      RecLong < "F2" & RecLat > 34 & RecLat < 40 ~ "sw",
      RecLong > "F1" & RecLat > 34 & RecLat < 40 ~ "se",
      RecLong < "F2" & RecLat > 45 ~ "nw",
      RecLong > "F1" & RecLat > 45 ~ "ne",
      RecLong > "F8" ~ "ka",
      RecLat < 35 & RecLat > 30 ~ "ch"
    ),
    af = case_when(
      area == "cw" ~ 28,
      area == "ce" ~ 33,
      area == "sw" ~ 12,
      area == "se" ~ 30,
      area == "nw" ~ 27,
      area == "ne" ~ 11,
      area == "ka" ~ 10,
      area == "ch" ~ 10
    )
  )


# write out lookup table
write.taf(statrec_lookup, dir = "data")
