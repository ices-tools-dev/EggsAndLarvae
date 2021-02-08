## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(stringr)

mkdir("data")

# read hauls meta-data
Current_station <- read.taf(taf.data.path("EH", "eh.csv"), colClasses = c("ELVolFlag" = "character"))
# read herring sample data
Current_lengths <- read.taf(taf.data.path("EM", "em.csv"))

# creating of variable "year" in length data
Current_lengths$year <- as.numeric(str_sub(Current_lengths$HaulID, 1, 4))

# clean NAs in data

# all number=NA set to 0
# TODO investigate why there are NAs, either remove or set to zero
Current_lengths$Number[is.na(Current_lengths$Number)] <- 0
Current_lengths$SubFactor[is.na(Current_lengths$SubFactor)] <- 1
Current_lengths$Number <- as.numeric(Current_lengths$Number)
Current_lengths$SubFactor <- as.numeric(Current_lengths$SubFactor)

# all missing DepthLower data replaced by DepthBottom - 5
Current_station$DepthLower[is.na(Current_station$DepthLower)] <- Current_station$DepthBottom - 5

# Warn about netopening area if NA

# merging of station data with length data
indexALL <-
  merge(
    Current_station[c("HaulID", "StartLatitude", "StartLongitude")],
    Current_lengths,
    by = "HaulID", all.x = TRUE
  )

# here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)
indexALL  <-
  subset(
    indexALL,
    indexALL$StartLongitude >= 9 |
    (indexALL$StartLatitude < 54.0 & (indexALL$Length > 18 | is.na(indexALL$Length))) |
    (indexALL$StartLatitude >= 54.0 & indexALL$StartLongitude < 6.0) |
    (indexALL$StartLatitude >= 57.0 & indexALL$StartLongitude < 9.0) |
    (indexALL$StartLongitude >= 6.0 & indexALL$StartLongitude < 9.0 & indexALL$StartLatitude >= 54.0 & indexALL$StartLatitude < 57.0 & (indexALL$Length > 18 | is.na(indexALL$Length)))
  )


# write out cleaned, merged and filtered data
write.taf(Current_station, dir = "data")
write.taf(indexALL, dir = "data")
