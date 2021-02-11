## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(stringr)
library(dplyr)

mkdir("model")

# read herring sample data
ehm <- read.taf("data/ehm.csv", colClasses = c("ELVolFlag" = "character")) %>% tibble()
# statrec lookup
statrec_lookup <- read.taf("data/statrec_lookup.csv")
# ibts stat recs
ibts_statrecs <- read.taf(taf.data.path("ibts_rects", "ibts_statrecs.csv"))




# Initiaton of loop to calculate MIK index time series for all survey years since 1992

# Here starts index calculation

# calculation of raised number of herring larvae per MIK-station (raised by subsampling factor per length class)
ehm_byhaul <-
  ehm %>%
  group_by(
    HaulID, Year, StartLatitude, StartLongitude, statrec, DepthBottom, DepthLower,
    Distance, FlowIntRevs, FlowIntCalibr, NetopeningArea, ELVolFlag
  ) %>%
  summarise(
    NumberLarvae = sum(Number * SubFactor, na.rm = TRUE),
    .groups = "drop"
  )

# from here onwards calculation of index as in IndexCalculation2015.R

# Calculation Number herring larvae per m2
ehm_byhaul <-
  ehm_byhaul %>%
  mutate(
    L.sqm = ifelse(
      ELVolFlag == "F",
      NumberLarvae * DepthLower * FlowIntCalibr / (FlowIntRevs * NetopeningArea),
      NumberLarvae * DepthLower / (Distance * NetopeningArea)
    )
  )

ehm_byhaul <-
  left_join(ehm_byhaul, statrec_lookup, by = "statrec")

# Calculation of mean herring larvae abundance per rectangle
ehm_byrect <-
  ehm_byhaul %>%
  group_by(Year, statrec, area, af) %>%
  summarise(
    L.sqm = mean(L.sqm, na.rm = TRUE),
    .groups = "drop"
  )

# filter for IBTS squares
RectAbun <-
  ehm_byrect %>%
  filter(statrec %in% ibts_statrecs$statrec)

write.taf(RectAbun, dir = "model")

# calculation of mean abundance per subarea, *not* filtered by IBTS squares
ehm_by_area <-
  ehm_byrect %>%
  group_by(
    Year, area, af
  ) %>%
  summarise(
    L.sqm = mean(L.sqm, na.rm = TRUE),
    .groups = "drop"
  )

aggArea <- ehm_by_area
# writing of table with subarea abundances
write.taf(aggArea, dir = "model")

# caculation of total herring larvae abundance per subarea per year
index_by_year <-
  ehm_by_area %>%
  mutate(
    miksec = L.sqm * af * 3086913600
  ) %>%
  group_by(Year) %>%
  summarise(
    index = sum(miksec, na.rm = TRUE) * 10^-9
  )

# writing index time series table
# TOD check format
write.taf(index_by_year, dir = "model")
