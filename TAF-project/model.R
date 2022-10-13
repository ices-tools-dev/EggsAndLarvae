## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(stringr)
library(dplyr)

mkdir("TAF-project/model")

# read herring sample data
ehm <- read.taf("TAF-project/data/ehm.csv") %>% tibble()
# statrec lookup
statrec_lookup <- read.taf("TAF-project/data/statrec_lookup.csv")
# ibts stat recs
ibts_statrecs <- read.taf("TAF-project/bootstrap/ibts_statrecs.csv")




# Initiaton of loop to calculate MIK index time series for all survey years since 1992

# Here starts index calculation

# calculation of raised number of herring larvae per MIK-station (raised by subsampling factor per length class)
# ehm$subFactor <- as.integer(ehm$subFactor)

ehm_byhaul <-
  ehm %>%
  group_by(
    haulId, year, startLatitude, startLongitude, statrec, depthBottom, depthLower,
    distance, flowIntRevs, flowIntCalibr, netopeningArea, elvolFlag
  ) %>%
  summarise(
    NumberLarvae = sum(number * subFactor, na.rm = TRUE),
    .groups = "drop"
  )

# from here onwards calculation of index as in IndexCalculation2015.R

# Calculation Number herring larvae per m2


ehm_byhaul <-
  ehm_byhaul %>%
  mutate(
    L.sqm = ifelse(
      elvolFlag == "F",
      NumberLarvae * depthLower * flowIntCalibr / (flowIntRevs * netopeningArea),
      NumberLarvae * depthLower / (distance * netopeningArea)
    )
  )



ehm_byhaul <-
  left_join(ehm_byhaul, statrec_lookup, by = "statrec")

# Calculation of mean herring larvae abundance per rectangle
ehm_byrect <-
  ehm_byhaul %>%
  group_by(year, statrec, area, af) %>%
  summarise(
    L.sqm = mean(L.sqm, na.rm = TRUE),
    .groups = "drop"
  )

# ehm_byrect$L.sqm[ehm_byrect$L.sqm == "NaN"]<-0
ehm_byrect$L.sqm[ehm_byrect$L.sqm == "Inf"]<-0


# filter for IBTS squares
RectAbun <-
  ehm_byrect %>%
  filter(statrec %in% ibts_statrecs$statrec)

write.taf(RectAbun, dir = "TAF-project/model")

# calculation of mean abundance per subarea, *not* filtered by IBTS squares
ehm_by_area <-
  ehm_byrect %>%
  group_by(
    year, area, af
  ) %>%
  summarise(
    L.sqm = mean(L.sqm, na.rm = TRUE),
    .groups = "drop"
  )


#AV, remove rows with no area attributed:

ehm_by_area <- na.omit(ehm_by_area) 

aggArea <- ehm_by_area
# writing of table with subarea abundances
write.taf(aggArea, dir = "TAF-project/model")

# caculation of total herring larvae abundance per subarea per year
index_by_year <-
  ehm_by_area %>%
  mutate(
    miksec = L.sqm * af * 3086913600
  ) %>%
  group_by(year) %>%
  summarise(
    index = sum(miksec, na.rm = TRUE) * 10^-9
  )

# writing index time series table
# TOD check format
write.taf(index_by_year, dir = "TAF-project/model")

