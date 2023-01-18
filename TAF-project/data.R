## Preprocess data, write TAF data tables

## Before:
## After:

library(icesTAF)
library(dplyr)
library(stringr)

mkdir("TAF-project/data")

# read hauls meta-data
eh <- read.taf("TAF-project/bootstrap/eh.csv") %>% tibble()
# read herring sample data
em <- read.taf("TAF-project/bootstrap/em.csv") %>% tibble()

# process eh data
eh <-
  eh %>%
  select(
    haulId, year, startLatitude, startLongitude, statrec, depthBottom, depthLower,
    distance, flowIntRevs, flowIntCalibr, netopeningArea, elvolFlag
  )

eh <- unique(eh)

# process em data
em <-
  em %>%
  select(
    haulId, length, number, subFactor, species
  )

# Select only herring data

em <-
  em %>% filter(species == "Clupea harengus")
em <- unique(em)

# clean NAs in data

# all number=NA set to 0
# TODO investigate why there are NAs, either remove or set to zero


#MAtthias suggests to move this to after the eh and em merging
em$number[is.na(em$number)] <- 0
em$subFactor[is.na(em$subFactor)] <- 1

# all missing DepthLower data replaced by DepthBottom - 5
eh$depthBottom <- as.numeric(eh$depthBottom)
eh$depthLower <- as.numeric(eh$depthLower)
eh$depthLower[is.na(eh$depthLower)] <- eh$depthBottom - 5
# add statrec form lat and long?

#Jan 2023: selection of variables for application of exclusion rule

eh_excl <-
        eh %>%
        select(
                haulId, startLatitude, startLongitude
        )

# em <-
#         em_mk %>%
#         select(
#                 HaulID, length, number, SubSamplingFactor
#         )

# em_mk$number[is.na(em_mk$number)] <- 0
# em_mk$SubSamplingFactor[is.na(em_mk$SubSamplingFactor)] <- 1 


# Jan 2023 start application of exclusion rule

str(em)
em$length <- as.numeric(em$length)


ehm_excl <- left_join(eh_excl, em, by = "haulId")

ehm_excl <-
        subset(
                ehm_excl,
                ehm_excl$startLongitude >= 9 |
                        (ehm_excl$startLatitude < 54.0 & (ehm_excl$length > 18 | is.na(ehm_excl$length))) |
                        (ehm_excl$startLatitude >= 54.0 & ehm_excl$startLongitude < 6.0) |
                        (ehm_excl$startLatitude >= 57.0 & ehm_excl$startLongitude < 9.0) |
                        (
                                ehm_excl$startLongitude >= 6.0 & ehm_excl$startLongitude < 9.0 &
                                        ehm_excl$startLatitude >= 54.0 & ehm_excl$startLatitude < 57.0 &
                                        (ehm_excl$length > 18 | is.na(ehm_excl$length))
                        )
        )

ehm_excl$number[is.na(ehm_excl$number)] <- 0
ehm_excl$subFactor[is.na(ehm_excl$subFactor)] <- 1 



# selection of larvae data variables from above results for the index calculation

ehm_select <-
        ehm_excl %>%
        select(
                haulId, length, number, subFactor
        )

ehm <- left_join(eh, ehm_select, by = "haulId")

# replace all NAs with 0 to obtain zero catches - this is necessary!!

ehm$number[is.na(ehm$number)] <- 0
ehm$subFactor[is.na(ehm$subFactor)] <- 1







# Warn about netopening area if NA

# merging of station data with length data, keep all eh records
# ehm <- left_join(eh, em, by = "haulId")

# ehm$number[is.na(ehm$number)] <- 0
# ehm$subFactor[is.na(ehm$subFactor)] <- 1




# here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)

#put Kateggat and Skagerrak apart:

# ehm2<- ehm %>% filter(ehm$startLongitude >= 9)
# 
# ehm<- ehm %>% filter(ehm$startLongitude < 9)
# 
# ehm3 <- ehm %>% filter(ehm$startLatitude < 54.0)
# 
# ehm <- ehm %>% filter(ehm$startLatitude >= 54.0)
# 
# ehm4 <- ehm %>% filter(ehm$startLatitude < 57.0 | ehm$startLongitude >= 6.0 & ehm$startLongitude < 9.0)
# 
# 
# ehm<-subset(ehm,(ehm$startLatitude>=54.0 &ehm$startLongitude<6.0)|(ehm$startLatitude>=57.0 &ehm$startLongitude<9.0)|ehm$startLongitude>=9|(ehm$startLatitude<54.0 &(ehm$length>18|is.na(ehm$length)))|(ehm$startLongitude>=6.0&ehm$startLongitude<9.0&ehm$startLatitude>=54.0&ehm$startLatitude<57.0&(ehm$length>18|is.na(ehm$length))))



# ehm  <-
#   subset(
#     ehm,
#     ehm$startLongitude >= 9 |
#     (ehm$startLatitude < 54.0 & (ehm$length > 18 | is.na(ehm$length))) |
#     (ehm$startLatitude >= 54.0 & ehm$startLongitude < 6.0) |
#     (ehm$startLatitude >= 57.0 & ehm$startLongitude < 9.0) |
#     (
#       ehm$startLongitude >= 6.0 & ehm$startLongitude < 9.0 &
#       ehm$startLatitude >= 54.0 & ehm$startLatitude < 57.0 &
#       (ehm$length > 18 | is.na(ehm$length))
#     )
#   )

# write out cleaned, merged and filtered data
write.taf(ehm, dir = "TAF-project/data")




# sub area lookup table
# TODO - move to bootstrap possible

#THIS IS NICE BUT IS NOT WORKING
# statrec_lookup <-
#   unique(eh["statrec"]) %>%
#   filter(statrec != "") %>%
#   mutate(
#     RecLat = substring(statrec, 1, 2),
#     RecLong = substring(statrec, 3, 4),
#     area = case_when(
#       RecLong < "F2" & RecLat > 39 & RecLat < 46 ~ "cw",
#       RecLong > "F1" & RecLat > 39 & RecLat < 46 ~ "ce",
#       RecLong < "F2" & RecLat > 34 & RecLat < 40 ~ "sw",
#       RecLong > "F1" & RecLat > 34 & RecLat < 40 ~ "se",
#       RecLong < "F2" & RecLat > 45 ~ "nw",
#       RecLong > "F1" & RecLat > 45 ~ "ne",
#       RecLong > "F8" ~ "ka",
#       RecLat < 35 & RecLat > 30 ~ "ch"
#     ),
#     af = case_when(
#       area == "cw" ~ 28,
#       area == "ce" ~ 33,
#       area == "sw" ~ 12,
#       area == "se" ~ 30,
#       area == "nw" ~ 27,
#       area == "ne" ~ 11,
#       area == "ka" ~ 10,
#       area == "ch" ~ 10
#     )
#   )


#OLD FASHIONED WAY:

statrec_lookup <-
  unique(eh["statrec"]) %>%
  filter(statrec != "")
statrec_lookup$RecLat<-str_sub(statrec_lookup$statrec,1,2)
statrec_lookup$RecLong<-str_sub(statrec_lookup$statrec,-2,-1)
statrec_lookup$area <- NA
statrec_lookup$area[statrec_lookup$RecLong<"F2" & statrec_lookup$RecLat>39 & statrec_lookup$RecLat<46]<-"cw"
statrec_lookup$area[statrec_lookup$RecLong>"F1" & statrec_lookup$RecLat>39 & statrec_lookup$RecLat<46]<-"ce"
statrec_lookup$area[statrec_lookup$RecLong<"F2" & statrec_lookup$RecLat>34 & statrec_lookup$RecLat<40]<-"sw"
statrec_lookup$area[statrec_lookup$RecLong>"F1" & statrec_lookup$RecLat>34 & statrec_lookup$RecLat<40]<-"se"
# statrec_lookup$area[statrec_lookup$RecLong<"F2" & statrec_lookup$RecLat>45]<-"nw"
statrec_lookup$area[statrec_lookup$RecLong>"E5"& statrec_lookup$RecLong<"F2" & statrec_lookup$RecLat>45]<-"nw"
statrec_lookup$area[statrec_lookup$RecLong>"F1" & statrec_lookup$RecLat>45]<-"ne"
statrec_lookup$area[statrec_lookup$RecLong>"F8"]<-"ka"
statrec_lookup$area[statrec_lookup$RecLat<"35" & statrec_lookup$RecLat>"30"]<-"ch"

statrec_lookup$af <- NA
statrec_lookup$af[statrec_lookup$area=="cw"]<-28
statrec_lookup$af[statrec_lookup$area=="ce"]<-33
statrec_lookup$af[statrec_lookup$area=="sw"]<-12
statrec_lookup$af[statrec_lookup$area=="se"]<-30
statrec_lookup$af[statrec_lookup$area=="nw"]<-27
statrec_lookup$af[statrec_lookup$area=="ne"]<-11
statrec_lookup$af[statrec_lookup$area=="ka"]<-10
statrec_lookup$af[statrec_lookup$area=="ch"]<-10



# write out lookup table
write.taf(statrec_lookup, dir = "TAF-project/data")

