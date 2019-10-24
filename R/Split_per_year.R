
##Script to separate multiannual submission files into separate files for each year
# May 2019, Adriana Villamor

setwd("B:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MEGS/")

library(dplyr)
library(tidyverse)

# Initial settings

dat <- read.csv("AZTI 2018 Egg submission.csv", header=FALSE, stringsAsFactors=FALSE)
unique(dat$V14)
country <- "ES-PV"

years <- unique(dat$V19)
eh <- dat %>% filter(V1 == "EH")
em <- dat %>% filter(V1 == "EM")

# This loop will save the csv for each year in the working directory 
for(i in 1:length(years)){
        pattern <- years[i]
        country_year <- paste0(country, pattern)
        eh_sub <- eh %>% filter(V19 == pattern)
        em$V2 <- as.character(em$V2)
        em_sub <- em %>%  filter(str_detect(V2, country_year))
        sub <- rbind(eh_sub, em_sub)
        write.table(sub, paste0("By_year/", country_year, ".csv"),
                    na = "",
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE)
}


## Attention!!:
# I did not take into account different ships, so if you have different ships 
# for the same year you shoul also separate those in two files.