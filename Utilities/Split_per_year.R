
##Script to separate multiannual submission files into separate files for each year
# May 2019, Adriana Villamor

setwd("B:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/")

library(dplyr)
library(tidyverse)

# Initial settings

# dat <- read.csv("AZTI 2018 Egg submission.csv", header=FALSE, stringsAsFactors=FALSE)
# unique(dat$V14)
# country <- "ES-PV"

head(MIK_alldat)

eh <- MIK_alldat %>% filter((X1 == "EH"))
em <- MIK_alldat %>% filter((X1 == "EM"))
countries <- unique(EH$X3)
countries[4] <- "SC"
EM <- EM2
EH$X53 <- NA 
EH$X26 <- lapply(EH$X26, as.integer)



# years <- unique(dat$V19)
# eh <- dat %>% filter(V1 == "EH")
# em <- dat %>% filter(V1 == "EM")

# This loop will save the csv for each country and year in the working directory 
for(i in 1:length(countries)){
        eh_sub <- EH %>% filter(X3 == countries[i])
        EM$X2 <- as.character(EM$X2)
        em_sub <- EM %>%  filter(str_detect(X2, countries[i]))
        years <- unique(eh_sub$X19)
        for(j in 1: length(years)){
                pattern <- years[j]
                country_year <- paste0(pattern,countries[i])
                eh_sub2 <- eh_sub %>% filter(X19 == years[j])
                em_sub2 <- em_sub %>%  filter(str_detect(X2, country_year))
                sub <- bind_rows(eh_sub2, em_sub2)
                sub <- apply(sub,2,as.character)
                write.table(sub, paste0("MIK/", country_year, ".csv"),
                    na = "",
                    sep = ",",
                    col.names = FALSE,
                    row.names = FALSE,
                    quote = FALSE)
        }
}
        

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