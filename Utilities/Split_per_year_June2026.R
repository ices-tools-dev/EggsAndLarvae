
##Script to separate multiannual submission files into separate files for each year
# June 2026, Maria Makri

setwd("P:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MIK/MariaM_ReUploadJune2026")

library(dplyr)
library(tidyverse)
library(readxl)
library(stringr)

# Initial settings

# dat <- read.csv("AZTI 2018 Egg submission.csv", header=FALSE, stringsAsFactors=FALSE)
# unique(dat$V14)
# country <- "ES-PV"

getwd()

list.files()


# head(MIK_alldat)
eh1<-read.csv("P:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MIK/Masterfiles_June2026/MIK1992_2025_Data_Stat.csv", sep=';', header = TRUE)
# Get the institute from the data in the database 

eh_dtb<- read.csv(paste0("P:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MIK/Masterfiles_June2026/EggsAndLarvae_EH_0715204393.csv")) 

eh <- eh1 %>%
        left_join(
                eh_dtb %>%
                        select(HaulID, Country, Ship, Year, SurveyPeriod, Institute),
                by = c("HaulID", "Country", "Ship", "Year")
        )

eh <- eh %>%
        rename(
                Notes = X,
                Distance = Distance.m,
                WireAngle = AngleOfWire,
                WireLength = LengthOfWire,
                NationalHaulID = Haul
        ) %>%
        rename_with(~ paste0(toupper(substr(., 1, 1)), substring(., 2))) %>%
        relocate(Institute, .after = Country) %>%
        relocate(Campaign, .after = 5) %>%
        relocate(SurveyPeriod, .after = Campaign) %>%
        mutate(
                Survey = ifelse(Survey == "MIK", "I4537", Survey),
                DepthBottom = NA_character_,
                SurTemp = NA_character_,
                Temp20m = NA_character_,
                Temp50m = NA_character_,
                Temp100m = NA_character_,
                BotTemp = NA_character_,
                SurSal = NA_character_,
                Sal20m = NA_character_,
                BotSal = NA_character_,
                GearDeployment = NA_character_,
                CodendMesh = NA_character_,
                ELHaulFlag = NA_character_,
                Netopening = NA_character_,
                FlowEfficiency = NA_character_,
                NetClogging = NA_character_,
                FlowmeterBrand = NA_character_,
                FlowExtRevs = NA_character_,
                FlowExtCalibr = NA_character_,
                VolumeFiltInt = NA_character_
        ) %>%
        relocate(
                DepthBottom, SurTemp, Temp20m, Temp50m, Temp100m,
                BotTemp, SurSal, Sal20m, BotSal,
                .after = Bdepth
        ) %>%
        relocate(Statrec, .after = BotSal) %>%
        relocate(GearDeployment, .after = Gear) %>%
        relocate(CodendMesh, .after = Meshtype) %>%
        relocate(ELHaulFlag, .after = HaulID) %>%
        relocate(NationalHaulID, .after = ELHaulFlag) %>%
        relocate(Netopening, .after = WireLength) %>%
        relocate(NetopeningArea, .after = Netopening) %>%
        relocate(FlowEfficiency, .after = NetopeningArea) %>%
        relocate(NetClogging, .after = FlowEfficiency) %>%
        relocate(FlowmeterBrand, .after = Flowmetertype) %>%
        relocate(FlowExtRevs, .after = FlowCalInt) %>%
        relocate(FlowExtCalibr, .after = FlowExtRevs) %>%
        relocate(VolumeFiltInt, .after = FlowExtCalibr) %>%
        replace(is.na(.), "")



# duplicates <- eh_dtb %>%
#         count(HaulID) %>%
#         filter(n > 1)
# 
# eh1 %>%
#         semi_join(duplicates, by = "HaulID") %>%
#         arrange(HaulID)


em <- read.csv("P:/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MIK/Masterfiles_June2026/MIK1992_2025_Data_Larvae.csv",sep=';', header = TRUE)

em <- em %>%
        rename(
                Lenth = length,
                RaisingFactor = SubSamplingFactor,
                Number = number
        ) %>%
        mutate(
                RecordType = "EM",
                Species = "Clupea harengus",
                ELSampleFlag = "",
                IndividualNumber = "",
                DevScale = "",
                DevStage = "",
                SpecIdentMethod = "",
                OilGlobulesFlag = "",
                OilGlobulesNumber = "",
                OilGlobulesDiam = "",
                PreservationMethod = "",
                Notes = ""
        ) %>%
        relocate(RecordType, .before = 1) %>%
        relocate(Species, .before = 3) %>%
        relocate(ELSampleFlag, .after = Species) %>%
        relocate(IndividualNumber, .after = ELSampleFlag) %>%
        relocate(DevScale, .after = Lenth) %>%
        relocate(DevStage, .after = DevScale) %>%
        relocate(SpecIdentMethod, .after = RaisingFactor) %>%
        relocate(OilGlobulesFlag, .after = SpecIdentMethod) %>%
        relocate(OilGlobulesNumber, .after = OilGlobulesFlag) %>%
        relocate(OilGlobulesDiam, .after = OilGlobulesNumber) %>%
        relocate(PreservationMethod, .after = OilGlobulesDiam) %>%
        relocate(Notes, .after = PreservationMethod)

em <- em %>%
        mutate(across(where(is.character), str_trim))

#eh <- DE_MIK %>% filter((V1 == "EH"))
# em <- em %>% filter((V1 == "EM"))
# unique(em$V3)
#Remove .sp from Species 

# em$V3 <- gsub(" sp.","", em$V3)
# em$V3 <- gsub('Sprattusattus','Sprattus sprattus', em$V3)
# em$V3 <- gsub('Argentinayraena','Argentina sphyraena', em$V3)
# em$V3 <- gsub('Branchiostoma lanceolatus','Branchiostoma lanceolatum', em$V3)
eh$Country <- trimws(eh$Country)

countries <- unique(eh$Country)
# countries[4] <- "SC"
# EM <- EM2
# EH$X53 <- NA 
# EH$X26 <- lapply(EH$X26, as.integer)

years <- unique(eh$Year)
# eh <- dat %>% filter(V1 == "EH")
# em <- dat %>% filter(V1 == "EM")

write.csv(
        eh,
        "D:/OneDrive - International Council for the Exploration of the Sea (ICES)/Profile/Desktop/Maria_ICES_dataflows/EggsAndLarvae_AV_MM_June2026/EggsAndLarvae/Utilities/Key_Comparisons/eh_masterfile_transformed.csv",
        row.names = FALSE
        
)# # This loop will save the csv for each country and year in the working directory 
# for(i in 1:length(countries)){
#         eh_sub <- eh %>% filter(Country == countries[i])
#         em$HaulID <- as.character(em$HaulID)
#         em_sub <- em %>%  filter(str_detect(HaulID, countries[i]))
#         years <- unique(eh_sub$Year)
#         for(j in 1: length(years)){
#                 pattern <- years[j]
#                 country_year <- paste0(pattern,countries[i])
#                 eh_sub2 <- eh_sub %>% filter(Year ==years[j])
#                 em_sub2 <- em_sub %>%  filter(str_detect(HaulID,country_year))
#                 sub <- bind_rows(eh_sub2, em_sub2)
#                 sub <- apply(sub,2,as.character)
#                 write.table(sub, paste0( country_year, ".csv"),
#                     na = "",
#                     sep = ",",
#                     col.names = TRUE,
#                     row.names = FALSE,
#                     quote = FALSE)
#         }
# }



# Add exception for Scotland 

for(i in 1:length(countries)){
        # EH country code
        country_code <- countries[i]
        # HaulID code (special case for Scotland)
        haul_code <- ifelse(country_code == "GB-SCT", "SC", country_code)
        eh_sub <- eh %>% filter(Country == country_code)
        em$HaulID <- as.character(em$HaulID)
        em_sub <- em %>% filter(str_detect(HaulID, haul_code))
        years <- unique(eh_sub$Year)
        for(j in 1:length(years)){
                pattern <- years[j]
                country_year <- paste0(pattern, haul_code)
                eh_sub2 <- eh_sub %>% filter(Year == years[j])
                em_sub2 <- em_sub %>% filter(str_detect(HaulID, country_year))
                sub <- bind_rows(eh_sub2, em_sub2)
                sub <- apply(sub, 2, as.character)
                write.table(
                        sub,
                        paste0(country_year, ".csv"),
                        na = "",
                        sep = ",",
                        col.names = TRUE,
                        row.names = FALSE,
                        quote = FALSE
                )
        }
}

outfile <- file.path(
        country_code,
        paste0(country_year, ".csv")
)

for(i in 1:length(countries)) {
        
        # EH country code
        country_code <- countries[i]
        
        # HaulID code (special case for Scotland)
        haul_code <- ifelse(country_code == "GB-SCT", "SC", country_code)
        
        # Create country folder if it does not exist
        country_folder <- file.path(getwd(), country_code)
        
        if (!dir.exists(country_folder)) {
                dir.create(country_folder, recursive = TRUE)
        }
        
        eh_sub <- eh %>% filter(Country == country_code)
        
        em$HaulID <- as.character(em$HaulID)
        em_sub <- em %>% filter(str_detect(HaulID, haul_code))
        
        years <- unique(eh_sub$Year)
        
        for(j in 1:length(years)) {
                
                pattern <- years[j]
                country_year <- paste0(pattern, haul_code)
                
                eh_sub2 <- eh_sub %>% filter(Year == years[j])
                em_sub2 <- em_sub %>% filter(str_detect(HaulID, country_year))
                
                outfile <- file.path(
                        country_folder,
                        paste0(country_year, ".csv")
                )
                
                # Write EH table
                write.table(
                        eh_sub2,
                        outfile,
                        na = "",
                        sep = ",",
                        col.names = TRUE,
                        row.names = FALSE,
                        quote = FALSE
                )
                
                # Blank line between tables
                # cat("\n", file = outfile, append = TRUE)
                
                # Write EM table with its own header
                write.table(
                        em_sub2,
                        outfile,
                        na = "",
                        sep = ",",
                        col.names = TRUE,
                        row.names = FALSE,
                        quote = FALSE,
                        append = TRUE
                )
        }
}





# for(i in 1:length(countries)) {
#         
#         # EH country code
#         country_code <- countries[i]
#         
#         # HaulID code (special case for Scotland)
#         haul_code <- ifelse(country_code == "GB-SCT", "SC", country_code)
#         
#         eh_sub <- eh %>% filter(Country == country_code)
#         
#         em$HaulID <- as.character(em$HaulID)
#         em_sub <- em %>% filter(str_detect(HaulID, haul_code))
#         
#         years <- unique(eh_sub$Year)
#         
#         for(j in 1:length(years)) {
#                 
#                 pattern <- years[j]
#                 country_year <- paste0(pattern, haul_code)
#                 
#                 eh_sub2 <- eh_sub %>% filter(Year == years[j])
#                 em_sub2 <- em_sub %>% filter(str_detect(HaulID, country_year))
#                 
#                 outfile <- paste0(country_year, ".csv")
#                 
#                 # Write EH table
#                 write.table(
#                         eh_sub2,
#                         outfile,
#                         na = "",
#                         sep = ",",
#                         col.names = TRUE,
#                         row.names = FALSE,
#                         quote = FALSE
#                 )
#                 
#                 # Blank line between tables
#                 cat("\n", file = outfile, append = TRUE)
#                 
#                 # Write EM table with its own header
#                 write.table(
#                         em_sub2,
#                         outfile,
#                         na = "",
#                         sep = ",",
#                         col.names = TRUE,
#                         row.names = FALSE,
#                         quote = FALSE,
#                         append = TRUE
#                 )
#         }
# }
# ``

toReuploadDepth <- read.csv("toReupload/toUploadDepth.csv")
colnames(toReuploadDepth) <- c('X','Y')
toReupload_ElVolFlag <- read.csv("toReupload/toreupload_ElVolFlag.csv")
colnames(toReupload_ElVolFlag) <- c('X','Y')
toReupload_Flow <- read.csv("toReupload/toUploadFlow.csv")
colnames(toReupload_Flow) <- c('X','Y')

toReupload<-rbind(toReuploadDepth,toReupload_ElVolFlag,toReupload_Flow)

toReupload<-as.data.frame(unique(toReupload$Y))
# for(country_code in countries){
#         
#         country_files <- list.files(country_code)
#         
#         matches <- intersect(country_files, files_to_copy)
#         
#         cat(country_code, ":", length(matches), "matches\n")
# }
# 


files_to_copy <- trimws(as.character(toReupload[[1]]))
files_to_copy <- paste0(files_to_copy, ".csv")

dir.create("ToReupload", showWarnings = FALSE)

for(country_code in countries) {
        
        source_folder <- file.path(getwd(), country_code)
        target_folder <- file.path(getwd(), "ToReupload", country_code)
        
        dir.create(
                target_folder,
                recursive = TRUE,
                showWarnings = FALSE
        )
        
        country_files <- list.files(
                source_folder,
                pattern = "\\.csv$",
                full.names = TRUE
        )
        
        matching_files <- country_files[
                basename(country_files) %in% files_to_copy
        ]
        
        if(length(matching_files) > 0) {
                
                file.copy(
                        from = matching_files,
                        to = file.path(
                                target_folder,
                                basename(matching_files)
                        ),
                        overwrite = TRUE
                )
                
                cat(country_code, ":", length(matching_files), "files copied\n")
        }
}




######### Τhis splits also per ship code, but it is the same number of files, so there is only one campaign per year and country

# for(i in seq_along(countries)) {
#         
#         # EH country code
#         country_code <- countries[i]
#         
#         # HaulID code (special case for Scotland)
#         haul_code <- ifelse(country_code == "GB-SCT", "SC", country_code)
#         
#         eh_sub <- eh %>%
#                 filter(Country == country_code)
#         
#         em$HaulID <- as.character(em$HaulID)
#         
#         years <- unique(eh_sub$Year)
#         
#         for(j in seq_along(years)) {
#                 
#                 current_year <- years[j]
#                 
#                 eh_year <- eh_sub %>%
#                         filter(Year == current_year)
#                 
#                 ships <- unique(eh_year$Ship)
#                 
#                 for(k in seq_along(ships)) {
#                         
#                         current_ship <- ships[k]
#                         
#                         eh_sub2 <- eh_year %>%
#                                 filter(Ship == current_ship)
#                         
#                         # Select corresponding EM records using EH HaulIDs
#                         haul_ids <- unique(as.character(eh_sub2$HaulID))
#                         
#                         em_sub2 <- em %>%
#                                 filter(HaulID %in% haul_ids)
#                         
#                         # Clean ship code for use in filename
#                         ship_code <- gsub("[^[:alnum:]_-]", "_", current_ship)
#                         
#                         # Output file name:
#                         # e.g. 2018_DK_DANA.csv
#                         outfile <- paste0(
#                                 current_year,
#                                 "_",
#                                 haul_code,
#                                 "_",
#                                 ship_code,
#                                 ".csv"
#                         )
#                         
#                         # Write EH table
#                         write.table(
#                                 eh_sub2,
#                                 outfile,
#                                 na = "",
#                                 sep = ",",
#                                 col.names = TRUE,
#                                 row.names = FALSE,
#                                 quote = FALSE
#                         )
#                         
#                         # Blank line between EH and EM tables
#                         cat("\n", file = outfile, append = TRUE)
#                         
#                         # Write EM table with header
#                         write.table(
#                                 em_sub2,
#                                 outfile,
#                                 na = "",
#                                 sep = ",",
#                                 col.names = TRUE,
#                                 row.names = FALSE,
#                                 quote = FALSE,
#                                 append = TRUE
#                         )
#                 }
#         }
# }

# unique(eh$Year)
# # This loop will save the csv for each year in the working directory 
# for(i in 1:length(years)){
#         pattern <- years[i]
#         country_year <- paste0(country, pattern)
#         eh_sub <- eh %>% filter(...19 == pattern)
#         em$...2 <- as.character(em$...2)
#         em_sub <- em %>%  filter(str_detect(...2, country_year))
#         sub <- rbind(eh_sub, em_sub)
#         write.table(sub, paste0("By_year/", country_year, ".csv"),
#                     na = "",
#                     sep = ",",
#                     col.names = FALSE,
#                     row.names = FALSE)
# }


## Attention!!:
# I did not take into account different ships, so if you have different ships 
# for the same year you shoul also separate those in two files.