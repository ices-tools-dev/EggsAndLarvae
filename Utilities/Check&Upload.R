
# Script to send files to DATSU and screen errors and to uploade in bulk to the database
# Colin Millar and Adriana Villamor, September 2019

library(dplyr)
library(tidyverse)
#install.packages("icesDatsu")
library(icesDatsu)

## Set the directory containing the files to be checked

fnames <- dir("//fs.ices.local/projects/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MIK/MariaM_ReUploadJune2026/ELVolFlag", full = TRUE)


require(icesConnect)
icesConnect::set_username("maria.makri")
icesConnect:::token_set_from_keyring("eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJodHRwOi8vc2NoZW1hcy54bWxzb2FwLm9yZy93cy8yMDA1LzA1L2lkZW50aXR5L2NsYWltcy9uYW1lIjoibWFyaWEubWFrcmlAaWNlcy5kayIsImp0aSI6IjUyMGU0ZTRkLTA1MTAtNGRmYy1hZTBjLTIwNDkwNzcxZTQ5NSIsImh0dHA6Ly9zY2hlbWFzLnhtbHNvYXAub3JnL3dzLzIwMDUvMDUvaWRlbnRpdHkvY2xhaW1zL2VtYWlsYWRkcmVzcyI6Im1hcmlhLm1ha3JpQGljZXMuZGsiLCJVc2VyRW1haWwiOiJtYXJpYS5tYWtyaUBpY2VzLmRrIiwiRW1haWwiOiJtYXJpYS5tYWtyaUBpY2VzLmRrIiwiZXhwIjoxNzkxOTcyNTc3LCJpc3MiOiJodHRwOi8vdGFmLmljZXMuZGsiLCJhdWQiOiJodHRwOi8vdGFmLmljZXMuZGsifQ.ytAJ5Hqkt96pNCd9o4kh_DBwy9psI1ifMLmbU3HoX-8", "maria.makri")


# set email of submitter

#Go to the ices page to generate a token


res <- 
    sapply(
      fnames[],
      uploadDatsuFileFireAndForget,
      dataSetVerID = 130
    )

#Το look at errors and messages of the screening result use these 
# getScreeningSessionDetails(id)
# messages <- getScreeningSessionMessages(id)


# If Number of errors is different from "-1", means there are some errors.
# Check calling the row:

check <- res %>% filter(NumberOfErrors != "-1")


#select each row one by one
url <- check$ScreenResultURL[1]

url <- as.character(url)

# Check DATSU reports for each one
utils::browseURL(url, browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

## repeat this step for each datsu report with some error, and fix whatever necessary



## Those with no errors can be uploaded

ready <- res %>% filter(NumberOfErrors == "-1")

# In this link you have to substitute the username and token
# USERNAME = your ices user name
# TOKEN = contact ices for the token: carlos@ices.dk 
ready$upload_link <- paste0("https://eggsandlarvae.ices.dk/EggsAndLarvaeWebServices.asmx/uploadEggsAndLarvaeFile?user=adriana.villamor&token=TOKEN&datsuSessionID=", ready$SessionID)


#Uploading to the database

for(i in 1:nrow(ready)){
        httr::GET(ready$upload_link)}





