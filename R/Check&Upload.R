
# Script to send files to DATSU and screen errors and to uploade in bulk to the database
# Colin Millar and Adriana Villamor, September 2019

library(dplyr)
library(tidyverse)

## Load function

screenFile <- function(filename, email, datatype, quiet = FALSE) {
  require(httr)
  # form content
  content <-
    list(
      Request =  sprintf("{FileName:'%s', EmailAddress:'%s', DataType:'%s'}", basename(filename), email, datatype),
      File = upload_file(filename)
    )
  
  # message
  if (!quiet) {
    message("POSTing ...  ", basename(filename))
  }
  
  # perform request
  x <- POST("http://datsu.ices.dk/DatsuRest/api/ScreenFile", 
            body = content, 
            encode = "multipart")
  
  # get results
  content(x)
}

## Set the directory containing the files to be checked

fnames <- dir("//fs.ices.local/projects/DP1/projects/EggLarvae_Database/Data/NEW FORMAT/MEGS/By_year", full = TRUE)

# set email of submitter

my_email = "youremail@wherever.com"

res <- 
  do.call(
    rbind,
    lapply(
      fnames[],
      screenFile,
      email = my_email,
      datatype = "EGGSANDLARVAE"
    ))

res <-as.data.frame(apply(res, 2, unlist))

# If Number of errors is different from "-1", means there are some errors.
# Check calling the row:

check <- res %>% filter(NumberOfErrors != "-1")


#select each row one by one
url <- check$ScreenResultURL[3]

url <- as.character(url)

# Check DATSU reports for each one
utils::browseURL(url, browser = "C:/Program Files (x86)/Google/Chrome/Application/chrome.exe")

## repeat this step for each datsu report with some error, and fix whatever necessary



## Those with no errors can be uploaded

ready <- res %>% filter(NumberOfErrors == "-1")

# In this link you have to substitute the username and token
# USERNAME = your ices user name
# TOKEN = contact ices for the token: carlos@ices.dk 
ready$upload_link <- paste0("http://eggsandlarvae.ices.dk/EggsAndLarvaeWebServices.asmx/uploadEggsAndLarvaeFile?user=USERNAME&token=TOKEN&datsuSessionID=", ready$SessionID)


#Uploading to the database

for(i in 1:nrow(ready)){
        httr::GET(ready$upload_link)}





