#' EH data (survey metadata) from Eggs and Larvae database
#'
#' @name EH
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)

# TODO: change to reading form web services
# url <- "https://eggsandlarvae.ices.dk/EggsAndLarvaeWebServices.asmx/getEggsAndLarvaeData?yearBegining=2008&yearEnd=2016&survey=MIK"
# out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

eh <-
  read.taf(
    taf.boot.path("initial/data/", "EH_EggsAndLarvaeDataSet20201019.csv"),
    colClasses = c("ELVolFlag" = "character")
  )

write.taf(eh)
