#' EM data (sample record) from Eggs and Larvae database
#'
#' @name EM
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)

# TODO: change to reading form web services
# url <- "https://eggsandlarvae.ices.dk/EggsAndLarvaeWebServices.asmx/getEggsAndLarvaeData?yearBegining=2008&yearEnd=2016&survey=MIK"
# out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

em <-
  read.taf(taf.boot.path("initial/data/", "EM_EggsAndLarvaeDataSet20201019.csv"))

write.taf(em)
