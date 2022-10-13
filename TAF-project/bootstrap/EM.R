#' EM data (sample record) from Eggs and Larvae database
#'
#' @name EM
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)

url <- "https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEM?yearBegining=1991&yearEnd=2022&survey=MIK"
out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

# em <-
#   read.taf(taf.boot.path("initial/data/", "EM_EggsAndLarvaeDataSet20201019.csv"))

em <- out

write.taf(em, file = "TAF-project/bootstrap/em.csv")
