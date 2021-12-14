#' EH data (survey metadata) from Eggs and Larvae database
#'
#' @name EH
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesTAF)

url <- "https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEH?yearBegining=1992&yearEnd=2021&survey=MIK"
out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

# eh <-
#   read.taf(
#     taf.boot.path("initial/data/", "EH_EggsAndLarvaeDataSet20201019.csv"),
#     colClasses = c("ELVolFlag" = "character")
#   )

eh <- out

write.taf(eh, file = "TAF-project/bootstrap/eh.csv")
