#' Statrects in the NS IBTS survey
#'
#' @name ibts_rects
#' @format csv file
#' @tafOriginator ICES
#' @tafYear 2020
#' @tafAccess Public
#' @tafSource script

library(icesVocab)

details <-
  getCodeDetail(code = "NS-IBTS-DATRAS", code_type = "DatasetVer")
details_statrec <-
  details$children$codes[details$children$code_types$Key == "StatRec", ]

ibts_statrecs <-
  data.frame(
    statrec = details_statrec$Key
  )

write.taf(ibts_statrecs, file = "TAF-project/bootstrap/ibts_statrecs.csv",quote = TRUE)
