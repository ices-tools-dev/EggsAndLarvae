## Run analysis, write model results

## Before:
## After:

library(icesTAF)
library(stringr)

mkdir("model")

# read hauls meta-data
Current_station <- read.taf("data/Current_station.csv", colClasses = c("ELVolFlag" = "character"))
# read herring sample data
indexALL <- read.taf("data/indexALL.csv")


# defining global variables "year" and "index" and file for MIK index time series (index_TS)
index_TS <- c("year", "index")
max_YR <- max(Current_station$Year)

# Initiaton of loop to calculate MIK index time series for all survey years since 1992

# i <- 2020
#for(i in 1992:max_YR) {
  #if (!any(Current_station$Year == i)) next

  # Here starts index calculation for each survey year
  # Subsetting for survey year
  indexSTAT <- subset(Current_station, Current_station$Year == i)
  indexSubset <- subset(indexALL, indexALL$year == i)

  # calculation of raised number per length class (raised by subsampling factor)
  indexSubset$NumberLarvae<-indexSubset$Number*indexSubset$SubFactor


  # calculation of number of herring larvae per MIK-station
  MIKindex_aggLV <-
    aggregate(
      indexSubset["NumberLarvae"],
      by = list(HaulID = indexSubset$HaulID), sum, na.rm = TRUE
    )

  # merging of aggregated larvae data (sum) with relevant station data
  Vars_sel <-
    c(
      "HaulID", "StartLatitude", "StartLongitude", "statrec", "DepthBottom", "DepthLower",
      "Distance", "FlowIntRevs", "FlowIntCalibr", "NetopeningArea", "ELVolFlag"
    )

  MIKindex_StatDat <- indexSTAT[Vars_sel]
  MIKindex_sumLV <- merge(MIKindex_StatDat, MIKindex_aggLV, by = "HaulID", all.x = TRUE)
  # if no lenght data for a station (NA) then no larvae seen
  #MIKindex_sumLV$NumberLarvae[is.na(MIKindex_sumLV$NumberLarvae)] <- 0

  # from here onwards calculation of index as in IndexCalculation2015.R

  # Calculation Number herring larvae per m2
  MIKindex_sumLV$L.sqm <-
    ifelse(
      MIKindex_sumLV$ELVolFlag == "F",
      MIKindex_sumLV$NumberLarvae * MIKindex_sumLV$DepthLower * MIKindex_sumLV$FlowIntCalibr / (MIKindex_sumLV$FlowIntRevs * MIKindex_sumLV$NetopeningArea),
      MIKindex_sumLV$NumberLarvae * MIKindex_sumLV$DepthLower / (MIKindex_sumLV$Distance * MIKindex_sumLV$NetopeningArea)
    )


  # allocation of subareas
  MIKindex_sumLV$RecLat<-str_sub(MIKindex_sumLV$statrec,1,2)
  MIKindex_sumLV$RecLong<-str_sub(MIKindex_sumLV$statrec,-2,-1)
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"cw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"ce"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"sw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"se"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>45]<-"nw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>45]<-"ne"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F8"]<-"ka"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLat<"35" & MIKindex_sumLV$RecLat>"30"]<-"ch"



  # Index calculation: Extraction of necessary variables Rectangle, area und L.sqm
  VarIndex<-c("statrec","area","L.sqm")
  MIKindex_calc<-MIKindex_sumLV[VarIndex]

  # Calculation of mean herring larvae abundance per rectangle
  aggRect<-aggregate(MIKindex_calc, by=list(Rectangle=MIKindex_calc$statrec,suba=MIKindex_calc$area), mean)

  # Creation and writing of table with mean abundance per rectangle
  aggRect$year<-i

  IBTS_Rects<-read.table("IBTS_Rects.txt",header=TRUE,sep=",")
  names(aggRect)[4]="dummy"

  MIK_RAbun<-merge(IBTS_Rects,aggRect,by="Rectangle",all.y=TRUE)

  write.table(MIK_RAbun,"RectAbun.txt",append=TRUE, sep=",")

  # extraction of data for mean abundance per Subarea
  A_Vars<-c("suba","L.sqm")
  MIKindex_byA<-aggRect[A_Vars]

  # calculation of mean abundance per subarea
  aggArea<-aggregate(MIKindex_byA, by=list(area=MIKindex_byA$suba), mean)

  # factors for index calculation by subarea
  aggArea$af[aggArea$area=="cw"]<-28
  aggArea$af[aggArea$area=="ce"]<-33
  aggArea$af[aggArea$area=="sw"]<-12
  aggArea$af[aggArea$area=="se"]<-30
  aggArea$af[aggArea$area=="nw"]<-27
  aggArea$af[aggArea$area=="ne"]<-11
  aggArea$af[aggArea$area=="ka"]<-10
  aggArea$af[aggArea$area=="ch"]<-10

  aggArea$year<-i

  # writing of table with subarea abundances
  write.table(aggArea,"aggArea.txt",append=TRUE, sep=",")

  # caculation of total herring larvae abundance per subarea
  aggArea$miksec<-aggArea$L.sqm*aggArea$af*3086913600

  # calculation of Index
  index<-sum(aggArea$miksec)/10**9

  # delivery of Index

  index_y<-c(i,index)
  index_TS<-rbind(index_TS,index_y)

  # counting hauls per rectangle
  AnzRects<-table(MIKindex_calc$statrec)
  write.table(AnzRects,"AnzRects.txt",append=TRUE, sep=",")

# }

# writing index time series table
write.table(index_TS,"index_TS.txt",sep=",")
