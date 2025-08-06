# opens Module stringr
library(stringr)

#din arbejdsmappe, lav desuden mapper heri der hedder "data" og "resultater
#læg filer der skal indlæses i data mappen
wd <- "Q:/20-forskning/20-dfad/users/jostou/home/25_04_25_sildelarve_database/"

# reading of station data
#url <-"https://eggsandlarvae.ices.dk/webservices/getEggsAndLarvaeData?yearBegining=2008&yearEnd=2016&month=&stage=&survey=MIK&species=&lastModifiedDate="

#url_historical<-"https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeData?Year=1991"
#hist <- jsonlite::fromJSON(url_historical, simplifyDataFrame = TRUE)

url_EH <-"https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEH?YearBegining=2024&SurveyCode=14537"
MIK_Station1 <- jsonlite::fromJSON(url_EH, simplifyDataFrame = TRUE)

url_EM <-"https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEM?YearBegining=2024&SurveyCode=14537"
MIKindex_lengths <- jsonlite::fromJSON(url_EM, simplifyDataFrame = TRUE)

#un<-as.data.frame(unique(MIKindex_lengths$haulId))

#MIK_Station<-read.csv(paste0(wd, "data/EggsAndLarvae_EH_0425395059.csv")) 
MIK_Station <- MIK_Station1[is.na(MIK_Station1$elhaulFlag), ]
# define Distance,FlowIntRevs,FlowIntCalibr as.numeric

MIK_Station$distance<-as.numeric(as.character(MIK_Station$distance))
MIK_Station$flowIntRevs<-as.numeric(as.character(MIK_Station$flowIntRevs))
MIK_Station$flowIntCalibr<-as.numeric(as.character(MIK_Station$flowIntCalibr))

# Reading of herring larvae data (numbers per Length class, unmeasured, subsampling factor)
#MIKindex_lengths<-read.csv(paste0(wd, "data/EggsAndLarvae_EM_0425395059.csv"))
MIKindex_lengths <- MIKindex_lengths[MIKindex_lengths$species == "Clupea harengus", ]

# creating of variable "year" in length data
MIKindex_lengths$year<-str_sub(MIKindex_lengths$haulId,1,4)
MIKindex_lengths$year<-as.numeric(MIKindex_lengths$year)
MIKindex_lengths$length<-as.numeric(MIKindex_lengths$length)

# defining global variables "year" and "index" and file for MIK index time series (index_TS) 
year<-"year"
index<-"index"
index_TS<-c(year,index)
max_YR<-max(MIK_Station$year)

# Initiaton of loop to calculate MIK index time series for all survey years since 1992

for(i in 1992:max_YR) {
  print(i)
  # Here starts index calculation for each survey year
  # Subsetting for survey year
  indexSTAT<-subset(MIK_Station,MIK_Station$year==i)
  indexHER<-subset(MIKindex_lengths,MIKindex_lengths$year==i)
  
  varPOS<-c("haulId","startLatitude","startLongitude")
  indexPOS<-indexSTAT[varPOS]
  
  # merging of station data with lenght data
  indexALL<-merge(indexPOS,indexHER, by=c("haulID"), all.x=TRUE)
  
  # here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)
  
  indexSubset<-
    subset(indexALL,
           (indexALL$startLatitude>=54&indexALL$startLongitude<6)
           |(indexALL$startLatitude>=57&indexALL$startLongitude<9)
           |indexALL$startLongitude>=9
           |(indexALL$startlatitude<54&(indexALL$length>18
                                        |is.na(indexALL$length)))
           |(indexALL$startlongitude>=6&indexALL$startlongitude<9&indexALL$startlatitude>=54&indexALL$startlatitude<57&(indexALL$length>18|is.na(indexALL$length))))
  
  # all number=NA set to 0 
  indexSubset$number[is.na(indexSubset$number)]<-0
  indexSubset$number<-as.numeric(indexSubset$number)
  
  indexSubset$raisingFactor[is.na(indexSubset$raisingFactor)]<-1
  indexSubset$raisingFactor<-as.numeric(indexSubset$raisingFactor)
  
  # calculation of raised Number per Length class (raised by subsampling factor)
  
  indexSubset$numberLarvae<-indexSubset$number*indexSubset$raisingFactor
  
  # calculation of number of herring larvae per MIK-station
  MIKindex_aggLV<-aggregate(subset(indexSubset,select=c("NumberLarvae")),
                            by=list(indexSubset$haulID),sum)
  
  names(MIKindex_aggLV)[1]<-"haulID"
  
  
  # merging of aggregated larvae data (sum) with relevant station data
  Vars_sel<-c("haulID","startLatitude","startLongitude","statrec","depthBottom",
              "depthLower","distance","flowIntRevs","flowIntCalibr","netopeningArea","eLVolFlag")
  
  MIKindex_StatDat<-indexSTAT[Vars_sel]
  MIKindex_sumLV<-merge(MIKindex_StatDat,MIKindex_aggLV,by="haulID",all.x=TRUE)
  
  # all NumberLarvae=NA set to 0
  MIKindex_sumLV$NumberLarvae[is.na(MIKindex_sumLV$NumberLarvae)]<-0
  
  # all missing DepthLower data replaced by DepthBottom - 5 
  MIKindex_sumLV$depthLower[is.na(MIKindex_sumLV$depthLower)] <- MIKindex_sumLV$depthBottom-5
  
  # from here onwards calculation of index as in IndexCalculation2015.R
  
  
  MIKindex_sumLV$L.sqm<- ifelse(MIKindex_sumLV$eLVolFlag=="F", 
                                MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$depthLower*MIKindex_sumLV$flowIntCalibr/(MIKindex_sumLV$flowIntRevs*MIKindex_sumLV$netopeningArea), 
                                ifelse(MIKindex_sumLV$eLVolFlag=="D", MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$depthLower/(MIKindex_sumLV$distance*MIKindex_sumLV$netopeningArea), 
                                       MIKindex_sumLV$numberLarvae*MIKindex_sumLV$depthLower/(3002*MIKindex_sumLV$netopeningArea)))
  
  
  # allocation of subareas
  MIKindex_sumLV$RecLat<-str_sub(MIKindex_sumLV$statrec,1,2)
  MIKindex_sumLV$RecLong<-str_sub(MIKindex_sumLV$statrec,-2,-1)
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E5" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"cw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"ce"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E7" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"sw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"se"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E5" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>45]<-"nw"
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
  
  IBTS_Rects<-read.table(paste0(wd, "data/IBTS_Rects.txt"), header=TRUE,sep=",")
  names(aggRect)[4]="dummy"
  
  MIK_RAbun<-merge(IBTS_Rects,aggRect,by="Rectangle",all.y=TRUE)
  
  #Only have one headder helper
  if (i == 1992){
    head = T
  } else{
    head = F
  }
  
  write.table(MIK_RAbun, paste0(wd, "results/RectAbun_database.txt"),
              append=TRUE, sep=",", row.names = F, col.names = head)
  
  # extraction of data for mean abundance per Subarea
  A_Vars<-c("suba","L.sqm")
  MIKindex_byA<-aggRect[A_Vars]
  
  #assume that missing is 0
  MIKindex_byA$L.sqm[is.na(MIKindex_byA$L.sqm)] <- 0
  
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
  write.table(aggArea, paste0(wd, "results/aggArea_database.txt"),
              append=TRUE, sep=",", row.names = F, col.names = head)
  
  # caculation of total herring larvae abundance per subarea
  aggArea$miksec<-aggArea$L.sqm*aggArea$af*3086913600 
  
  # calculation of Index
  index<-sum(aggArea$miksec)/10**9
  
  # delivery of Index
  
  index_y<-c(i,index)
  index_TS<-rbind(index_TS,index_y)
  
  # counting hauls per rectangle
  AnzRects<-table(MIKindex_calc$statrec)
  AnRct<-as.data.frame.table(AnzRects)
  names(AnRct)[1]<-"Rectangle"
  MIK_NosPerRect<-merge(IBTS_Rects,AnRct,by="Rectangle",all=TRUE)
  MIK_NosPerRect$Freq[is.na(MIK_NosPerRect$Freq)]<-0
  
  MIK_NosPerRect$year <- i
  
  
  write.table(MIK_NosPerRect, paste0(wd, "results/AnzRects_database.txt"),
              append=TRUE, sep=",", row.names = F, col.names = head)
  
}

# writing index time series table
write.table(index_TS, paste0(wd, "results/index_TS_database.txt"),
            sep=",", row.names = F, col.names = F, quote = F)

#one year


  indexSTAT<-subset(MIK_Station,MIK_Station$year==2024)
  indexHER<-subset(MIKindex_lengths,MIKindex_lengths$year==2024)
  
  varPOS<-c("haulId","startLatitude","startLongitude")
  indexPOS<-indexSTAT[varPOS]
  
  # merging of station data with lenght data
  indexALL<-merge(indexPOS,indexHER, by=c("haulId"), all.x=TRUE)
  
  # here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)
  
 #  indexSubset<-
 #    subset(indexALL,
 #           (indexALL$startLatitude>=54&indexALL$startLongitude<6)
 #           |(indexALL$startLatitude>=57&indexALL$startLongitude<9)
 #           |indexALL$startLongitude>=9
 #           |(indexALL$startlatitude<54&(indexALL$length>18
 #                                        |is.na(indexALL$length)))
 #           |(indexALL$startlongitude>=6&indexALL$startlongitude<9&indexALL$startlatitude>=54&indexALL$startlatitude<57&(indexALL$length>18|is.na(indexALL$length))))
 # str(indexALL) 
 
indexSubset <- subset(indexALL,
                       (startLatitude >= 54 & startLongitude < 6) |
                         (startLatitude >= 57 & startLongitude < 9) |
                         (startLongitude >= 9) |
                         (startLatitude < 54 &
                            (ifelse(length > 18, TRUE, is.na(length)))) |
                         (startLongitude >= 6 & startLongitude < 9 &
                            startLatitude >= 54 & startLatitude < 57 &
                            (ifelse(length > 18, TRUE, is.na(length))))
 )
 
 
 
 
 
 
 
  # all number=NA set to 0 
  indexSubset$number[is.na(indexSubset$number)]<-0
  indexSubset$number<-as.numeric(indexSubset$number)
  
  indexSubset$raisingFactor[is.na(indexSubset$raisingFactor)]<-1
  indexSubset$raisingFactor<-as.numeric(indexSubset$raisingFactor)
  
  # calculation of raised Number per Length class (raised by subsampling factor)
  
  indexSubset$numberLarvae<-indexSubset$number*indexSubset$raisingFactor
  
  # calculation of number of herring larvae per MIK-station
  MIKindex_aggLV<-aggregate(subset(indexSubset,select=c("numberLarvae")),
                            by=list(indexSubset$haulId),sum)
  
  names(MIKindex_aggLV)[1]<-"haulId"
  #statRec does not exist in the ICES database so I removed it (Maria Makri  August 2025)
  
  # merging of aggregated larvae data (sum) with relevant station data
  Vars_sel<-c("haulId","startLatitude","startLongitude", 'statrec', "depthBottom",
              "depthLower","distance","flowIntRevs","flowIntCalibr","netopeningArea","elvolFlag")
  
  MIKindex_StatDat<-indexSTAT[Vars_sel]
  MIKindex_sumLV<-merge(MIKindex_StatDat,MIKindex_aggLV,by="haulId",all.x=TRUE)
  
  # all NumberLarvae=NA set to 0
  MIKindex_sumLV$NnumberLarvae[is.na(MIKindex_sumLV$numberLarvae)]<-0
  
  
  MIKindex_sumLV$depthLower<- as.numeric(  MIKindex_sumLV$depthLower)
  MIKindex_sumLV$depthBottom <- as.numeric(MIKindex_sumLV$depthBottom)
  # all missing DepthLower data replaced by DepthBottom - 5 
  MIKindex_sumLV$depthLower[is.na(MIKindex_sumLV$depthLower)] <- MIKindex_sumLV$depthBottom-5
  
  
  str(MIKindex_sumLV)
  
  # from here onwards calculation of index as in IndexCalculation2015.R
  
  #change netopening data type to numeric 
  
  MIKindex_sumLV$netopeningArea<- as.numeric(MIKindex_sumLV$netopeningArea)
  
  MIKindex_sumLV$L.sqm<- ifelse(MIKindex_sumLV$elvolFlag=="F", 
                                MIKindex_sumLV$numberLarvae*MIKindex_sumLV$depthLower*MIKindex_sumLV$flowIntCalibr/(MIKindex_sumLV$flowIntRevs*MIKindex_sumLV$netopeningArea), 
                                ifelse(MIKindex_sumLV$elvolFlag=="D", MIKindex_sumLV$numberLarvae*MIKindex_sumLV$depthLower/(MIKindex_sumLV$distance*MIKindex_sumLV$netopeningArea), 
                                       MIKindex_sumLV$numberLarvae*MIKindex_sumLV$depthLower/(3002*MIKindex_sumLV$netopeningArea)))
  
  
  # allocation of subareas
  MIKindex_sumLV$RecLat<-str_sub(MIKindex_sumLV$statrec,1,2)
  MIKindex_sumLV$RecLong<-str_sub(MIKindex_sumLV$statrec,-2,-1)
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E5" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"cw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>39 & MIKindex_sumLV$RecLat<46]<-"ce"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E7" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"sw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>34 & MIKindex_sumLV$RecLat<40]<-"se"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"E5" & MIKindex_sumLV$RecLong<"F2" & MIKindex_sumLV$RecLat>45]<-"nw"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F1" & MIKindex_sumLV$RecLat>45]<-"ne"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLong>"F8"]<-"ka"
  MIKindex_sumLV$area[MIKindex_sumLV$RecLat<"35" & MIKindex_sumLV$RecLat>"30"]<-"ch"
  
  
  
  # Index calculation: Extraction of necessary variables Rectangle, area und L.sqm
  VarIndex<-c("statrec","area","L.sqm")
  MIKindex_calc<-MIKindex_sumLV[VarIndex]
  
  # Calculation of mean herring larvae abundance per rectangle
  aggRect<-aggregate(MIKindex_calc, by=list(Rectangle=MIKindex_calc$statrec,suba=MIKindex_calc$area), mean)
  
  # Creation and writing of table with mean abundance per rectangle
  aggRect$year<-2024
  
  getwd()
  
  IBTS_Rects<-read.table("D:/OneDrive - International Council for the Exploration of the Sea (ICES)/Profile/Documents/EggsAndLarvae/Indices Calculations/MIK/IBTS_Rects.txt", header=TRUE,sep=",")
  names(aggRect)[4]="dummy"
  
  MIK_RAbun<-merge(IBTS_Rects,aggRect,by="Rectangle",all.y=TRUE)
  # 
  # #Only have one headder helper
  # if (i == 1992){
  #   head = T
  # } else{
  #   head = F
  # }
  
  HEAD= F
  
  write.table(MIK_RAbun, "RectAbun_database_ΜΜ_060825.txt",
              append=TRUE, sep=",", row.names = F, col.names = T)
  
  # extraction of data for mean abundance per Subarea
  A_Vars<-c("suba","L.sqm")
  MIKindex_byA<-aggRect[A_Vars]
  
  #assume that missing is 0
  MIKindex_byA$L.sqm[is.na(MIKindex_byA$L.sqm)] <- 0
  
  # calculation of mean abundance per subarea
  aggArea<-aggregate(MIKindex_byA, by=list(area=MIKindex_byA$suba), mean)
  
  str(MIKindex_byA)
  # factors for index calculation by subarea 
  aggArea$af[aggArea$area=="cw"]<-28
  aggArea$af[aggArea$area=="ce"]<-33
  aggArea$af[aggArea$area=="sw"]<-12
  aggArea$af[aggArea$area=="se"]<-30
  aggArea$af[aggArea$area=="nw"]<-27
  aggArea$af[aggArea$area=="ne"]<-11
  aggArea$af[aggArea$area=="ka"]<-10
  aggArea$af[aggArea$area=="ch"]<-10
  
  aggArea$year<-2024
  
  # writing of table with subarea abundances
  write.table(aggArea, "aggArea_database_MM_060825.txt",
              append=TRUE, sep=",", row.names = F)
  
  # caLculation of total herring larvae abundance per subarea
  aggArea$miksec<-aggArea$L.sqm*aggArea$af*3086913600 
  
  # calculation of Index
  index<-sum(aggArea$miksec)/10**9
  
  # delivery of Index
  
  index_y<-c(2024,index)
  index_TS<-rbind(index_TS,index_y)
  
  # counting hauls per rectangle
  AnzRects<-table(MIKindex_calc$statrec)
  AnRct<-as.data.frame.table(AnzRects)
  names(AnRct)[1]<-"Rectangle"
  MIK_NosPerRect<-merge(IBTS_Rects,AnRct,by="Rectangle",all=TRUE)
  MIK_NosPerRect$Freq[is.na(MIK_NosPerRect$Freq)]<-0
  
  MIK_NosPerRect$year <- i
  
  
  write.table(MIK_NosPerRect, paste0(wd, "results/AnzRects_database.txt"),
              append=TRUE, sep=",", row.names = F, col.names = head)
  
}

# writing index time series table
write.table(index_TS, paste0(wd, "results/index_TS_database.txt"),
            sep=",", row.names = F, col.names = F, quote = F)






