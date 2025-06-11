# opens Module stringr
library(stringr)

#din arbejdsmappe, lav desuden mapper heri der hedder "data" og "resultater
#læg filer der skal indlæses i data mappen
wd <- "Q:/20-forskning/20-dfad/users/jostou/home/25_04_25_sildelarve_database/"

# reading of station data
#url <-"https://eggsandlarvae.ices.dk/webservices/getEggsAndLarvaeData?yearBegining=2008&yearEnd=2016&month=&stage=&survey=MIK&species=&lastModifiedDate="

url_EH <-"https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEH?YearBegining=2024&SurveyCode=14537"
MIK_Station <- jsonlite::fromJSON(url_EH, simplifyDataFrame = TRUE)

url_EM <-"https://eggsandlarvae.ices.dk/api/getEggsAndLarvaeDataEM?YearBegining=2024&SurveyCode=14537"
MIKindex_lengths <- jsonlite::fromJSON(url_EM, simplifyDataFrame = TRUE)


#MIK_Station<-read.csv(paste0(wd, "data/EggsAndLarvae_EH_0425395059.csv")) 
MIK_Station <- MIK_Station[is.na(MIK_Station$elhaulFlag), ]
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

for(i in 2024:max_YR) {
  print(i)
  # Here starts index calculation for each survey year
  # Subsetting for survey year
  indexSTAT<-subset(MIK_Station,MIK_Station$year==i)
  indexHER<-subset(MIKindex_lengths,MIKindex_lengths$year==i)
  
  varPOS<-c("haulId","startLatitude","startLongitude")
  indexPOS<-indexSTAT[varPOS]
  
  # merging of station data with lenght data
  indexALL<-merge(indexPOS,indexHER, by=c("HaulID"), all.x=TRUE)
  
  # here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)
  
  indexSubset<-
    subset(indexALL,
           (indexALL$StartLatitude>=54&indexALL$StartLongitude<6)
           |(indexALL$StartLatitude>=57&indexALL$StartLongitude<9)
           |indexALL$StartLongitude>=9
           |(indexALL$StartLatitude<54&(indexALL$Length>18
                                   |is.na(indexALL$Length)))
           |(indexALL$StartLongitude>=6&indexALL$StartLongitude<9&indexALL$StartLatitude>=54&indexALL$StartLatitude<57&(indexALL$Length>18|is.na(indexALL$Length))))
  
  # all number=NA set to 0 
  indexSubset$Number[is.na(indexSubset$Number)]<-0
  indexSubset$Number<-as.numeric(indexSubset$Number)
  
  indexSubset$RaisingFactor[is.na(indexSubset$RaisingFactor)]<-1
  indexSubset$RaisingFactor<-as.numeric(indexSubset$RaisingFactor)
  
  # calculation of raised Number per Length class (raised by subsampling factor)
  
  indexSubset$NumberLarvae<-indexSubset$Number*indexSubset$RaisingFactor
  
  # calculation of number of herring larvae per MIK-station
  MIKindex_aggLV<-aggregate(subset(indexSubset,select=c("NumberLarvae")),
                            by=list(indexSubset$HaulID),sum)
  
  names(MIKindex_aggLV)[1]<-"HaulID"
  
  
  # merging of aggregated larvae data (sum) with relevant station data
  Vars_sel<-c("HaulID","StartLatitude","StartLongitude","statrec","DepthBottom",
              "DepthLower","Distance","FlowIntRevs","FlowIntCalibr","NetopeningArea","ELVolFlag")
  
  MIKindex_StatDat<-indexSTAT[Vars_sel]
  MIKindex_sumLV<-merge(MIKindex_StatDat,MIKindex_aggLV,by="HaulID",all.x=TRUE)
  
  # all NumberLarvae=NA set to 0
  MIKindex_sumLV$NumberLarvae[is.na(MIKindex_sumLV$NumberLarvae)]<-0
  
  # all missing DepthLower data replaced by DepthBottom - 5 
  MIKindex_sumLV$DepthLower[is.na(MIKindex_sumLV$DepthLower)] <- MIKindex_sumLV$DepthBottom-5
  
  # from here onwards calculation of index as in IndexCalculation2015.R
  
  
  MIKindex_sumLV$L.sqm<- ifelse(MIKindex_sumLV$ELVolFlag=="F", 
                                MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$DepthLower*MIKindex_sumLV$FlowIntCalibr/(MIKindex_sumLV$FlowIntRevs*MIKindex_sumLV$NetopeningArea), 
                                ifelse(MIKindex_sumLV$ELVolFlag=="D", MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$DepthLower/(MIKindex_sumLV$Distance*MIKindex_sumLV$NetopeningArea), 
                                       MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$DepthLower/(3002*MIKindex_sumLV$NetopeningArea)))
  
  
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








