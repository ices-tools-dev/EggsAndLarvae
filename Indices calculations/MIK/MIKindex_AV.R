# July 2020, Adriana Villamor
# Modifications on Matthias script to download the data directly from the database
# and to adapt it to a TAF process.

# opens Module stringr
library(stringr)
library(icesDatras)


# reading of station data
MIK_Station<-read.table("MIK1992_current_statDAT_170320.txt", header=TRUE, sep=",") 
names(MIK_Station)

unique(MIK_Station$Gear)
unique(MIK_Station$HaulID)
MIK_Station$Ship <- as.character(MIK_Station$Ship)
unique(MIK_Station$Ship)
unique(MIK_Station$Meshtype)

MIK_Dat<-read.table("MIK1992_current_DB_Lv_130320.txt", header=TRUE, sep=",") 
names(MIK_Dat$HaulID)


# Get data in database, 2020 for the time being
require(XML)

url <-"https://eggsandlarvae.ices.dk/EggsAndLarvaeWebServices.asmx/getEggsAndLarvaeData?yearBegining=2008&yearEnd=2016&month=&stage=&survey=MIK&species=&lastModifiedDate="

out <- jsonlite::fromJSON(url, simplifyDataFrame = TRUE)

Current_station <- EH_EggsAndLarvaeDataSet20201019
Current_lengths <- EM_EggsAndLarvaeDataSet20201019
names(Current_station)
names(MIK_Station)
names(Current_lengths)
names(MIKindex_lengths)

# define Distance.m,FlowRevsInt,FlowCalInt as.numeric
str(Current_station)
# I think this is not needed
# MIK_Station$Distance.m<-as.numeric(as.character(MIK_Station$Distance.m))
# MIK_Station$FlowRevsInt<-as.numeric(as.character(MIK_Station$FlowRevsInt))
# MIK_Station$FlowCalInt<-as.numeric(as.character(MIK_Station$FlowCalInt))



# Reading of herring larvae data (numbers per length class, unmeasured, subsampling factor)

# MIKindex_lengths<-read.table("MIK1992_current_DB_Lv_130320.txt", header=TRUE, sep=",")

# creating of variable "year" in length data

Current_lengths$year<-str_sub(Current_lengths$HaulID,1,4)
str(Current_lengths)
Current_lengths$year <- as.numeric(Current_lengths$year)
# MIKindex_lengths$year<-str_sub(MIKindex_lengths$HaulID,1,4)
# MIKindex_lengths$year<-as.numeric(MIKindex_lengths$year)

# defining global variables "year" and "index" and file for MIK index time series (index_TS) 
year<-"year"
index<-"index"
index_TS<-c(year,index)
# max_YR<-max(MIK_Station$Year)
max_YR<-max(Current_station$Year)

# Initiaton of loop to calculate MIK index time series for all survey years since 1992

for(i in 1992:max_YR) {

# Here starts index calculation for each survey year
# Subsetting for survey year
indexSTAT<-subset(Current_station,Current_station$Year==i)
indexHER<-subset(Current_lengths,Current_lengths$year==i)

varPOS<-c("HaulID","StartLatitude","StartLongitude")
indexPOS<-indexSTAT[varPOS]

# merging of station data with length data
indexALL<-merge(indexPOS,indexHER, by=c("HaulID"), all.x=TRUE)

# here defining geographic boundaries for the "Exclusion Rule" and defining critical length (here CL > 18)

# AV: for year 2020 I remain with no data

indexSubset<-subset(indexALL,(indexALL$StartLatitude>=54.0 &indexALL$StartLongitude<6.0)|(indexALL$StartLatitude>=57.0 &indexALL$StartLongitude<9.0)|indexALL$StartLongitude>=9|(indexALL$StartLatitude<54.0 &(indexALL$Length>18|is.na(indexALL$Length)))|(indexALL$StartLongitude>=6.0&indexALL$StartLongitude<9.0&indexALL$StartLatitude>=54.0&indexALL$ShootLat<57.0&(indexALL$Length>18|is.na(indexALL$Length))))

# all number=NA set to 0 
indexSubset$Number[is.na(indexSubset$Number)]<-0
indexSubset$SubFactor[is.na(indexSubset$SubFactor)]<-1
indexSubset$Number<-as.numeric(indexSubset$Number)
indexSubset$SubFactor<-as.numeric(indexSubset$SubFactor)

# calculation of raised number per length class (raised by subsampling factor)

indexSubset$NumberLarvae<-indexSubset$Number*indexSubset$SubFactor


# calculation of number of herring larvae per MIK-station
MIKindex_aggLV<-aggregate(subset(indexSubset,select=c("NumberLarvae")),by=list(indexSubset$HaulID),sum)
   
names(MIKindex_aggLV)[1]<-"HaulID"



# merging of aggregated larvae data (sum) with relevant station data
Vars_sel<-c("HaulID","ShootLat","ShootLong","statrec","Bdepth","Sdepth","Distance.m","FlowRevsInt","FlowCalInt","netopeningArea","EVolFlag")

MIKindex_StatDat<-indexSTAT[Vars_sel]
MIKindex_sumLV<-merge(MIKindex_StatDat,MIKindex_aggLV,by="HaulID",all.x=TRUE)

# all NumberLarvae=NA set to 0
MIKindex_sumLV$NumberLarvae[is.na(MIKindex_sumLV$NumberLarvae)]<-0

# all missing Sdepth data replaced by Bdepth - 5 
MIKindex_sumLV$Sdepth[is.na(MIKindex_sumLV$Sdepth)]<-MIKindex_sumLV$Bdepth-5
       
# from here onwards calculation of index as in IndexCalculation2015.R

# Calculation Number herring larvae per m2
MIKindex_sumLV$L.sqm<- ifelse(MIKindex_sumLV$EVolFlag=="f", MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$Sdepth*MIKindex_sumLV$FlowCalInt/(MIKindex_sumLV$FlowRevsInt*MIKindex_sumLV$netopeningArea), MIKindex_sumLV$NumberLarvae*MIKindex_sumLV$Sdepth/(MIKindex_sumLV$Distance.m*MIKindex_sumLV$netopeningArea))


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

}
# writing index time series table
write.table(index_TS,"index_TS.txt",sep=",")







