library(data.table)
library(icesDatras)
library(ggplot2)
library(dplyr)

wd <-  "D:/OneDrive - International Council for the Exploration of the Sea (ICES)/Profile/Desktop/Maria_ICES_dataflows/Jonathan_scripts/"
setwd(wd)
list.files()
IBTS_Rects<-read.table(paste0( "IBTS_Rects.txt"), header=TRUE,sep=",")

# you need to download for all species and all types of samples to get all stations....
#thereafter you have to down sample
org_s <- read.csv2(paste0(wd, "MIK1992_2025_Data_stat.csv"), dec = ".") 
########27-09-2026#############
dtb_s <- read.csv(paste0(wd, "EggsAndLarvae_EH_0715204393.csv")) 


dtb_s <- read.csv(paste0(wd, "EggsAndLarvae_EH_2026_09_27_14_31_23.csv")) 
dtb_s <- dtb_s[dtb_s$ELHaulFlag == "", ]
unique_combos <- dtb_s %>%
  distinct(Country, Institute)
org_l <- read.csv2(paste0(wd, "MIK1992_2025_Data_Larvae.csv"), dec = ".")

dtb_l <- read.csv(paste0(wd, "EggsAndLarvae_EM_0715204393.csv"))

dtb_l <- read.csv(paste0(wd, "EggsAndLarvae_EM_2026_09_27_14_31_23.csv")) 

dtb_l <- dtb_l[dtb_l$Species == "Clupea harengus", ]

org_l$Year<-substr(org_l$HaulID,1,4)
dtb_l$Year<-substr(dtb_l$HaulID,1,4)

org_s <- org_s[org_s$statrec %in% IBTS_Rects$Rectangle, ]
dtb_s <- dtb_s[dtb_s$statrec %in% IBTS_Rects$Rectangle, ]

org_l <- org_l[org_l$HaulID %in% org_s$HaulID, ]
dtb_l <- dtb_l[dtb_l$HaulID %in% dtb_s$HaulID, ]

years = unique(org_s$Year)

## #obvius misses
for(yr in years) {
  print(yr)
  #just the latest year to allign
  orgS <- org_s[org_s$Year == yr, ]
  dtbS <- dtb_s[dtb_s$Year == yr, ]
  
  orgL <- org_l[org_l$Year == yr, ]
  dtbL <- dtb_l[dtb_l$Year == yr, ]
  
  #number of zero
  n0_org <- length(orgS$HaulID[! orgS$HaulID %in% orgL$HaulID])
  n0_dtb <- length(dtbS$HaulID[! dtbS$HaulID %in% dtbL$HaulID])
  
  #miss mach non 0 stations
  miss_org <- orgS[orgS$HaulID %in% orgL$HaulID, ]
  miss_dtb <- dtbS[dtbS$HaulID %in% dtbL$HaulID, ]
  
  st_miss_org <- length(miss_dtb$HaulID[! miss_dtb$HaulID %in% miss_org$HaulID])
  st_miss_dtb <- length(miss_org$HaulID[! miss_org$HaulID %in% miss_dtb$HaulID])
  
  
  #############################################
  ## lengths
  setDT(orgL)
  setDT(dtbL)
  
  orgL$tot = round(orgL$number * orgL$SubSamplingFactor)
  dtbL$tot = round(dtbL$Number * dtbL$RaisingFactor)
  
  l1 <- orgL[ ,. (n_org = sum(number),
                   fact = unique(round(SubSamplingFactor))),
               by = .(Year, HaulID)]
  
  l2 <- dtbL[ ,. (n_dtb = sum(Number),
                   fact = unique(round(RaisingFactor))),
               by = .(HaulID)]
  
  l <- merge(l1, l2, by = c("HaulID", "fact"), all = T)
  l$n_diff <- l$n_org - l$n_dtb
  
  miss_l <- l[l$n_diff > 0 | is.na(l$n_org) | is.na(l$n_dtb), ]
  
  #############################################
  #var mis
  setDT(orgS)
  setDT(dtbS)
  
  volflag <- merge(orgS[, c("HaulID", "Distance.m", "Sdepth", "FlowRevsInt","FlowCalInt","EVolFlag")], 
                   dtbS[, c("HaulID", "Distance", "DepthLower", "FlowIntRevs","FlowIntCalibr","ELVolFlag")],
                   by = "HaulID")
  
  volflag[is.na(volflag)] <- 0
  volflag$miss_dist <- abs(as.numeric(volflag$Distance.m) - as.numeric(volflag$Distance)) > 1
  volflag$miss_Sdepth <- abs(as.numeric(volflag$Sdepth) - as.numeric(volflag$DepthLower)) > 0.1
  volflag$miss_flowRev <-abs(as.numeric( volflag$FlowRevsInt) - as.numeric(volflag$FlowIntRevs)) > 1
  volflag$miss_volflag <- volflag$EVolFlag != tolower(volflag$ELVolFlag)
  
  miss_var = data.frame(Year = yr,
                        diff_num_mesured = ifelse(nrow(miss_l) > 0, "Y", "N"),
                        diff_non_0_stations = ifelse(nrow(miss_org) != nrow(miss_dtb), "Y", "N"),
                        miss_0_stations = ifelse(n0_org != n0_dtb, "Y", "N"),
                        miss_Sdepth = ifelse(TRUE %in% volflag$miss_Sdepth, "Y", "N"),
                        miss_volflag = ifelse(TRUE %in% volflag$miss_volflag, "Y", "N"),
                        miss_flowRev = ifelse(TRUE %in% volflag$miss_flowRev, "Y", "N"),
                        miss_dist = ifelse(TRUE %in% volflag$miss_dist, "Y", "N")
                        )
  

  miss_var$explenation = ifelse("Y" %in% miss_var, "Y", "N")
  #############################################
  
  #write out results
  nams <- F
  if (yr == min(years))
    nams = T
  
  appen <- T
  if (yr == min(years))
    appen = F
  
  
  write.table(data.frame(year = yr, 
                         n0_org = n0_org, n0_dtb = n0_dtb,
                         st_miss_org = st_miss_org, st_miss_dtb = st_miss_dtb), 
              paste0(wd, "results/miss_stations.csv"), 
              row.names = F, col.names = nams, sep = ";", dec = ",", quote = F,
              append = appen)
  
  write.table(miss_l, paste0(wd, "results/miss_lengths.csv"), 
              row.names = F, col.names = nams, sep = ";", dec = ",", quote = F,
              append = appen)
  
  write.table(miss_var, paste0(wd, "results/miss_overwive.csv"), 
              row.names = F, col.names = nams, sep = ";", dec = ",", quote = F,
              append = appen)
}



##### deep dive based on overview #####

## mis Sdepth
sdepth <- merge(org_s[, c("Year", "HaulID", "Sdepth")], 
                dtb_s[ , c("HaulID", "DepthLower")],
                by = "HaulID")

sdepth$diff <- abs(sdepth$Sdepth - sdepth$DepthLower)
sdepth <- sdepth[sdepth$diff > 0, ]

sdepth$Year <- as.factor(sdepth$Year)
ggplot()+
  geom_point(data = sdepth, aes(x = HaulID, y = diff, color = Year))+
  scale_y_continuous(trans='log10')+
  theme(legend.position = "none",
        axis.text.x=element_blank())

ggsave(paste0(wd, "results/Sdepth.png"), 
              width = 400, height = 200, units = 'mm', dpi = 200)

## mis volflag
volflag <- merge(org_s[, c("Year", "HaulID", "EVolFlag")], 
                dtb_s[ , c("HaulID", "ELVolFlag")],
                by = "HaulID")

volflag$miss_volflag <- volflag$EVolFlag != tolower(volflag$ELVolFlag)

volflag <- volflag[volflag$miss_volflag == TRUE, ]
volflag$files<-substr(volflag$HaulID, 1, 6)
diff <- data.frame(table(volflag$Year, volflag$EVolFlag))

ggplot()+
  geom_bar(data = diff, aes(fill= Var1, x = Var2, y = Freq), 
           position="stack", stat = "identity")

ggsave(paste0(wd, "results/volFlag.png"), 
              width = 400, height = 200, units = 'mm', dpi = 200)

## mis flowRev
flow <- merge(org_s[, c("Year", "HaulID", "FlowRevsInt")], 
                 dtb_s[ , c("HaulID", "FlowIntRevs")],
                 by = "HaulID")

flow$FlowRevsInt[flow$FlowRevsInt %in% c("", "  ")] <- 0
flow$FlowRevsInt[flow$FlowRevsInt == "247?"] <- 0
flow[is.na(flow)] <- 0

flow$diff <-abs(as.numeric( flow$FlowRevsInt) - as.numeric(flow$FlowIntRevs))
flow <- flow[flow$diff > 0, ]

flow$Year <- as.factor(flow$Year)
ggplot()+
  geom_point(data = flow, aes(x = HaulID, y = diff, color = Year))+
  scale_y_continuous(trans='log10')+
  theme(axis.text.x=element_blank())

ggsave(paste0(wd, "results/flow.png"), 
       width = 400, height = 200, units = 'mm', dpi = 200)


## miss distance
dist <- merge(org_s[, c("Year", "HaulID", "Distance.m")], 
              dtb_s[ , c("HaulID", "Distance")],
              by = "HaulID")

dist[is.na(dist)] <- 0
dist$diff <-abs(as.numeric(dist$Distance.m) - as.numeric(dist$Distance))
dist <- dist[dist$diff > 0.1, ]

dist$Year <- as.factor(dist$Year)
ggplot()+
  geom_point(data = dist, aes(x = HaulID, y = diff, color = Year))+
  scale_y_continuous(trans='log10')+
  theme(legend.position = "none",
        axis.text.x=element_blank())

ggsave(paste0(wd, "results/dist.png"), 
       width = 400, height = 200, units = 'mm', dpi = 200)


## missing length overviews
len <- read.csv2(paste0(wd, "results/miss_lengths.csv"))
len <- len[!is.na(len$n_diff), ]


len$Year <- as.factor(len$Year)
ggplot()+
  geom_point(data = len, aes(x = HaulID, y = n_diff, color = Year))+
  theme(legend.position = "none",
        axis.text.x=element_blank())

ggsave(paste0(wd, "results/total_numbers_diff.png"), 
       width = 400, height = 200, units = 'mm', dpi = 200)

only_in_dtb <- anti_join(dtb_s, org_s, by = "HaulID")


## missing stations overviews
st <- read.csv2(paste0(wd, "results/miss_stations.csv"))
st$n0_diff <- abs(st$n0_org - st$n0_dtb)

#not missing but duplicated
org_dub <- org_s$HaulID[duplicated(org_s$HaulID)]
dtb_dub <- dtb_s$HaulID[duplicated(dtb_s$HaulID)]

#missing non 0 stations
n0_miss <- unique(st[st$n0_diff > 0 , c("year", "n0_org", "n0_dtb")])

setDT(n0_miss)
n0_miss <- melt(n0_miss, id.vars = c("year"),
                measure.vars = c("n0_org", "n0_dtb"))

n0_miss$year <- as.character(n0_miss$year)
ggplot()+
  geom_bar(data = n0_miss, aes(fill= variable, x = year, y = value), 
           position="dodge", stat = "identity")
  

ggsave(paste0(wd, "results/diff_n0.png"), 
       width = 400, height = 200, units = 'mm', dpi = 200)


# diff stations
st <- st[! (st$st_miss_org == 0 & st$st_miss_dtb == 0), ]

setDT(st)
st <- melt(st, id.vars = c("year"),
                measure.vars = c("st_miss_org", "st_miss_dtb"), 
           value.name = "n_miss_stations")

st$year <- as.character(st$year)
ggplot()+
  geom_bar(data = st, aes(fill= variable, x = year, y = n_miss_stations), 
           position="dodge", stat = "identity")


ggsave(paste0(wd, "results/n_miss_stations.png"), 
       width = 400, height = 200, units = 'mm', dpi = 200)







