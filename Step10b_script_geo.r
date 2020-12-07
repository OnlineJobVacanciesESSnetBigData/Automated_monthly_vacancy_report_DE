####################################################################################################
#################Program for creating regional OJA data analogous to Australian Labour department
#####################################################################################################

# Copyright 2020 DESTATIS
#  
# Licensed under the EUPL, Version 1.2 or – as soon as they will be approved by the European Commission - subsequent versions of the EUPL (the "Licence");
# You may not use this work except in compliance with the Licence.
# You may obtain a copy of the Licence at:
#  *https://joinup.ec.europa.eu/software/page/eupl5
#  
# Unless required by applicable law or agreed to inwriting, software distributed under the Licence is distributed on an "AS IS" basis,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the Licence for the specific language governing permissions and limitations under the Licence.

rm(list = ls())

#Chosedate  for which the report should be displayed
library(lubridate)
rdate <- ymd("2020-3-1")

# Validity #
valid <- 30

#adapt paths according to your own file structure

path <- "alldata_june20/"
resultspath <- "Results/like_AU/"

options(scipen = 999)


library(RColorBrewer)
#library(xlsx)
library(readxl)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(Hmisc)
library(data.table)
library(parallel)
library(fst)
library(magrittr)
library(sf)


dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date", "idmacro_region",  "macro_region", "idregion", "region", "idprovince", "province"), as.data.table = TRUE)

#small issue with Halle
dframe$province[dframe$idprovince=="DEE02"] <- "Halle (Saale)"

#save some province lists to disc for use in later stages
province <- unique(dframe$idprovince)
province <- province[startsWith(province , "DE") & !is.na(province)]

names <- character()
for (i in province) {
  tframe <- dframe[idprovince==i]
  tmp <- names(table(tframe$province))[which.max(table(tframe$province))]
  if (is_null(tmp) == TRUE) {
    tmp <- NA
  }
  names <- c(names, tmp)
}
province_names <- data.frame(state = names, key = province)
saveRDS(province_names, file = paste0(resultspath, "province_names_", rdate, ".rds") )


# read in the shapefiles ======================================

nuts <- st_read("Auxiliary data/NUTS_RG_10M_2016_3035.shp")
de <- nuts[nuts$CNTR_CODE == "DE" & nuts$LEVL_CODE==0,]

sfile <- st_read("Auxiliary data/CommutingZonesShapefile/Germany.shp")

# keep only xy dimensions
sflile <- st_zm(sfile, drop = TRUE, what = "ZM")

sfile <- subset(sfile, select =  -c(class_code, iso3, name))

sfile$fuacode_si <- as.character(sfile$fuacode_si)

st_geometry(sfile) <- sfile$geometry

# merge FUA data ====================================

#keep only obs with nuts non-missing

dframe <- subset(dframe, !is.na(idprovince))

fua <- read_excel("Auxiliary data/Kreise_zu_FUAs_raumlich_Verbindung_modified.xlsx")

fua <- subset(fua, select= c("fuacode_si", "NUTS"))

colnames(fua) <- c("fuacode_si", "idprovince")
dframe <- left_join(dframe,fua)
setDT(dframe)

length(unique(dframe$fuacode_si))

describe(dframe$fuacode_si)

dframe <- dframe[!is.na(dframe$fuacode_si),]


################### monthly index 
years <- c(2018, 2019, 2020)

#initial matrix for FUA specific means
states <- unique(dframe$fuacode_si)

statemeans <- setNames(data.frame(matrix(ncol = (length(states)+2), nrow = 0)), c("date", states, "year"))
statemeans %<>% mutate_if(is.logical,as.numeric)


for (l in years) {
  
  # load daylist
  
  daylist <- readRDS(file = paste0(path, "daylist_step3_", valid, "d_", l , ".rds"))
  
  # define the sequence of months as days 
  min(daylist[[1]]$grab_date)
  max(daylist[[length(daylist)]]$grab_date)
  
  dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 
  seq1 <- dates %>% group_by(month = month(dates))  %>% filter(dates==min(dates)) 
  seq1 <- seq1$dates
  
  seq2 <- dates %>% group_by(month = month(dates))  %>% filter(dates==max(dates)) 
  seq2 <- seq2$dates
  
  # - Loop over each day of a month and count the number of job ads which are active on this day
  # - Take the monthly average over these daily job ad counts 
  
  wmeans <- numeric()
  
  #intermediate matrix for state results
  smeans <- setNames(data.frame(matrix(ncol = (length(states)+2), nrow = 0)), c("date", states, "year"))
  smeans %<>% mutate_if(is.logical,as.numeric)
  
  
  for (k  in (1:length(seq1))) {
    period <- seq(seq1[k], seq2[k], 1)
    
    nvec <- numeric()
    j <- 1
    
    #placeholder for state and occupation results
    smat <- data.frame()
    
    for (i in as.character(period)) {
      tmp <- daylist[[paste(i)]]
      
      #merge additional variables not contained in daylist
      tmp <- left_join(tmp, dframe, by = "general_id")
      
      tmp <- tmp[!is.na(tmp$fuacode_si),]
      
      nvec[j] <- count(tmp)$n
      
      ##########loop over urban areas states#####################################
      h <- 1
      
      svec <- numeric()
      
      for (g in 1:length(states)) {
        st <- states[g]
        
        svec[h] <- count(subset(tmp, fuacode_si == st))$n
        
        h <- h+1
        
      }
      smat <- rbind(smat, svec)
     
      j <- j + 1
    }
    
    wmeans[k] <- mean(nvec)
    
    tvec  <- as.numeric(c(seq1[k], colMeans(smat) ))
    smeans <- rbind(smeans, tvec)
    
  }
  
  #put the yearly results together
  
  if (l == years[1]) {
    allmeans <- wmeans
    seq <- seq1
    
  } else {
    
    allmeans <- c(allmeans, wmeans)
    seq <- c(seq, seq1)
  }
  
  
  # for states
  
  smeans$year <-l
  colnames(smeans) <- c("date", states, "year")
  
  statemeans <- rbind(statemeans, smeans)
  colnames(statemeans) <- c("date", states, "year")
  
  
}

rm(daylist, tmp)

save(list = c("statemeans", "allmeans", "sfile", "fua", "de", "nuts"), file = paste0(resultspath, "geo_intermediate_results.rdata"))

#load(paste0(resultspath, "geo_intermediate_results.rdata"))

statemeans$date <- as.Date(statemeans$date, origin = "1970-01-01")

thismonth <- statemeans[nrow(statemeans),]
thismonth <- subset(thismonth, select = -year)

thismonth_long <- gather(thismonth, key = date )
colnames(thismonth_long) <- c("fuacode_si", "job_ads")

thismonth_long <- left_join(thismonth_long, sfile, by = "fuacode_si")

thismonth_long$fuaname <- as.character(thismonth_long$fuaname)
thismonth_long$fuaname_en <- as.character(thismonth_long$fuaname_en)

thismonth_long$job_ads <- round(thismonth_long$job_ads)

st_geometry(thismonth_long) <- thismonth_long$geometry

thismonth_long <- st_zm(thismonth_long, drop = TRUE, what = "ZM")

thismonth_long$label <- paste0(thismonth_long$fuaname_en, "\n ", as.character(thismonth_long$job_ads))

#test <- thismonth_long[is.na(thismonth_long$fuaname),]

thismonth_long$nudge_x <- 0

thismonth_long$nudge_x[thismonth_long$fuaname_en == "Dusseldorf"] <- -1

thismonth_long$nudge_y <- 0

thismonth_long$nudge_y[thismonth_long$fuaname_en == "Bremerhaven"] <- +0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Braunschweig"] <- +0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Frankfurt am Main"] <- +0.15
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Darmstadt"] <- -0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Kaiserslautern"] <- +0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Villingen-Schwenningen"] <- +0.15
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Zwickau"] <- -0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Bremen"] <- -0.1
thismonth_long$nudge_y[thismonth_long$fuaname_en == "Monchengladbach"] <- -0.1

options(warn = - 1)
  
ggplot(thismonth_long) +
  geom_sf( aes(fill = job_ads)) + theme_void() +
  theme(panel.grid.major = element_line(colour = "transparent")) +
  labs() +
  scale_fill_gradient2(name = "Job ads",low="lightblue", high="red") +
  geom_sf_text(aes(label = fuaname_en), size = 2, colour = "black", nudge_y = thismonth_long$nudge_y , nudge_x = thismonth_long$nudge_x)+
  geom_sf(data=de,alpha = 0)

ggsave(paste0(resultspath, "Geo_pstocks_FUA.png"), width = 20, height = 13.3, units = "cm")

options(warn = 0)


