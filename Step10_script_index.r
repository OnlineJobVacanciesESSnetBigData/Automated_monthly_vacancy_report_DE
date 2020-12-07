####################################################################################################
#################Program for creating data for OJA indix analogous to Australian Labour department
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


dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date", "idesco_level_1", "esco_level_1" , "idmacro_region",  "macro_region" , "ideducational_level",  "educational_level"   ), as.data.table = TRUE)

#save some name lists to disc for use in later stages
states <- unique(dframe$idmacro_region)
states <- states[startsWith(states , "DE") & !is.na(states)]

names <- character()
for (i in states) {
  tframe <- dframe[idmacro_region==i]
  tmp <- names(table(tframe$macro_region))[which.max(table(tframe$macro_region))]
  names <- c(names, tmp)
}
snames <- data.frame(state = names, key = states)
saveRDS(snames, file = paste0(resultspath, "snames_", rdate, ".rds") )
 
##################
occs <- unique(dframe$idesco_level_1)
occs <- occs[!is.na(occs)]

names <- character()
for (i in occs) {
  tframe <- dframe[idesco_level_1==i]
  tmp <- names(table(tframe$esco_level_1))[which.max(table(tframe$esco_level_1))]
  names <- c(names, tmp)
}
onames <- data.frame(occupation = names, key = occs)
saveRDS(onames, file = paste0(resultspath, "onames_", rdate, ".rds") )


##################
educs <- unique(dframe$ideducational_level)
educs <- educs[!is.na(educs)]

names <- character()
for (i in educs) {
  tframe <- dframe[ideducational_level==i]
  tmp <- names(table(tframe$educational_level))[which.max(table(tframe$educational_level))]
  names <- c(names, tmp)
}
enames <- data.frame(education = names, key = educs)
saveRDS(enames, file = paste0(resultspath, "enames_", rdate, ".rds") )



################### monthly index 
years <- c(2018, 2019, 2020)

#initial matrix for state specific means
states <- unique(dframe$idmacro_region)
states <- states[startsWith(states , "DE") & !is.na(states)]

statemeans <- setNames(data.frame(matrix(ncol = 18, nrow = 0)), c("date", states, "year"))
statemeans %<>% mutate_if(is.logical,as.numeric)

#initial matrix for occupational groups
occs <- unique(dframe$idesco_level_1)
occs <- occs[!is.na(occs)]
  
occmeans <- setNames(data.frame(matrix(ncol = (length(occs)+2), nrow = 0)), c("date", occs, "year"))
occmeans %<>% mutate_if(is.logical,as.numeric)


#initial matrix for educational categories
educs <- unique(dframe$ideducational_level)
educs <- educs[!is.na(educs)]

edumeans <- setNames(data.frame(matrix(ncol = (length(educs)+2), nrow = 0)), c("date", educs, "year"))
edumeans %<>% mutate_if(is.logical,as.numeric)



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
  smeans <- setNames(data.frame(matrix(ncol = 18, nrow = 0)), c("date", states, "year"))
  smeans %<>% mutate_if(is.logical,as.numeric)
  
  #intermediate matrix for occupation results
  omeans <- setNames(data.frame(matrix(ncol = (length(occs)+2), nrow = 0)), c("date", occs, "year"))
  omeans %<>% mutate_if(is.logical,as.numeric)
  
  #initial matrix for educational categories
  emeans <- setNames(data.frame(matrix(ncol = (length(educs)+2), nrow = 0)), c("date", educs, "year"))
  emeans %<>% mutate_if(is.logical,as.numeric)
  
  for (k  in (1:length(seq1))) {
    period <- seq(seq1[k], seq2[k], 1)
    
    nvec <- numeric()
    j <- 1
    
    #placeholder for state and occupation results
    smat <- data.frame()
    omat <- data.frame()
    emat <- data.frame()
    
    
    for (i in as.character(period)) {
      tmp <- daylist[[paste(i)]]
      
      #merge additional variables not contained in daylist
      tmp <- left_join(tmp, dframe, by = "general_id")
      
      nvec[j] <- count(tmp)$n
      
        ##########loop over federal states#####################################
        h <- 1
        
        svec <- numeric()
        for (g in 1:length(states)) {
          st <- states[g]
          
          svec[h] <- count(subset(tmp, idmacro_region == st))$n
          
          h <- h+1
          
        }
        smat <- rbind(smat, svec)
        
        ##########loop over occupation groups#####################################
        m <- 1
        
        ovec <- numeric()
        for (g in 1:length(occs)) {
          st <- occs[g]
          
          ovec[m] <- count(subset(tmp, idesco_level_1 == st))$n
          
          m <- m+1
          
        }
        omat <- rbind(omat, ovec)
        
        ##########loop over eduction groups#####################################
        n <- 1
        
        evec <- numeric()
        for (g in 1:length(educs)) {
          st <- educs[g]
          
          evec[n] <- count(subset(tmp, ideducational_level == st))$n
          
          n <- n+1
          
        }
        emat <- rbind(emat, evec)
        
      j <- j + 1
    }
    
    wmeans[k] <- mean(nvec)
    
    tvec  <- as.numeric(c(seq1[k], colMeans(smat) ))
    smeans <- rbind(smeans, tvec)
    
    otvec <- as.numeric(c(seq1[k], colMeans(omat) ))
    omeans <- rbind(omeans, otvec)
    
    etvec <- as.numeric(c(seq1[k], colMeans(emat) ))
    emeans <- rbind(emeans, etvec)
    
    
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
  
  # for occupations
  
  omeans$year <-l
  colnames(omeans) <- c("date", occs, "year")
  
  occmeans <- rbind(occmeans, omeans)
  colnames(occmeans) <- c("date", occs, "year")
  
  # for education
  
  emeans$year <-l
  colnames(emeans) <- c("date", educs, "year")
  edumeans <- rbind(edumeans, emeans)
  colnames(edumeans) <- c("date", educs, "year")
  
  
}

rm(daylist, tmp)

save(list = c("edumeans", "occmeans", "statemeans", "allmeans"), file = paste0(resultspath, "intermediate_results_", rdate, ".rdata"))

#load(file = paste0(resultspath, "intermediate_results.rdata"))

date <- (seq)
wmat <- data.frame(date = as_date(date), cedefop_oja = allmeans)
wmat$year <- year(wmat$date)

###############Changes over time############=====

######total pseudostocks########

totaltab <- data.frame(type = c("mtm", "mtmp", "ytm", "ytmp"), current_t = c(rdate, rdate, rdate, rdate), lagged_t=c(rdate-months(1),  rdate-years(1)   , rdate-months(1), rdate-years(1) ) ) 

#MTM in oja and percentage-----------------
mtm <- wmat$cedefop_oja[wmat$date==rdate]-wmat$cedefop_oja[wmat$date==(rdate-months(1))]

mtmp <- (wmat$cedefop_oja[wmat$date==rdate]/wmat$cedefop_oja[wmat$date==(rdate-months(1))])-1

#YTM --------
ytm <- wmat$cedefop_oja[wmat$date==rdate]-wmat$cedefop_oja[wmat$date==(rdate-years(1))]

ytmp <- (wmat$cedefop_oja[wmat$date==rdate]/wmat$cedefop_oja[wmat$date==(rdate-years(1))])-1

totaltab$value <- c(mtm, mtmp,ytm,  ytmp)

rownames(totaltab) <- totaltab[,1]

######state specific pseudostocks########

statestab <- data.frame(type = c("mtm", "mtmp", "ytm", "ytmp"), current_t = c(rdate, rdate, rdate, rdate), lagged_t=c(rdate-months(1),  rdate-years(1)   , rdate-months(1), rdate-years(1) ) ) 

#MTM in oja and percentage-----------------

for (i in states) {
  
  mtm <- round((statemeans[statemeans$date==rdate, i] - statemeans[statemeans$date==(rdate-months(1)), i]), 0)
  
  mtmp <- round(((statemeans[statemeans$date==rdate, i] / statemeans[statemeans$date==(rdate-months(1)), i])-1), 3)
 
  ytm <- round((statemeans[statemeans$date==rdate, i] - statemeans[statemeans$date==(rdate-years(1)), i]), 0)
  
  ytmp <- round(((statemeans[statemeans$date==rdate, i] / statemeans[statemeans$date==(rdate-years(1)), i])-1),3)
  
  statestab <- cbind(statestab, c(mtm, mtmp, ytm, ytmp))
  
}

colnames(statestab) <- c("type", "current_t", "lagged_t", states)

rownames(statestab) <- statestab[,1]

######occupation  pseudostocks########

occutab <- data.frame(type = c("mtm", "mtmp", "ytm", "ytmp"), current_t = c(rdate, rdate, rdate, rdate), lagged_t=c(rdate-months(1),  rdate-years(1)   , rdate-months(1), rdate-years(1) ) ) 

#MTM in oja and percentage-----------------

for (i in occs) {
  
  mtm <- round((occmeans[occmeans$date==rdate, i] - occmeans[occmeans$date==(rdate-months(1)), i]), 0)
  
  mtmp <- round(((occmeans[occmeans$date==rdate, i] / occmeans[occmeans$date==(rdate-months(1)), i])-1), 3)
  
  ytm <- round((occmeans[occmeans$date==rdate, i] - occmeans[occmeans$date==(rdate-years(1)), i]), 0)
  
  ytmp <- round(((occmeans[occmeans$date==rdate, i] / occmeans[occmeans$date==(rdate-years(1)), i])-1), 3)
  
  occutab <- cbind(occutab, c(mtm, mtmp, ytm, ytmp))
  
}

colnames(occutab) <- c("type", "current_t", "lagged_t", occs)

rownames(occutab) <- occutab[,1]

######edu  pseudostocks########

edutab <- data.frame(type = c("mtm", "mtmp", "ytm", "ytmp"), current_t = c(rdate, rdate, rdate, rdate), lagged_t=c(rdate-months(1),  rdate-years(1)   , rdate-months(1), rdate-years(1) ) ) 

#MTM in oja and percentage-----------------

for (i in educs) {
  
  mtm <- round((edumeans[edumeans$date==rdate, i] - edumeans[edumeans$date==(rdate-months(1)), i]), 0)
  
  mtmp <- round(((edumeans[edumeans$date==rdate, i] / edumeans[edumeans$date==(rdate-months(1)), i])-1), 3)
  
  ytm <- round((edumeans[edumeans$date==rdate, i] - edumeans[edumeans$date==(rdate-years(1)), i]), 0)
  
  ytmp <- round(((edumeans[edumeans$date==rdate, i] / edumeans[edumeans$date==(rdate-years(1)), i])-1), 3)
  
  edutab <- cbind(edutab, c(mtm, mtmp, ytm, ytmp))
  
}

colnames(edutab) <- c("type", "current_t", "lagged_t", educs)

rownames(edutab) <- edutab[,1]

save(list = c("totaltab", "occutab", "edutab", "statestab"), file = paste0(resultspath, "tables_", rdate ,".rdata"))

