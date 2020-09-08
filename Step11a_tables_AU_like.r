####################################################################################################
#################Program for creating tables for OJA indix analogous to Australian Labour department
#####################################################################################################

rm(list = ls())

library(lubridate)

#Chosedate  for which the report should be displayed
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
library(reshape2)
library(Hmisc)
library(data.table)
library(fst)
library(magrittr)
library(knitr)

load(paste0(resultspath, "tables_", rdate ,".rdata"))

load(paste0(resultspath, "intermediate_results_", rdate, ".rdata"))

snames <- readRDS(paste0(resultspath, "snames_", rdate, ".rds"))
onames <- readRDS(paste0(resultspath, "onames_", rdate, ".rds"))
enames <- readRDS( paste0(resultspath, "enames_", rdate, ".rds"))


########Table of federal states ==============

for (i in 1:nrow(statestab)) {
  tmp <- gather(statestab[i,4:ncol(statestab)])
  
  if (i == 1) {
    ntab <- tmp
  } else {
    
    ntab <- left_join(ntab, tmp, by= "key")
  }
}

ntab[,3] <-ntab[,3]*100 
ntab[,5] <-ntab[,5]*100 

ntab$stock <- round(as.numeric(statemeans[nrow(statemeans), 2:(ncol(statemeans)-1)]),0)

snames <- snames %>% mutate_all(as.character)

ntab <- left_join(snames, ntab, by= "key")

ntab <- subset(ntab, select = -key)

colnames(ntab) <- c("Federal state", "monthly change (no)", "monthly change (%)", "yearly change", "yearly change (%)", "stock")


########Table of occupation groups ==============

for (i in 1:nrow(occutab)) {
  tmp <- gather(occutab[i,4:ncol(occutab)])
  
  if (i == 1) {
    ntab <- tmp
  } else {
      
    ntab <- left_join(ntab, tmp, by= "key")
    }
}

ntab[,3] <-ntab[,3]*100 
ntab[,5] <-ntab[,5]*100 

ntab$stock <- round(as.numeric(occmeans[nrow(occmeans), 2:(ncol(occmeans)-1)]),0)

onames <- onames %>% mutate_all(as.character)

ntab <- left_join(onames, ntab, by= "key")

ntab <- subset(ntab, select = -key)

colnames(ntab) <- c("Occupation", "monthly change (no)", "monthly change (%)", "yearly change", "yearly change (%)", "stock")


########Table of education groups ==============

for (i in 1:nrow(edutab)) {
  tmp <- gather(edutab[i,4:ncol(edutab)])
  
  if (i == 1) {
    ntab <- tmp
  } else {
    
    ntab <- left_join(ntab, tmp, by= "key")
  }
}

ntab[,3] <-ntab[,3]*100 
ntab[,5] <-ntab[,5]*100 

ntab$stock <- round(as.numeric(edumeans[nrow(edumeans), 2:(ncol(edumeans)-1)]),0)

enames <- enames %>% mutate_all(as.character)

ntab <- left_join(enames, ntab, by= "key")

ntab <- subset(ntab, select = -key)

colnames(ntab) <- c("Education", "monthly change (no)", "monthly change (%)", "yearly change", "yearly change (%)", "stock")

ntab$Education <- c("Short-cycle tertiary", "Upper secondary", "Master or equivalent", "Bachelor or equivalent", "Post-secondary non-tertiary", "Doctoral or equivalent", "Lower secondary", "Primary"  )


