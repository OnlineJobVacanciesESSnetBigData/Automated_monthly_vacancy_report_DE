####################################################################################################
#################Program for creating regional OJA data analogous to Australian Labour department
#####################################################################################################

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


dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date", "idmacro_region",  "macro_region", ""   ), as.data.table = TRUE)
