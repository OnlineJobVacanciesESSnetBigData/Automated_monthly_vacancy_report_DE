
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


dframe <- read_fst(paste0(path,"OJVsample_step1_redux.fst"), c("general_id", "grab_date", "expire_date", "idesco_level_1", "esco_level_1" , "idesco_level_2", "esco_level_2", "idesco_level_3", "esco_level_3", "idesco_level_4", "esco_level_4",  "companyname", "idsector", "sector"  ), as.data.table = TRUE)


# filter staffing agencies ---------

dframe$companyname <- tolower(dframe$companyname)

# load list of typical elements of staffing agencies names:
keywords <- read_excel("Keywords_staffing_firms.xls" )

lengtha <- sum(!is.na(keywords$combinea))
lengthb <- sum(!is.na(keywords$combineb))
lengthc <- sum(!is.na(keywords$identified))

aword <- as.character(keywords$combinea[1:lengtha])
bword <- as.character(keywords$combineb[1:lengthb])
clear <- as.character(keywords$identified[1:lengthc])

grid <- CJ(aword,bword)

c_1 <- character()
c_2 <- character()
for (i in 1:dim(grid)[1]) {
  c_1[i] <- paste(grid[i,1],grid[i,2])
  c_2[i] <- paste0(grid[i,1],grid[i,2])
}

keys <- c(c_1,c_2,clear)

# removal of rows with companyname in list:
ismiss <- dframe[is.na(companyname)]
dframe <- filter(dframe, !str_detect(companyname, paste(keys, collapse = '|')))
dframe <- bind_rows(dframe, ismiss)
setDT(dframe)
rm(ismiss)


########################################################
###########Top 10 Occupations========
#####################################################

##################
occs <- unique(dframe$idesco_level_3)
occs <- occs[!is.na(occs)]

names <- character()
for (i in occs) {
  tframe <- dframe[idesco_level_3==i]
  tmp <- names(table(tframe$esco_level_3))[which.max(table(tframe$esco_level_3))]
  names <- c(names, tmp)
}
onames <- data.frame(occupation = names, key = occs)


##################################

l <- year(rdate)

daylist <- readRDS(file = paste0(path, "daylist_step3_", valid, "d_", l , ".rds"))

# define the sequence of months as days 
min(daylist[[1]]$grab_date)
max(daylist[[length(daylist)]]$grab_date)

dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 
seq1 <- dates %>% group_by(month = month(dates))  %>% filter(dates==min(dates)) 
seq1 <- seq1$dates

seq2 <- dates %>% group_by(month = month(dates))  %>% filter(dates==max(dates)) 
seq2 <- seq2$dates

seq1 <- tail(seq1, 1)
seq2 <- tail(seq2, 1)

#intermediate matrix for occupation results
omeans <- setNames(data.frame(matrix(ncol = (length(occs)+2), nrow = 0)), c("date", occs, "year"))
omeans %<>% mutate_if(is.logical,as.numeric)

#for (k  in (1:length(seq1))) {
  period <- seq(seq1, seq2, 1)
  
  nvec <- numeric()
  j <- 1
  
  #placeholder for state and occupation results
  omat <- data.frame()
  
for (i in as.character(period)) {
    tmp <- daylist[[paste(i)]]
    
    #merge additional variables not contained in daylist
    tmp <- left_join(tmp, dframe, by = "general_id")
    
    m <- 1
    
    ovec <- numeric()
    for (g in 1:length(occs)) {
      st <- occs[g]
      
      ovec[m] <- count(subset(tmp, idesco_level_3 == st))$n
      
      m <- m+1
      
    }
    omat <- rbind(omat, ovec)
    
    j <- j + 1
}
    
  omeans <- data.frame(ads = colMeans(omat)) 
  omeans$key <- occs
  rownames(omeans) <- occs
  onames$key <- as.character(onames$key)
  omeans <- left_join(omeans, onames, by= "key")


# Top 10 ESCO 3 digits ===================================

esco3count <- omeans[order(omeans$ads, decreasing = TRUE),]

esco3count$share <- round(esco3count$ads/sum(esco3count$ads), 4)*100

esco3count <- data.frame(occupation = esco3count$occupation, job_ads = round(esco3count$ads), share = esco3count$share)

esco3top10 <- esco3count[1:10,]


########################################################
###########Top 10 Employers=======
#####################################################
dframe$companyname <- gsub("\t|\n|\r","",dframe$companyname)

#name vectors
comps <- unique(dframe$companyname)
comps <- comps[!is.na(comps)]

# preselection
ccount <- count(dframe, companyname)
ccount <- ccount[order(ccount$n, decreasing = TRUE),]
empty_as_na <- function(x){
  x[!str_detect(x, "")] <- NA
  return(x)
}

setDT(ccount)
ccount <- ccount %>% mutate_at("companyname", empty_as_na)
ccount <- ccount[1:2000,]

loop <- ccount$companyname[!is.na(ccount$companyname)]

names <- character()
ids <- character()
for (i in loop) {
  tframe <- dframe[companyname==i]
  tmp <- names(table(tframe$sector))[which.max(table(tframe$sector))]
  tmp2 <- names(table(tframe$idsector))[which.max(table(tframe$idsector))]
  names <- c(names, tmp)
  ids <- c(ids, tmp2)
}

sectors <- data.frame(key = loop, idsector = ids, sector = names, stringsAsFactors = FALSE)

# 
# sectors <- subset(dframe, select = c("companyname", "sector", "idsector"))
# 
# sectors$dup <- ifelse(duplicated(sectors$companyname), 1, 0)
# sectors <- subset(sectors, dup == 0)
# sectors <- subset(sectors, select = -dup)



###
l <- year(rdate)

daylist <- readRDS(file = paste0(path, "daylist_step3_", valid, "d_", l , ".rds"))

# define the sequence of months as days 
min(daylist[[1]]$grab_date)
max(daylist[[length(daylist)]]$grab_date)

dates <- data.frame(dates = seq(as.Date(names(daylist)[1])  ,  as.Date(names(daylist[length(daylist)]))  ,"days")) 
seq1 <- dates %>% group_by(month = month(dates))  %>% filter(dates==min(dates)) 
seq1 <- seq1$dates

seq2 <- dates %>% group_by(month = month(dates))  %>% filter(dates==max(dates)) 
seq2 <- seq2$dates

seq1 <- tail(seq1, 1)
seq2 <- tail(seq2, 1)

#intermediate matrix for occupation results
cmeans <- setNames(data.frame(matrix(ncol = (length(loop)+2), nrow = 0), stringsAsFactors = FALSE), c("date", loop, "year"))
cmeans %<>% mutate_if(is.logical,as.numeric)

period <- seq(seq1, seq2, 1)

nvec <- numeric()
j <- 1

#placeholder for company results
cmat <- data.frame()

for (i in as.character(period)) {
  tmp <- daylist[[paste(i)]]
  
  #merge additional variables not contained in daylist
  tmp <- left_join(tmp, dframe, by = "general_id")
  
  m <- 1
  
  cvec <- numeric()
  for (g in 1:length(loop)) {
    st <- loop[g]
    
    cvec[m] <- count(subset(tmp, companyname == st))$n
    
    m <- m+1
    
  }
  cmat <- rbind(cmat, cvec)
  
  j <- j + 1
}

colnames(cmat) <- loop

cmeans <- data.frame(ads = colMeans(cmat)) 


cmeans <- cmeans[order(cmeans$ads, decreasing = TRUE), , drop = FALSE]

#test2 <- dframe[str_detect(dframe$companyname, "Personaldienstleistungen"),]

top10 <- cmeans[1:10,, drop = FALSE]

top10$key <- rownames(top10)

top10 <- left_join(top10, sectors, by= "key")

top10 <- subset(top10, select = -idsector)
colnames(top10) <- c("Job_ads", "Companyname", "Industry sector")
companytop10 <- top10

save(list = c("esco3top10", "companytop10") , file = paste0(resultspath, "top10_", rdate, ".rdata"))


