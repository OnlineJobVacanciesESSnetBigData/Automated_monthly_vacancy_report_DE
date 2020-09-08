####################################################################################################
#################Program for creating the graphs for OJA indix analogous to Australian Labour department
#####################################################################################################

rm(list = ls())

library(lubridate)

#Chose date  for which the report should be displayed
rdate <- ymd("2020-3-1")
#start date of time series
startdate <- ymd("2018-7-1")

elapsed_months <- function(end_date, start_date) {
  ed <- as.POSIXlt(end_date)
  sd <- as.POSIXlt(start_date)
  12 * (ed$year - sd$year) + (ed$mon - sd$mon)
}

mdiff <- elapsed_months(rdate, startdate)

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
library(fst)
library(magrittr)
library(sf)

load(paste0(resultspath, "tables_", rdate ,".rdata"))

load(paste0(resultspath, "intermediate_results_", rdate, ".rdata"))

snames <- readRDS(paste0(resultspath, "snames_", rdate, ".rds"))
onames <- readRDS(paste0(resultspath, "onames_", rdate, ".rds"))
enames <- readRDS( paste0(resultspath, "enames_", rdate, ".rds"))

#################### Plot the monthly index against UE rate ================================

index <- allmeans/allmeans[1]

date <- (as.Date(statemeans$date, origin = "1970-01-01") )

wmat <- data.frame( date = date, oja_index = index)

#ue <- read_excel("Auxiliary data/unemployment/unemployment_timeline.xls", trim_ws = TRUE)

filename <- Sys.glob("Auxiliary data/unemployment/endergebnisse_monat*")

ue1 <- read_excel(filename, trim_ws = TRUE, sheet = "Monat_Originalwerte_Quoten", range = paste0("A339:A",(339+mdiff)) , col_names = "date")

ue2 <- read_excel(filename, trim_ws = TRUE, sheet = "Monat_Originalwerte_Quoten", range = paste0("CO339:CO",(339+mdiff)) ,col_names = "unemployment_rate")

ue <- data.frame(ue1, ue2)

ue$date <- ymd(ue$date)

uemat <- left_join(wmat, ue, by="date")

#uemat$ue_index <- uemat$unemployment_rate/uemat$unemployment_rate[1]

uemat_long <- reshape2::melt(subset(uemat), id.vars = "date")

coeff <- (uemat$unemployment_rate[1])-1

ggplot(uemat, aes(x=date))+
  geom_line(aes(y = oja_index, color = "red"))+
  geom_line(aes(y = unemployment_rate-coeff , color = "blue"))+
  geom_point(aes(y = oja_index, color = "red"))+
  geom_point(aes(y = unemployment_rate-coeff , color = "blue"))+
  scale_y_continuous(
    # Features of the first axis
    name = "Job ad index",
      # Add a second axis and specify its features
    sec.axis = sec_axis(~.+coeff, name="Unemployment rate"))+
  theme(legend.title=element_blank(), axis.text.x=element_text(angle=45,hjust=1,size=8), legend.position="top")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y")+
  labs( caption = "Job ad index normalized to 07.2018. Unemployment source: German Labour Force Survey") + 
  scale_colour_discrete(labels=c("Job ad index", "Unemployment rate")) 

ggsave(paste0(resultspath, "ps_monthly_index_againstUER_", valid, ".png"), width = 15, height = 12, units = "cm")

##################### Plot stock by state =========================

sfile <- st_read("Auxiliary data/NUTS_RG_10M_2016_3035.shp")
de <- sfile[sfile$CNTR_CODE == "DE",]

names(de)[names(de) == 'NUTS_ID'] <- "idmacro_region"

laender <- de[de$LEVL_CODE == 1,]

lnames <-  data.frame(idmacro_region = laender$idmacro_region, NUTS_NAME = laender$NUTS_NAME)

drops <- c("FID","CNTR_CODE","LEVL_CODE")
laender <- laender[ , !(names(laender) %in% drops)]
laender$idmacro_region <- as.character(laender$idmacro_region)

y <- data.frame(ads = as.numeric(statemeans[nrow(statemeans),2:(ncol(statemeans)-1)]), idmacro_region = colnames(statemeans[,2:(ncol(statemeans)-1)]))

y$idmacro_region <- as.character(y$idmacro_region)
y$ads <- round(y$ads)

ygeo <-left_join(y, laender, by = "idmacro_region")

#ygeo$ads <- round(ygeo$ads/1000)

st_geometry(ygeo) <- ygeo$geometry

ygeo$label <- paste0(ygeo$NUTS_NAME, "\n ", as.character(ygeo$ads))

ygeo$nudge_y <- 0
ygeo$nudge_y[ygeo$NUTS_NAME == "BRANDENBURG"] <- -50000
ygeo$nudge_y[ygeo$NUTS_NAME == "SACHSEN"] <- 5000
ygeo$nudge_y[ygeo$NUTS_NAME == "SACHSEN-ANHALT"] <- -10000
ygeo$nudge_y[ygeo$NUTS_NAME == "MECKLENBURG-VORPOMMERN"] <- -5000
ygeo$nudge_y[ygeo$NUTS_NAME == "SAARLAND"] <- -20000
ygeo$nudge_y[ygeo$NUTS_NAME == "BERLIN"] <- 30000

ygeo$nudge_x <- 0
ygeo$nudge_x[ygeo$NUTS_NAME == "THÜRINGEN"] <- -30000
ygeo$nudge_x[ygeo$NUTS_NAME == "MECKLENBURG-VORPOMMERN"] <- 60000
ygeo$nudge_x[ygeo$NUTS_NAME == "SAARLAND"] <- -30000
ygeo$nudge_x[ygeo$NUTS_NAME == "BERLIN"] <- 30000

ggplot(ygeo)+
  geom_sf(aes(fill = ads))+theme_void()+
  theme(panel.grid.major = element_line(colour = "transparent"))+
  labs()+
  scale_fill_gradient2(name="Monthly stock of\nonline job ads", high = "deepskyblue3")+
  geom_sf_label(data = ygeo, aes(label = label), size = 2, colour = "black", nudge_y = ygeo$nudge_y , nudge_x = ygeo$nudge_x)

ggsave(paste0(resultspath, "Geo_pstocks_NUTS1.png"), width = 20, height = 13.3, units = "cm")

##################### Plot annual change by state =========================

mat <- subset(statestab, select = - c(type, current_t,  lagged_t) )
mat <- mat[4,]
mat <- gather(mat)

mat$value <- mat$value*100

mat <- left_join(mat, snames)
mat <- mat %>% mutate(pos = value >= 0)

ggplot(mat, aes(state, value,  fill = pos)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "", y = "percentage change",
        title = "Annual change by federal state (%)") + 
        theme(legend.position="none", axis.text.x = element_text(angle=60, vjust=1, hjust= 1)) +
  geom_text(aes(y = value, label = value), color = "black", size = 2.5, position = position_stack(vjust = 0.5, reverse = FALSE))

ggsave(paste0(resultspath, "states_annual.png"), width = 15, height = 10, units = "cm")


##################### Plot annual change by education  level =========================

mat <- subset(edutab, select = - c(type, current_t,  lagged_t) )
mat <- mat[4,]
mat <- gather(mat)

mat$value <- mat$value*100

enames <- enames %>% mutate_all(as.character)

mat <- left_join(mat, enames)
mat <- mat %>% mutate(pos = value >= 0)

ggplot(mat, aes(education, value,  fill = pos)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "", y = "percentage change",
        title = "Annual change by education (%)") + 
  theme(legend.position="none", axis.text.x = element_text(angle=60, vjust=1, hjust= 1)) +
  geom_text(aes(y = value, label = value), color = "black", size = 2.5, position = position_stack(vjust = 0.5, reverse = FALSE))+ scale_fill_brewer(palette = "Set2")

ggsave(paste0(resultspath, "educs_annual.png"), width = 15, height = 10, units = "cm")



##################### Plot annual change by occupation =========================

mat <- subset(occutab, select = - c(type, current_t,  lagged_t) )
mat <- mat[4,]
mat <- gather(mat)

mat$value <- mat$value*100

onames <- onames %>% mutate_all(as.character)

mat <- left_join(mat, onames)
mat <- mat %>% mutate(pos = value >= 0)

ggplot(mat, aes(occupation, value,  fill = pos)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs( x = "", y = "percentage change",
        title = "Annual change by occupation group (%)") + 
  theme(legend.position="none", axis.text.x = element_text(angle=60, vjust=1, hjust= 1)) +
  geom_text(aes(y = value, label = value), color = "black", size = 2.5, position = position_stack(vjust = 0.5, reverse = FALSE))+ scale_fill_brewer(palette = "Set1")

ggsave(paste0(resultspath, "occs_annual.png"), width = 15, height = 10, units = "cm")

