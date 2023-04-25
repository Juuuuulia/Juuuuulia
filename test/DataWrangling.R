## Load necessary libraries
library(dplyr)
library(tidyr)
library(reshape2)
library(reshape)
library(purrr)
library(RColorBrewer)
library(viridis)
## Set work directory
getwd()
setwd(
  '/Users/stella/Desktop/test'
)
setwd("/Users/stella/Desktop/test")

##########################################
####        CBP dataset        ####
##########################################
## Read employment data
data_cbp <- read.csv("/Users/stella/Desktop/test/data/CBP.CB1400CBP-2023-03-20T092427.csv", stringsAsFactors = FALSE)
## Check and remove NA. In the dataset we have 'na',
## let's change that with NA and then remove it

data_cbp[data_cbp == "na"] <- NA
data_cbp <- data_cbp %>% drop_na()
table(is.na(data_cbp))

replaceCommas<-function(x){
  x<-as.numeric(gsub("\\,", "", x))
}

data_cbp$ESTAB <- replaceCommas(data_cbp$ESTAB)
data_cbp$EMP <- replaceCommas(data_cbp$EMP)
data_cbp$PAYQTR1 <- replaceCommas(data_cbp$PAYQTR1)
data_cbp$PAYANN <- replaceCommas(data_cbp$PAYANN)

# check the na count again
sapply(data_cbp, function(x)
  sum(is.na(x)))

# update the levels again
data_cbp <- droplevels(data_cbp)
## Save as .rds extension for Shiny
saveRDS(data_cbp, file = "data/cbp.rds")
data_cbp <- readRDS("data/cbp.rds")
str(data_cbp)

##########################################
####        PARTNER dataset         ####
##########################################
## Read graduate data
data_partner <- read.csv('data/WITS-Partner.csv')
head(data_partner)
str(data_partner)

data_partner[data_partner == "na"] <- NA
data_partner <- data_partner %>% drop_na()
table(is.na(data_partner))

data_partner$Partner.Name<-as.factor(data_partner$Partner.Name)
data_partner$Year<-as.factor(data_partner$Year)

## Save as .rds extension for Shiny
saveRDS(data_partner, file = "data/data_partner.rds")
##########################################
####        FUEL dataset         ####
##########################################
data_fuel <- read.csv("/Users/stella/Desktop/test/data/DWM_MICROPLOT_FUEL.csv", stringsAsFactors = FALSE)
## Read fuel data
data_fuel <- read.csv('data/DWM_MICROPLOT_FUEL.csv')
head(data_fuel)
str(data_fuel)

data_fuel[data_fuel == "na"] <- NA
data_fuel <- data_fuel %>% drop_na()
table(is.na(data_fuel))

data_fuel$STATE<-as.factor(data_fuel$STATE)
data_fuel$INVYR<-as.numeric(data_fuel$INVYR)
data_fuel$PLOT<-as.numeric(data_fuel$PLOT)
data_fuel$SUBP<-as.numeric(data_fuel$SUBP)
data_fuel$LVSHRBCD<-as.numeric(data_fuel$LVSHRBCD)
data_fuel$DSHRBCD<-as.numeric(data_fuel$DSHRBCD)
data_fuel$LVHRBCD<-as.numeric(data_fuel$LVHRBCD)
data_fuel$DHRBCD<-as.numeric(data_fuel$DHRBCD)
data_fuel$LITTERCD<-as.numeric(data_fuel$LITTERCD)
data_fuel$LVSHRBHT<-as.numeric(data_fuel$LVSHRBHT)
data_fuel$DSHRBHT<-as.numeric(data_fuel$DSHRBHT)
data_fuel$LVHRBHT<-as.numeric(data_fuel$LVHRBHT)
data_fuel$DHRBHT<-as.numeric(data_fuel$DHRBHT)
## Save as .rds extension for Shiny
saveRDS(data_fuel, file = "data/data_fuel.rds")

##########################################
####        COARSE dataset         ####
##########################################
data_coarse <- read.csv("/Users/stella/Desktop/test/data/DWM_COARSE_WOODY_DEBRIS.csv", stringsAsFactors = FALSE)
## Read fuel data
data_coarse <- read.csv('data/DWM_COARSE_WOODY_DEBRIS.csv')
head(data_coarse)
str(data_coarse)

data_coarse[data_coarse == "na"] <- NA
data_coarse <- data_coarse %>% drop_na()
table(is.na(data_coarse))

data_coarse$HOLLOWCD <-as.factor(data_coarse$HOLLOWCD)
data_coarse$STATE <-as.factor(data_coarse$STATE)
data_coarse$SPCD<-as.numeric(data_coarse$SPCD)
data_coarse$DECAYCD<-as.numeric(data_coarse$DECAYCD)
data_coarse$TRANSDIA<-as.numeric(data_coarse$TRANSDIA)
data_coarse$SMALLDIA<-as.numeric(data_coarse$SMALLDIA)
data_coarse$LARGEDIA<-as.numeric(data_coarse$LARGEDIA)
data_coarse$LENGTH<-as.numeric(data_coarse$LENGTH)
data_coarse$CWDHSTCD<-as.numeric(data_coarse$CWDHSTCD)
data_coarse$INVYR<-as.numeric(data_coarse$INVYR)
data_coarse$STATECD<-as.numeric(data_coarse$STATECD)
data_coarse$COUNTYCD<-as.numeric(data_coarse$COUNTYCD)
data_coarse$PLOT<-as.numeric(data_coarse$PLOT)
data_coarse$SUBP<-as.numeric(data_coarse$SUBP)
data_coarse$TRANSECT<-as.numeric(data_coarse$TRANSECT)
data_coarse$CWDID<-as.numeric(data_coarse$CWDID)
data_coarse$MEASYEAR<-as.numeric(data_coarse$MEASYEAR)

## Save as .rds extension for Shiny
saveRDS(data_coarse, file = "data/data_coarse.rds")
