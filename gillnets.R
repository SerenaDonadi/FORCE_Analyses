rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------
library(gplots)
library(ggplot2)
library(lattice)
library(nlme)
library(MASS)
library(piecewiseSEM)
library(lme4)
library(car)
library(visreg)
library(dplyr)
library(ExcelFunctionsR)


#####
# Read Datasets
####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

### 1) gillnets
gillnets <- read.csv2("gillnet-data.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(gillnets)
unique(gillnets$Lokal)

# make column with only year
head(gillnets$Fiskedatum)
gillnets$year<-as.numeric(LEFT(gillnets$Fiskedatum,4))
summary(gillnets$year)
hist(gillnets$year)

# make column with only month
gillnets$month<-as.numeric(RIGHT(LEFT(gillnets$Fiskedatum,7),2))
summary(gillnets$month)
hist(gillnets$month)

# rename columns before merging
gillnets0 <- rename(gillnets, location = 'Lokal')

### 2) temp
temp_gillnet <- read.csv2("df_gillnet_temp.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(temp_gillnet)

# merge and keep all records in left dataset, and only matching record in righ dataset
gillnets1<-left_join(gillnets0, temp_gillnet, by = c("year","location")) # 


#####
# Subsets
#####

# take only august data for now:
gillnets2<-gillnets1 %>% 
  filter(month == 8)

# keep only GODKAND "JA"?
table(gillnets$GODKAND,gillnets$month)
gillnets3<-gillnets2 %>% 
  filter(GODKAND  == "JA ")

# keep only Störning  "NEJ"?
table(gillnets$Störning,gillnets$month)
gillnets4<-gillnets3 %>% 
  filter(Störning  == "NEJ")


