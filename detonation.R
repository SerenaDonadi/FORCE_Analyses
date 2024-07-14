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
deto <- read.csv2("detonation-data.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(deto)
unique(deto$Lokal)

# make column with only year
head(deto$Fiskedatum)
deto$year<-as.numeric(RIGHT(deto$Fiskedatum,4))
summary(deto$year)
hist(deto$year)

# make column with only month
deto$month<-as.numeric(LEFT(RIGHT(deto$Fiskedatum,7),2))
summary(deto$month)
hist(deto$month)

# rename columns before merging
gillnets0 <- rename(gillnets, location = 'Lokal')

### 2) temp
temp_deto <- read.csv2("df_juv_temp.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(temp_deto)

# make column with only year
head(temp_deto$date)
temp_deto$year<-as.numeric(RIGHT(temp_deto$date,4))
summary(temp_deto$year)
hist(temp_deto$year)

# make column with only month
temp_deto$month<-as.numeric(LEFT(RIGHT(temp_deto$date,7),2))
summary(temp_deto$month)
hist(temp_deto$month)

# before merging:
# understand how the temp vaariables are aggregated (month, day, year)
# rename variables

# merge

#####
# Subsets
#####
