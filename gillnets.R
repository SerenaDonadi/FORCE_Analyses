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

# merge and keep all records in left dataset, and only matching record in righ dataset
gillnets1<-left_join(gillnets0, temp_gillnet, by = c("year","location")) # 

# rename temp to be more specific
gillnets1 <- rename(gillnets1, avg_year_temp = 'temp')
head(gillnets1)


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

# check and remove outliers:
summary(gillnets4)
# remove giant perch, keep gädda
gillnets5<-gillnets4[!(gillnets4$Artbestämning  == "Abborre" & gillnets4$LANGDGRUPP_LANGD > 70),]

# remove NA in response
gillnets5a<-gillnets5 %>%
  filter(!LANGDGRUPP_LANGD == "NA") %>%
  filter(!LANGDGRUPP_ANTAL == "NA") %>%
  filter(!Ansträngning == "NA")

summary(gillnets5a)
str(gillnets5a)

#####
# Grouping
#####

gillnets6<-gillnets5a %>% 
  group_by(LANGDGRUPP_LANGD, location, Artbestämning,year) %>%
  summarise(sum_LANGDGRUPP_ANTAL=sum(LANGDGRUPP_ANTAL ,na.rm=TRUE),
            avg_year_temp=mean(avg_year_temp)
  ) 

#Note: to bring lat and long I shuld convert the longitude/latitude coordinates to 3d cartesian coordinates (x,y,z). Average these 
# (to give a cartesian vector), and then convert back again. Check scripts and other solution here: 
# https://gis.stackexchange.com/questions/7555/computing-an-averaged-latitude-and-longitude-coordinates
# I skip it for now

# Ansträngning may be (check with Peter) given at the level of spatial coordinates - I can't sum up or average the values at this level. I suggest to
# calculate first CPUE and then group, or make a separate grouped dataset and merge

head(gillnets6)
summary(gillnets6)
# there are outiers - check!
