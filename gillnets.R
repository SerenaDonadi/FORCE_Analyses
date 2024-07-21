rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------

library(gplots)

library(tidyverse)
# library(ggplot2)
#library(dplyr)
#library(tidyr)
library(lattice)
library(nlme)
library(MASS)
library(piecewiseSEM)
library(lme4)
library(car)
library(visreg)

library(ExcelFunctionsR)
library(plyr)


#####
# Read Datasets
####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("gillnet-data.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) gillnets
gillnets <- read.csv2("gillnet-data.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".")  ## I can't read the last file that Agnes sent me,check
head(gillnets)

# make column with only year
head(gillnets$Fiskedatum)
gillnets$year<-as.numeric(RIGHT(gillnets$Fiskedatum,4))
summary(gillnets$year)
hist(gillnets$year)

# make column with only month
gillnets$month<-as.numeric(LEFT(RIGHT(gillnets$Fiskedatum,7),2))
summary(gillnets$month)
hist(gillnets$month)

# rename columns before merging
gillnets0 <- rename(gillnets, location = 'Lokal')

### 2) temp
temp_gillnet <- read.csv2("df_gillnet_temp.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")

# merge and keep all records in left dataset, and only matching record in right dataset
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

# keep only GODKAND "JA"? But why I don't see NAs?
table(gillnets2$GODKAND)
unique(gillnets2$GODKAND) # they should be in there, but without quote because it is not considered a level
# when showing a dataset R uses <NA>, this is just the way it displays NA in a factor
# however, if I use this, it will remove NEJ but also NAs (hence ingen fångst)
#gillnets3<-gillnets2[!gillnets2$GODKAND=="NEJ",]
#unique(gillnets3$GODKAND) # not here
#table(gillnets2$Art,gillnets2$GODKAND)
#table(gillnets3$Art,gillnets3$GODKAND) # but here
# so I trY
gillnets3a<-subset(gillnets2, GODKAND=="JA "| is.na(GODKAND)) # | is or
unique(gillnets3a$GODKAND) 
table(gillnets3a$Art,gillnets3a$GODKAND) # ok!

      
# keep only Störning  "NEJ"?
unique(gillnets3a$Störning) # there are no NAs
table(gillnets3a$Störning,gillnets3a$month)
gillnets4<-gillnets3a %>% 
  filter(Störning  == "NEJ")

# remove columns not needed:
gillnets4<-gillnets4 %>%
  select(-c(Info_publik,GODKAND ,STORNINGAR)) 


# keep only Ansträngning = 1?
table(gillnets4$Ansträngning)
unique(gillnets4$Ansträngning) # there are NAs. If I don't want to retain them:
gillnets5<-gillnets4 %>% 
  filter(Ansträngning  == 1 ) 

# check and remove outliers:
summary(gillnets5)

gillnets5 %>%
  filter(Art  == "Abborre") %>%
  filter(LANGDGRUPP_LANGD>70)
# remove giant perch 
gillnets6<-gillnets5[!(gillnets5$Art  == "Abborre" & gillnets5$LANGDGRUPP_LANGD > 70),]

# substitute NA to values of temp at the time of fishing equal to 999 (a bit too warm)
gillnets6$Temp_vittjning_vid_redskap[gillnets6$Temp_vittjning_vid_redskap==999] <- NA
hist(gillnets5$Temp_vittjning_vid_redskap)
hist(gillnets6$Temp_vittjning_vid_redskap)

## TO DO: maybe remove some values of Redskapsdetaljnummer, I didn't get which ones and why they are there

summary(gillnets4$Ansträngning)
summary(gillnets5$Ansträngning)
summary(gillnets6$Ansträngning) # why? I have here >100 records, with NAs for all variables. but the 2 rows less than before
table(gillnets5$Ansträngning)
table(gillnets6$Ansträngning)

gillnets6 %>%
  filter(is.na(Ansträngning))

#remove NA in response (but not in LANGDGRUPP_LANGD or LANGDGRUPP_ANTAL, otherwise I delete also inge fångst)
gillnets7<-gillnets6 %>%
  filter(!is.na(Ansträngning)) # no need I selected for now only Ansträngning=1. Correction: needed bc when creating dataset 6 it generates >100 records with NAs


#####
# Grouping
#####

# create size classes:
table(gillnets7$LANGDGRUPP_LANGD) # most values are recorded as *.5
# makes classes of 1 cm as 0.5-1.4, 1.5-2.4,2.5-3.4 etc etc if measurements were rounded up to *.5 value
# or 1-2,2-3,4-5 etc etc if measurements were taken as closest *.5 value

# this will take forever...
#gillnets_CPUE$length_group_cat<-as.factor(ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<1.4, '1.5',
#                                                 ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<1.5, '1.5', 
#                                                 ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<2.5, '2.5'))))

# so I round each number - revise once I know how the categories were made
#####
# using round:
length_group1<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, round)
hist(length_group1)
hist(gillnets7$LANGDGRUPP_LANGD) # some differences
round_any(2.5, 1, round) # 2
round_any(1.5, 1, round) # 2 Why both are rounded to 2??
round_any(0.5, 1, round) # 0
# this may not be the best way

# using ceiling or floor
length_group2<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, ceiling) 
length_group3<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, floor)
hist(length_group1)
hist(length_group2)
hist(length_group3)
# the best is number 2
table(length_group2)
#####
gillnets7$length_group<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, ceiling) # subtract 0.5 for original values

head(gillnets7)

gillnets7 %>%
  filter(Art=="_ingen fångst")

### create a dataset where 1 row corresponds to 1 individual
# this is for the calculation of length distribution indexes
# OBS: I have to remove NAs from LANGDGRUPP_ANTAL. But to calculate CPUE (and effort) consider dataset with NAs (gillnets7)

# remove NAs in LANGDGRUPP_ANTAL and repeat rows as many times as LANGDGRUPP_ANTAL:
gillnets_indiv<- gillnets7 %>%
  filter(!is.na(LANGDGRUPP_ANTAL)) %>%
  uncount(LANGDGRUPP_ANTAL)

head(gillnets_indiv)

# calulate mean, median and L90 and skewenss for Abborre, for each location and year:
gillnets_length_indexes<- gillnets_indiv %>%
  filter(Art=="Abborre") %>%
  group_by(location, year) %>%
  summarise(mean_length=mean(length_group ,na.rm=TRUE),
            median_length=median(length_group ,na.rm=TRUE)
  ) 
head(gillnets_length_indexes)

### calculate CPUE per size categories, using dataset with ingen fångst
# first, calculate sum of indiv per size category
gillnets_freq<-gillnets7 %>% 
  group_by(location, year, Art, length_group) %>%
  summarise(tot_number=sum(LANGDGRUPP_ANTAL ,na.rm=TRUE)
  ) 

# second, calculate effort per each location and year
gillnets_effort<-gillnets7 %>% 
  group_by(location, year) %>%
  summarise(number_nets=n_distinct(OBS_ID), # check! if correct divide sum_LANGDGRUPP_ANTAL by number_nets to obtain CPUE
            avg_year_temp=mean(avg_year_temp)
  ) 

# Note1: to bring lat and long I shuold convert the longitude/latitude coordinates to 3d cartesian coordinates (x,y,z). Average these 
# (to give a cartesian vector), and then convert back again. Check scripts and other solution here: 
# https://gis.stackexchange.com/questions/7555/computing-an-averaged-latitude-and-longitude-coordinates
# I skip it for now

# Note2: Ansträngning may be (check with Peter) given at the level of spatial coordinates, that is net within lokal.Given that I consider
# only Ansträngning = 1, i.e. 1 net per day or night, I count how many nets per lokal to extract the effort. Revise if consider
# other values of Ansträngning

# merge and keep all records in left dataset, and only matching record in right dataset
gillnets_CPUE<-left_join(gillnets_freq, gillnets_effort, by = c("location","year")) 

summary(gillnets_CPUE)
hist(gillnets_CPUE$number_nets)
# check:
filter(gillnets_CPUE, number_nets == 182)
# is it possible that 182 nets were deployed in 2018 in Blekinge län??
filter(gillnets_CPUE, tot_number==4766)
# stsp in Gaviksfjärden

# third, calculate CPUE
gillnets_CPUE$CPUE<-gillnets_CPUE$tot_number/gillnets_CPUE$number_nets
hist(gillnets_CPUE$CPUE)

head(gillnets_CPUE)


### calculate tot CPUE (pooled across size categories)
gillnets_totCPUE<-gillnets_CPUE %>% 
  group_by(location, year, Art) %>%
  summarise(totCPUE=sum(CPUE ,na.rm=TRUE)
  ) 


# SUMMARY of key dataset
# gillnets_CPUE: replicated at level of location, year, size categories - useful for plotting
# gillnets_totCPUE: replicated at level of location, year
# gillnets_length_indexes: replicated at level of location, year but only for Abborre - useful for stat. 

# merge/combine as you like!
# likely combine gillnets_length_indexes with totCPUE, and put totCPUE of different spp in columns
# PS: add L90 and skeweness ;)

#####
# exploratory plots
#####
# overall barplot for all locations and years but different spp
ggplot(gillnets_CPUE, aes(x=length_group, y=CPUE)) +
  geom_bar(stat="identity")+
  facet_wrap(~Art)+
  theme_bw(base_size=15)

# Abborre dataset
gillnets_CPUE_abbo<-gillnets_CPUE %>%
  filter(Art == "Abborre")

# barplots of Abbo for all years but different locations
ggplot(gillnets_CPUE_abbo, aes(x=length_group, y=CPUE, col=location)) +
  geom_bar(stat="identity")+
  facet_wrap(~location)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

# barplots of Abbo for different years in one specific location
ggplot(subset(gillnets_CPUE_abbo, location %in% "Askrikefjärden"), aes(x=length_group, y=CPUE)) +
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme_bw(base_size=15)+
  theme(legend.position="none")





