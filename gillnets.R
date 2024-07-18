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

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("gillnet-data.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) gillnets
gillnets <- read.csv2("gillnet-data.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".")  ## I can't read the last file that Agnes sent me,check
head(gillnets)
unique(gillnets$Lokal)

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
unique(gillnets2$GODKAND) # they should be in there
gillnets3<-gillnets2[!gillnets2$GODKAND=="NEJ",]
unique(gillnets3$GODKAND) 
      
# keep only Störning  "NEJ"?
table(gillnets3$Störning,gillnets3$month)
gillnets4<-gillnets3 %>% 
  filter(Störning  == "NEJ")

# keep only Ansträngning = 1?
table(gillnets4$Ansträngning)
gillnets5<-gillnets4 %>% 
  filter(Ansträngning  == 1)

# check and remove outliers:
summary(gillnets5)

gillnets5 %>%
  filter(Art  == "Abborre") %>%
  filter(LANGDGRUPP_LANGD>70)
# remove giant perch
gillnets6<-gillnets5[!(gillnets5$Art  == "Abborre" & gillnets5$LANGDGRUPP_LANGD > 70),]

# substitute NA to values of temp at the time of fishing equal to 999 (a bit too warm)
gillnets6$Temp_vittjning_vid_redskap[gillnets6$Temp_vittjning_vid_redskap==999] <- NA
hist(gillnets6$Temp_vittjning_vid_redskap)

## TO DO: maybe remove some values of Redskapsdetaljnummer, I didn't get which ones and why they are there

#remove NA in response
gillnets7<-gillnets6 %>%
#  filter(!LANGDGRUPP_LANGD == "NA") %>%  #wait, not here, otherwise I delete also inge fångst, which is needed in case of nets where no sp were found
#  filter(!LANGDGRUPP_ANTAL == "NA") %>%
  filter(!Ansträngning == "NA") # no need I selected for now only Ansträngning =1. Correction: needed bc in creating dataset 6 it is generating a bit more than 100 records with all NAs

#####
# Grouping
#####

# pool data for lokal and year
gillnets_antal<-gillnets7 %>% 
  group_by(location, year, Art, LANGDGRUPP_LANGD) %>%
  summarise(sum_LANGDGRUPP_ANTAL=sum(LANGDGRUPP_ANTAL ,na.rm=TRUE)
  ) 

# Note: to bring lat and long I shuold convert the longitude/latitude coordinates to 3d cartesian coordinates (x,y,z). Average these 
# (to give a cartesian vector), and then convert back again. Check scripts and other solution here: 
# https://gis.stackexchange.com/questions/7555/computing-an-averaged-latitude-and-longitude-coordinates
# I skip it for now

gillnets_effort<-gillnets7 %>% 
  group_by(location, year) %>%
  summarise(number_nets=n_distinct(OBS_ID), # check! if correct divide sum_LANGDGRUPP_ANTAL by number_nets to obtain CPUE
            avg_year_temp=mean(avg_year_temp)
  ) 

# Ansträngning may be (check with Peter) given at the level of spatial coordinates, that is net within lokal.Given that I consider
# only Ansträngning = 1, i.e. 1 net per day or night, I count how many nets per lokal to extract the effort. Revise if consider
# other values of Ansträngning

# merge and keep all records in left dataset, and only matching record in right dataset
gillnets_CPUE<-left_join(gillnets_antal, gillnets_effort, by = c("location","year")) 

summary(gillnets_CPUE)
hist(gillnets_CPUE$number_nets)
# check:
filter(gillnets_CPUE, number_nets == 182)
# is it possible that 182 nets were deployed in 2018 in Blekinge län??
filter(gillnets_CPUE, sum_LANGDGRUPP_ANTAL==4766)
# stsp in Gaviksfjärden

# calculate CPUE
gillnets_CPUE$CPUE<-gillnets_CPUE$sum_LANGDGRUPP_ANTAL/gillnets_CPUE$number_nets
hist(gillnets_CPUE$CPUE)
summary(gillnets_CPUE$CPUE)

# aggregate in size classes:
table(gillnets_CPUE$LANGDGRUPP_LANGD) # most values are recorded as *.5
# makes classes of 1 cm as 0.5-1.4, 1.5-2.4,2.5-3.4 etc etc if measurements were rounded up to *.5 value
# or 1-2,2-3,4-5 etc etc if measurements were taken as closest *.5 value

# this will take forever...
gillnets_CPUE$length_group_cat<-as.factor(ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<1.4, '1.5',
                                                 ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<1.5, '1.5', 
                                                 ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<2.5, '2.5', 
                                                        ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<3.5, '3.5', 
                                                               ifelse(gillnets_CPUE$LANGDGRUPP_LANGD<4.5, '4.5', 'E'))))))

# so I round each number - revise once I know how the categories were made
library(plyr)
#####
# using round:
length_group1<-round_any(gillnets_CPUE$LANGDGRUPP_LANGD, 1, round)
hist(length_group)
hist(gillnets_CPUE$LANGDGRUPP_LANGD) # some differences
round_any(2.5, 1, round) # 2
round_any(1.5, 1, round) # 2 Why both are rounded to 2??
round_any(0.5, 1, round) # 0
# this may not be the best way

# using ceiling or floor
length_group2<-round_any(gillnets_CPUE$LANGDGRUPP_LANGD, 1, ceiling) 
length_group3<-round_any(gillnets_CPUE$LANGDGRUPP_LANGD, 1, floor)
hist(length_group1)
hist(length_group2)
hist(length_group3)
# the best is number 2
table(gillnets_CPUE$length_group2)
#####
gillnets_CPUE$length_group<-round_any(gillnets_CPUE$LANGDGRUPP_LANGD, 1, ceiling) 

unique(gillnets_CPUE$Art)

# Abborre dataset
gillnets_CPUE_abbo<-gillnets_CPUE %>%
  filter(Art == "Abborre")

# overall distribution: all years and locations
ggplot(gillnets_CPUE_abbo, aes(x=length_group)) +
  geom_bar()

# wait, I am doing this right? I should account for CPUE 
