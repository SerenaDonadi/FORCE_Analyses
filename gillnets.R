rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
library(dplyr)
#library(tidyr)
library(gplots)
library(lattice)
library(nlme)
# library(MASS) # potenital name clash problem for function select in dplyr
library(piecewiseSEM)
library(lme4)
library(car)
library(visreg)

library(ExcelFunctionsR)
#library(plyr)


#####
# Read Datasets
#####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("gillnet-data-unfiltered-NEW.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) gillnets
# gillnets <- read.csv2("gillnet-data-unfiltered-NEW.csv",fileEncoding="UTF-8",  header=TRUE, sep=",", dec=".") 
head(gillnets)

###UPDATE: NEW FILE WITH some areas previously excluded (Forsmark 2019, Holmon 2015, Muskö 2013), and
# column "Artbestämning" with infor on okänd art (but eventually is really few indiv that are usefl for us):
guess_encoding("gillnet-data-unfiltered.csv", n_max = 1000)
gillnets <- read.csv2("gillnet-data-unfiltered.csv",fileEncoding="UTF-8",  header=TRUE, sep=",", dec=".") 
head(gillnets0)

# delete this after everything is running
table(gillnets$Art)
table(gillnets0$Art)
table(gillnets0$Artbestämning)

gillnets0 %>%
  filter(Art == "Ok nd") %>%
  select(Artbestämning) %>%
  unique()

# do it later if I have time. or maybe merge it at a certain point


# make column with only month
head(gillnets$Fiskedatum)
gillnets$month<-as.numeric(LEFT(RIGHT(gillnets$Fiskedatum,7),2))
summary(gillnets$month)
hist(gillnets$month)

gillnets$date<-as.Date(gillnets$Fiskedatum, format ="%d/%m/%Y")

# rename columns before merging
gillnets <- rename(gillnets, location = 'Lokal')
gillnets <- rename(gillnets, year = 'År')

### 2) temp

# avg year temp from satellite
temp_gillnet_year <- read.csv2("df_gillnet_temp.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".") # year avg temp
# avg day temp from satellite
temp_gillnet_day <- read.csv2("temperature-data.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")  # daily temp

# make column with only year
head(temp_gillnet_day$date)
temp_gillnet_day$year<-as.numeric(LEFT(temp_gillnet_day$date,4))
summary(temp_gillnet_day$year)
hist(temp_gillnet_day$year)

# make column with only month
temp_gillnet_day$month<-as.numeric(LEFT(RIGHT(temp_gillnet_day$date,5),2))
summary(temp_gillnet_day$month)
hist(temp_gillnet_day$month)

# set the format for date:
#temp_gillnet_day$date<-as.Date(temp_gillnet_day$date,"%d/%m/%Y")

## OBS in same cases temp doesn´t vary between January and April. It could depend on that there is ice, or a problem with data extraction
# this is mainly found in the 80s. but also later, see Råneå example. check what parameters to extract and which years we consider
ggplot(subset(temp_gillnet_day, location %in% c("Asköfjärden","Askrikefjärden","Finbo, Åland",
                                                "Forsmark","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kumlinge, Åland","Kvädöfjärden",
                                                "Lagnö","Långvindsfjärden","Norrbyn","Råneå","Simpevarp",
                                                "Torhamn, Karlskrona Ö skärgård")), 
              aes(x = date , y = temp)) +
  geom_point()+
  facet_wrap(~location)+
  labs(title="daily temp ")+
  theme_classic(base_size=13)

ggplot(subset(temp_gillnet_day, location %in% c("Råneå")), aes(x = date , y = temp)) +
  geom_point()+
  facet_wrap(~location)+
  labs(title="daily temp ")+
  theme_classic(base_size=13)

# should I merge based on location or Fångstområde? location
sort(unique(temp_gillnet_day$location)) #62
sort(unique(temp_gillnet_year$location)) #62
sort(unique(gillnets$Fångstområde)) #111
sort(unique(gillnets$location))  # 60
# later addition: but now we are running analyses at the level of Fångstområde...ask Agnes?

# what locations are in the temp dataset but not in the gillnets dataset? Björnöfjärden and Älgöfjärden
temp_gillnet_day %>%
  filter(!(location %in% gillnets$location)) %>%
  select(location) %>%
  unique()

# merge and keep all records in left dataset, and only matching record in right dataset
gillnets1<-left_join(gillnets, temp_gillnet_year, by = c("year","location")) # 

# rename temp to be more specific
gillnets1 <- rename(gillnets1, avg_year_temp = 'temp')
head(gillnets1)

# merge and keep all records in left dataset, and only matching record in right dataset
is.Date(temp_gillnet_day$date)
gillnets1a<-left_join(gillnets1, temp_gillnet_day, by = c("date","month","year","location")) # 

# rename temp to be more specific: we don't know whether is daily avg or one time, how many time the satellite passed by, and at what time
gillnets1a <- rename(gillnets1a, day_temp = 'temp')
head(gillnets1a)

# any missing values for any location in the gillnets dataset?
# not for day temp, but yes for year temp: they are the data from 2021-2023
gillnets1a %>%
  filter(is.na(day_temp)) %>%
  select(location) %>%
  unique()
summary(gillnets1a$day_temp)

gillnets1a %>%
  filter(is.na(avg_year_temp)) %>%
  select(location) %>%
  unique()
gillnets1a %>%
  filter(is.na(avg_year_temp)) %>%
  select(year) %>%
  unique()
summary(gillnets1a$avg_year_temp)


##### check correlations between temp values ####
# check how well the temp measured in the field correlate con satellite temp: do it in a separate section, check there is some weird stuff
ggplot(subset(subset(gillnets1a, Temp_vittjning_vid_redskap != 999) , location %in% c("Asköfjärden","Askrikefjärden","Finbo", 
                                                #"Åland","Forsmark","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kumlinge, Åland","Kvädöfjärden",
                                                #"Lagnö","Långvindsfjärden","Norrbyn","Råneå","Simpevarp",
                                                "Torhamn, Karlskrona Ö skärgård")),
                 aes(x = Temp_vittjning_vid_redskap , y = day_temp, fill= month)) +
  geom_point()+
  facet_wrap(~location)+
  geom_smooth(method = "lm")+
  labs(title="")+
  theme_classic(base_size=13)

# do it later, after cleaning and grouping the dataset

#####
# Subsets
#####

# filter out stations that are disturbed 
unique(gillnets1a$Störning) # all NEJ. Not in the most recent version
gillnets1b = subset(gillnets1a, Störning == "NEJ" | is.na(Störning)) # 

## filter out stations that are not GODKAND
unique(gillnets1b$GODKAND) # JA or NA. Not in the most recent version
gillnets1c = subset(gillnets1b, GODKAND == "JA " | is.na(GODKAND))  # 

# Note: why I don't see NAs with table(gillnets1$GODKAND)? 
# unique(gillnets$GODKAND) # they should be in there, but without quote because it is not considered a level
# when showing a dataset R uses <NA>, this is just the way it displays NA in a factor
# however, if I use: gillnets3<-gillnets2[!gillnets2$GODKAND=="NEJ",] # it will remove NEJ but also NAs (hence ingen fångst), so I use:
#gillnets2 = subset(gillnets1, GODKAND == "JA " | is.na(GODKAND))  # none
#table(gillnets2$Art,gillnets2$GODKAND) # ok!

# filter out stations 991-995 (hydrographic stations).
sort(unique(gillnets1c$StationsNr)) # I don't see any
gillnets2 = subset(gillnets1c, !(StationsNr %in% 991:995)) 

# remove restricted data 
unique(gillnets2$Behörighet) # none is "Restriktion".Not in the most recent version
table(gillnets2$Behörighet) 
gillnets2a = subset(gillnets2, !(Behörighet %in% "Restriktion")) 

# intern may have something odd, check them: boh
subset(gillnets2a, Behörighet == "Intern")

# check samples that may not have Ansträngning = 1: all good.Not in the most recent version
table(gillnets2a$Ansträngning)
gillnets2b = subset(gillnets2a, Ansträngning == "1")

# check samples that may have effort expressed in hours:none
unique(gillnets2b$Fisketid_enhet)
unique(gillnets2b$Ansträngning_enhet)

# check samples that have effort different from 1: remove them. none in the most recent version
unique(gillnets2b$Fisketid)
#gillnets2<-subset(gillnets1a, Fisketid != 12)

# check where the station number is listed, as in some cases it may be listed in the information column
unique(gillnets2b$station)
unique(gillnets2b$StationsNr)
unique(gillnets2b$Information)

# check comments and remove possibly weird samples:
unique(gillnets2b$Info_publik)

subset(gillnets2b, Info_publik == "Låg vattentemp och många störda stationer pga igensatta nät har gett mycket liten fångst ")
ggplot(subset(gillnets2b, location  == "Råneå"), aes(x = year, y = Antal)) +
  geom_point(size=2)+ 
  facet_wrap(~Info_publik)+
  theme_bw(base_size=15)
# Antal is quite low, but also comparable with year 2009 and 2012. 
# I'll keep it for now. if I want to remove it use: 
# not this script it, it removes too many rows: # gillnets3<-subset(gillnets2, Info_publik != "Låg vattentemp och många störda stationer pga igensatta nät har gett mycket liten fångst ")
# same with this: #gillnets3<-gillnets2 %>%
#  filter(Info_publik != "Låg vattentemp och många störda stationer pga igensatta nät har gett mycket liten fångst ")
# if I use this script it is messing up with NAs: # gillnets3<-gillnets2[! gillnets2$Info_publik == "Låg vattentemp och många störda stationer pga igensatta nät har gett mycket liten fångst ",]
# This looks ok:
#litecatch <- which(with(gillnets2, Info_publik == "Låg vattentemp och många störda stationer pga igensatta nät har gett mycket liten fångst "))
# gillnets3 <- gillnets2[ -litecatch, ]

unique(gillnets2b$Störning) 

subset(gillnets2b, Info_publik == "Många fiskar har angivits som skadad fångst, ospecificerat vad/vem som orsakat skadorna på fisken." )
# looks ok, keep

subset(gillnets2b, Info_publik == "En station 2009-08-14 är fiskad 2 nätter." & Fiskedatum == "14/08/2009")
# remove:
gillnets3<-gillnets2b[!(gillnets2b$Info_publik == 'En station 2009-08-14 är fiskad 2 nätter.' & 
                          gillnets2b$Fiskedatum == '14/08/2009'),]
# now this script seems to work

# keep only Ansträngning = 1?
table(gillnets3$Ansträngning)
unique(gillnets3$Ansträngning) # there are NAs. If I don't want to retain them:
gillnets4<-gillnets3 %>% 
  filter(Ansträngning  == 1 ) 

## TO DO: maybe remove some values of Redskapsdetaljnummer, I didn't get which ones and why they are there. no need 

# remove columns not needed:
gillnets5<-gillnets4 %>%
  select(-c(Info_publik,GODKAND ,STORNINGAR))

# remove fish < 12 cm, which are poorly sampled by gillnets:
gillnets6<-gillnets5 %>% 
  filter(LANGDGRUPP_LANGD >= 12 | is.na(LANGDGRUPP_LANGD)) # to keep also values "ingen fångst"

# check and remove outliers:
summary(gillnets6)

gillnets6 %>%
  filter(Art  == "Abborre") %>%
  select(LANGDGRUPP_LANGD)%>%
  unique() 

gillnets6 %>%
  filter(Art  == "Abborre") %>%
  filter(LANGDGRUPP_LANGD > 70)
  
# remove giant perch - # no need with the new dataset
#gillnets6<-gillnets5[!(gillnets5$Art  == "Abborre" & gillnets5$LANGDGRUPP_LANGD > 70),]

# substitute NA to values of temp at the time of fishing equal to 999 (a bit too warm)
sort(unique(gillnets6$Temp_vittjning_vid_redskap))
gillnets6$Temp_vittjning_vid_redskap[gillnets6$Temp_vittjning_vid_redskap==999] <- NA
hist(gillnets6$Temp_vittjning_vid_redskap)

# take only august data for now:
gillnets7<-gillnets6 %>% 
  filter(month == 8)

# check if there is anything weird here: ok
#gillnets7 %>% 
#  filter(Behörighet  == "Intern" ) 
unique(gillnets7$FISKE)
#svartmunnad_dataset<-gillnets4 %>% 
#  filter(FISKE  == "Svartmunnad Smörbult Inventering" ) 
#unique(svartmunnad_dataset$Info_publik)
#unique(svartmunnad_dataset$Information)


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
library(plyr)
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
library(plyr)
gillnets7$length_group<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, ceiling) # I have to subtract 0.5 to obtain original values

head(gillnets7)

gillnets7 %>%
  filter(Art=="_ingen fångst")

# rename sublocations for merging later:
gillnets7 <- rename(gillnets7, sub.location = 'Fångstområde')


# extract only date*location (lat and long) and export for Ingrid to extract temperature data from loggers
# this extraction was done with a previous version of the data (less samples)
#####
gillnets7_to_Ingrid<-gillnets7 %>%
  select(c(År,Fiskedatum ,location,Fångstområde , StationsNr, Lat_grader,Long_grader, Temp_vittjning_vid_redskap)) 
# remove duplicates
duplicated(gillnets7_to_Ingrid)
gillnets7_to_Ingrid2 = gillnets7_to_Ingrid[!duplicated(gillnets7_to_Ingrid),]
head(gillnets7_to_Ingrid2)
library(openxlsx)
write.xlsx(gillnets7_to_Ingrid2, file="C:/Users/sedi0002/Google Drive/FORCE/Output/gillnets7_to_Ingrid2.xlsx",
           sheetName = "", colNames = TRUE, rowNames = TRUE, append = F)

# rename columns 
gillnets7_to_Ingrid2 <- rename(gillnets7_to_Ingrid2, year = 'År')
gillnets7_to_Ingrid2 <- rename(gillnets7_to_Ingrid2, catch_date = 'Fiskedatum')
gillnets7_to_Ingrid2 <- rename(gillnets7_to_Ingrid2, long = 'Long_grader')
gillnets7_to_Ingrid2 <- rename(gillnets7_to_Ingrid2, lat = 'Lat_grader')

#####

### create a dataset where 1 row corresponds to 1 individual
# this is for the calculation of length distribution indexes
# OBS: I have to remove NAs from LANGDGRUPP_ANTAL. But to calculate CPUE (and effort) consider dataset with NAs (gillnets7)

# remove NAs in LANGDGRUPP_ANTAL and repeat rows as many times as LANGDGRUPP_ANTAL:
gillnets_indiv<- gillnets7 %>%
  filter(!is.na(LANGDGRUPP_ANTAL)) %>%
  uncount(LANGDGRUPP_ANTAL)

head(gillnets_indiv)

library(datawizard)

# before running group_by I may need to detach package plyr, If I used it:
detach("package:plyr", unload=TRUE)
unloadNamespace("plyr")
detach("package:ExcelFunctionsR", unload=TRUE)
library(dplyr)

# # Convert dates to Date objects, so that I can calculate an avg date to use later with climwin:
#library(lubridate)
#gillnets_indiv$date <- dmy(gillnets_indiv$Fiskedatum)

# use location or sub.location (Fångstområde) for grouping? Fångstområde
table(gillnets_indiv$location, gillnets_indiv$Fångstområde)
unique(gillnets_indiv$Fångstområde) #78
unique(gillnets_indiv$location) #52

# rename column Fångstområde as sub.location:
gillnets_indiv <- rename(gillnets_indiv, sub.location = 'Fångstområde')

gillnets_indiv %>%
  filter(Art=="Abborre") %>%
  filter(sub.location=="Vaxholm")%>%
  filter(year=="2016")

# calulate mean, median and L90 and skewenss, kurtosis for Abborre, for each location, sublocation? and year:
is.Date(gillnets_indiv$date)
gillnets_length_indexes<- gillnets_indiv %>%
  filter(Art=="Abborre") %>%
  group_by(location, sub.location, year) %>%
  summarise(field_temp= mean(Temp_vittjning_vid_redskap,na.rm=TRUE),
            mean_length=mean(length_group ,na.rm=TRUE),
            n_fish=n(), ## 
            median_length=median(length_group ,na.rm=TRUE),
            L90=quantile(length_group ,0.90,na.rm=TRUE),
            sk1=skewness(length_group ,remove_na = TRUE, type = "1", iterations = 100),
            sk2=skewness(length_group ,remove_na = TRUE, type = "2", iterations = 100),
            ku1=kurtosis(length_group ,remove_na = TRUE, type = "1", iterations = 100),
            ku2=skewness(length_group ,remove_na = TRUE, type = "2", iterations = 100),
            # calculate an avg date of sampling:
            date=mean(date ,na.rm=TRUE))
            # count how many sampling days per location and year:
            #n_days=n_distinct(date)) # this doesn't work. I don't need it for now

head(gillnets_length_indexes)
table(gillnets_length_indexes$n_fish)
#table(gillnets_length_indexes$n_days)

gillnets_length_indexes %>%
  filter(n_fish==2055) %>%
  select(location,sub.location,year,date)

gillnets_indiv %>%
  filter(Art=="Abborre") %>%
  filter(sub.location=="Kumlinge, Åland" & year==2022)%>%
  select(date)%>%
  unique()
  
# check if I can skip SE calculation
#s1<-skewness(gillnets_indiv$length_group ,remove_na = TRUE, type = "1", iterations = 100)
#s11<-skewness(gillnets_indiv$length_group ,remove_na = TRUE, type = "1")

# From R help(skeweness) in R: there are three different methods for estimating skewness, as discussed in Joanes and Gill (1988):
# Type "1" is the "classical" method, which is g1 = (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5
# Type "2" first calculates the type-1 skewness, then adjusts the result: G1 = g1 * sqrt(n * (n - 1)) / (n - 2). This is what SAS and SPSS usually return.
# Type "3" first calculates the type-1 skewness, then adjusts the result: b1 = g1 * ((1 - 1 / n))^1.5. This is what Minitab usually returns.apparently there are severalk ways to calculate skeweness based on the type of distribution of my data. check:
# same for kurtosis. I calculate type 1 and 2. They are very similar for skewness, but larger differences for kurtosis


### calculate CPUE per size categories, using dataset with ingen fångst
# first, calculate sum of indiv per size category
gillnets_freq<-gillnets7 %>% 
  group_by(location,sub.location, year, Art, length_group) %>%
  summarise(number_indiv=sum(LANGDGRUPP_ANTAL ,na.rm=TRUE)
  ) 

# second, calculate effort per each location and year
gillnets_effort<-gillnets7 %>% 
  group_by(location,sub.location, year) %>%
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
gillnets_CPUE<-left_join(gillnets_freq, gillnets_effort, by = c("location","sub.location","year")) 

summary(gillnets_CPUE)
hist(gillnets_CPUE$number_nets)
summary(gillnets_CPUE$number_nets)

# check:
filter(gillnets_CPUE, number_nets == 90)
# is it possible that 153 nets were deployed in 2017 in Blekinge län??

# third, calculate CPUE
gillnets_CPUE$CPUE<-gillnets_CPUE$number_indiv/gillnets_CPUE$number_nets
hist(gillnets_CPUE$CPUE)

head(gillnets_CPUE)

# if I want to use CPUE per size category as explanatory, I should go to wide format
# spread rows into columns to obtain CPUE of spp SLIT BY SIZE CLASSES and retain zeros - only for perch!

# Abborre dataset
gillnets_CPUE_abbo<-gillnets_CPUE %>%
  filter(Art == "Abborre")
unique(gillnets_CPUE_abbo$length_group)
#remova NA in length grupp:
gillnets_CPUE_abbo<-gillnets_CPUE_abbo %>%
  filter(!is.na(length_group)) 
gillnets_CPUE_abbo$length_group<-as.factor(gillnets_CPUE_abbo$length_group)
# remove number_indiv, which is not needed and is hindering pivot_wider from grouping by site and year:
gillnets_CPUE_abbo<-gillnets_CPUE_abbo %>%
  select(-c(number_indiv, number_nets))
# sort descending dataset by location, year and length group
gillnets_CPUE_abbo<-gillnets_CPUE_abbo[order(gillnets_CPUE_abbo$location,gillnets_CPUE_abbo$sub.location, gillnets_CPUE_abbo$year, gillnets_CPUE_abbo$length_group),]

# check overall frequency for each size classes
plot(gillnets_CPUE_abbo$length_group, gillnets_CPUE_abbo$CPUE)


library(tidyr)
gillnets_CPUE_abbo_wide<-pivot_wider(gillnets_CPUE_abbo, names_from = length_group, values_from = CPUE)
# replace with 0 only for spp, not temp! check that there are no zeros
summary(gillnets_CPUE_abbo_wide$avg_year_temp)
gillnets_CPUE_abbo_wide[is.na(gillnets_CPUE_abbo_wide)] <- 0  
gillnets_CPUE_abbo_wide$avg_year_temp[gillnets_CPUE_abbo_wide$avg_year_temp==0] <- NA

levels(gillnets_CPUE_abbo$length_group)
# show names of columns:
colnames(gillnets_CPUE_abbo_wide)
# rename columns:
gillnets_CPUE_abbo_wide2 <- rename(gillnets_CPUE_abbo_wide, 
                                   CPUE_Abborre_12cm = '12',
                                   CPUE_Abborre_13cm = '13',
                                   CPUE_Abborre_14cm = '14',
                                   CPUE_Abborre_15cm = '15',
                                   CPUE_Abborre_16cm = '16',
                                   CPUE_Abborre_17cm = '17',
                                   CPUE_Abborre_18cm = '18',
                                   CPUE_Abborre_19cm = '19',
                                   CPUE_Abborre_20cm = '20',
                                   CPUE_Abborre_21cm = '21',
                                   CPUE_Abborre_22cm = '22',
                                   CPUE_Abborre_23cm = '23',
                                   CPUE_Abborre_24cm = '24',
                                   CPUE_Abborre_25cm = '25',
                                   CPUE_Abborre_26cm = '26',
                                   CPUE_Abborre_27cm = '27',
                                   CPUE_Abborre_28cm = '28',
                                   CPUE_Abborre_29cm = '29',
                                   CPUE_Abborre_30cm = '30',
                                   CPUE_Abborre_31cm = '31',
                                   CPUE_Abborre_32cm = '32',
                                   CPUE_Abborre_33cm = '33',
                                   CPUE_Abborre_34cm = '34',
                                   CPUE_Abborre_35cm = '35',
                                   CPUE_Abborre_36cm = '36',
                                   CPUE_Abborre_37cm = '37',
                                   CPUE_Abborre_38cm = '38',
                                   CPUE_Abborre_39cm = '39',
                                   CPUE_Abborre_40cm = '40',
                                   CPUE_Abborre_41cm = '41',
                                   CPUE_Abborre_42cm = '42',
                                   CPUE_Abborre_43cm = '43',
                                   CPUE_Abborre_44cm = '44',
                                   CPUE_Abborre_45cm = '45',
                                   CPUE_Abborre_46cm = '46',
                                   CPUE_Abborre_47cm = '47',
                                   CPUE_Abborre_48cm = '48',
                                   CPUE_Abborre_49cm = '49')

### calculate tot CPUE (pooled across size categories). Bring along number indiv and number of nets and avg temp
gillnets_totCPUE<-gillnets_CPUE %>% 
  group_by(location,sub.location, year, Art) %>%
  summarise(totCPUE=sum(CPUE ,na.rm=TRUE),
            tot_number=sum(number_indiv ,na.rm=TRUE),
            number_nets=mean(number_nets ,na.rm=TRUE),
            avg_year_temp=mean(avg_year_temp ,na.rm=TRUE)
  ) 

head(gillnets_totCPUE)

table(gillnets_totCPUE$Art) # only 22 Okänd, no worth bothering

# SUMMARY of key datasets
# gillnets_CPUE: replicated at level of location, year, size categories (and spp) - useful for plotting
# gillnets_CPUE_abbo_wide2: replicated at level of location, with size categories in columns - only abbo
# gillnets_totCPUE: replicated at level of location, year (and spp)
# gillnets_length_indexes: replicated at level of location, year but only for Abborre - useful for stat. 

# merge/combine as you like!
# combine gillnets_length_indexes with totCPUE, and put totCPUE of different spp in columns:

# spread rows into columns to obtain CPUE of spp as predictors and retain zeros
gillnets_totCPUE_wide<-pivot_wider(gillnets_totCPUE, names_from = Art, values_from = c(totCPUE, tot_number))
gillnets_totCPUE_wide[is.na(gillnets_totCPUE_wide)] <- 0 # wait, replace with 0 only for spp, not temp!
gillnets_totCPUE_wide$avg_year_temp[gillnets_totCPUE_wide$avg_year_temp==0] <- NA

table(gillnets$Art)
summary(gillnets_totCPUE_wide)

# check how many location*year had zero abborre: ok, 4 cases. With the new dataset is 1
gillnets_totCPUE_wide %>%
  filter(totCPUE_Abborre == 0)

# select spp to use as predictors: Stensimpa and småpigg are not included as they are all below 12 cm
# run this if it can't find function" select"(caused by name clash in package MASS):
#find("select")
#select <- dplyr::select

gillnets_totCPUE_wide_select<-gillnets_totCPUE_wide %>%
  select(c(location,sub.location,year,avg_year_temp, number_nets,tot_number_Abborre, totCPUE_Abborre,
           totCPUE_Björkna,totCPUE_Braxen,totCPUE_Mört,totCPUE_Id, totCPUE_Löja, # cyprinids
           totCPUE_Ruda,totCPUE_Sarv,totCPUE_Stäm, totCPUE_Sutare, totCPUE_Vimma,  # cyprinids (preys when small)
           'totCPUE_Sill/Strömming', totCPUE_Skarpsill,# clupeids
           totCPUE_Storspigg, # stsp
           totCPUE_Gös,totCPUE_Gädda, # competitors
           'totCPUE_Svart smörbult', 'totCPUE_Svartmunnad smörbult' # preys when small
           )) 

colnames(gillnets_totCPUE_wide)
sort(unique(gillnets_totCPUE$Art))


# merge gillnets_length_indexes table with CPUE of spp:
gillnets_pool0<-left_join(gillnets_length_indexes, gillnets_totCPUE_wide_select, by = c("location","sub.location","year")) # 
# merge with CPUE of Abbo from different size classes:
gillnets_CPUE_abbo_wide2<-gillnets_CPUE_abbo_wide2 %>%
  select(-c(Art, avg_year_temp))
gillnets_pool<-left_join(gillnets_pool0, gillnets_CPUE_abbo_wide2, by = c("location","sub.location","year")) 

# add variables such as clupeids, gobids, cyprinids, competitors, preys..
gillnets_pool$clupeids<-gillnets_pool$totCPUE_Skarpsill + gillnets_pool$'totCPUE_Sill/Strömming'
gillnets_pool$cyprinids<-gillnets_pool$totCPUE_Björkna + gillnets_pool$totCPUE_Braxen + 
  gillnets_pool$totCPUE_Mört + gillnets_pool$totCPUE_Id + gillnets_pool$totCPUE_Löja + 
  gillnets_pool$totCPUE_Ruda + gillnets_pool$totCPUE_Sarv + gillnets_pool$totCPUE_Stäm + 
  gillnets_pool$totCPUE_Sutare + gillnets_pool$totCPUE_Vimma
gillnets_pool$competitors<-gillnets_pool$totCPUE_Gös + gillnets_pool$totCPUE_Gädda
gillnets_pool$gobies<-gillnets_pool$'totCPUE_Svart smörbult' + gillnets_pool$'totCPUE_Svartmunnad smörbult'
gillnets_pool$all_prey<-gillnets_pool$clupeids+gillnets_pool$cyprinids+gillnets_pool$gobies
# and pool abbo size classes using a threshold for a diet shift (Jacobson et al. 2019: above 25cm, more than 50% of the diet is made by fish)
gillnets_pool$CPUE_Abborre_less25 <-gillnets_pool$CPUE_Abborre_13cm+gillnets_pool$CPUE_Abborre_14cm+
  gillnets_pool$CPUE_Abborre_15cm+gillnets_pool$CPUE_Abborre_16cm+gillnets_pool$CPUE_Abborre_17cm+gillnets_pool$CPUE_Abborre_18cm+
  gillnets_pool$CPUE_Abborre_19cm+gillnets_pool$CPUE_Abborre_20cm+gillnets_pool$CPUE_Abborre_21cm+gillnets_pool$CPUE_Abborre_22cm+
  gillnets_pool$CPUE_Abborre_23cm+gillnets_pool$CPUE_Abborre_24cm
gillnets_pool$CPUE_Abborre_25andabove <-gillnets_pool$CPUE_Abborre_25cm+gillnets_pool$CPUE_Abborre_26cm+
  gillnets_pool$CPUE_Abborre_27cm+gillnets_pool$CPUE_Abborre_28cm+
  gillnets_pool$CPUE_Abborre_29cm+gillnets_pool$CPUE_Abborre_30cm+gillnets_pool$CPUE_Abborre_21cm+gillnets_pool$CPUE_Abborre_31cm+
  gillnets_pool$CPUE_Abborre_32cm+gillnets_pool$CPUE_Abborre_33cm+gillnets_pool$CPUE_Abborre_34cm+gillnets_pool$CPUE_Abborre_35cm+
  gillnets_pool$CPUE_Abborre_36cm+gillnets_pool$CPUE_Abborre_37cm+gillnets_pool$CPUE_Abborre_38cm+gillnets_pool$CPUE_Abborre_39cm+
  gillnets_pool$CPUE_Abborre_40cm+gillnets_pool$CPUE_Abborre_41cm+gillnets_pool$CPUE_Abborre_42cm+gillnets_pool$CPUE_Abborre_43cm+
  gillnets_pool$CPUE_Abborre_44cm+gillnets_pool$CPUE_Abborre_45cm+gillnets_pool$CPUE_Abborre_46cm+gillnets_pool$CPUE_Abborre_47cm+
  gillnets_pool$CPUE_Abborre_48cm+gillnets_pool$CPUE_Abborre_49cm

##### calculate lags: ####
# sort, if needed, by consecutive years per location
head(gillnets_pool) # no need but I'll do it anyway:
gillnets_pool_lag<-gillnets_pool %>% 
  arrange(location,sub.location, year) 
head(gillnets_pool_lag)

# from:https://stackoverflow.com/questions/26291988/how-to-create-a-lag-variable-within-each-group
# introduce lags WITHIN groups using dplyr:

gillnets_pool_lag <- 
  gillnets_pool_lag %>%
  group_by(location, sub.location) %>%
  mutate(avg_year_temp_1YearBefore = dplyr::lag(avg_year_temp, n = 1, default = NA)) %>%
  mutate(avg_year_temp_2YearBefore = dplyr::lag(avg_year_temp, n = 2, default = NA)) %>%
  
  mutate(totCPUE_Abborre_1YearBefore = dplyr::lag(totCPUE_Abborre, n = 1, default = NA)) %>%
  mutate(totCPUE_Mört_1YearBefore = dplyr::lag(totCPUE_Mört, n = 1, default = NA)) %>%
  mutate(totCPUE_Löja_1YearBefore = dplyr::lag(totCPUE_Löja, n = 1, default = NA)) %>%
  mutate(totCPUE_Storspigg_1YearBefore = dplyr::lag(totCPUE_Storspigg, n = 1, default = NA)) %>%
  mutate(clupeids_1YearBefore = dplyr::lag(clupeids, n = 1, default = NA)) %>%
  mutate(cyprinids_1YearBefore = dplyr::lag(cyprinids, n = 1, default = NA)) %>%
  mutate(competitors_1YearBefore = dplyr::lag(competitors, n = 1, default = NA)) %>%
  mutate(gobies_1YearBefore = dplyr::lag(gobies, n = 1, default = NA)) %>%
  mutate(all_prey_1YearBefore = dplyr::lag(all_prey, n = 1, default = NA)) %>%
  
  mutate(totCPUE_Abborre_2YearBefore = dplyr::lag(totCPUE_Abborre, n = 2, default = NA)) %>%
  mutate(totCPUE_Mört_2YearBefore = dplyr::lag(totCPUE_Mört, n = 2, default = NA)) %>%
  mutate(totCPUE_Löja_2YearBefore = dplyr::lag(totCPUE_Löja, n = 2, default = NA)) %>%
  mutate(totCPUE_Storspigg_2YearBefore = dplyr::lag(totCPUE_Storspigg, n = 2, default = NA)) %>%
  mutate(clupeids_2YearBefore = dplyr::lag(clupeids, n = 2, default = NA)) %>%
  mutate(cyprinids_2YearBefore = dplyr::lag(cyprinids, n = 2, default = NA)) %>%
  mutate(competitors_2YearBefore = dplyr::lag(competitors, n = 2, default = NA)) %>%
  mutate(gobies_2YearBefore = dplyr::lag(gobies, n = 2, default = NA)) %>%
  mutate(all_prey_2YearBefore = dplyr::lag(all_prey, n = 2, default = NA)) %>%
  
  mutate(totCPUE_Abborre_3YearBefore = dplyr::lag(totCPUE_Abborre, n = 3, default = NA)) %>%
  mutate(totCPUE_Mört_3YearBefore = dplyr::lag(totCPUE_Mört, n = 3, default = NA)) %>%
  mutate(totCPUE_Löja_3YearBefore = dplyr::lag(totCPUE_Löja, n = 3, default = NA)) %>%
  mutate(totCPUE_Storspigg_3YearBefore = dplyr::lag(totCPUE_Storspigg, n = 3, default = NA)) %>%
  mutate(clupeids_3YearBefore = dplyr::lag(clupeids, n = 3, default = NA)) %>%
  mutate(cyprinids_3YearBefore = dplyr::lag(cyprinids, n = 3, default = NA)) %>%
  mutate(competitors_3YearBefore = dplyr::lag(competitors, n = 3, default = NA)) %>%
  mutate(gobies_3YearBefore = dplyr::lag(gobies, n = 3, default = NA)) %>%
  mutate(all_prey_3YearBefore = dplyr::lag(all_prey, n = 3, default = NA)) %>%
  
  mutate(totCPUE_Abborre_4YearBefore = dplyr::lag(totCPUE_Abborre, n = 4, default = NA)) %>%
  mutate(totCPUE_Mört_4YearBefore = dplyr::lag(totCPUE_Mört, n = 4, default = NA)) %>%
  mutate(totCPUE_Löja_4YearBefore = dplyr::lag(totCPUE_Löja, n = 4, default = NA)) %>%
  mutate(totCPUE_Storspigg_4YearBefore = dplyr::lag(totCPUE_Storspigg, n = 4, default = NA)) %>%
  mutate(clupeids_4YearBefore = dplyr::lag(clupeids, n = 4, default = NA)) %>%
  mutate(cyprinids_4YearBefore = dplyr::lag(cyprinids, n = 4, default = NA)) %>%
  mutate(competitors_4YearBefore = dplyr::lag(competitors, n = 4, default = NA)) %>%
  mutate(gobies_4YearBefore = dplyr::lag(gobies, n = 4, default = NA)) %>%
  mutate(all_prey_4YearBefore = dplyr::lag(all_prey, n = 4, default = NA)) %>%
  
  mutate(totCPUE_Abborre_5YearBefore = dplyr::lag(totCPUE_Abborre, n = 5, default = NA)) %>%
  mutate(totCPUE_Mört_5YearBefore = dplyr::lag(totCPUE_Mört, n = 5, default = NA)) %>%
  mutate(totCPUE_Löja_5YearBefore = dplyr::lag(totCPUE_Löja, n = 5, default = NA)) %>%
  mutate(totCPUE_Storspigg_5YearBefore = dplyr::lag(totCPUE_Storspigg, n = 5, default = NA)) %>%
  mutate(clupeids_5YearBefore = dplyr::lag(clupeids, n = 5, default = NA)) %>%
  mutate(cyprinids_5YearBefore = dplyr::lag(cyprinids, n = 5, default = NA)) %>%
  mutate(competitors_5YearBefore = dplyr::lag(competitors, n = 5, default = NA)) %>%
  mutate(gobies_5YearBefore = dplyr::lag(gobies, n = 5, default = NA)) %>%
  mutate(all_prey_5YearBefore = dplyr::lag(all_prey, n = 5, default = NA)) %>%
  
  mutate(CPUE_Abborre_less25_1YearBefore = dplyr::lag(CPUE_Abborre_less25, n = 1, default = NA)) %>%
  mutate(CPUE_Abborre_less25_2YearBefore = dplyr::lag(CPUE_Abborre_less25, n = 2, default = NA)) %>%
  mutate(CPUE_Abborre_less25_3YearBefore = dplyr::lag(CPUE_Abborre_less25, n = 3, default = NA)) %>%
  mutate(CPUE_Abborre_less25_4YearBefore = dplyr::lag(CPUE_Abborre_less25, n = 4, default = NA)) %>%
  mutate(CPUE_Abborre_less25_5YearBefore = dplyr::lag(CPUE_Abborre_less25, n = 5, default = NA)) %>%
  
  mutate(CPUE_Abborre_25andabove_1YearBefore = dplyr::lag(CPUE_Abborre_25andabove, n = 1, default = NA)) %>%
  mutate(CPUE_Abborre_25andabove_2YearBefore = dplyr::lag(CPUE_Abborre_25andabove, n = 2, default = NA)) %>%
  mutate(CPUE_Abborre_25andabove_3YearBefore = dplyr::lag(CPUE_Abborre_25andabove, n = 3, default = NA)) %>%
  mutate(CPUE_Abborre_25andabove_4YearBefore = dplyr::lag(CPUE_Abborre_25andabove, n = 4, default = NA)) %>%
  mutate(CPUE_Abborre_25andabove_5YearBefore = dplyr::lag(CPUE_Abborre_25andabove, n = 5, default = NA)) 
  
  
  
# add integrated estimates of CPUE of spp over multiple years:
## avg
# abbo
gillnets_pool_lag$totCPUE_Abborre_avg_since_1YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore)/2
gillnets_pool_lag$totCPUE_Abborre_avg_since_2YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore)/3
gillnets_pool_lag$totCPUE_Abborre_avg_since_3YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore)/4
gillnets_pool_lag$totCPUE_Abborre_avg_since_4YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_4YearBefore)/5
gillnets_pool_lag$totCPUE_Abborre_avg_since_5YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_4YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_5YearBefore)/6

# abbo less than 25 cm:
gillnets_pool_lag$CPUE_Abborre_less25_avg_since_1YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore)/2
gillnets_pool_lag$CPUE_Abborre_less25_avg_since_2YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore)/3
gillnets_pool_lag$CPUE_Abborre_less25_avg_since_3YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore)/4
gillnets_pool_lag$CPUE_Abborre_less25_avg_since_4YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_4YearBefore)/5
gillnets_pool_lag$CPUE_Abborre_less25_avg_since_5YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_4YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_less25_5YearBefore)/6

# abbo 25 cm and above:
gillnets_pool_lag$CPUE_Abborre_25andabove_avg_since_1YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore)/2
gillnets_pool_lag$CPUE_Abborre_25andabove_avg_since_2YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore)/3
gillnets_pool_lag$CPUE_Abborre_25andabove_avg_since_3YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore)/4
gillnets_pool_lag$CPUE_Abborre_25andabove_avg_since_4YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_4YearBefore)/5
gillnets_pool_lag$CPUE_Abborre_25andabove_avg_since_5YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_4YearBefore+
                                                            gillnets_pool_lag$CPUE_Abborre_25andabove_5YearBefore)/6

# mört
gillnets_pool_lag$totCPUE_Mört_avg_since_1YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                            gillnets_pool_lag$totCPUE_Mört_1YearBefore)/2
gillnets_pool_lag$totCPUE_Mört_avg_since_2YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                            gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_2YearBefore)/3
gillnets_pool_lag$totCPUE_Mört_avg_since_3YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                            gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_3YearBefore)/4
gillnets_pool_lag$totCPUE_Mört_avg_since_4YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                            gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_4YearBefore)/5
gillnets_pool_lag$totCPUE_Mört_avg_since_5YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                            gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_4YearBefore+
                                                            gillnets_pool_lag$totCPUE_Mört_5YearBefore)/6
#clupeids
gillnets_pool_lag$clupeids_avg_since_1YearBefore<-(gillnets_pool_lag$clupeids+
                                                         gillnets_pool_lag$clupeids_1YearBefore)/2
gillnets_pool_lag$clupeids_avg_since_2YearBefore<-(gillnets_pool_lag$clupeids+
                                                         gillnets_pool_lag$clupeids_1YearBefore+
                                                         gillnets_pool_lag$clupeids_2YearBefore)/3
gillnets_pool_lag$clupeids_avg_since_3YearBefore<-(gillnets_pool_lag$clupeids+
                                                         gillnets_pool_lag$clupeids_1YearBefore+
                                                         gillnets_pool_lag$clupeids_2YearBefore+
                                                         gillnets_pool_lag$clupeids_3YearBefore)/4
gillnets_pool_lag$clupeids_avg_since_4YearBefore<-(gillnets_pool_lag$clupeids+
                                                         gillnets_pool_lag$clupeids_1YearBefore+
                                                         gillnets_pool_lag$clupeids_2YearBefore+
                                                         gillnets_pool_lag$clupeids_3YearBefore+
                                                         gillnets_pool_lag$clupeids_4YearBefore)/5
gillnets_pool_lag$clupeids_avg_since_5YearBefore<-(gillnets_pool_lag$clupeids+
                                                         gillnets_pool_lag$clupeids_1YearBefore+
                                                         gillnets_pool_lag$clupeids_2YearBefore+
                                                         gillnets_pool_lag$clupeids_3YearBefore+
                                                         gillnets_pool_lag$clupeids_4YearBefore+
                                                         gillnets_pool_lag$clupeids_5YearBefore)/6
# cyprinids
gillnets_pool_lag$cyprinids_avg_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore)/2
gillnets_pool_lag$cyprinids_avg_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore)/3
gillnets_pool_lag$cyprinids_avg_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore)/4
gillnets_pool_lag$cyprinids_avg_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore)/5
gillnets_pool_lag$cyprinids_avg_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore+
                                                            gillnets_pool_lag$cyprinids_5YearBefore)/6
# competitors
gillnets_pool_lag$cyprinids_avg_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore)/2
gillnets_pool_lag$cyprinids_avg_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore)/3
gillnets_pool_lag$cyprinids_avg_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore)/4
gillnets_pool_lag$cyprinids_avg_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore)/5
gillnets_pool_lag$cyprinids_avg_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore+
                                                            gillnets_pool_lag$cyprinids_5YearBefore)/6
# gobies
gillnets_pool_lag$cyprinids_avg_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore)/2
gillnets_pool_lag$cyprinids_avg_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore)/3
gillnets_pool_lag$cyprinids_avg_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore)/4
gillnets_pool_lag$cyprinids_avg_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore)/5
gillnets_pool_lag$cyprinids_avg_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore+
                                                            gillnets_pool_lag$cyprinids_5YearBefore)/6
# all prey
gillnets_pool_lag$cyprinids_avg_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore)/2
gillnets_pool_lag$cyprinids_avg_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore)/3
gillnets_pool_lag$cyprinids_avg_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore)/4
gillnets_pool_lag$cyprinids_avg_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore)/5
gillnets_pool_lag$cyprinids_avg_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                            gillnets_pool_lag$cyprinids_1YearBefore+
                                                            gillnets_pool_lag$cyprinids_2YearBefore+
                                                            gillnets_pool_lag$cyprinids_3YearBefore+
                                                            gillnets_pool_lag$cyprinids_4YearBefore+
                                                            gillnets_pool_lag$cyprinids_5YearBefore)/6
## sum
# abbo
gillnets_pool_lag$totCPUE_Abborre_sum_since_1YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore)
gillnets_pool_lag$totCPUE_Abborre_sum_since_2YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore)
gillnets_pool_lag$totCPUE_Abborre_sum_since_3YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore)
gillnets_pool_lag$totCPUE_Abborre_sum_since_4YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_4YearBefore)
gillnets_pool_lag$totCPUE_Abborre_sum_since_5YearBefore<-(gillnets_pool_lag$totCPUE_Abborre+
                                                            gillnets_pool_lag$totCPUE_Abborre_1YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_2YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_3YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_4YearBefore+
                                                            gillnets_pool_lag$totCPUE_Abborre_5YearBefore)
# abbo less than 25 cm:
gillnets_pool_lag$CPUE_Abborre_less25_sum_since_1YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore)
gillnets_pool_lag$CPUE_Abborre_less25_sum_since_2YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore)
gillnets_pool_lag$CPUE_Abborre_less25_sum_since_3YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore)
gillnets_pool_lag$CPUE_Abborre_less25_sum_since_4YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_4YearBefore)
gillnets_pool_lag$CPUE_Abborre_less25_sum_since_5YearBefore<-(gillnets_pool_lag$CPUE_Abborre_less25+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_1YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_2YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_3YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_4YearBefore+
                                                                gillnets_pool_lag$CPUE_Abborre_less25_5YearBefore)

# abbo 25 cm and above:
gillnets_pool_lag$CPUE_Abborre_25andabove_sum_since_1YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore)
gillnets_pool_lag$CPUE_Abborre_25andabove_sum_since_2YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore)
gillnets_pool_lag$CPUE_Abborre_25andabove_sum_since_3YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore)
gillnets_pool_lag$CPUE_Abborre_25andabove_sum_since_4YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_4YearBefore)
gillnets_pool_lag$CPUE_Abborre_25andabove_sum_since_5YearBefore<-(gillnets_pool_lag$CPUE_Abborre_25andabove+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_1YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_2YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_3YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_4YearBefore+
                                                                    gillnets_pool_lag$CPUE_Abborre_25andabove_5YearBefore)

# mört
gillnets_pool_lag$totCPUE_Mört_sum_since_1YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                         gillnets_pool_lag$totCPUE_Mört_1YearBefore)
gillnets_pool_lag$totCPUE_Mört_sum_since_2YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                         gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_2YearBefore)
gillnets_pool_lag$totCPUE_Mört_sum_since_3YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                         gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_3YearBefore)
gillnets_pool_lag$totCPUE_Mört_sum_since_4YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                         gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_3YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_4YearBefore)
gillnets_pool_lag$totCPUE_Mört_sum_since_5YearBefore<-(gillnets_pool_lag$totCPUE_Mört+
                                                         gillnets_pool_lag$totCPUE_Mört_1YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_2YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_3YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_4YearBefore+
                                                         gillnets_pool_lag$totCPUE_Mört_5YearBefore)
#clupeids
gillnets_pool_lag$clupeids_sum_since_1YearBefore<-(gillnets_pool_lag$clupeids+
                                                     gillnets_pool_lag$clupeids_1YearBefore)
gillnets_pool_lag$clupeids_sum_since_2YearBefore<-(gillnets_pool_lag$clupeids+
                                                     gillnets_pool_lag$clupeids_1YearBefore+
                                                     gillnets_pool_lag$clupeids_2YearBefore)
gillnets_pool_lag$clupeids_sum_since_3YearBefore<-(gillnets_pool_lag$clupeids+
                                                     gillnets_pool_lag$clupeids_1YearBefore+
                                                     gillnets_pool_lag$clupeids_2YearBefore+
                                                     gillnets_pool_lag$clupeids_3YearBefore)
gillnets_pool_lag$clupeids_sum_since_4YearBefore<-(gillnets_pool_lag$clupeids+
                                                     gillnets_pool_lag$clupeids_1YearBefore+
                                                     gillnets_pool_lag$clupeids_2YearBefore+
                                                     gillnets_pool_lag$clupeids_3YearBefore+
                                                     gillnets_pool_lag$clupeids_4YearBefore)
gillnets_pool_lag$clupeids_sum_since_5YearBefore<-(gillnets_pool_lag$clupeids+
                                                     gillnets_pool_lag$clupeids_1YearBefore+
                                                     gillnets_pool_lag$clupeids_2YearBefore+
                                                     gillnets_pool_lag$clupeids_3YearBefore+
                                                     gillnets_pool_lag$clupeids_4YearBefore+
                                                     gillnets_pool_lag$clupeids_5YearBefore)
# cyprinids
gillnets_pool_lag$cyprinids_sum_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore)
gillnets_pool_lag$cyprinids_sum_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore)
gillnets_pool_lag$cyprinids_sum_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore)
gillnets_pool_lag$cyprinids_sum_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore)
gillnets_pool_lag$cyprinids_sum_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore+
                                                      gillnets_pool_lag$cyprinids_5YearBefore)
# competitors
gillnets_pool_lag$cyprinids_sum_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore)
gillnets_pool_lag$cyprinids_sum_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore)
gillnets_pool_lag$cyprinids_sum_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore)
gillnets_pool_lag$cyprinids_sum_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore)
gillnets_pool_lag$cyprinids_sum_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore+
                                                      gillnets_pool_lag$cyprinids_5YearBefore)
# gobies
gillnets_pool_lag$cyprinids_sum_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore)
gillnets_pool_lag$cyprinids_sum_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore)
gillnets_pool_lag$cyprinids_sum_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore)
gillnets_pool_lag$cyprinids_sum_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore)
gillnets_pool_lag$cyprinids_sum_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore+
                                                      gillnets_pool_lag$cyprinids_5YearBefore)
# all prey
gillnets_pool_lag$cyprinids_sum_since_1YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore)
gillnets_pool_lag$cyprinids_sum_since_2YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore)
gillnets_pool_lag$cyprinids_sum_since_3YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore)
gillnets_pool_lag$cyprinids_sum_since_4YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore)
gillnets_pool_lag$cyprinids_sum_since_5YearBefore<-(gillnets_pool_lag$cyprinids+
                                                      gillnets_pool_lag$cyprinids_1YearBefore+
                                                      gillnets_pool_lag$cyprinids_2YearBefore+
                                                      gillnets_pool_lag$cyprinids_3YearBefore+
                                                      gillnets_pool_lag$cyprinids_4YearBefore+
                                                      gillnets_pool_lag$cyprinids_5YearBefore)


# fix column names for variables that contain more variables: not done yet! maybe using'' works
#head(gillnets_pool)
#colnames(gillnets_pool)
#gillnets_pool$sk1[1]
#skew1<-gillnets_pool$sk1$Skewness
#gillnets_pool<-cbind(gillnets_pool,skew1)
#gillnets_pool$skew1<-gillnets_pool$sk1[1]
# Agnes suggest this: attributes(df_mod$distance_sc)$`scaled:scale`

# add n = n years of sampling for each sub.location
gillnets_pool_lag_time<-gillnets_pool_lag %>%
  add_count(sub.location)
colnames(gillnets_pool_lag_time)[colnames(gillnets_pool_lag_time)=="n"]<-"n_sampled_years_per_site"

table(gillnets_pool_lag_time$year)
# 2021 has 30 locations

table(gillnets_pool_lag_time$year,gillnets_pool_lag_time$sub.location)

# make one (or two) subset for time series analyses and one with only spatial replication
gillnets_pool_lag_time # all replicates: 424
gillnets_pool_lag_time10<-filter(gillnets_pool_lag_time, n_sampled_years_per_site>9) # only time series with at least 10 years sampling: 285
gillnets_pool_lag_time2<-filter(gillnets_pool_lag_time, n_sampled_years_per_site>2) # all replicates except location with less than 3 sampling years: 369
gillnets_pool_lag_time2021<-filter(gillnets_pool_lag_time, year==2021) # only the year with most sampled locations, 2021: 30

# PS: consider spatial corr based on lat and long, but for tha I need to bring/average them from the original dataset

# responses: mean_length, median_length, L90, sk1$Skewness, sk2$Skewness, ku1$Kurtosis, ku2$Skewness
# drivers: avg_year_temp, totCPUE_Abborre, totCPUE_Mört, totCPUE_Gädda, totCPUE_Storspigg, totCPUE_Rötsimpa, totCPUE_Bergsimpa
# size classes of abborre, lagged variables. Year. N years sampled per location. random: location, year
# see more details in the length age analyses scripts

# ready for analyses! 
# OBS: check how many fish are used to calculate the indexes and compare with Örjan guidelines


# SUMMARY of key datasets
# gillnets_CPUE: include CPUE separated for size categories. Replicated at level of location, year, size categories (and spp) - useful for plotting
# gillnets_CPUE_abbo: only for perch
# gillnets_pool_lag: include length indexes for Abborre and tot CPUE of Abborre and  other spp, and CPUE of 
# abborrre of different size classes, and lagged variables. Replicated at level of location, year - useful for stat. 

#####
# exploratory plots
#####
# overall barplot for all locations and years pooled but different spp
ggplot(gillnets_CPUE, aes(x=length_group, y=CPUE)) +
  geom_bar(stat="identity")+
  facet_wrap(~Art)+
  theme_bw(base_size=15)

# Abborre dataset
gillnets_CPUE_abbo<-gillnets_CPUE %>%
  filter(Art == "Abborre")

ggplot(gillnets_CPUE_abbo, aes(x=length_group, y=CPUE, col=location)) +
  geom_point()+
  facet_wrap(~location)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

# barplots of Abbo for all years but different locations: NO! DON'T USE BAR IF i HAVE MULTIPLE VALUES PER GROUP (HERE YEAR) AS IT WILL SUM THEM UP
ggplot(gillnets_CPUE_abbo, aes(x=length_group, y=CPUE, col=location)) +
  geom_bar(stat="identity")+
  facet_wrap(~location)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

# if I want to see the means:
avg<-tapply(gillnets_CPUE_abbo$CPUE,list(gillnets_CPUE_abbo$length_group,gillnets_CPUE_abbo$location),mean)
sdpl<-tapply(gillnets_CPUE_abbo$CPUE,list(gillnets_CPUE_abbo$length_group,gillnets_CPUE_abbo$location),sd)
l<-tapply(gillnets_CPUE_abbo$CPUE,list(gillnets_CPUE_abbo$length_group,gillnets_CPUE_abbo$location),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=F,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,ylim=c(0,7),main = "CPUE Abborre") 

# less crowded fig:
# Calculate means and standard deviations
summary_gillnets_CPUE_abbo <- gillnets_CPUE_abbo %>%
  group_by(location, length_group) %>%
  summarise(
    mean_value = mean(CPUE),
    sd_value = sd(CPUE)
  )

# Create the bar chart
ggplot(summary_gillnets_CPUE_abbo, aes(x = length_group, y = mean_value, col = location)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), 
                width = 0.2, position = position_dodge(0.7)) +
  facet_wrap(~location)+
    labs(title = "Bar Chart with Means and Standard Deviations",
       x = "length_group",
       y = "CPUE Abborre") +
  theme(legend.position="none")

# barplots of Abbo for different years in one specific location
ggplot(subset(gillnets_CPUE_abbo, location %in% "Asköfjärden"), aes(x=length_group, y=CPUE)) +
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

ggplot(subset(gillnets_CPUE_abbo, location %in% "Askrikefjärden"), aes(x=length_group, y=CPUE)) +
  geom_point()+
  facet_wrap(~year)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

# plots for 2023:
ggplot(subset(gillnets_CPUE_abbo, year %in% "2023"), aes(x=length_group, y=CPUE)) +
  geom_bar(stat="identity")+
  facet_wrap(~location)+
  theme_bw(base_size=15)+
  theme(legend.position="none")

#####
# stats on length indexes - REVISE USING ALSO SUBLOCATION, STSP ETC ETC - SEE NOTES
#####

# check spatial and temporal replication:
summary(gillnets_pool)
table(gillnets_pool$location,gillnets_pool$year)
table(gillnets_pool$year,gillnets_pool$sub.location)
count(gillnets_pool, 'location') 


# exploratory plots:
ggplot(gillnets_pool, aes(x=avg_year_temp, y=ku1$Kurtosis, col=year)) +
  geom_point(size=2)+ 
  facet_wrap(~location)+
  theme_bw(base_size=15)

ggplot(gillnets_pool, aes(x=avg_year_temp, y=mean_length, col=location)) +
  geom_point(size=2)+ 
  facet_wrap(~year)+
  theme_bw(base_size=15)

ggplot(gillnets_pool, aes(x=totCPUE_Mört, y=mean_length, col=year)) +
  geom_point(size=2)+ 
  facet_wrap(~location)+
  theme_bw(base_size=15)

# distributional properties
hist(gillnets_pool_lag_time$mean_length)
hist(gillnets_pool_lag_time$avg_year_temp)
hist(gillnets_pool_lag_time$totCPUE_Abborre)
hist(gillnets_pool_lag_time$'sk1$Skewness')


# collinearity
plot(gillnets_pool$avg_year_temp,gillnets_pool$totCPUE_Abborre)
plot(gillnets_pool$avg_year_temp,gillnets_pool$totCPUE_Mört)
plot(gillnets_pool$totCPUE_Abborre,gillnets_pool$totCPUE_Mört)

# time series analyses: mean length

# inlcude correlation STR using all replicate (gillnets_pool)
M0<-gls(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        method="REML",na.action=na.omit, data=gillnets_pool)
vif(M0)
M1<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,method="REML",na.action=na.omit, data=gillnets_pool)
M2<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corCompSymm(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M3<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corExp(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M4<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M5<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M6<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corGaus(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M7<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corSpher(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
AIC(M0,M1,M2,M3,M4,M5,M6,M7)
# best M3 and M4

# check variance str:
M4<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M8<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
M9<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ totCPUE_Abborre),
        method="REML",na.action=na.omit, data=gillnets_pool)
M10<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ totCPUE_Mört),
        method="REML",na.action=na.omit, data=gillnets_pool)
M11<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
M12<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ totCPUE_Abborre),
        method="REML",na.action=na.omit, data=gillnets_pool)
M13<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ totCPUE_Mört),
         method="REML",na.action=na.omit, data=gillnets_pool)
M14<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ avg_year_temp),
         method="REML",na.action=na.omit, data=gillnets_pool)
M15<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ totCPUE_Abborre),
         method="REML",na.action=na.omit, data=gillnets_pool)
M16<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ totCPUE_Mört),
         method="REML",na.action=na.omit, data=gillnets_pool)
M17<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varIdent(form =~ 1|location),
         method="REML",na.action=na.omit, data=gillnets_pool)
AIC(M4,M8,M9)
# best M8

# final
M8<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)

library(ggeffects)
pred <- ggpredict(M8, "avg_year_temp")
plot(pred)
pred2 <- ggpredict(M8, "totCPUE_Abborre")
plot(pred2)

library(effects)
eff.p1 <- effect("avg_year_temp", M8)
plot(eff.p1)

# adding year
M8a<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört+year,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
vif(M8a)
anova.lme(M8a, type = "marginal", adjustSigma = F) 
rsquared(M8a)
summary(M8a)

# adding interaction temp*density
M8b<-lme(mean_length~avg_year_temp*totCPUE_Abborre+totCPUE_Mört+year,
         random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
         method="REML",na.action=na.omit, data=gillnets_pool)
anova.lme(M8b, type = "marginal", adjustSigma = F) 
rsquared(M8b)
summary(M8b)

pred <- ggpredict(M8b, c("avg_year_temp", "totCPUE_Abborre"))
plot(pred)

# using only long time series: gillnets_pool_time1:
M8<-lme(mean_length~avg_year_temp*totCPUE_Abborre+totCPUE_Mört+ year,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool_time1)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)

# using all replicates except sites with less than 2 sampling:
M8<-lme(mean_length~avg_year_temp*totCPUE_Abborre+totCPUE_Mört+year,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool_time2)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)

#### Skewness ####
# time series analyses: I need to fix the name, see above when defyning the gillnet_pool dataset

#hist(gillnets_pool$sk1$Skewness)

# rename: dont work
head(gillnets_pool)
#gillnets_pool <- rename(gillnets_pool, sk1 = 'sk1') # first name is the new one, second is the old one
#colnames(gillnets_pool)
#gillnets_pool<-as.data.frame(gillnets_pool)
#colnames(gillnets_pool)[6:9] <- c("skew1", "skew2","kur1", "kur2")
# check attributes
#attributes(gillnets_pool$skew1)$`scaled:scale`

#attributes(gillnets_pool$skew1)
#un<-unclass(gillnets_pool$skew1)
#x <- cbind(a = 1:3, pi = pi) # simple matrix with dimnames
#attributes(x)

#gillnets_pool$sk1<-gillnets_pool$skew1[1]

## strip an object's attributes:
#attributes(x) <- NULL

# try this, suggested by Agnes (not sure/check how):
attributes(df_mod$distance_sc)$`scaled:scale`


#### time series analyses: L90 ####

hist(gillnets_pool$L90)

# inlcude correlation STR using all replicates (gillnets_pool)
M0<-gls(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        method="REML",na.action=na.omit, data=gillnets_pool)
vif(M0)
M1<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,method="REML",na.action=na.omit, data=gillnets_pool)
M2<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corCompSymm(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M3<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corExp(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M4<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M5<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M6<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corGaus(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M7<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corSpher(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
AIC(M0,M1,M2,M3,M4,M5,M6,M7)
# best M3 and M4

# check variance str:
M4<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool)
M8<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
M9<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ totCPUE_Abborre),
        method="REML",na.action=na.omit, data=gillnets_pool)
AIC(M4,M8,M9)
# best M8

# final
M8<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)

library(ggeffects)
pred2 <- ggpredict(M8, "totCPUE_Abborre")
plot(pred2)

# adding year
M8a<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört+year,
         random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
         method="REML",na.action=na.omit, data=gillnets_pool)
vif(M8a)
anova.lme(M8a, type = "marginal", adjustSigma = F) 
rsquared(M8a)
summary(M8a)

# adding interaction temp*density
M8b<-lme(L90~avg_year_temp*totCPUE_Abborre+totCPUE_Mört+year,
         random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
         method="REML",na.action=na.omit, data=gillnets_pool)
anova.lme(M8b, type = "marginal", adjustSigma = F) 
rsquared(M8b)
summary(M8b)

#pred <- ggpredict(M8b, c("avg_year_temp", "totCPUE_Abborre"))
#plot(pred)

# using only long time series: gillnets_pool_time1:
M8<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool_time1)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)

# using all replicates except sites with less than 2 sampling:
M8<-lme(L90~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool_time2)
anova.lme(M8, type = "marginal", adjustSigma = F) 
rsquared(M8)
summary(M8)
plot(M8)



#####
# climwin on mean length
#####
library(climwin)
# following steps from the help vignette, basic analyses. the add advanced options

head(gillnets_pool)
head(temp_gillnet_day)

# check if date is a date: 
class(gillnets_pool$date)
class(temp_gillnet_day$date)

# if not:
gillnets_pool$date <- as.Date(gillnets_pool$date)
temp_gillnet_day$date <- as.Date(temp_gillnet_day$date)

# check if it is the right format for date. Ita hs to be dd/mm/yyyy format.
gillnets_pool$date[1]
temp_gillnet_day$date[1]

# Format date
gillnets_pool$date_formatted<- format(gillnets_pool$date, format="%d/%m/%Y")
gillnets_pool$date_formatted[1]
class(gillnets_pool$date_formatted) # but now it is a charachter, does it matter? it works!
temp_gillnet_day$date_formatted<- format(temp_gillnet_day$date, format="%d/%m/%Y")
temp_gillnet_day$date_formatted[1]

# check locations in common:
unique(gillnets_pool$location)
unique(temp_gillnet_day$location)
# subset climate dataset so that only locations in the gillnet pool dtaset are included:
temp_gillnet_day2<-temp_gillnet_day %>%
  filter(location %in% unique(gillnets_pool$location))

# maybe I need the same temporal overlap? It shouldn't matter, but check
summary(gillnets_pool$date)
summary(temp_gillnet_day$date)

# subset temp datset to retain dates between 1996 and 2023
temp_gillnet_day3<-temp_gillnet_day2 %>%
  filter(year>1996)

# this is the best model: but corAR doesn't run
baseline = lme(mean_length~totCPUE_Abborre,
               random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
               method="REML",na.action=na.omit, data=gillnets_pool)

# only intercept model
MassWin <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                      cdate = temp_gillnet_day3$date_formatted,
                      bdate = gillnets_pool$date_formatted,
                      baseline = lme(mean_length~1,
                                     random=~1|location,
                                     method="REML",na.action=na.omit, data=gillnets_pool),
                      cinterval = "day",
                      range = c(150, 0), # these will be from March to end of July
                      type = "absolute", refday = c(31, 07),
                      stat = "mean",
                      func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))
# with perch density as covariate
##### use as demo ####
MassWin0 <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                      cdate = temp_gillnet_day3$date_formatted,
                      bdate = gillnets_pool$date_formatted,
                      baseline = lme(mean_length~totCPUE_Abborre,
                                     random=~1|location,
                                     method="REML",na.action=na.omit, data=gillnets_pool),
                      cinterval = "day",
                      range = c(150, 0), # these will be from March to end of July
                      type = "absolute", refday = c(31, 07),
                      stat = "mean",
                      func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))

# it seems like the corAR str gives problems. let's go on without for now

head(MassWin0[[1]]$Dataset)
# the best climate window is 37 - 10 days before 31 July, equivalent to temperature between June 24 and July 21.

MassWin0[[1]]$BestModel

head(MassWin0[[1]]$BestModelData)

MassOutput0 <- MassWin0[[1]]$Dataset
plotdelta(dataset = MassOutput0)

# accounting for overfitting: randomization of data (TO RUN)
MassRand0 <- randwin(repeats = 5,
                     xvar = list(Temp = temp_gillnet_day3$temp),
                       cdate = temp_gillnet_day3$date_formatted,
                       bdate = gillnets_pool$date_formatted,
                       baseline = lme(mean_length~totCPUE_Abborre,
                                      random=~1|location,
                                      method="REML",na.action=na.omit, data=gillnets_pool),
                       cinterval = "day",
                       range = c(150, 0), # these will be from March to end of July
                       type = "absolute", refday = c(31, 07),
                       stat = "mean",
                       func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))
# check if deltaAIC are are very different from those in the output above
MassRand0[[1]]

# estimate how likely our observed result would be at random using pvalue.
length(unique(gillnets_pool$year))
pvalue(dataset = MassWin0[[1]]$Dataset, datasetrand = MassRand0[[1]], metric = "C", sample.size = 22)
# compare histograms of deltaAICc
MassOutput0 <- MassWin0[[1]]$Dataset
MassRand0 <- MassRand0[[1]]
plothist(dataset = MassOutpu0t, datasetrand = MassRand0)

# using model weights to obtain a confidence window: 
# consider all models that make up the top 95% of model weights (the 95% confidence set).
# we can be 95% confident that the true ‘best’ model falls within the shaded region.
plotweights(dataset = MassOutput0)

# visualize the spread of model coefficients across all fitted climate windows. 
plotbetas(dataset = MassOutput0)

# boxplots of the start and end point of all climate windows that make up the 95% confidence set
plotwin(dataset = MassOutput0)

# generate BestModel and BestModelData
MassSingle0 <- singlewin(xvar = list(Temp = temp_gillnet_day3$temp),
                       cdate = temp_gillnet_day3$date_formatted,
                       bdate = gillnets_pool$date_formatted,
                       baseline = lme(mean_length~totCPUE_Abborre,
                                      random=~1|location,
                                      method="REML",na.action=na.omit, data=gillnets_pool),
                       cinterval = "day",
                       range = c(37, 10), # these will be from March to end of July
                       type = "absolute", refday = c(31, 07),
                       stat = "mean",
                       func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))

# plot the predictions of our best model over the biological dat to see how well climate within this period actually 
# explains variation. Not working:  plotbest is currently not available with nlme of coxph models
plotbest(dataset = MassOutput0,
         bestmodel = MassSingle0$BestModel, 
         bestmodeldata = MassSingle0$BestModelData)

# all plots (not working for this case)
plotall(dataset = MassOutput0,
        datasetrand = MassRand0,
        bestmodel = MassSingle0$BestModel, 
        bestmodeldata = MassSingle0$BestModelData)

##### introducing interaction:####
# create a variable 'climate' as a placeholder to specify where climate data will be included in hte model
gillnets_pool$climate <- 1
# not running with the random factor and the interaction. maybe bc some sites have only 1 year sampled? consider a
# subset with time series, no less than 3 year per location:
gillnets_pool_time2
# or at least 10:
gillnets_pool_time1
# consider in hte temp dataset only locations found in gillnets_pool_time1
temp_gillnet_time1 <- temp_gillnet_day3 %>%
  filter(location %in% unique(gillnets_pool_time1$location))

#nope. Use site as fixed, in interaction
MassWin0bis <- slidingwin(xvar = list(Temp = temp_gillnet_time1$temp),
                       cdate = temp_gillnet_time1$date_formatted,
                       bdate = gillnets_pool_time1$date_formatted,
                       baseline = lm(mean_length~totCPUE_Abborre*climate*location,
                                      data=gillnets_pool_time1),
                       cinterval = "day",
                       range = c(150, 0), # these will be from March to end of July
                       type = "absolute", refday = c(31, 07),
                       stat = "mean",
                       func = "lin"))

head(MassWin0bis[[1]]$Dataset)

MassWin0bis[[1]]$BestModel

head(MassWin0bis[[1]]$BestModelData)

MassOutput0bis <- MassWin0bis[[1]]$Dataset
plotdelta(dataset = MassOutput0bis)

plotweights(dataset = MassOutput0bis)

plotbetas(dataset = MassOutput0bis)

plotwin(dataset = MassOutput0bis)

# try to include random factor using lme4:
MassRand0tris <- slidingwin(xvar = list(Temp = temp_gillnet_time1$temp),
                     cdate = temp_gillnet_time1$date_formatted,
                     bdate = gillnets_pool_time1$date_formatted,
                     baseline = lmer(mean_length~totCPUE_Abborre*climate
                                    + (1|location), REML =F,
                                    na.action=na.omit, data=gillnets_pool_time1),
                     cinterval = "day",
                     range = c(150, 0), # these will be from March to end of July
                     type = "absolute", refday = c(31, 07),
                     stat = "mean",
                     func = "lin", spatial = list(gillnets_pool_time1$location, y = temp_gillnet_time1$location))
head(MassRand0tris[[1]]$Dataset)

MassRand0tris[[1]]$BestModel

head(MassRand0tris[[1]]$BestModelData)

MassOutput0tris <- MassRand0tris[[1]]$Dataset
plotdelta(dataset = MassOutput0tris)

plotweights(dataset = MassOutput0tris)

plotbetas(dataset = MassOutput0tris)

plotwin(dataset = MassOutput0tris)

##### testing sum of days with temp > 10 ####

# still using only the long time series
MassRand0a <- slidingwin(xvar = list(Temp = temp_gillnet_time1$temp),
                            cdate = temp_gillnet_time1$date_formatted,
                            bdate = gillnets_pool_time1$date_formatted,
                            baseline = lmer(mean_length~totCPUE_Abborre*climate
                                            + (1|location), REML =F,
                                            na.action=na.omit, data=gillnets_pool_time1),
                            cinterval = "day",
                            range = c(150, 0), # these will be from March to end of July
                            upper = 10, binary = TRUE, # sum of days with temp > 10
                            type = "absolute", refday = c(31, 07),
                            stat = "sum",
                            func = "lin", spatial = list(gillnets_pool_time1$location, y = temp_gillnet_time1$location))
head(MassRand0a[[1]]$Dataset)

MassRand0a[[1]]$BestModel

head(MassRand0a[[1]]$BestModelData)

MassOutput0a <- MassRand0a[[1]]$Dataset
plotdelta(dataset = MassOutput0a)

plotweights(dataset = MassOutput0a)

plotbetas(dataset = MassOutput0a)

plotwin(dataset = MassOutput0a)

##### testing sum of degrees above 10
# still using only the long time series
MassRand0b <- slidingwin(xvar = list(Temp = temp_gillnet_time1$temp),
                         cdate = temp_gillnet_time1$date_formatted,
                         bdate = gillnets_pool_time1$date_formatted,
                         baseline = lmer(mean_length~totCPUE_Abborre*climate
                                         + (1|location), REML =F,
                                         na.action=na.omit, data=gillnets_pool_time1),
                         cinterval = "day",
                         range = c(150, 0), # these will be from March to end of July
                         upper = 10, binary = F, # sum of degrees > 10
                         type = "absolute", refday = c(31, 07),
                         stat = "sum",
                         func = "lin", spatial = list(gillnets_pool_time1$location, y = temp_gillnet_time1$location))
head(MassRand0b[[1]]$Dataset)

MassRand0b[[1]]$BestModel

head(MassRand0b[[1]]$BestModelData)

MassOutput0b <- MassRand0b[[1]]$Dataset
plotdelta(dataset = MassOutput0b)

plotweights(dataset = MassOutput0b)

plotbetas(dataset = MassOutput0b)

plotwin(dataset = MassOutput0b)

##### looking at max temp windows #####
MassWin1 <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                       cdate = temp_gillnet_day3$date_formatted,
                       bdate = gillnets_pool$date_formatted,
                       baseline = lme(mean_length~totCPUE_Abborre,
                                      random=~1|location,
                                      method="REML",na.action=na.omit, data=gillnets_pool),
                       cinterval = "day",
                       range = c(150, 0), # these will be from March to end of July
                       type = "absolute", refday = c(31, 07),
                       stat = "max",
                       func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))

head(MassWin1[[1]]$Dataset)

MassWin1[[1]]$BestModel

head(MassWin1[[1]]$BestModelData)

MassOutput1 <- MassWin1[[1]]$Dataset
plotdelta(dataset = MassOutput1)

plotweights(dataset = MassOutput1)

plotbetas(dataset = MassOutput1)

plotwin(dataset = MassOutput1)



##### using quadratic relationship ######
Clim_quadr <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                       cdate = temp_gillnet_day3$date_formatted,
                       bdate = gillnets_pool$date_formatted,
                       baseline = lme(mean_length~totCPUE_Abborre,
                                      random=~1|location,
                                      method="REML",na.action=na.omit, data=gillnets_pool),
                       cinterval = "day",
                       range = c(150, 0), # these will be from March to end of July
                       type = "absolute", refday = c(31, 07),
                       stat = "mean",
                       func = "quad", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))

head(Clim_quadr[[1]]$Dataset)

Clim_quadr[[1]]$BestModel

head(Clim_quadr[[1]]$BestModelData)

Clim_quadr_Output <- Clim_quadr[[1]]$Dataset
plotdelta(dataset = Clim_quadr_Output)

plotweights(dataset = Clim_quadr_Output)

plotbetas(dataset = Clim_quadr_Output)

plotwin(dataset = Clim_quadr_Output)

##### time back 1 year, linear####
Clim_lin_1year <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                         cdate = temp_gillnet_day3$date_formatted,
                         bdate = gillnets_pool$date_formatted,
                         baseline = lme(mean_length~totCPUE_Abborre,
                                        random=~1|location,
                                        method="REML",na.action=na.omit, data=gillnets_pool),
                         cinterval = "day",
                         range = c(365, 0), # these will be from July the year before
                         type = "absolute", refday = c(31, 07),
                         stat = "mean",
                         func = "lin", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))


head(Clim_lin_1year[[1]]$Dataset)

Clim_lin_1year[[1]]$BestModel

head(Clim_lin_1year[[1]]$BestModelData)

Clim_lin_1year_Output <- Clim_lin_1year[[1]]$Dataset
plotdelta(dataset = Clim_lin_1year_Output)

plotweights(dataset = Clim_lin_1year_Output)

plotbetas(dataset = Clim_lin_1year_Output)

plotwin(dataset = Clim_lin_1year_Output)

##### time back 1 year, quadratic####
Clim_quad_1year <- slidingwin(xvar = list(Temp = temp_gillnet_day3$temp),
                             cdate = temp_gillnet_day3$date_formatted,
                             bdate = gillnets_pool$date_formatted,
                             baseline = lme(mean_length~totCPUE_Abborre,
                                            random=~1|location,
                                            method="REML",na.action=na.omit, data=gillnets_pool),
                             cinterval = "day",
                             range = c(365, 0), # these will be from July the year before
                             type = "absolute", refday = c(31, 07),
                             stat = "mean",
                             func = "quad", spatial = list(gillnets_pool$location, y = temp_gillnet_day3$location))


head(Clim_quad_1year[[1]]$Dataset)

Clim_quad_1year[[1]]$BestModel

head(Clim_quad_1year[[1]]$BestModelData)

Clim_quad_1year_Output <- Clim_quad_1year[[1]]$Dataset
plotdelta(dataset = Clim_quad_1year_Output)

plotweights(dataset = Clim_quad_1year_Output)

plotbetas(dataset = Clim_quad_1year_Output)

plotwin(dataset = Clim_quad_1year_Output)

# at site level:
sort(unique(gillnets_pool$location))
##### Askrikefjärden ####
gillnets_pool_Askrikefjärden<-gillnets_pool %>%
  filter(location %in% "Askrikefjärden")

temp_gillnet_day3_Askrikefjärden<-temp_gillnet_day3 %>%
  filter(location %in% "Askrikefjärden")
# USE ONLY LINEAR RELATIONSHIP and mean
Clim_1year_Askrikefjärden <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Askrikefjärden$temp),
                             cdate = temp_gillnet_day3_Askrikefjärden$date_formatted,
                             bdate = gillnets_pool_Askrikefjärden$date_formatted,
                             baseline = lme(mean_length~totCPUE_Abborre,
                                            random=~1|location,
                                            method="REML",na.action=na.omit, data=gillnets_pool_Askrikefjärden),
                             cinterval = "day",
                             range = c(365, 0), # these will be from July the year before
                             type = "absolute", refday = c(31, 07),
                             stat = c("mean"),
                             func = c("lin"), 
                             spatial = list(gillnets_pool_Askrikefjärden$location, y = temp_gillnet_day3_Askrikefjärden$location))

head(Clim_1year_Askrikefjärden[[1]]$Dataset)

Clim_1year_Askrikefjärden[[1]]$BestModel

head(Clim_1year_Askrikefjärden[[1]]$BestModelData)

Clim_1year_Askrikefjärden_Output <- Clim_1year_Askrikefjärden[[1]]$Dataset
plotdelta(dataset = Clim_1year_Askrikefjärden_Output)

plotweights(dataset = Clim_1year_Askrikefjärden_Output)

plotbetas(dataset = Clim_1year_Askrikefjärden_Output)

plotwin(dataset = Clim_1year_Askrikefjärden_Output)

##### Asköfjärden####
gillnets_pool_Asköfjärden<-gillnets_pool %>%
  filter(location %in% "Asköfjärden")
temp_gillnet_day3_Asköfjärden<-temp_gillnet_day3 %>%
  filter(location %in% "Asköfjärden")

Clim_1year_Asköfjärden <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Asköfjärden$temp),
                                        cdate = temp_gillnet_day3_Asköfjärden$date_formatted,
                                        bdate = gillnets_pool_Asköfjärden$date_formatted,
                                        baseline = lme(mean_length~totCPUE_Abborre,
                                                       random=~1|location,
                                                       method="REML",na.action=na.omit, data=gillnets_pool_Asköfjärden),
                                        cinterval = "day",
                                        range = c(365, 0), # these will be from July the year before
                                        type = "absolute", refday = c(31, 07),
                                        stat = c("mean"),
                                        func = c("lin"), 
                                        spatial = list(gillnets_pool_Asköfjärden$location, 
                                                       y = temp_gillnet_day3_Asköfjärden$location))

head(Clim_1year_Asköfjärden[[1]]$Dataset)

Clim_1year_Asköfjärden[[1]]$BestModel

head(Clim_1year_Asköfjärden[[1]]$BestModelData)

Clim_1year_Asköfjärden_Output <- Clim_1year_Asköfjärden[[1]]$Dataset
plotdelta(dataset = Clim_1year_Asköfjärden_Output)

plotweights(dataset = Clim_1year_Asköfjärden_Output)

plotbetas(dataset = Clim_1year_Asköfjärden_Output)

plotwin(dataset = Clim_1year_Asköfjärden_Output)

Clim_1year_Asköf_relat_wind <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Asköfjärden$temp),
                                     cdate = temp_gillnet_day3_Asköfjärden$date_formatted,
                                     bdate = gillnets_pool_Asköfjärden$date_formatted,
                                     baseline = lme(mean_length~totCPUE_Abborre,
                                                    random=~1|location,
                                                    method="REML",na.action=na.omit, data=gillnets_pool_Asköfjärden),
                                     cinterval = "day",
                                     range = c(365, 0), # these will be from July the year before
                                     type = "relative",
                                     stat = c("mean"),
                                     func = c("lin"), 
                                     spatial = list(gillnets_pool_Asköfjärden$location, 
                                                    y = temp_gillnet_day3_Asköfjärden$location))

##### Aspöja ####
gillnets_pool_Aspöja<-gillnets_pool %>%
  filter(location %in% "Aspöja")
temp_gillnet_day3_Aspöja<-temp_gillnet_day3 %>%
  filter(location %in% "Aspöja")

Clim_1year_Aspöja <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Aspöja$temp),
                                        cdate = temp_gillnet_day3_Aspöja$date_formatted,
                                        bdate = gillnets_pool_Aspöja$date_formatted,
                                        baseline = lme(mean_length~totCPUE_Abborre,
                                                       random=~1|location,
                                                       method="REML",na.action=na.omit, data=gillnets_pool_Aspöja),
                                        cinterval = "day",
                                        range = c(365, 0), # these will be from July the year before
                                        type = "absolute", refday = c(31, 07),
                                        stat = c("max","mean"),
                                        func = c("lin","quad"), 
                                        spatial = list(gillnets_pool_Aspöja$location, 
                                                       y = temp_gillnet_day3_Aspöja$location))
##### Blekinge län####  
gillnets_pool_Blekinge_län<-gillnets_pool %>%
  filter(location %in% "Blekinge län")
temp_gillnet_day3_Blekinge_län<-temp_gillnet_day3 %>%
  filter(location %in% "Blekinge län")

Clim_1year_Blekinge_län <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Blekinge_län$temp),
                                cdate = temp_gillnet_day3_Blekinge_län$date_formatted,
                                bdate = gillnets_pool_Blekinge_län$date_formatted,
                                baseline = lme(mean_length~totCPUE_Abborre,
                                               random=~1|location,
                                               method="REML",na.action=na.omit, data=gillnets_pool_Blekinge_län),
                                cinterval = "day",
                                range = c(365, 0), # these will be from July the year before
                                type = "absolute", refday = c(31, 07),
                                stat = c("max","mean"),
                                func = c("lin","quad"), 
                                spatial = list(gillnets_pool_Blekinge_län$location, 
                                               y = temp_gillnet_day3_Blekinge_län$location))
##### Bråviken #### 
gillnets_pool_Bråviken<-gillnets_pool %>%
  filter(location %in% "Bråvikens kustvatten")
temp_gillnet_day3_Bråviken<-temp_gillnet_day3 %>%
  filter(location %in% "Bråvikens kustvatten")

Clim_1year_Bråviken <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Bråviken$temp),
                                        cdate = temp_gillnet_day3_Bråviken$date_formatted,
                                        bdate = gillnets_pool_Bråviken$date_formatted,
                                        baseline = lme(mean_length~totCPUE_Abborre,
                                                       random=~1|location,
                                                       method="REML",na.action=na.omit, data=gillnets_pool_Bråviken),
                                        cinterval = "day",
                                        range = c(365, 0), # these will be from July the year before
                                        type = "absolute", refday = c(31, 07),
                                        stat = c("max","mean"),
                                        func = c("lin","quad"), 
                                        spatial = list(gillnets_pool_Bråviken$location, 
                                                       y = temp_gillnet_day3_Bråviken$location))
##### Bulleröfjärden ####
gillnets_pool_Bulleröfjärden<-gillnets_pool %>%
  filter(location %in% "Bulleröfjärden")
temp_gillnet_day3_Bulleröfjärden<-temp_gillnet_day3 %>%
  filter(location %in% "Bulleröfjärden")

clim_1year_Bulleröfjärden <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Bulleröfjärden$temp),
                                        cdate = temp_gillnet_day3_Bulleröfjärden$date_formatted,
                                        bdate = gillnets_pool_Bulleröfjärden$date_formatted,
                                        baseline = lme(mean_length~totCPUE_Abborre,
                                                       random=~1|location,
                                                       method="REML",na.action=na.omit, data=gillnets_pool_Bulleröfjärden),
                                        cinterval = "day",
                                        range = c(365, 0), # these will be from July the year before
                                        type = "absolute", refday = c(31, 07),
                                        stat = c("max","mean"),
                                        func = c("lin","quad"), 
                                        spatial = list(gillnets_pool_Bulleröfjärden$location, 
                                                       y = temp_gillnet_day3_Bulleröfjärden$location))



  
  
##### Råneå ####
gillnets_pool_Råneå<-gillnets_pool %>%
  filter(location %in% "Råneå")
temp_gillnet_day3_Råneå<-temp_gillnet_day3 %>%
  filter(location %in% "Råneå")



##### Torhamn, Karlskrona Ö skärgård####
gillnets_pool_Torhamn<-gillnets_pool %>%
  filter(location %in% "Torhamn, Karlskrona Ö skärgård")
temp_gillnet_day3_Torhamn<-temp_gillnet_day3 %>%
  filter(location %in% "Torhamn, Karlskrona Ö skärgård")
# I should use a temporal corr str but doesn't work
Clim_1year_Torhamn <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Torhamn$temp),
                                     cdate = temp_gillnet_day3_Torhamn$date_formatted,
                                     bdate = gillnets_pool_Torhamn$date_formatted,
                                     baseline = lm(mean_length~totCPUE_Abborre,
                                                    na.action=na.omit, data=gillnets_pool_Torhamn),
                                     cinterval = "day",
                                     range = c(365, 0), # these will be from July the year before
                                     type = "absolute", refday = c(31, 07),
                                     stat = "mean",
                                     func = "lin")
Clim_1year_Torhamn_quadr <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Torhamn$temp),
                                 cdate = temp_gillnet_day3_Torhamn$date_formatted,
                                 bdate = gillnets_pool_Torhamn$date_formatted,
                                 baseline = lm(mean_length~totCPUE_Abborre,
                                               na.action=na.omit, data=gillnets_pool_Torhamn),
                                 cinterval = "day",
                                 range = c(365, 0), # these will be from July the year before
                                 type = "absolute", refday = c(31, 07),
                                 stat = "mean",
                                 func = "quad")


head(Clim_1year_Torhamn_quadr[[1]]$Dataset)

Clim_1year_Torhamn_quadr[[1]]$BestModel

head(Clim_1year_Torhamn_quadr[[1]]$BestModelData)

Clim_1year_Torhamn_quadr_Output <- Clim_1year_Torhamn_quadr[[1]]$Dataset
plotdelta(dataset = Clim_1year_Torhamn_quadr_Output)

plotweights(dataset = Clim_1year_Torhamn_quadr_Output)

plotbetas(dataset = Clim_1year_Torhamn_quadr_Output)

plotwin(dataset = Clim_1year_Torhamn_quadr_Output)


##### Valjeviken #### 
# no, only 1 year of data, that is, one value of the response variable
##### Långvindsfjärden#########
sort(unique(gillnets_pool$location))

gillnets_pool_Langvid<-gillnets_pool %>%
  filter(location %in% "Långvindsfjärden")
temp_gillnet_day3_Langvid<-temp_gillnet_day3 %>%
  filter(location %in% "Långvindsfjärden")
# I should use a temporal corr str but doesn't work
Clim_1year_Langvid <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Langvid$temp),
                                 cdate = temp_gillnet_day3_Langvid$date_formatted,
                                 bdate = gillnets_pool_Langvid$date_formatted,
                                 baseline = lm(mean_length~totCPUE_Abborre, data=gillnets_pool_Langvid),
                                 cinterval = "day",
                                 range = c(365, 0), # these will be from July the year before
                                 type = "absolute", refday = c(31, 07),
                                 stat = "mean",
                                 func = "lin")

Clim_1year_Langvid_quad <- slidingwin(xvar = list(Temp = temp_gillnet_day3_Langvid$temp),
                                 cdate = temp_gillnet_day3_Langvid$date_formatted,
                                 bdate = gillnets_pool_Langvid$date_formatted,
                                 baseline = lm(mean_length~totCPUE_Abborre, data=gillnets_pool_Langvid),
                                 cinterval = "day",
                                 range = c(365, 0), # these will be from July the year before
                                 type = "absolute", refday = c(31, 07),
                                 stat = "mean",
                                 func = "quad")


head(Clim_1year_Langvid_quad[[1]]$Dataset)

Clim_1year_Langvid_quad[[1]]$BestModel

head(Clim_1year_Langvid_quad[[1]]$BestModelData)

Clim_1year_Langvid_quad_Output <- Clim_1year_Langvid_quad[[1]]$Dataset
plotdelta(dataset = Clim_1year_Langvid_quad_Output)

plotweights(dataset = Clim_1year_Langvid_quad_Output)

plotbetas(dataset = Clim_1year_Langvid_quad_Output)

plotwin(dataset = Clim_1year_Langvid_quad_Output)
