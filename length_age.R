rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------

#library(tidyverse)
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
library(ggeffects)

library(ExcelFunctionsR)
#library(plyr)


#####
# Read Datasets
#####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("BIAS_stickleback.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) length age dataset
length_age <- read.csv2("perch-length-age.csv",fileEncoding ="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

head(length_age)

# make column with only month
head(length_age$catch_date)
length_age$month<-as.numeric(LEFT(RIGHT(length_age$catch_date,7),2))
summary(length_age$month)
hist(length_age$month)

# make column with only day of the month
length_age$day_of_month<-as.numeric(LEFT(length_age$catch_date,2))
summary(length_age$day_of_month)
hist(length_age$day_of_month)

### 2) read stsp data:
stsp <- read.csv2("BIAS_stickleback.csv",fileEncoding ="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(stsp)

### 3) read distance from open sea data:
dist_offshore <- read.csv2("distance_from_open_sea.csv",fileEncoding ="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(dist_offshore)

### 4) additional distance from open sea data:
dist_offshore_additional <- read.csv2("missing-distance.csv",fileEncoding ="ISO-8859-1",  header=TRUE, sep=",", dec=".")

# Thoughts:
# does it matter how the fish was taken? E.g. gear, program, disturbance...not really if I need the age for length classes, 
# but I need time and space variables (year, month, location), and sex and aging method. 
# Consider only female! Or separate analysis for male, as male responds different to temp (Lundgren 2015, van Dorst 2024)
# exclude growth during the catch year, since this did not represent a full year of growth, van Dorst 2024)
summary(length_age)
unique(length_age$aging_method) # 5 levels
unique(length_age$comments)
hist(length_age$age)

# Questions:
unique(length_age$sex) 
length_age %>% 
  filter(sex == "Båda kön")

# are different method for aging comparable? boh. ask Martina Blass or Yvette

# what does it mean "båda kön"?? Comments: "Hermafrodit - underutvecklade gonader". Exclude it? consider only F
# in the gillnets dataset there is no gender reported. How to go about it? maybe compare M vs F and see whether differences are significant
# in "sampling method" I find "Stratifierat på honor i 2,5 cm-klasser", "Stratifierat cm-klasser enl. blankett 80"..relevant?
# what do gonad status levels correspond to? relevant for us? I don't think so but better to confirm

# how to use age-lenth info to assign age to the gillnets data?
# gillnets dataset is meant to be used only for covariates and length distribution!

#####
# Subsets and merge
#####

# exclude obs with missing age and missing length:
length_age1<-length_age[!is.na(length_age$age),]
length_age2<-length_age1[!is.na(length_age1$total_length),]

# exclude obs not approved
unique(length_age2$approved) # no NAs
length_age3<-subset(length_age2, approved != "NEJ")

# check/exclude weird stuff based on "comments": 
unique(length_age3$comments)
#####
# "Längden i originalprotokoll 152, ändrad till 252 baserat på ålder och somatisk vikt (samt totalvikt på originalprotokollet)." Ok
#"längd lite osäker" OK
#"OBS! Ändra tot.längd till 241mm\n"  OK
# "Antingen felaktig vikt eller längd, NM 2018-11-12" REMOVE
# "Antingen felaktig vikt eller längd, NM 2018-11-20" REMOVE
# "Sektion 1/längden eller vikten stämmer ej"         REMOVE to be safe                                                                          

length_age3 %>% 
  filter(comments == "Längden i originalprotokoll 152, ändrad till 252 baserat på ålder och somatisk vikt (samt totalvikt på originalprotokollet).")
ggplot(length_age3, aes(x = somatic_weight , y = total_length, color = year)) +
  geom_point()+
  theme_minimal()
ggplot(subset(length_age3, year %in% "2021"), aes(x = somatic_weight , y = total_length)) +
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "längd lite osäker")
ggplot(subset(length_age3, year %in% "2007"), aes(x = age , y = total_length)) +
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "OBS! Ändra tot.längd till 241mm\n")
ggplot(subset(length_age3, year %in% "2022"), aes(x = somatic_weight , y = total_length)) +
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "Antingen felaktig vikt eller längd, NM 2018-11-20")
ggplot(subset(length_age3, year %in% "2018"), aes(x = somatic_weight , y = total_length)) +
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "Sektion 1/längden eller vikten stämmer ej")
ggplot(subset(length_age3, year %in% "2004"), aes(x = age , y = total_length)) +
  geom_point()+
  theme_minimal()
#####
length_age4<-subset(length_age3, !(comments == "Antingen felaktig vikt eller längd, NM 2018-11-12" |
                                     comments == "Antingen felaktig vikt eller längd, NM 2018-11-20" |
                                     comments == "Sektion 1/längden eller vikten stämmer ej"))

# exclude Simpevarp and Biotest Forsmark (but not "Forsmark"), as here water is almost 10 degrees warmer
length_age4 %>%
  filter(location =="Simpevarp") # ca 7600 obs
length_age4 %>%
  filter(location =="Biotestsjön, Forsmark") #ca 2300 obs
length_age5 = subset(length_age4, !(location == "Simpevarp"))
length_age6 = subset(length_age5, !(location == "Biotestsjön, Forsmark"))

# check aging methods: 
table(length_age6$aging_method) # 5 levels
# OBS: Gällock can be ok, unless the perch is old.
# Magnus Kokkin says that Fjäll/Gällock med stödstruktur otolit is ok
# YH suggests to exclude samples were no method is reported:
length_age7 = subset(length_age6, !(aging_method == ""))

##### check frozen samples - skip #####
# some is unclear whether freezing occurred after length measures
length_age3 %>% 
  filter(comments == "fryst") #ca 538 obs. Asköfjärden 2005, Norrbyn 2004 and 2006
ggplot(subset(length_age3, comments %in% "fryst"), aes(x = year , y = total_length, color = age)) +
  facet_wrap(~location)+
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "Har varit fryst") #ca 100 ob. Asköfjärden 2009
ggplot(subset(length_age3, comments %in% "Har varit fryst"), aes(x = year , y = total_length, color = age)) +
  facet_wrap(~location)+
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "Alla har varit frysta.") #ca 200 ob. Asköfjärden 2010
ggplot(subset(length_age3, comments %in% "Alla har varit frysta."), aes(x = year , y = total_length, color = age)) +
  facet_wrap(~location)+
  geom_point()+
  theme_minimal()

length_age3 %>% 
  filter(comments == "Alla har varit frysta. Somatisk vikt ej rimlig, struken, KerSod 20101112.") #1 obs. Asköfjärden 2010

length_age3 %>% 
  filter(comments == "Fryst för isotopprov.") #ca 180 ob. Forsmark 2021
ggplot(subset(length_age3, comments %in% "Fryst för isotopprov."), aes(x = year , y = total_length, color = age)) +
  facet_wrap(~location)+
  geom_point()+
  theme_minimal()
# many other "Fryst för isotopprov.bla bla bla"

# studies point to a length decline of 1-2% after freezing for percids. 
# https://www.tandfonline.com/doi/abs/10.1577/1548-8659(1974)103%3C136%3AEOFAFO%3E2.0.CO%3B2
# https://www.sciencedirect.com/science/article/pii/S0165783609001763
# https://onlinelibrary.wiley.com/doi/full/10.1111/j.1439-0426.2012.01958.x.
# If so, that is small enough to ignore - but this of course depends on how big this is in relation to any size 
# differences/changes/effects we may find.if we suspect that freezing is inconsistently reported it may make more 
# sense to not apply any correction factor. however, Since there seems to be some patterns in which locations froze 
# their fish (or in who reports freezing…) it may also be good to compare these patterns to our results 
# -> maybe good to keep info on frozen fish

# check with Noora about the methodology (were the fish frozen before or after the length measurements) and the 
# accuracy in recording freezing by field staff

#####
# select only august samples? I think so
length_age8<-length_age7 %>% 
  filter(month == 8)

summary(length_age8)

# take only female
length_age9<-length_age8 %>% 
  filter(sex == "Hona")

# take only samples >2000
table(length_age9$year)
length_age10<-length_age9 %>% 
  filter(year > 2000)

# set the format for date: may not be right, I saw a mismatch between "year" and "catch date" after transformation of the layout
# length_age10$catch_date<-as.Date(length_age10$catch_date, "%d/%m/%y")

##### extract datasets for Ingrid and for Agnes ####
# extrcat only date*location (lat and long) and export for Ingrid to extract temperature data from loggers
length_age10_to_Ingrid<-length_age10 %>%
  select(c(year,catch_date ,location,long, lat,sub.location)) 
# remove duplicates
duplicated(length_age10_to_Ingrid)
length_age10_to_Ingrid2 = length_age10_to_Ingrid[!duplicated(length_age10_to_Ingrid),]
head(length_age10_to_Ingrid2)
library(openxlsx)
write.xlsx(length_age10_to_Ingrid2, file="C:/Users/sedi0002/Google Drive/FORCE/Output/length_age10_to_Ingrid2.xlsx",
           sheetName = "", colNames = TRUE, rowNames = TRUE, append = F)
# if I want only the coordinates:
length_age10_coordinates<-length_age10 %>%
  select(c(location,long, lat,sub.location))
# remove duplicates
length_age10_coordinates2 = length_age10_coordinates[!duplicated(length_age10_coordinates),]
write.xlsx(length_age10_coordinates2, file="G:/My Drive/FORCE/Output/length_age10_coordinates2.xlsx",
           sheetName = "", colNames = TRUE, rowNames = TRUE, append = F)

# check overlap with gillnet:
overlap_check<-inner_join(length_age10_to_Ingrid2, gillnets7_to_Ingrid2, by = c("year", "location"), relationship = "many-to-many")

head(length_age10_to_Ingrid2)
head(gillnets7_to_Ingrid2)
# boh...is null...

# exctract dataset for Agnes to get stsp estimates:
write.xlsx(length_age10, file="G:/My Drive/FORCE/Output/length_age10.xlsx",
           sheetName = "", colNames = TRUE, rowNames = TRUE, append = F)


##### check differences in F vs M####
ggplot(length_age5, aes(x = age , y = total_length)) +
  geom_point()+
  facet_wrap(~sex)+
  geom_smooth()+ 
  theme_classic(base_size=13)

# calculate mean length at age for F and M:
avg<-tapply(length_age5$total_length,list(length_age5$sex,length_age5$age),mean)
sdpl<-tapply(length_age5$total_length,list(length_age5$sex,length_age5$age),sd)
l<-tapply(length_age5$total_length,list(length_age5$sex,length_age5$age),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "total_length") 

#####
# merge with stsp, distance from offshore, temp (from satellite) and CPUE of spp (from gillnet data)
# are there any locations in the length dataset that are not in the gillnets dataset? Mönsterås - 1 location will have no CPUE and population data 
#####
unique(length_age10$location) # 28
unique(gillnets_pool$location) # 51
length_age10 %>%
  filter(!(location %in% gillnets_pool$location)) %>%
  select(location) %>%
  unique()
# is there a location called Mönsterås in the original dataset? or maybe under Fångstområde? NO
sort(unique(gillnets1a$Fångstområde ))
sort(unique(gillnets1a$location))
gillnets1a %>%
  filter(location %in% c("Mönsterås"))
# and location*year?
length_age10 %>%
  filter(!(location %in% gillnets_pool$location & year %in% gillnets_pool$year)) %>%
  select(location, year) %>%
  unique()
# out of how many location*year:
length_age10 %>%
  select(location, year) %>%
  unique() %>%
  count()
  
# and sub.location?
unique(length_age10$sub.location) # 37
unique(gillnets_indiv$Fångstområde) #77
length_age10 %>%
  filter(!(sub.location %in% gillnets_indiv$Fångstområde)) %>%
  select(sub.location) %>%
  unique()
# and sub.location*year?
length_age10 %>%
  filter(!(sub.location %in% gillnets_indiv$Fångstområde & year %in% gillnets_indiv$year)) %>%
  select(sub.location, year) %>%
  unique()
#out of how many sub.location*year:
length_age10 %>%
  select(sub.location, year) %>%
  unique()

table(length_age10$gear_code,length_age10$sub.location, length_age10$year)

# however, in some cases, even though I have the location in both datasets, the year is not the same, so I get no
# covariates for the length at age data analysis

length_age10 %>%
  filter(location == "Aspöja")
gillnets_pool %>%
  filter(location == "Aspöja")

table(length_age10$location, length_age10$year)
table(gillnets_pool$location, gillnets_pool$year)

#different gear, in gillnets we have filterd out based on K064. Now fixed

# check coordinates for Kvädöfjärden:
length_age10 %>%
  filter(location == "Kvädöfjärden") %>%
  select(sub.location, lat, long) %>%
  unique()

#####
# merge with stsp data and dist from offshore. 

# convert gear to factor, rename it and rename its levels:
stsp$gear_code <- as.factor(stsp$gear)
# Check the current levels
levels(stsp$gear_code)
# Rename the levels
levels(as.factor(length_age10$gear_code))
levels(stsp$gear_code) <- c("K009","K053", "K059","K064")
# delete  column gear
stsp$gear <- NULL

# calculate lag and cumulative estimates for stsp
# sort, if needed, by consecutive years per location
stsp_lag<-stsp %>% 
  arrange(location,sub.location,gear_code,year) 
head(stsp_lag)

stsp_lag <- stsp_lag %>%
  group_by(location, sub.location,gear_code) %>%
  mutate(BIASmean_1YearBefore = dplyr::lag(BIASmean, n = 1, default = NA)) %>%
  mutate(BIASmean_2YearBefore = dplyr::lag(BIASmean, n = 2, default = NA)) %>%
  mutate(BIASmean_3YearBefore = dplyr::lag(BIASmean, n = 3, default = NA)) %>%
  mutate(BIASmean_4YearBefore = dplyr::lag(BIASmean, n = 4, default = NA)) %>%
  mutate(BIASmean_5YearBefore = dplyr::lag(BIASmean, n = 5, default = NA)) 

stsp_lag$BIASmean_avg_since_1YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore)/2
stsp_lag$BIASmean_avg_since_2YearBefore<-(stsp_lag$BIASmean+
                                                         stsp_lag$BIASmean_1YearBefore+
                                                         stsp_lag$BIASmean_2YearBefore)/3
stsp_lag$BIASmean_avg_since_3YearBefore<-(stsp_lag$BIASmean+
                                                         stsp_lag$BIASmean_1YearBefore+
                                                         stsp_lag$BIASmean_2YearBefore+
                                                         stsp_lag$BIASmean_3YearBefore)/4
stsp_lag$BIASmean_avg_since_4YearBefore<-(stsp_lag$BIASmean+
                                                         stsp_lag$BIASmean_1YearBefore+
                                                         stsp_lag$BIASmean_2YearBefore+
                                                         stsp_lag$BIASmean_3YearBefore+
                                                         stsp_lag$BIASmean_4YearBefore)/5
stsp_lag$BIASmean_avg_since_5YearBefore<-(stsp_lag$BIASmean+
                                                         stsp_lag$BIASmean_1YearBefore+
                                                         stsp_lag$BIASmean_2YearBefore+
                                                         stsp_lag$BIASmean_3YearBefore+
                                                         stsp_lag$BIASmean_4YearBefore+
                                                         stsp_lag$BIASmean_5YearBefore)/6

stsp_lag$BIASmean_sum_since_1YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore)
stsp_lag$BIASmean_sum_since_2YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore+
                                            stsp_lag$BIASmean_2YearBefore)
stsp_lag$BIASmean_sum_since_3YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore+
                                            stsp_lag$BIASmean_2YearBefore+
                                            stsp_lag$BIASmean_3YearBefore)
stsp_lag$BIASmean_sum_since_4YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore+
                                            stsp_lag$BIASmean_2YearBefore+
                                            stsp_lag$BIASmean_3YearBefore+
                                            stsp_lag$BIASmean_4YearBefore)
stsp_lag$BIASmean_sum_since_5YearBefore<-(stsp_lag$BIASmean+
                                            stsp_lag$BIASmean_1YearBefore+
                                            stsp_lag$BIASmean_2YearBefore+
                                            stsp_lag$BIASmean_3YearBefore+
                                            stsp_lag$BIASmean_4YearBefore+
                                            stsp_lag$BIASmean_5YearBefore)

# merge with stsp data:  merge and keep all records in left dataset, and only matching record in right dataset
length_age10a<-left_join(length_age10, stsp_lag, by = c("year","location","sub.location","gear_code")) 

# same for distance from offshore data:
# convert gear to factor, rename it and rename its levels:
dist_offshore$gear_code <- as.factor(dist_offshore$gear)
# Check the current levels
levels(dist_offshore$gear_code)
# Rename the levels
levels(dist_offshore$gear_code) <- c("K009","K053", "K059","K064")
# delete  column gear
dist_offshore$gear <- NULL

# I got an additional data of distance from offshore:
head(dist_offshore_additional)
#rename column distance:
dist_offshore_additional <- dist_offshore_additional %>%
  rename(distance = dist.to.offshore)
# remove lat and long columns:
dist_offshore_additional <- dist_offshore_additional %>%
  select(-lat, -long)

# check gear in sub.locations: ok
table(dist_offshore_additional$sub.location,dist_offshore_additional$gear_code) 
table(length_age10b$sub.location,length_age10b$gear_code) 

# cacluate avg by sublocations (as now distance varies with stations within sub.locations)
dist_offshore_additional_avg <- dist_offshore_additional %>%
  group_by(location, sub.location, gear_code) %>%
  summarise(distance = mean(distance, na.rm = TRUE)) 
# remove Östra Gotlands m kustvatten, which I already have:
dist_offshore_additional_avg2<-subset(dist_offshore_additional_avg, sub.location != "Ö Gotlands m kustvatten")

# stack the files with distance data:
head(dist_offshore)
head(dist_offshore_additional_avg2)
#Reorder columns of dist_offshore_additional_avg to match df1
dist_offshore_additional_avg_aligned <- dist_offshore_additional_avg2[, names(dist_offshore)]
# Stack the datasets
dist_all <- rbind(dist_offshore, dist_offshore_additional_avg_aligned)
# remove duplicate (NAs):
dist_all2<-na.omit(dist_all)

# merge with dist from offshore data:
length_age10b<-left_join(length_age10a, dist_all2, by = c("location","sub.location","gear_code")) 

# merge with satellite temp data:
# calculate lag and cumulative estimates for temp
# sort, if needed, by consecutive years per location
temp_satellite_all_lag<-temp_satellite_all %>% 
  arrange(location,sub.location,gear_code,year) 
head(temp_satellite_all_lag)

temp_satellite_all_lag <- temp_satellite_all_lag %>%
  group_by(location, sub.location,gear_code) %>%
  mutate(avg_temp_year_1YearBefore = dplyr::lag(avg_temp_year, n = 1, default = NA)) %>%
  mutate(avg_temp_year_2YearBefore = dplyr::lag(avg_temp_year, n = 2, default = NA)) %>%
  mutate(avg_temp_year_3YearBefore = dplyr::lag(avg_temp_year, n = 3, default = NA)) %>%
  mutate(avg_temp_year_4YearBefore = dplyr::lag(avg_temp_year, n = 4, default = NA)) %>%
  mutate(avg_temp_year_5YearBefore = dplyr::lag(avg_temp_year, n = 5, default = NA)) %>%
  
  mutate(dd_year_1YearBefore = dplyr::lag(dd_year, n = 1, default = NA)) %>%
  mutate(dd_year_2YearBefore = dplyr::lag(dd_year, n = 2, default = NA)) %>%
  mutate(dd_year_3YearBefore = dplyr::lag(dd_year, n = 3, default = NA)) %>%
  mutate(dd_year_4YearBefore = dplyr::lag(dd_year, n = 4, default = NA)) %>%
  mutate(dd_year_5YearBefore = dplyr::lag(dd_year, n = 5, default = NA)) %>%
  
  mutate(avg_temp_summer_1YearBefore = dplyr::lag(avg_temp_summer, n = 1, default = NA)) %>%
  mutate(avg_temp_summer_2YearBefore = dplyr::lag(avg_temp_summer, n = 2, default = NA)) %>%
  mutate(avg_temp_summer_3YearBefore = dplyr::lag(avg_temp_summer, n = 3, default = NA)) %>%
  mutate(avg_temp_summer_4YearBefore = dplyr::lag(avg_temp_summer, n = 4, default = NA)) %>%
  mutate(avg_temp_summer_5YearBefore = dplyr::lag(avg_temp_summer, n = 5, default = NA)) %>%
  
  mutate(avg_temp_winter_1YearBefore = dplyr::lag(avg_temp_winter, n = 1, default = NA)) %>%
  mutate(avg_temp_winter_2YearBefore = dplyr::lag(avg_temp_winter, n = 2, default = NA)) %>%
  mutate(avg_temp_winter_3YearBefore = dplyr::lag(avg_temp_winter, n = 3, default = NA)) %>%
  mutate(avg_temp_winter_4YearBefore = dplyr::lag(avg_temp_winter, n = 4, default = NA)) %>%
  mutate(avg_temp_winter_5YearBefore = dplyr::lag(avg_temp_winter, n = 5, default = NA)) %>%
  
  mutate(avg_temp_exceeding_10_year_1YearBefore = dplyr::lag(avg_temp_exceeding_10_year, n = 1, default = NA)) %>%
  mutate(avg_temp_exceeding_10_year_2YearBefore = dplyr::lag(avg_temp_exceeding_10_year, n = 2, default = NA)) %>%
  mutate(avg_temp_exceeding_10_year_3YearBefore = dplyr::lag(avg_temp_exceeding_10_year, n = 3, default = NA)) %>%
  mutate(avg_temp_exceeding_10_year_4YearBefore = dplyr::lag(avg_temp_exceeding_10_year, n = 4, default = NA)) %>%
  mutate(avg_temp_exceeding_10_year_5YearBefore = dplyr::lag(avg_temp_exceeding_10_year, n = 5, default = NA)) %>%

  mutate(n_days_exceeding_10_year_1YearBefore = dplyr::lag(n_days_exceeding_10_year, n = 1, default = NA)) %>%
  mutate(n_days_exceeding_10_year_2YearBefore = dplyr::lag(n_days_exceeding_10_year, n = 2, default = NA)) %>%
  mutate(n_days_exceeding_10_year_3YearBefore = dplyr::lag(n_days_exceeding_10_year, n = 3, default = NA)) %>%
  mutate(n_days_exceeding_10_year_4YearBefore = dplyr::lag(n_days_exceeding_10_year, n = 4, default = NA)) %>%
  mutate(n_days_exceeding_10_year_5YearBefore = dplyr::lag(n_days_exceeding_10_year, n = 5, default = NA)) %>%

  mutate(first_day_exceeding_10_julian_1YearBefore = dplyr::lag(first_day_exceeding_10_julian, n = 1, default = NA)) %>%
  mutate(first_day_exceeding_10_julian_2YearBefore = dplyr::lag(first_day_exceeding_10_julian, n = 2, default = NA)) %>%
  mutate(first_day_exceeding_10_julian_3YearBefore = dplyr::lag(first_day_exceeding_10_julian, n = 3, default = NA)) %>%
  mutate(first_day_exceeding_10_julian_4YearBefore = dplyr::lag(first_day_exceeding_10_julian, n = 4, default = NA)) %>%
  mutate(first_day_exceeding_10_julian_5YearBefore = dplyr::lag(first_day_exceeding_10_julian, n = 5, default = NA))

    
temp_satellite_all_lag$temp_year_avg_since_1YearBefore<-(temp_satellite_all_lag$avg_temp_jan_jul+
                                                          temp_satellite_all_lag$avg_temp_year_1YearBefore)/2
temp_satellite_all_lag$temp_year_avg_since_2YearBefore<-(temp_satellite_all_lag$avg_temp_jan_jul+
                                                      temp_satellite_all_lag$avg_temp_year_1YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_2YearBefore)/3
temp_satellite_all_lag$temp_year_avg_since_3YearBefore<-(temp_satellite_all_lag$avg_temp_jan_jul+
                                                      temp_satellite_all_lag$avg_temp_year_1YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_2YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_3YearBefore)/4
temp_satellite_all_lag$temp_year_avg_since_4YearBefore<-(temp_satellite_all_lag$avg_temp_jan_jul+
                                                      temp_satellite_all_lag$avg_temp_year_1YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_2YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_3YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_4YearBefore)/5
temp_satellite_all_lag$temp_year_avg_since_5YearBefore<-(temp_satellite_all_lag$avg_temp_jan_jul+
                                                      temp_satellite_all_lag$avg_temp_year_1YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_2YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_3YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_4YearBefore+
                                                      temp_satellite_all_lag$avg_temp_year_5YearBefore)/6

temp_satellite_all_lag$temp_summer_avg_since_1YearBefore<-(temp_satellite_all_lag$avg_temp_april_jul+
                                                           temp_satellite_all_lag$avg_temp_summer_1YearBefore)/2
temp_satellite_all_lag$temp_summer_avg_since_2YearBefore<-(temp_satellite_all_lag$avg_temp_april_jul+
                                                           temp_satellite_all_lag$avg_temp_summer_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_2YearBefore)/3
temp_satellite_all_lag$temp_summer_avg_since_3YearBefore<-(temp_satellite_all_lag$avg_temp_april_jul+
                                                           temp_satellite_all_lag$avg_temp_summer_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_3YearBefore)/4
temp_satellite_all_lag$temp_summer_avg_since_4YearBefore<-(temp_satellite_all_lag$avg_temp_april_jul+
                                                           temp_satellite_all_lag$avg_temp_summer_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_3YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_4YearBefore)/5
temp_satellite_all_lag$temp_summer_avg_since_5YearBefore<-(temp_satellite_all_lag$avg_temp_april_jul+
                                                           temp_satellite_all_lag$avg_temp_summer_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_3YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_4YearBefore+
                                                           temp_satellite_all_lag$avg_temp_summer_5YearBefore)/6

temp_satellite_all_lag$temp_winter_avg_since_1YearBefore<-(temp_satellite_all_lag$avg_temp_jan_mar+
                                                             temp_satellite_all_lag$avg_temp_winter_1YearBefore)/2
temp_satellite_all_lag$temp_winter_avg_since_2YearBefore<-(temp_satellite_all_lag$avg_temp_jan_mar+
                                                             temp_satellite_all_lag$avg_temp_winter_1YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_2YearBefore)/3
temp_satellite_all_lag$temp_winter_avg_since_3YearBefore<-(temp_satellite_all_lag$avg_temp_jan_mar+
                                                             temp_satellite_all_lag$avg_temp_winter_1YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_2YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_3YearBefore)/4
temp_satellite_all_lag$temp_winter_avg_since_4YearBefore<-(temp_satellite_all_lag$avg_temp_jan_mar+
                                                             temp_satellite_all_lag$avg_temp_winter_1YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_2YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_3YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_4YearBefore)/5
temp_satellite_all_lag$temp_winter_avg_since_5YearBefore<-(temp_satellite_all_lag$avg_temp_jan_mar+
                                                             temp_satellite_all_lag$avg_temp_winter_1YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_2YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_3YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_4YearBefore+
                                                             temp_satellite_all_lag$avg_temp_winter_5YearBefore)/6

temp_satellite_all_lag$temp_exceeding_10_year_avg_since_1YearBefore<-(temp_satellite_all_lag$avg_temp_exceeding_10_jan_jul+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_1YearBefore)/2
temp_satellite_all_lag$temp_exceeding_10_year_avg_since_2YearBefore<-(temp_satellite_all_lag$avg_temp_exceeding_10_jan_jul+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_2YearBefore)/3
temp_satellite_all_lag$temp_exceeding_10_year_avg_since_3YearBefore<-(temp_satellite_all_lag$avg_temp_exceeding_10_jan_jul+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_3YearBefore)/4
temp_satellite_all_lag$temp_exceeding_10_year_avg_since_4YearBefore<-(temp_satellite_all_lag$avg_temp_exceeding_10_jan_jul+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_3YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_4YearBefore)/5
temp_satellite_all_lag$temp_exceeding_10_year_avg_since_5YearBefore<-(temp_satellite_all_lag$avg_temp_exceeding_10_jan_jul+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_1YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_2YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_3YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_4YearBefore+
                                                           temp_satellite_all_lag$avg_temp_exceeding_10_year_5YearBefore)/6

temp_satellite_all_lag$dd_year_sum_since_1YearBefore<-temp_satellite_all_lag$dd_jan_jul+
                                                           temp_satellite_all_lag$dd_year_1YearBefore
temp_satellite_all_lag$dd_year_sum_since_2YearBefore<-temp_satellite_all_lag$dd_jan_jul+
                                                           temp_satellite_all_lag$dd_year_1YearBefore+
                                                           temp_satellite_all_lag$dd_year_2YearBefore
temp_satellite_all_lag$dd_year_sum_since_3YearBefore<-temp_satellite_all_lag$dd_jan_jul+
                                                           temp_satellite_all_lag$dd_year_1YearBefore+
                                                           temp_satellite_all_lag$dd_year_2YearBefore+
                                                           temp_satellite_all_lag$dd_year_3YearBefore
temp_satellite_all_lag$dd_year_sum_since_4YearBefore<-temp_satellite_all_lag$dd_jan_jul+
                                                           temp_satellite_all_lag$dd_year_1YearBefore+
                                                           temp_satellite_all_lag$dd_year_2YearBefore+
                                                           temp_satellite_all_lag$dd_year_3YearBefore+
                                                           temp_satellite_all_lag$dd_year_4YearBefore
temp_satellite_all_lag$dd_year_sum_since_5YearBefore<-temp_satellite_all_lag$dd_jan_jul+
                                                           temp_satellite_all_lag$dd_year_1YearBefore+
                                                           temp_satellite_all_lag$dd_year_2YearBefore+
                                                           temp_satellite_all_lag$dd_year_3YearBefore+
                                                           temp_satellite_all_lag$dd_year_4YearBefore+
                                                           temp_satellite_all_lag$dd_year_5YearBefore

temp_satellite_all_lag$dd_year_avg_since_1YearBefore<-(temp_satellite_all_lag$dd_jan_jul+
                                                         temp_satellite_all_lag$dd_year_1YearBefore)/2
temp_satellite_all_lag$dd_year_avg_since_2YearBefore<-(temp_satellite_all_lag$dd_jan_jul+
                                                         temp_satellite_all_lag$dd_year_1YearBefore+
                                                         temp_satellite_all_lag$dd_year_2YearBefore)/3
temp_satellite_all_lag$dd_year_avg_since_3YearBefore<-(temp_satellite_all_lag$dd_jan_jul+
                                                         temp_satellite_all_lag$dd_year_1YearBefore+
                                                         temp_satellite_all_lag$dd_year_2YearBefore+
                                                         temp_satellite_all_lag$dd_year_3YearBefore)/4
temp_satellite_all_lag$dd_year_avg_since_4YearBefore<-(temp_satellite_all_lag$dd_jan_jul+
                                                         temp_satellite_all_lag$dd_year_1YearBefore+
                                                         temp_satellite_all_lag$dd_year_2YearBefore+
                                                         temp_satellite_all_lag$dd_year_3YearBefore+
                                                         temp_satellite_all_lag$dd_year_4YearBefore)/5
temp_satellite_all_lag$dd_year_avg_since_5YearBefore<-(temp_satellite_all_lag$dd_jan_jul+
                                                         temp_satellite_all_lag$dd_year_1YearBefore+
                                                         temp_satellite_all_lag$dd_year_2YearBefore+
                                                         temp_satellite_all_lag$dd_year_3YearBefore+
                                                         temp_satellite_all_lag$dd_year_4YearBefore+
                                                         temp_satellite_all_lag$dd_year_5YearBefore)/6

temp_satellite_all_lag$n_days_exceeding_10_year_avg_since_1YearBefore<-(temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore)/2
temp_satellite_all_lag$n_days_exceeding_10_year_avg_since_2YearBefore<-(temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore)/3
temp_satellite_all_lag$n_days_exceeding_10_year_avg_since_3YearBefore<-(temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore)/4
temp_satellite_all_lag$n_days_exceeding_10_year_avg_since_4YearBefore<-(temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_4YearBefore)/5
temp_satellite_all_lag$n_days_exceeding_10_year_avg_since_5YearBefore<-(temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_4YearBefore+
                                                                        temp_satellite_all_lag$n_days_exceeding_10_year_5YearBefore)/6

temp_satellite_all_lag$n_days_exceeding_10_year_sum_since_1YearBefore<-temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore
temp_satellite_all_lag$n_days_exceeding_10_year_sum_since_2YearBefore<-temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore
temp_satellite_all_lag$n_days_exceeding_10_year_sum_since_3YearBefore<-temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore
temp_satellite_all_lag$n_days_exceeding_10_year_sum_since_4YearBefore<-temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_4YearBefore
temp_satellite_all_lag$n_days_exceeding_10_year_sum_since_5YearBefore<-temp_satellite_all_lag$n_days_exceeding_10_jan_jul+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_1YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_2YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_3YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_4YearBefore+
                                                                          temp_satellite_all_lag$n_days_exceeding_10_year_5YearBefore

temp_satellite_all_lag$first_day_exceeding_10_julian_avg_since_1YearBefore<-(temp_satellite_all_lag$first_day_exceeding_10_julian+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_1YearBefore)/2
temp_satellite_all_lag$first_day_exceeding_10_julian_avg_since_2YearBefore<-(temp_satellite_all_lag$first_day_exceeding_10_julian+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_1YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_2YearBefore)/3
temp_satellite_all_lag$first_day_exceeding_10_julian_avg_since_3YearBefore<-(temp_satellite_all_lag$first_day_exceeding_10_julian+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_1YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_2YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_3YearBefore)/4
temp_satellite_all_lag$first_day_exceeding_10_julian_avg_since_4YearBefore<-(temp_satellite_all_lag$first_day_exceeding_10_julian+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_1YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_2YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_3YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_4YearBefore)/5
temp_satellite_all_lag$first_day_exceeding_10_julian_avg_since_5YearBefore<-(temp_satellite_all_lag$first_day_exceeding_10_julian+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_1YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_2YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_3YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_4YearBefore+
                                                                          temp_satellite_all_lag$first_day_exceeding_10_julian_5YearBefore)/6


# merge with temp data:  merge and keep all records in left dataset, and only matching record in right dataset
length_age10c<-left_join(length_age10b, temp_satellite_all_lag, by = c("year","location","sub.location","gear_code")) 


# merge with gillnets data:   group by sublocation!
length_age11<-left_join(length_age10c, gillnets_pool_lag, by = c("year","location", "sub.location")) 
head(length_age11)

# remove unnecessary columns:
length_age12<-length_age11 %>%
  select(-c(species,program,survey, gear, gear.code, ID, aging_method,somatic_weight_type,sex, comments,approved,
            sampling_method))

summary(length_age12)

# convert negative values of dist from offshore to zeros:
hist(length_age12$distance)
length_age12$distance[length_age12$distance < 0] <- 0
# check dist from offshore for Gotland: ok
unique(length_age12$sub.location)
length_age12 %>%
  filter(location == "Östra Gotlands m kustvatten") %>%
  select(distance) %>%
  unique()

# assign covariates specifically to each fish based on its size and age:
# create subsets for each age class, work out the covariates, and then stack them:
length_age12_age2<-length_age12 %>% 
  filter(age == 2)
length_age12_age3<-length_age12 %>% 
  filter(age == 3)
length_age12_age4<-length_age12 %>% 
  filter(age == 4)
length_age12_age5<-length_age12 %>% 
  filter(age == 5)

# copy all integrated measurements (avg) of CPUE of spp and conspecifics over the whole life span of that age class:
# age 2
length_age12_age2$BIASmean_avg_lifespan<-length_age12_age2$BIASmean_avg_since_2YearBefore
length_age12_age2$cyprinids_avg_lifespan<-length_age12_age2$cyprinids_avg_since_2YearBefore
length_age12_age2$totCPUE_Mört_avg_lifespan<-length_age12_age2$totCPUE_Mört_avg_since_2YearBefore
length_age12_age2$all_prey_avg_lifespan<-length_age12_age2$all_prey_avg_since_2YearBefore
length_age12_age2$totCPUE_Abborre_avg_lifespan<-length_age12_age2$totCPUE_Abborre_avg_since_2YearBefore

length_age12_age2$temp_year_avg_lifespan<-length_age12_age2$temp_year_avg_since_2YearBefore
length_age12_age2$temp_summer_avg_lifespan<-length_age12_age2$temp_summer_avg_since_2YearBefore
length_age12_age2$temp_winter_avg_lifespan<-length_age12_age2$temp_winter_avg_since_2YearBefore
length_age12_age2$temp_exceeding_10_year_avg_lifespan<-length_age12_age2$temp_exceeding_10_year_avg_since_2YearBefore
length_age12_age2$dd_year_sum_lifespan<-length_age12_age2$dd_year_sum_since_2YearBefore
length_age12_age2$dd_year_avg_lifespan<-length_age12_age2$dd_year_avg_since_2YearBefore
length_age12_age2$n_days_exceeding_10_year_avg_lifespan<-length_age12_age2$n_days_exceeding_10_year_avg_since_2YearBefore
length_age12_age2$n_days_exceeding_10_year_sum_lifespan<-length_age12_age2$n_days_exceeding_10_year_sum_since_2YearBefore
length_age12_age2$first_day_exceeding_10_julian_avg_lifespan<-length_age12_age2$first_day_exceeding_10_julian_avg_since_2YearBefore

# to match each fish with conspecific of the same length, split the dataset:
length_age12_age2_less25<-length_age12_age2 %>%
  filter(total_length < 250)
length_age12_age2_25andabove<-length_age12_age2 %>%
  filter(total_length > 249)
length_age12_age2_less25$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age2_less25$CPUE_Abborre_less25_avg_since_2YearBefore
length_age12_age2_25andabove$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age2_25andabove$CPUE_Abborre_25andabove_avg_since_2YearBefore
# later I will need this too:
length_age12_age2_less25$CPUE_Abbo_samesize<-length_age12_age2_less25$CPUE_Abborre_less25
length_age12_age2_25andabove$CPUE_Abbo_samesize<-length_age12_age2_25andabove$CPUE_Abborre_25andabove

# age3
length_age12_age3$BIASmean_avg_lifespan<-length_age12_age3$BIASmean_avg_since_3YearBefore
length_age12_age3$cyprinids_avg_lifespan<-length_age12_age3$cyprinids_avg_since_3YearBefore
length_age12_age3$totCPUE_Mört_avg_lifespan<-length_age12_age3$totCPUE_Mört_avg_since_3YearBefore
length_age12_age3$all_prey_avg_lifespan<-length_age12_age3$all_prey_avg_since_3YearBefore
length_age12_age3$totCPUE_Abborre_avg_lifespan<-length_age12_age3$totCPUE_Abborre_avg_since_3YearBefore

length_age12_age3$temp_year_avg_lifespan<-length_age12_age3$temp_year_avg_since_3YearBefore
length_age12_age3$temp_summer_avg_lifespan<-length_age12_age3$temp_summer_avg_since_3YearBefore
length_age12_age3$temp_winter_avg_lifespan<-length_age12_age3$temp_winter_avg_since_3YearBefore
length_age12_age3$temp_exceeding_10_year_avg_lifespan<-length_age12_age3$temp_exceeding_10_year_avg_since_3YearBefore
length_age12_age3$dd_year_sum_lifespan<-length_age12_age3$dd_year_sum_since_3YearBefore
length_age12_age3$dd_year_avg_lifespan<-length_age12_age3$dd_year_avg_since_3YearBefore
length_age12_age3$n_days_exceeding_10_year_avg_lifespan<-length_age12_age3$n_days_exceeding_10_year_avg_since_3YearBefore
length_age12_age3$n_days_exceeding_10_year_sum_lifespan<-length_age12_age3$n_days_exceeding_10_year_sum_since_3YearBefore
length_age12_age3$first_day_exceeding_10_julian_avg_lifespan<-length_age12_age3$first_day_exceeding_10_julian_avg_since_3YearBefore

# to match each fish with conspecific of the same length, split the dataset:
length_age12_age3_less25<-length_age12_age3 %>%
  filter(total_length < 250)
length_age12_age3_25andabove<-length_age12_age3 %>%
  filter(total_length > 249)
length_age12_age3_less25$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age3_less25$CPUE_Abborre_less25_avg_since_3YearBefore
length_age12_age3_25andabove$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age3_25andabove$CPUE_Abborre_25andabove_avg_since_3YearBefore
# later I will need this too:
length_age12_age3_less25$CPUE_Abbo_samesize<-length_age12_age3_less25$CPUE_Abborre_less25
length_age12_age3_25andabove$CPUE_Abbo_samesize<-length_age12_age3_25andabove$CPUE_Abborre_25andabove

# age4
length_age12_age4$BIASmean_avg_lifespan<-length_age12_age4$BIASmean_avg_since_4YearBefore
length_age12_age4$cyprinids_avg_lifespan<-length_age12_age4$cyprinids_avg_since_4YearBefore
length_age12_age4$totCPUE_Mört_avg_lifespan<-length_age12_age4$totCPUE_Mört_avg_since_4YearBefore
length_age12_age4$all_prey_avg_lifespan<-length_age12_age4$all_prey_avg_since_4YearBefore
length_age12_age4$totCPUE_Abborre_avg_lifespan<-length_age12_age4$totCPUE_Abborre_avg_since_4YearBefore

length_age12_age4$temp_year_avg_lifespan<-length_age12_age4$temp_year_avg_since_4YearBefore
length_age12_age4$temp_summer_avg_lifespan<-length_age12_age4$temp_summer_avg_since_4YearBefore
length_age12_age4$temp_winter_avg_lifespan<-length_age12_age4$temp_winter_avg_since_4YearBefore
length_age12_age4$temp_exceeding_10_year_avg_lifespan<-length_age12_age4$temp_exceeding_10_year_avg_since_4YearBefore
length_age12_age4$dd_year_sum_lifespan<-length_age12_age4$dd_year_sum_since_4YearBefore
length_age12_age4$dd_year_avg_lifespan<-length_age12_age4$dd_year_avg_since_4YearBefore
length_age12_age4$n_days_exceeding_10_year_avg_lifespan<-length_age12_age4$n_days_exceeding_10_year_avg_since_4YearBefore
length_age12_age4$n_days_exceeding_10_year_sum_lifespan<-length_age12_age4$n_days_exceeding_10_year_sum_since_4YearBefore
length_age12_age4$first_day_exceeding_10_julian_avg_lifespan<-length_age12_age4$first_day_exceeding_10_julian_avg_since_4YearBefore

# to match each fish with conspecific of the same length, split the dataset:
length_age12_age4_less25<-length_age12_age4 %>%
  filter(total_length < 250)
length_age12_age4_25andabove<-length_age12_age4 %>%
  filter(total_length > 249)
length_age12_age4_less25$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age4_less25$CPUE_Abborre_less25_avg_since_4YearBefore
length_age12_age4_25andabove$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age4_25andabove$CPUE_Abborre_25andabove_avg_since_4YearBefore
# later I will need this too:
length_age12_age4_less25$CPUE_Abbo_samesize<-length_age12_age4_less25$CPUE_Abborre_less25
length_age12_age4_25andabove$CPUE_Abbo_samesize<-length_age12_age4_25andabove$CPUE_Abborre_25andabove

# age 5
length_age12_age5$BIASmean_avg_lifespan<-length_age12_age5$BIASmean_avg_since_5YearBefore
length_age12_age5$cyprinids_avg_lifespan<-length_age12_age5$cyprinids_avg_since_5YearBefore
length_age12_age5$totCPUE_Mört_avg_lifespan<-length_age12_age5$totCPUE_Mört_avg_since_5YearBefore
length_age12_age5$all_prey_avg_lifespan<-length_age12_age5$all_prey_avg_since_5YearBefore
length_age12_age5$totCPUE_Abborre_avg_lifespan<-length_age12_age5$totCPUE_Abborre_avg_since_5YearBefore

length_age12_age5$temp_year_avg_lifespan<-length_age12_age5$temp_year_avg_since_5YearBefore
length_age12_age5$temp_summer_avg_lifespan<-length_age12_age5$temp_summer_avg_since_5YearBefore
length_age12_age5$temp_winter_avg_lifespan<-length_age12_age5$temp_winter_avg_since_5YearBefore
length_age12_age5$temp_exceeding_10_year_avg_lifespan<-length_age12_age5$temp_exceeding_10_year_avg_since_5YearBefore
length_age12_age5$dd_year_sum_lifespan<-length_age12_age5$dd_year_sum_since_5YearBefore
length_age12_age5$dd_year_avg_lifespan<-length_age12_age5$dd_year_avg_since_5YearBefore
length_age12_age5$n_days_exceeding_10_year_avg_lifespan<-length_age12_age5$n_days_exceeding_10_year_avg_since_5YearBefore
length_age12_age5$n_days_exceeding_10_year_sum_lifespan<-length_age12_age5$n_days_exceeding_10_year_sum_since_5YearBefore
length_age12_age5$first_day_exceeding_10_julian_avg_lifespan<-length_age12_age5$first_day_exceeding_10_julian_avg_since_5YearBefore

# to match each fish with conspecific of the same length, split the dataset:
length_age12_age5_less25<-length_age12_age5 %>%
  filter(total_length < 250)
length_age12_age5_25andabove<-length_age12_age5 %>%
  filter(total_length > 249)
length_age12_age5_less25$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age5_less25$CPUE_Abborre_less25_avg_since_5YearBefore
length_age12_age5_25andabove$CPUE_Abbo_samesize_avg_lifespan<-length_age12_age5_25andabove$CPUE_Abborre_25andabove_avg_since_5YearBefore
# later I will need this too:
length_age12_age5_less25$CPUE_Abbo_samesize<-length_age12_age5_less25$CPUE_Abborre_less25
length_age12_age5_25andabove$CPUE_Abbo_samesize<-length_age12_age5_25andabove$CPUE_Abborre_25andabove

# stack all subsets:
length_age12_stack<-rbind(length_age12_age2_less25, length_age12_age2_25andabove, 
                          length_age12_age3_less25, length_age12_age3_25andabove,
                          length_age12_age4_less25, length_age12_age4_25andabove,
                          length_age12_age5_less25, length_age12_age5_25andabove)



unique(sort(length_age12$location))
table(length_age12$location, length_age12$year)

#####
# exploration plots
#####

# temp variables vs length split by age
ggplot(length_age12_stack, aes(x = first_day_exceeding_10_julian_avg_lifespan , y = total_length)) +
  geom_point()+
  facet_wrap(~age)+
  geom_smooth(method ="lm")+ 
  theme_classic(base_size=13)

# temp variables vs length split by sublocation
ggplot(length_age12_stack, aes(x = temp_year_avg_lifespan , y = total_length)) +
  geom_point()+
  facet_wrap(~sub.location)+
  geom_smooth(method ="lm")+ 
  theme_classic(base_size=13)

df <- data.frame(length_age12_stack[,c("temp_year_avg_lifespan","temp_summer_avg_lifespan",
                                       "temp_winter_avg_lifespan","temp_exceeding_10_year_avg_lifespan",
                                       "dd_year_sum_lifespan","dd_year_avg_lifespan",
                                       "n_days_exceeding_10_year_avg_lifespan", "n_days_exceeding_10_year_sum_lifespan",
                                       "first_day_exceeding_10_julian_avg_lifespan")])
# plot pairwise scatterplots of covariates:
pairs(df)

# plot avg year temp per year and sub.location
ggplot(length_age12_stack, aes(x = year , y = avg_year_temp)) +
  geom_point()+
  facet_wrap(~sub.location)+
  #geom_smooth()+ 
  theme_classic(base_size=13)

# length at age vs ..:
ggplot(subset(length_age12, age %in% "2"), aes(x = BIASmean , y = total_length)) +
  geom_point()+
  #facet_wrap(~year)+
  geom_smooth(method = "lm")+ 
  labs(title="Age 2")+
  theme_classic(base_size=13)

# split by year. Do it for age 2 and 3 and 4
ggplot(subset(length_age7_age3, year %in% c(2002:2020)), aes(x = avg_year_temp , y = total_length)) +
  geom_point()+
  facet_wrap(~year)+
  #geom_smooth()+ 
  labs(title="Age 3")+
  theme_classic(base_size=13)

# length at age vs densities of conspp
ggplot(length_age7_age4, aes(x = totCPUE_Abborre , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  labs(title="Age 4")+
  theme_classic(base_size=13)

# length at age vs densities of spp
ggplot(length_age12_age2, aes(x = all_prey , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  labs(title="Age 2")+
  theme_classic(base_size=13)

# length at age vs  lat: but probably collinear with temp
ggplot(length_age12_age2, aes(x = avg_year_temp , y = lat)) +
  geom_point()

# length at age vs year
ggplot(length_age7_age2to4, aes(x = year , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~age)+
  labs(title="")+
  theme_classic(base_size=13)

# length at age vs locations and year
ggplot(length_age12_age5, aes(x = year , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~location)+
  labs(title="age 5")+
  theme_classic(base_size=13)

# length at age vs locations and temp
ggplot(length_age7_age2, aes(x = avg_year_temp , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~location)+
  labs(title="age 2")+
  theme_classic(base_size=13)

# temp vs locations and year
ggplot(length_age7_age2, aes(x = year , y = avg_year_temp)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~location)+
  labs(title="avg year temp")+
  theme_classic(base_size=13)

table(length_age7_age2$year, length_age7_age2$location)

# length at age vs locations and year, selected time series
length_age7_age2_TS<-subset(length_age7_age2, location %in% c("Asköfjärden","Askrikefjärden","Finbo, Åland",
                                                              "Forsmark","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kumlinge, Åland","Kvädöfjärden",
                                                              "Lagnö","Långvindsfjärden","Norrbyn","Råneå","Simpevarp",
                                                              "Torhamn, Karlskrona Ö skärgård"))
ggplot(subset(length_age7_age2, location %in% c("Asköfjärden","Askrikefjärden","Finbo, Åland",
                                                "Forsmark","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kumlinge, Åland","Kvädöfjärden",
                                                "Lagnö","Långvindsfjärden","Norrbyn","Råneå","Simpevarp",
                                                "Torhamn, Karlskrona Ö skärgård") &
                year %in% c(2002:2022)), aes(x = year , y = total_length)) +
  geom_point()+
  geom_smooth(method = "gam")+ 
  facet_wrap(~location)+
  labs(title="age 2")+
  theme_classic(base_size=13)

# length at age vs locations and temp, selected time series
ggplot(subset(length_age7_age2, location %in% c("Asköfjärden","Askrikefjärden","Finbo, Åland",
                                                "Forsmark","Gaviksfjärden","Holmön","Kinnbäcksfjärden","Kumlinge, Åland","Kvädöfjärden",
                                                "Lagnö","Långvindsfjärden","Norrbyn","Råneå","Simpevarp",
                                                "Torhamn, Karlskrona Ö skärgård") &
                year %in% c(2002:2022)), aes(x = avg_year_temp , y = total_length)) +
  geom_point()+
  geom_smooth(method = "gam")+ 
  facet_wrap(~location)+
  labs(title="age 2")+
  theme_classic(base_size=13)

# how to use age-lenth info to assign age to the gillnets data? We won't!

# check L-A in Askrikefjärden, split by year:
ggplot(subset(length_age7, location %in% "Askrikefjärden"), aes(x = age , y = total_length)) +
  geom_point()+
  #facet_wrap(~year)+
  geom_smooth(method = "gam")+ 
  labs(title="Askrikefjärden ")+
  theme_classic(base_size=13)

length_age7_Askrikefjärden<-length_age7 %>% 
  filter(location == "Askrikefjärden")
avg<-tapply(length_age7_Askrikefjärden$total_length,list(length_age7_Askrikefjärden$year,length_age7_Askrikefjärden$age),mean)
sdpl<-tapply(length_age7_Askrikefjärden$total_length,list(length_age7_Askrikefjärden$year,length_age7_Askrikefjärden$age),sd)
l<-tapply(length_age7_Askrikefjärden$total_length,list(length_age7_Askrikefjärden$year,length_age7_Askrikefjärden$age),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "total_length Askrikefjärden") 

# check another site, with lower replication:
unique(length_age7_age2to4$location)
table(length_age7_age2to4$location)

length_age7_age2to4_Vinö<-length_age7_age2to4 %>% 
  filter(location == "Vinö")
avg<-tapply(length_age7_age2to4_Vinö$total_length,list(length_age7_age2to4_Vinö$age,length_age7_age2to4_Vinö$year),mean)
sdpl<-tapply(length_age7_age2to4_Vinö$total_length,list(length_age7_age2to4_Vinö$age,length_age7_age2to4_Vinö$year),sd)
l<-tapply(length_age7_age2to4_Vinö$total_length,list(length_age7_age2to4_Vinö$age,length_age7_age2to4_Vinö$year),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "total_length in Vinö") 

length_age7_age2to4_Aspöja<-length_age7_age2to4 %>% 
  filter(location == "Aspöja")
avg<-tapply(length_age7_age2to4_Aspöja$total_length,list(length_age7_age2to4_Aspöja$age,length_age7_age2to4_Aspöja$year),mean)
sdpl<-tapply(length_age7_age2to4_Aspöja$total_length,list(length_age7_age2to4_Aspöja$age,length_age7_age2to4_Aspöja$year),sd)
l<-tapply(length_age7_age2to4_Aspöja$total_length,list(length_age7_age2to4_Aspöja$age,length_age7_age2to4_Aspöja$year),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "total_length in Aspöja") 

# check differences between sites, pooling all years

avg<-tapply(length_age10_age2to4$total_length,list(length_age10_age2to4$lat,length_age10_age2to4$age),mean)
sdpl<-tapply(length_age10_age2to4$total_length,list(length_age10_age2to4$lat,length_age10_age2to4$age),sd)
l<-tapply(length_age10_age2to4$total_length,list(length_age10_age2to4$lat,length_age10_age2to4$age),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "total_length") 


#####
# statistical analysis - age 2
#####
summary(length_age12_age2)

# covariates: 
# date of catch (day_of_month) to account for the extra growth since age determination. maybe transform to ordinal number. 
# Temp variables: now I have only avg_year_temp, calculate and import others.
# stsp densities* dist from offshore: BIASmean * distance
# Conspecific densities: as total (totCPUE_Abborre) or split by size classes (pooled somehow): CPUE_Abborre_25andabove,CPUE_Abborre_less25  
# clupeids, gobies, cyprinids, competitors, all_preys, totCPUE_Mört
# year, if not collinear
# n_fish if I considered avg by sublocation. or set a threshold for a minimum number of fish measured per replicate
# random: year within sub.location. also sublocation in location. Gear, even though don't think is useful. 

# consider lags and cumulative estimates for abbo,totCPUE_Mört, clupeids, cyprinids, all_preys, competitors
# lag: totCPUE_Abborre_1YearBefore, totCPUE_Abborre_2YearBefore...MAYBE SKIP
# avg: totCPUE_Abborre_avg_since_1YearBefore, totCPUE_Abborre_avg_since_2YearBefore...
# sum: totCPUE_Abborre_sum_since_1YearBefore, totCPUE_Abborre_sum_since_2YearBefore...
# same for temp: TO DO
# same for stsp: BIASmean_avg_since_1YearBefore, BIASmean_avg_since_2YearBefore..BIASmean_sum_since_1YearBefore, BIASmean_sum_since_2YearBefore
# same for size classes, but pooled: CPUE_Abborre_25andabove_avg_since_1YearBefore...CPUE_Abborre_25andabove_sum_since_1YearBefore...

### example of beyond optimal model (temp variables missing for now):
# linear models:
M1<-lm(total_length~avg_year_temp+day_of_month +
         BIASmean + distance + # BIASmean_avg_since_1YearBefore * distance + BIASmean_avg_since_2YearBefore +
         # BIASmean_sum_since_1YearBefore * distance + BIASmean_sum_since_2YearBefore
         totCPUE_Abborre + # totCPUE_Abborre_avg_since_1YearBefore + totCPUE_Abborre_avg_since_2YearBefore +
         # totCPUE_Abborre_sum_since_1YearBefore + totCPUE_Abborre_sum_since_2YearBefore +
         CPUE_Abborre_25andabove + # CPUE_Abborre_25andabove_avg_since_1YearBefore + CPUE_Abborre_25andabove_avg_since_2YearBefore +
         # CPUE_Abborre_25andabove_sum_since_1YearBefore + CPUE_Abborre_25andabove_sum_since_2YearBefore +
         CPUE_Abborre_less25 + # CPUE_Abborre_less25_avg_since_1YearBefore + CPUE_Abborre_less25_avg_since_2YearBefore +
         # CPUE_Abborre_less25_sum_since_1YearBefore + CPUE_Abborre_less25_sum_since_2YearBefore +
         competitors + # competitors_avg_since_1YearBefore + competitors_avg_since_2YearBefore +
         # competitors_sum_since_1YearBefore + competitors_sum_since_2YearBefore +
         totCPUE_Mört  +# totCPUE_Mört_avg_since_1YearBefore + totCPUE_Mört_avg_since_2YearBefore +
         # totCPUE_Mört_sum_since_1YearBefore + totCPUE_Mört_sum_since_2YearBefore +
         cyprinids + # cyprinids_avg_since_1YearBefore + cyprinids_avg_since_2YearBefore +
         # cyprinids_sum_since_1YearBefore + cyprinids_sum_since_2YearBefore +
         clupeids + # clupeids_avg_since_1YearBefore + clupeids_avg_since_2YearBefore +
         # clupeids_sum_since_1YearBefore + clupeids_sum_since_2YearBefore +
         gobies + # gobies_avg_since_1YearBefore + gobies_avg_since_2YearBefore +
         # gobies_sum_since_1YearBefore + gobies_sum_since_2YearBefore +
         all_prey + # all_prey_avg_since_1YearBefore + all_prey_avg_since_2YearBefore +
         # all_prey_sum_since_1YearBefore + all_prey_sum_since_2YearBefore +
         year,
       # random=~1|sub.location, #weights=varFixed(~ avg_year_temp), method = "ML", 
       na.action = "na.fail", # na.action = na.pass, na.action = na.omit, na.action = "na.exclude",
       data=clean_data)


# OBS: as a second step, consider effects of temp (possibly different variables) on predictors, maybe SEM

# OBS: since I have multiple values per location and year, I can't model a temporal correlation structure
# more complex than a symmetrical one unless I pool the values, i.e. take the mean length per sublocation and 
# year. I can try and compare both approach, i.e. (1) all values retained with a simple corr str, and (2) the 
# means with more complex corr str. try also in gls and in gamm: correlation = corGaus(form =~ sub.location|Year,nugget=TRUE)
# together with a spatial random factor location/sublocation

# OBS: consider gamm, see clupeids R scripts

# try with or without Råneå 2008

# using the MuMIn package:
library(MuMIn)

# the function na.action = "na.fail"  prevents fitting models to different datasets due to NA

# I think I should use here ML since I am doing model selection for the fixed part. But then I get also the coefficients, for
# which I would rather use REML. After researching on the topic I found: "using likelihood-based methods (including AIC) to 
# compare two models with different fixed effects that are fitted by REML will generally lead to nonsense. In Faraway (2006, 
# Extending the linear model with R (p. 156): The reason is that REML estimates the random effects by considering linear 
# combinations of the data that remove the fixed effects. If these fixed effects are changed, the likelihoods of the two models
# will not be directly comparable"

# AICc should be used instead AIC when sample size is small in comparison to the number of estimated parameters (Burnham & 
# Anderson 2002 recommend its use when n/K < 40. (Burnham, K. P. and Anderson, D. R. 2002 Model selection and multimodel 
# inference: a practical information-theoretic approach. 2nd ed. New York, Springer-Verlag)
# By default, AICc is used to rank models and obtain model weights
# for more info: https://cran.r-project.org/web/packages/MuMIn/MuMIn.pdf


##### approach 1 all values retained with a simple corr str ####

# collinearity: correlation matrix:
df <- data.frame(length_age12_age2$avg_year_temp, length_age12_age2$avg_year_temp_1YearBefore, length_age12_age2$avg_year_temp_2YearBefore,
                 length_age12_age2$totCPUE_Abborre, length_age12_age2$competitors,
                 length_age12_age2$totCPUE_Mört,length_age12_age2$totCPUE_Löja +length_age12_age2$totCPUE_Storspigg,
                 length_age12_age2$clupeids,length_age12_age2$cyprinids,length_age12_age2$gobies,
                 length_age12_age2$all_prey)
cor(df) 
# plot pariwise scatterplots of covariates:
pairs(df)

# with lagged variables
df_lag <- data.frame(length_age12_age2$avg_year_temp_1YearBefore, length_age12_age2$avg_year_temp_2YearBefore,
                 length_age12_age2$totCPUE_Abborre_1YearBefore, length_age12_age2$competitors_1YearBefore,
                 length_age12_age2$totCPUE_Mört_1YearBefore,length_age12_age2$totCPUE_Löja_1YearBefore +length_age12_age2$totCPUE_Storspigg_1YearBefore,
                 length_age12_age2$clupeids_1YearBefore,length_age12_age2$cyprinids_1YearBefore,length_age12_age2$gobies_1YearBefore,
                 length_age12_age2$all_prey_1YearBefore)
# plot pariwise scatterplots of covariates:
pairs(df_lag)

# distributional properties
hist(length_age12_age2$total_length)
hist(length_age12_age2$gobies) # consider log transf
hist(length_age12_age2$totCPUE_Löja) # consider log transf
hist(length_age12_age2$competitors) # consider log transf
hist(length_age12_age2$all_prey)


# check collinearity with vif
M0<- lm(total_length ~ avg_year_temp + # avg_year_temp_1YearBefore + avg_year_temp_2YearBefore +  # temp
          totCPUE_Abborre + competitors + #  conspecifics, competitors
          #totCPUE_Mört + totCPUE_Löja +    # food single spp
          #clupeids + cyprinids + gobies +  # food pooled spp
          all_prey +                       # food total
          #totCPUE_Abborre_1YearBefore + competitors_1YearBefore + 
          #totCPUE_Mört_1YearBefore + totCPUE_Löja_1YearBefore + totCPUE_Storspigg +
          #clupeids_1YearBefore + cyprinids_1YearBefore + gobies_1YearBefore + 
          #all_prey_1YearBefore +
        field_temp + # account for different catchability of gillnets with temp (if not collinear, otherwise test on residuals)
          day_of_month, # account for extra growth in august until catch
        data = length_age12_age2)
vif(M0)

# beyond optimal model: M0

# test for temporal corr str: 
M0<-gls(total_length ~ avg_year_temp + # avg_year_temp_1YearBefore + avg_year_temp_2YearBefore +  # temp
          totCPUE_Abborre + competitors + #  conspecifics, competitors
          #totCPUE_Mört + totCPUE_Löja +    # food single spp
          #clupeids + cyprinids + gobies +  # food pooled spp
          all_prey +                       # food total
          #totCPUE_Abborre_1YearBefore + competitors_1YearBefore + 
          #totCPUE_Mört_1YearBefore + totCPUE_Löja_1YearBefore + totCPUE_Storspigg +
          #clupeids_1YearBefore + cyprinids_1YearBefore + gobies_1YearBefore + 
          #all_prey_1YearBefore +
          field_temp + # account for different catchability of gillnets with temp (if not collinear, otherwise test on residuals)
          day_of_month, # account for extra growth in august until catch
        method="REML",na.action=na.omit, data=length_age12_age2)

M1<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M2<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location,correlation=corCompSymm(form=~year),method="REML",na.action=na.omit, data=length_age12_age2)
M3<-gls(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        correlation = corGaus(form =~ location|year,nugget=TRUE),method="REML",na.action=na.omit, data=length_age12_age2)
AIC(M0,M1,M2)
# best is M1

# check variance str:
M1<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M3<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        weights=varFixed(~ avg_year_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M4<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        weights=varFixed(~ totCPUE_Abborre),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
#M5<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
#        weights=varFixed(~ competitors),
#        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M6<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        weights=varFixed(~ all_prey),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M7<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        weights=varFixed(~ field_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M8<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
         weights=varFixed(~ year),
         random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
AIC(M1,M3,M4,M6,M7,M8)

# variance str that are not working currently:
#####
M8b<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ avg_year_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M9<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ totCPUE_Abborre),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M10<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ competitors),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M11<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ all_prey),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M12<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ field_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)

M13<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varConstPower(~ avg_year_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M14<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varConstPower(~ totCPUE_Abborre),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M15<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
         weights=varConstPower(~ competitors),
         random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M16<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
         weights=varConstPower(~ all_prey),
         random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M17<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
         weights=varConstPower(~ field_temp),
         random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)

M18<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
         weights=varIdent(form =~ 1|location),
         random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)

M20<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varPower(~ year),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M21<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varConstPower(~ year),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
#####
# final preliminary
M3<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + day_of_month,
        random=~1|location,weights=varFixed(~ avg_year_temp), 
        method="REML",na.action=na.omit, data=length_age12_age2)

anova.lme(M3, type = "marginal", adjustSigma = F) 
rsquared(M3)
summary(M3)
plot(M3)

# I can't run the usual script for figure, fix it. It is because of the format of catch date. transform into ordinal. Now ok

library(ggeffects)
predict_response(M3, terms = "avg_year_temp")

pred<-ggpredict(M3)
pred <- ggpredict(M3, "totCPUE_Abborre")
pred <- ggemmeans(M3, "totCPUE_Abborre")
pred <- ggeffect(M3, "totCPUE_Abborre")
plot(pred)

#####
# comparing variables with lags, one at a time, without var str:
M1<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M2<-lme(total_length~avg_year_temp_1YearBefore +totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M3<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
anova.lme(M1, type = "marginal", adjustSigma = F) 
anova.lme(M2, type = "marginal", adjustSigma = F) 
anova.lme(M3, type = "marginal", adjustSigma = F) 
rsquared(M1)
rsquared(M2)
rsquared(M3)
# higher marginal R2 with avg year temp 2 year before

M3<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M4<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre_1YearBefore + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
anova.lme(M3, type = "marginal", adjustSigma = F) 
anova.lme(M4, type = "marginal", adjustSigma = F) 
rsquared(M3)
rsquared(M4)
summary(M3)
summary(M4)

# ns (positive) coeff for Abbo same year
# signif negative coeff for Abbo 1 year before
# R2 similar

M3<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M5<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors_1YearBefore + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
anova.lme(M3, type = "marginal", adjustSigma = F) 
anova.lme(M5, type = "marginal", adjustSigma = F) 
rsquared(M3)
rsquared(M5)
summary(M3)
summary(M5)
# competitors same year and 1 year before have similar (positive and signif) effects

M4<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre_1YearBefore + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M6<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre_1YearBefore + competitors_1YearBefore + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
rsquared(M4)
rsquared(M6)

M3<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors + all_prey + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
M7<-lme(total_length~avg_year_temp_2YearBefore +totCPUE_Abborre + competitors + all_prey_1YearBefore + field_temp + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
rsquared(M3)
rsquared(M7)
anova.lme(M3, type = "marginal", adjustSigma = F) 
anova.lme(M7, type = "marginal", adjustSigma = F) 
summary(M3)
summary(M7)

# all prey same year and 1 year before have similar (positive and signif) effects

#####
# testing conspecific densities from size classes:
summary(length_age12_age2)
#length_age12_age2$CPUEabbo_13_14cm<-length_age12_age2$'13'+length_age12_age2$'14'
#length_age12_age2$CPUEabbo_13_15cm<-length_age12_age2$'15'+length_age12_age2$CPUEabbo_13_14cm
#length_age12_age2$CPUEabbo_13_16cm<-length_age12_age2$'16'+length_age12_age2$CPUEabbo_13_15cm

# I remove totCPUE abbo:
M0<-lm(total_length~avg_year_temp_2YearBefore + competitors + all_prey + field_temp + day_of_month
       + CPUEabbo_13_14cm,
        data=length_age12_age2)
vif(M0)
M3a<-lme(total_length~avg_year_temp_2YearBefore +CPUEabbo_13_16cm + competitors + all_prey  + day_of_month,
        random=~1|location, method="REML",na.action=na.omit, data=length_age12_age2)
rsquared(M3a)
anova.lme(M3a, type = "marginal", adjustSigma = F) 
summary(M3a)

# negative effects of conspecifics of similar size (and same year), for all pooled size classes tested

# NB: I can remove field temp, as it affect the catch ability of gillnets, but not the length of fish

##### using MuMi - FAILED #### 
library(MuMIn)

# The dredge function from the MuMIn package requires consistent handling of missing values to ensure that 
# all sub-models are fitted to the same dataset. So I make a pilot model on a clean dataset with no zeros
# Alternatively, I could run random forest

clean_data <- na.omit(length_age12_age2)

# linear models:
M1<-lm(total_length~avg_year_temp+day_of_month +
          BIASmean + distance + # BIASmean_avg_since_1YearBefore * distance + BIASmean_avg_since_2YearBefore +
          # BIASmean_sum_since_1YearBefore * distance + BIASmean_sum_since_2YearBefore
          totCPUE_Abborre + # totCPUE_Abborre_avg_since_1YearBefore + totCPUE_Abborre_avg_since_2YearBefore +
          # totCPUE_Abborre_sum_since_1YearBefore + totCPUE_Abborre_sum_since_2YearBefore +
          CPUE_Abborre_25andabove + # CPUE_Abborre_25andabove_avg_since_1YearBefore + CPUE_Abborre_25andabove_avg_since_2YearBefore +
          # CPUE_Abborre_25andabove_sum_since_1YearBefore + CPUE_Abborre_25andabove_sum_since_2YearBefore +
          CPUE_Abborre_less25 + # CPUE_Abborre_less25_avg_since_1YearBefore + CPUE_Abborre_less25_avg_since_2YearBefore +
          # CPUE_Abborre_less25_sum_since_1YearBefore + CPUE_Abborre_less25_sum_since_2YearBefore +
          competitors + # competitors_avg_since_1YearBefore + competitors_avg_since_2YearBefore +
          # competitors_sum_since_1YearBefore + competitors_sum_since_2YearBefore +
          totCPUE_Mört  +# totCPUE_Mört_avg_since_1YearBefore + totCPUE_Mört_avg_since_2YearBefore +
          # totCPUE_Mört_sum_since_1YearBefore + totCPUE_Mört_sum_since_2YearBefore +
          cyprinids + # cyprinids_avg_since_1YearBefore + cyprinids_avg_since_2YearBefore +
          # cyprinids_sum_since_1YearBefore + cyprinids_sum_since_2YearBefore +
          clupeids + # clupeids_avg_since_1YearBefore + clupeids_avg_since_2YearBefore +
          # clupeids_sum_since_1YearBefore + clupeids_sum_since_2YearBefore +
          gobies + # gobies_avg_since_1YearBefore + gobies_avg_since_2YearBefore +
          # gobies_sum_since_1YearBefore + gobies_sum_since_2YearBefore +
          all_prey + # all_prey_avg_since_1YearBefore + all_prey_avg_since_2YearBefore +
          # all_prey_sum_since_1YearBefore + all_prey_sum_since_2YearBefore +
          year,
        # random=~1|sub.location, #weights=varFixed(~ avg_year_temp), method = "ML", 
        na.action = "na.fail", # na.action = na.pass, na.action = na.omit, na.action = "na.exclude",
        data=clean_data)

# to get all possible models:
dM1<-dredge(M1,rank = "AICc", extra = c("R^2"))
# to keep only models containing HERR_above18_B_km2_4root+herrbelow18_sprat_4root
#ddM1<-dredge(M1,rank = "AICc", extra = c("R^2","vif"), fixed = c("HERR_above18_B_km2_4root","herrbelow18_sprat_4root"))
print(dM1)
coefTable(dM1)
# export to excel

#failed

##### gamm ####
library(mgcv)

# list of all possible predictors (temp variables missing now)
M1<-lm(total_length~avg_year_temp+day_of_month +
         BIASmean + distance + # BIASmean_avg_since_1YearBefore * distance + BIASmean_avg_since_2YearBefore +
         # BIASmean_sum_since_1YearBefore * distance + BIASmean_sum_since_2YearBefore
         totCPUE_Abborre + # totCPUE_Abborre_avg_since_1YearBefore + totCPUE_Abborre_avg_since_2YearBefore +
         # totCPUE_Abborre_sum_since_1YearBefore + totCPUE_Abborre_sum_since_2YearBefore +
         CPUE_Abborre_25andabove + # CPUE_Abborre_25andabove_avg_since_1YearBefore + CPUE_Abborre_25andabove_avg_since_2YearBefore +
         # CPUE_Abborre_25andabove_sum_since_1YearBefore + CPUE_Abborre_25andabove_sum_since_2YearBefore +
         CPUE_Abborre_less25 + # CPUE_Abborre_less25_avg_since_1YearBefore + CPUE_Abborre_less25_avg_since_2YearBefore +
         # CPUE_Abborre_less25_sum_since_1YearBefore + CPUE_Abborre_less25_sum_since_2YearBefore +
         competitors + # competitors_avg_since_1YearBefore + competitors_avg_since_2YearBefore +
         # competitors_sum_since_1YearBefore + competitors_sum_since_2YearBefore +
         totCPUE_Mört  +# totCPUE_Mört_avg_since_1YearBefore + totCPUE_Mört_avg_since_2YearBefore +
         # totCPUE_Mört_sum_since_1YearBefore + totCPUE_Mört_sum_since_2YearBefore +
         cyprinids + # cyprinids_avg_since_1YearBefore + cyprinids_avg_since_2YearBefore +
         # cyprinids_sum_since_1YearBefore + cyprinids_sum_since_2YearBefore +
         clupeids + # clupeids_avg_since_1YearBefore + clupeids_avg_since_2YearBefore +
         # clupeids_sum_since_1YearBefore + clupeids_sum_since_2YearBefore +
         gobies + # gobies_avg_since_1YearBefore + gobies_avg_since_2YearBefore +
         # gobies_sum_since_1YearBefore + gobies_sum_since_2YearBefore +
         all_prey + # all_prey_avg_since_1YearBefore + all_prey_avg_since_2YearBefore +
         # all_prey_sum_since_1YearBefore + all_prey_sum_since_2YearBefore +
         year,
       # random=~1|sub.location, #weights=varFixed(~ avg_year_temp), method = "ML", 
       na.action = na.omit, #na.action = "na.fail", # na.action = na.pass, na.action = "na.exclude",
       data=length_age12_age2)
plot(length_age12_age2$cyprinids, length_age12_age2$gobies) # all prey  and gobies together gives alias coeff
vif(M1)

# with avg of predictors over the life span of fish:
# check collinearity: stsp and year don't get along well
M1<-lm(total_length~avg_year_temp+day_of_month +
         BIASmean_avg_since_2YearBefore + distance +
         #totCPUE_Abborre_avg_since_2YearBefore +
         CPUE_Abborre_25andabove_avg_since_2YearBefore +
         CPUE_Abborre_less25_avg_since_2YearBefore +
         competitors_avg_since_2YearBefore + 
         #totCPUE_Mört_avg_since_2YearBefore +
         cyprinids_avg_since_2YearBefore +
         clupeids_avg_since_2YearBefore +
         gobies_avg_since_2YearBefore,
         #all_prey_avg_since_2YearBefore,
         #year,
       # random=~1|sub.location, #weights=varFixed(~ avg_year_temp), method = "ML", 
       na.action = na.omit, #na.action = "na.fail", # na.action = na.pass, na.action = "na.exclude",
       data=length_age12_age2)
vif(M1)
# all prey  and gobies together gives alias coeff. I keep the prey separated for now
plot(length_age12_age2$cyprinids_avg_since_2YearBefore, length_age12_age2$gobies_avg_since_2YearBefore)
plot(length_age12_age2$year, length_age12_age2$BIASmean_avg_since_2YearBefore)

# beyond optimal model
M1<-lm(total_length~avg_year_temp+day_of_month +
         BIASmean_avg_since_2YearBefore * distance +
         #totCPUE_Abborre_avg_since_2YearBefore +
         CPUE_Abborre_25andabove_avg_since_2YearBefore +
         CPUE_Abborre_less25_avg_since_2YearBefore +
         competitors_avg_since_2YearBefore + 
         #totCPUE_Mört_avg_since_2YearBefore +
         cyprinids_avg_since_2YearBefore +
         clupeids_avg_since_2YearBefore +
         gobies_avg_since_2YearBefore,
       #all_prey_avg_since_2YearBefore,
       #year,
       # random=~1|sub.location, #weights=varFixed(~ avg_year_temp), method = "ML", 
       na.action = na.omit, #na.action = "na.fail", # na.action = na.pass, na.action = "na.exclude",
       data=length_age12_age2)
summary(M1)

# GAM
# using non linear models (with no temporal or spatial correl). leave temp out for now - IT TAKES FOREVER, REDUCE!
M1<-gam(total_length ~ te(BIASmean_avg_since_2YearBefore,distance) +
          s(CPUE_Abborre_25andabove_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(competitors_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(clupeids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(gobies_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"),
          method = "REML",data=length_age12_age2)
# REDUCED/modify smooth:
M1<-gam(total_length ~ s(BIASmean_avg_since_2YearBefore,distance) +
          s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"),
        method = "REML",data=length_age12_age2)
# with linear interaction term: and gear
M1<-gam(total_length ~ BIASmean_avg_since_2YearBefore*distance + factor(gear_code)+day_of_month +
          s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
          s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"),
        method = "REML",data=length_age12_age2)

summary(M1)
anova(M1) # useful when I have a factor, gives a overall F test
# The fx and k means that the amount of smoothing is not fixed to a predeternmined value;hence, cross-validation is used to estimate the optimal amount of smoothing. 
# bs is for cubic regression spline to be used. try either "cr" or "cs".
# if convergence problems: The option "control = lmc," can be used to ensure convergence
# One option is to increase the number of iterations in the routine or reduce the convergence criteria, see the help file of gamm
# Other options are to fix the degrees of freedom (and not use cross-validation) 

# plots
plot(M1)
plot.gam(M1,  shade=TRUE, residuals=TRUE, rug=T,pers=F, all.terms=T,shade.col = 2,by.resids=T,  scheme=3) 
plot.gam(M1,  shade=TRUE, residuals=F, rug=T,pers=F, all.terms=T,shade.col = 2, scheme=2)
plot.gam(M1, select=2, shade=TRUE, residuals=F, rug=T,pers=F, all.terms=F,shade.col=2, scheme=0, 
         seWithMean=T, pages=0) # change scheme and shade.col for different colors
vis.gam(M1, theta = 120, color = "heat")
vis.gam(M1, view=c("BIASmean_avg_since_2YearBefore","distance"), plot.type="contour", color="cm", n.grid=60) # color="bw", for no color


# try more graphic options here:
#https://cran.r-project.org/web/packages/tidymv/vignettes/predict-gam.html
# https://cran.r-project.org/web/packages/mgcViz/vignettes/mgcviz.html
#http://zevross.com/blog/2014/09/15/recreate-the-gam-partial-regression-smooth-plots-from-r-package-mgcv-with-a-little-style/
# for more options see: https://www.rdocumentation.org/packages/mgcv/versions/1.8-40/topics/plot.gam

# diagnostic:
gam.check (M1)
E <- resid(M1, type = "deviance")
Fit <- fitted(M1)
hist(E)
plot(x = Fit,y = E,xlab = "Fitted values", ylab = "Residuals",main = "Residuals versus fitted values") 
# or:
plot(M1, resid(., type = "n") ~ fitted(.),abline = 0, col = 1)

# with temp correlat: if not working -> try to change smoothers type
# temporal correlation nested within locatiion or sublocation doen't make sense, as I am not measuring the same fish in consecutive year
# rather, I will use location/sublocation as random, as well as year (which may not be significant once I get temperature variable)
# and try also corr based on lat and long
lmc <- lmeControl(niterEM = 5000,msMaxIter = 1000)
f <- formula(total_length ~ BIASmean_avg_since_2YearBefore*distance + factor(gear_code)+day_of_month +
               s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
               s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"))
M1<-gamm(f,control = lmc, method = "REML",data=length_age12_age2)
M2<-gamm(f,random = list(year =~ 1),control = lmc, method = "REML",data=length_age12_age2)
M3<-gamm(f,random = list(sub.location =~ 1),control = lmc, method = "REML",data=length_age12_age2)
M4<-gamm(f,random = list(location =~ 1),control = lmc, method = "REML",data=length_age12_age2)
M5<-gamm(f,correlation = corGaus(form =~ lat,nugget=TRUE),control = lmc, method = "REML",data=length_age12_age2)
#M6<-gamm(f,correlation = corGaus(form =~ lat + long,nugget=TRUE),control = lmc, method = "REML",data=length_age12_age2)
#M7<-gamm(f, correlation = corGaus(form =~ lat + long,nugget=TRUE),random = list(year =~ 1),
#         control = lmc, method = "REML",data=length_age12_age2)
AIC(M1,M2,M3,M4) 
# M3 best!

# add variance str (weights = varFixed(~ Year), weights = varIdent(form =~ 1|region)), possibly with control=lmc. Possibly change type of smoother
M3<-gamm(f,random = list(sub.location =~ 1),control = lmc, method = "REML",data=length_age12_age2)
M4<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ year),
         control = lmc, method = "REML",data=length_age12_age2)
M5<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ BIASmean_avg_since_2YearBefore),
         control = lmc, method = "REML",data=length_age12_age2)
M6<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ distance),
         control = lmc, method = "REML",data=length_age12_age2)
M7<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ day_of_month),
         control = lmc, method = "REML",data=length_age12_age2)
M8<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ CPUE_Abborre_less25_avg_since_2YearBefore),
         control = lmc, method = "REML",data=length_age12_age2)
M9<-gamm(f,random = list(sub.location =~ 1),weights = varFixed(~ all_prey_avg_since_2YearBefore),
         control = lmc, method = "REML",data=length_age12_age2)
M10<-gamm(f,random = list(sub.location =~ 1),weights = varIdent(form =~ 1|sub.location),
         control = lmc, method = "REML",data=length_age12_age2)
M11<-gamm(f,random = list(sub.location =~ 1),weights = varIdent(form =~ 1|location),
          control = lmc, method = "REML",data=length_age12_age2)
M12<-gamm(f,random = list(sub.location =~ 1),weights = varIdent(form =~ 1|gear_code),
          control = lmc, method = "REML",data=length_age12_age2)
AIC(M3,M4,M5,M7,M8,M9,M10,M11,M12)
# best: M10 and 11
f <- formula(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
               s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
               s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"))
M10<-gamm(f,random = list(sub.location =~ 1),weights = varIdent(form =~ 1|sub.location),
          control = lmc, method = "REML",data=length_age12_age2)
summary(M10$gam) # very low R2
anova(M10$gam)
plot(M10$gam)
plot(M10$lme)
summary(M10$lme)
plot(M10$lme, resid(., type = "n") ~ fitted(.),abline = 0, col = 1)
plot(M10$lme, resid(., type = "n") ~ CPUE_Abborre_less25_avg_since_2YearBefore,abline = 0, col = 1)

# plot linear term:
library(ggeffects)
pred <- ggpredict(M10, c("BIASmean_avg_since_2YearBefore", "distance"))
plot(pred)

# check with the method of cross validation if smoother gives only one degree of freedom. effect is linear if edf ~1
# amount of smoothing: look at edf (effective degrees of freedom). A high value (8-10 or higher) means that the curve is highly non-linear, 
# whereas a smoother with 1 degree of freedom is a straight line
# if I remove all smoothers, compare with ML: meglio gamm
M11<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
            s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
            s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"),
          random = list(sub.location =~ 1),weights = varIdent(form =~ 1|sub.location),
          control = lmc, na.action = na.omit,method = "ML",data=length_age12_age2)
M12<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + all_prey_avg_since_2YearBefore, 
         random = list(sub.location =~ 1),weights = varIdent(form =~ 1|sub.location),
         control = lmc,na.action = na.omit, method = "ML",data=length_age12_age2)
AIC(M11$lme, M12)

# to fix heteroschedasticity, try with different error distribution: family= "Gamma","quasipoisson","tw", "nb", scat,gaulss()
# remove variance str, as they don't run, indeed it doesn't make sense, as I account for hereroschedasticity via error distribution
f <- formula(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
               s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
               s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"))
M11<-gamm(f,random = list(sub.location =~ 1),weights = varIdent(form =~ 1|sub.location),
          control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11$gam) # R 0.05
gam.check(M11$gam)
M11a<-gamm(f,random = list(sub.location =~ 1),#weights = varIdent(form =~ 1|sub.location),
           family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11a$gam) #R = 0.09
gam.check(M11a$gam)
M11b<-gamm(f,random = list(sub.location =~ 1),#weights = varIdent(form =~ 1|sub.location),
           family= "quasipoisson", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11b$gam) # 0.06
gam.check(M11b$gam)
M11c<-gamm(f,random = list(sub.location =~ 1),#weights = varIdent(form =~ 1|sub.location),
           family= "tw", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11c$gam) # 0.07
gam.check(M11c$gam)
M11d<-gamm(f,random = list(sub.location =~ 1),#weights = varIdent(form =~ 1|sub.location),
           family= "nb", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11d$gam) # 0.08
gam.check(M11d$gam)
M11e<-gamm(f,random = list(sub.location =~ 1),#weights = varIdent(form =~ 1|sub.location),
           family= "scat", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11e$gam) # 0.03
gam.check(M11e$gam)

# best
M11a<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
             s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
             s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
           random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M11a$gam)
summary(M11a$lme)
plot(M11a$gam)
gam.check(M11a$gam)

# plot linear term:
library(ggeffects)
pred <- ggpredict(M11a, c("BIASmean_avg_since_2YearBefore", "distance"))
plot(pred)

# try different smooothers: cr, cs, tp - not much differnce
M2<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
             s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cs") +
             s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cs"), method = "REML",
           random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M2$gam) # R2 is 0.09. interaction ns

M2<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "tp") +
           s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "tp"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M2$gam) # R2 is 0.09. interaction ns

# try different fixed structure and compare to M11a
M3<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(CPUE_Abborre_25andabove_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
           random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M3$gam) # less R2

M4<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(totCPUE_Abborre_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(all_prey_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M4$gam) # meglio, R2 0.11. interaction signif: but it looks weak
pred <- ggpredict(M4, c("BIASmean_avg_since_2YearBefore", "distance"))
plot(pred)

M3<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M3$gam) # # super R is 0.16

M4<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M4$gam) # super R is 27
plot(M4$gam)
vis.gam(M4, theta = 120, n.grid = 50, lwd = 0.4)

# to know about smoother types, and how to tune a smooth:
# https://stat.ethz.ch/R-manual/R-devel/library/mgcv/html/smooth.terms.html#:~:text=Smooth%20terms%20are%20specified%20in%20a%20gam%20formula,and%20users%20can%20add%20smooth%20classes%20(see%20user.defined.smooth).
# https://stats.stackexchange.com/questions/243367/smoothing-methods-for-gam-in-mgcv-package
# https://stats.stackexchange.com/questions/12223/how-to-tune-smoothing-in-mgcv-gam-model

# if I use location in the fixed str (but not as diffenet smoother so far): doesn't run

# instead of the isotropic smooth ("s()"), try tensor product smooth ("te()"). I used it for the spatial component before
M5<-gamm(total_length ~ te(BIASmean_avg_since_2YearBefore,distance) +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M5$gam) # 0.20
plot(M5$gam)
vis.gam(M5$gam, view = c("BIASmean_avg_since_2YearBefore", "distance"),theta = 120, n.grid = 50, lwd = 0.4)
plot(M5$gam, page = 1, scheme = 2)

# try isotropic smoother instead of linear term for the interaction:
M6<-gamm(total_length ~ s(BIASmean_avg_since_2YearBefore,distance) +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "REML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M6$gam) # 0.28
vis.gam(M6$gam, view = c("BIASmean_avg_since_2YearBefore", "distance"),theta = 120, n.grid = 50, lwd = 0.4)
plot(M6$gam, page = 1, scheme = 2)

# remove the interaction and compare to M4:
M4<-gamm(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "ML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M4$gam) # super R is 27
plot(M4$gam)
vis.gam(M4, theta = 120, n.grid = 50, lwd = 0.4)

M1<-gamm(total_length ~ BIASmean_avg_since_2YearBefore+distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "ML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M1$gam) # 0.28
plot(M1$gam)
pred <- ggpredict(M1, c("BIASmean_avg_since_2YearBefore"))
plot(pred)
# the effects of stsp seem to be negative now: is bc of gobies? No, it is positive. but why the coeff is negative??

# without distance?
M2<-gamm(total_length ~ BIASmean_avg_since_2YearBefore +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "ML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M2$gam) # 0.18

# with smoother for stsp
M3<-gamm(total_length ~ s(BIASmean_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "ML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M3$gam) # 0.32

# preliminary final gamm: M3, amybe with REML and maybe without gobies
M3<-gamm(total_length ~ s(BIASmean_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +distance +gear_code + day_of_month +
           s(CPUE_Abborre_less25_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr") +
           s(gobies,fx = FALSE, k = -1, bs = "cr") +
           s(cyprinids_avg_since_2YearBefore,fx = FALSE, k = -1, bs = "cr"), method = "ML",
         random = list(sub.location =~ 1), family= "Gamma", control = lmc, na.action = na.omit,data=length_age12_age2)
summary(M3$gam) # 0.32
plot(M3$gam)
plot.gam(M3$gam,  shade=TRUE, residuals=TRUE, rug=T,pers=F, all.terms=T,shade.col = 2,by.resids=T,  scheme=3) 
pred <- ggpredict(M3, c("BIASmean_avg_since_2YearBefore"))
plot(pred)
gam.check(M3$gam)

##### LINEAR MODELS ####
# going linear, using info gathered above on bets fixed and random str.
# tuning random factor
M1b<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
         random=~1|sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age2)
M1c<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_age2)
AIC(M1b,M1c) # not much difference. but I should go for the nested str, according to Smith
M1d<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
         random=~1|location/sub.location, control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_age2)
AIC(M1c,M1d)


M1c<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_age2)
anova.lme(M1c, type = "marginal", adjustSigma = F) 
rsquared(M1c)
summary(M1c)
plot(M1c)
pred <- ggpredict(M1c, c("BIASmean_avg_since_2YearBefore", "distance")) # not running now
plot(pred)
# remove ranodm factor to plot with visreg:
#M1a<-gls(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
#          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
#        weights = varIdent(form =~ 1|sub.location), control = lmc,
#        na.action = na.omit, method = "REML",data=length_age12_age2)
#visreg(M1a, "BIASmean_avg_since_2YearBefore", by = "distance")
#visreg2d(M1a,x="distance",y="BIASmean_avg_since_2YearBefore",plot.type="image")

# # compare lme with variance str and glmm with gamma or other distrib: failed to converge
M2<-glmer(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies  
           +(1|sub.location),family=poisson,  control = lmc,
         na.action = na.omit, data=length_age12_age2)
summary(M2)

# remove interaction or other term and compare with LRT:
M1<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_age2)
M2<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month +
           CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_age2)
anova(M1,M2) #interaction is border line signif, gobies keep

# adding temp: collinearity? ok
M0<-lm(total_length ~ BIASmean_avg_since_2YearBefore+distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
        na.action = na.omit,data=length_age12_age2)
vif(M0)

M1<-lme(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age2)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)
# interaction became very signif. Plot it: not working now, seems to be ue to a bug
pred <- ggpredict(M1, terms = c("BIASmean_avg_since_2YearBefore", "distance"))
unique(length_age12_age2$BIASmean_avg_since_2YearBefore)
# use lmer instead: interaction is still very signif
M1a<-lmer(total_length ~ BIASmean_avg_since_2YearBefore*distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies +
            (1|location/sub.location), 
        na.action = na.omit, data=length_age12_age2)
M1b<-lmer(total_length ~ BIASmean_avg_since_2YearBefore+distance +gear_code + day_of_month + avg_year_temp +
            CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies +
            (1|location/sub.location), 
          na.action = na.omit, data=length_age12_age2)
anova(M1a,M1b)
# I am not able to compare it to the previous model
anova.lme(M1a, type = "marginal", adjustSigma = F) 
summary(M1a)
# but at leat I can plot the interaction
pred <- ggpredict(M1a, terms = c("BIASmean_avg_since_2YearBefore", "distance"))
plot(pred)

# preliminary final linear model:
M0<-lme(total_length ~ BIASmean_avg_since_2YearBefore+distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_2YearBefore + cyprinids_avg_since_2YearBefore + gobies, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age2)
anova.lme(M0, type = "marginal", adjustSigma = F) 
rsquared(M0)
summary(M0)
plot(M0)

pred <- ggpredict(M0, "CPUE_Abborre_less25_avg_since_2YearBefore")
predict(M0)

# check missing replicates:
length_age12_age2 %>%
  filter(is.na(BIASmean_avg_since_2YearBefore) | is.na(distance) | is.na(avg_year_temp) |is.na(gobies) |
           is.na(gear_code) | is.na(day_of_month) | is.na(CPUE_Abborre_less25_avg_since_2YearBefore) 
         |is.na(cyprinids_avg_since_2YearBefore)) %>%
  select(location, sub.location, year) %>%
  unique()

length_age12_age2 %>%
  filter(is.na(BIASmean_avg_since_2YearBefore)) %>%
  select(location, sub.location, year) %>%
  unique()

# check location and sublocation: 11 group for both? yes
subset(length_age12_age2, ! (is.na(BIASmean_avg_since_2YearBefore)| is.na(distance) | is.na(avg_year_temp) |is.na(gobies) |
                               is.na(gear_code) | is.na(day_of_month) | is.na(CPUE_Abborre_less25_avg_since_2YearBefore) 
                             |is.na(cyprinids_avg_since_2YearBefore)))%>%
         select(location, sub.location) %>%
         unique()
  

###### approach 2: means and more complex corr str - SKIP ####

# pool values per location and year and calculate the mean
# bring along also the number of samples per location and year to use as weight
# n_=n_distinct()

colnames(length_age12_age2)

length_age12_age2_pooled<-length_age12_age2 %>%
  group_by(location, year) %>%
  summarise(avg_total_length = mean(total_length, na.rm = TRUE),
            avg_year_temp = mean(avg_year_temp, na.rm = TRUE),
            avg_year_temp_1YearBefore = mean(avg_year_temp_1YearBefore, na.rm = TRUE),
            avg_year_temp_2YearBefore = mean(avg_year_temp_2YearBefore, na.rm = TRUE),
            totCPUE_Abborre = mean(totCPUE_Abborre, na.rm = TRUE),
            competitors = mean(competitors, na.rm = TRUE),
            all_prey = mean(all_prey, na.rm = TRUE),
            totCPUE_Abborre_1YearBefore = mean(totCPUE_Abborre_1YearBefore, na.rm = TRUE),
            competitors_1YearBefore = mean(competitors_1YearBefore, na.rm = TRUE),
            all_prey_1YearBefore = mean(all_prey_1YearBefore, na.rm = TRUE),
            avg_CPUEabbo_13_14cm = mean(CPUEabbo_13_14cm, na.rm = TRUE),
            avg_CPUEabbo_13_15cm = mean(CPUEabbo_13_15cm, na.rm = TRUE),
            avg_CPUEabbo_13_16cm = mean(CPUEabbo_13_16cm, na.rm = TRUE),
            #avg_field_temp = mean(field_temp, na.rm = TRUE), # this affect the catchbility, that is the CPUE, not the length. potentially test it in an interactuon with CPUE
            avg_day_of_month = mean(day_of_month, na.rm = TRUE),
            n_samples = n()) 

summary(length_age12_age2_pooled)

table(length_age12_age2_pooled$n_samples)
#####
# show me where I have NAs
length_age12_age2_pooled %>%
  filter(is.na(totCPUE_Abborre) | is.na(competitors) | 
           is.na(all_prey) | is.na(avg_day_of_month)) 

# check what type of gear:
length_age12_age2%>%
  filter(is.na(totCPUE_Abborre) | is.na(competitors) | 
           is.na(all_prey)) %>%
  select(gear_code, location, year) %>%
  unique()


# show me where I have sample = 1:Kvädöfjärden
length_age12_age2_pooled %>%
  filter(n_samples == 1) 
# show me where I have sample = 291: Blekinge län
length_age12_age2_pooled %>%
  filter(n_samples == 291)

# it looks like I have some NAs. 19 (out of 250) site*year combinations not found in the gillnets dataset. Why?
# However, not  big deal. Kvädöfjärden had only 1 fish. 
length_age12_age2 %>%
  filter(location == "Aspöja")
length_age10 %>%
  filter(location == "Aspöja")
gillnets_pool %>%
  filter(location == "Aspöja")
gillnets_pool %>%
  filter(location == "Forsmark")


#####

# collinearity: correlation matrix:
df <- data.frame(length_age12_age2_pooled$avg_year_temp, length_age12_age2_pooled$avg_year_temp_1YearBefore, length_age12_age2_pooled$avg_year_temp_2YearBefore,
                 length_age12_age2_pooled$totCPUE_Abborre, length_age12_age2_pooled$competitors,
                 length_age12_age2_pooled$all_prey)
# plot pariwise scatterplots of covariates:
pairs(df)

# with lagged variables
df_lag <- data.frame(length_age12_age2_pooled$avg_year_temp_1YearBefore, length_age12_age2_pooled$avg_year_temp_2YearBefore,
                     length_age12_age2_pooled$totCPUE_Abborre_1YearBefore, length_age12_age2_pooled$competitors_1YearBefore,
                     length_age12_age2_pooled$all_prey_1YearBefore)
# plot pariwise scatterplots of covariates:
pairs(df_lag)

# distributional properties
hist(length_age12_age2_pooled$avg_total_length)
hist(length_age12_age2_pooled$totCPUE_Abborre) 
hist(length_age12_age2_pooled$competitors) # consider log transf
hist(length_age12_age2_pooled$all_prey)


# check collinearity with vif
M0<- lm(avg_total_length ~ avg_year_temp + # avg_year_temp_1YearBefore + avg_year_temp_2YearBefore +  # temp
          totCPUE_Abborre + competitors + #  conspecifics, competitors
          #totCPUE_Mört + totCPUE_Löja +    # food single spp
          #clupeids + cyprinids + gobies +  # food pooled spp
          all_prey +                       # food total
          #totCPUE_Abborre_1YearBefore + competitors_1YearBefore + 
          #totCPUE_Mört_1YearBefore + totCPUE_Löja_1YearBefore + totCPUE_Storspigg +
          #clupeids_1YearBefore + cyprinids_1YearBefore + gobies_1YearBefore + 
          #all_prey_1YearBefore +
          avg_day_of_month, # account for extra growth in august until catch
        data = length_age12_age2_pooled)
vif(M0)

# try weighted regression on n samples - beyond optimal model
M1<-lm(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
       weights= n_samples,data=length_age12_age2_pooled)
# compare:
summary(M0)
summary(M1)
# similar results. However, if I use weight, not sure how to incorporate corr str

# without weight:

# useful random str for later. remove samples if it doesn't run (see above)
M2<-gls(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        method="REML",na.action=na.omit,,data=length_age12_age2_pooled)
M3<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corExp(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M4<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M6<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corGaus(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M7<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corSpher(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
AIC(M2,M3,M4,M5,M6,M7)
# best M3 and M4 

# variance str:
M4<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M8<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        weights=varFixed(~ avg_year_temp),
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M9<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
        weights=varFixed(~ totCPUE_Abborre),
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M10<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
         weights=varFixed(~ competitors),
         random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M11<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey  + avg_day_of_month,
         weights=varFixed(~ all_prey),
         random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
AIC(M4,M8,M9,M11)

# final preliminary
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M5, type = "marginal", adjustSigma = F) 
rsquared(M5)
summary(M5)
plot(M5)

# testing lags
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M5, type = "marginal", adjustSigma = F) 
rsquared(M5)
summary(M5)

M6<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey_1YearBefore + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M6, type = "marginal", adjustSigma = F) 
rsquared(M6)
summary(M6)

# totCPUE_Abborre_1YearBefore, _1YearBefore, all_prey_1YearBefore: not much difference.

# testing conspecifics size classes
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M5, type = "marginal", adjustSigma = F) 
rsquared(M5)
summary(M5)

M7<-lme(avg_total_length~avg_year_temp+avg_CPUEabbo_13_16cm  + competitors + all_prey + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M7, type = "marginal", adjustSigma = F) 
rsquared(M7)
summary(M7)
 

#OBS: If I don't use weight, maybe I should delete samples with n samples < of a certain value. Which one?

table(length_age12_age2_pooled$n_samples)
# if I use only site*location with at least 30 fish measured, I lose 48 replicates

length_age12_age2_pooled_nsample30<-length_age12_age2_pooled %>%
  filter(n_samples >= 30)

# final preliminary
M5a<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_day_of_month,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled_nsample30)
anova.lme(M5a, type = "marginal", adjustSigma = F) 
rsquared(M5a)
summary(M5a)
plot(M5a)






#####
# statistical analysis - age 3
#####

# using the random str from the best preliminary model from age 2:
hist(length_age12_age3$total_length)
# I keep conspecifics below 25. Possibly replace density of conspecifics based on pooled size classes up to 
# the max of total length in the subset

# collinearity: remove year (collinear with stsp)
M0<-lm(total_length ~ BIASmean_avg_since_3YearBefore+distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_3YearBefore + cyprinids_avg_since_3YearBefore + gobies, 
        na.action = na.omit, data=length_age12_age3)
vif(M0)

M1<-lme(total_length ~ BIASmean_avg_since_3YearBefore*distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_3YearBefore + cyprinids_avg_since_3YearBefore + gobies, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age3)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# plotting intearction with ggpredict fromlmer:
M2<-lmer(total_length ~ BIASmean_avg_since_3YearBefore*distance +gear_code + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_3YearBefore + cyprinids_avg_since_3YearBefore + gobies +
           (1|location/sub.location),
        na.action = na.omit,data=length_age12_age3)
pred <- ggpredict(M2, terms = c("BIASmean_avg_since_3YearBefore", "distance"))
plot(pred)
# without interaction: dosn't converge
M2a<-lmer(total_length ~ BIASmean_avg_since_3YearBefore+distance +gear_code + day_of_month + avg_year_temp +
           CPUE_Abborre_less25_avg_since_3YearBefore + cyprinids_avg_since_3YearBefore + gobies +
           (1|location/sub.location),
         na.action = na.omit,data=length_age12_age3)
pred <- ggpredict(M2, terms = c("BIASmean_avg_since_3YearBefore"))
plot(pred)

#####
# statistical analysis - age 4
#####
# using the random str from the best preliminary model from age 2:
hist(length_age12_age4$total_length)
# replace conspecifics below 25 with above. 

# collinearity: remove year (collinear with stsp)
M0<-lm(total_length ~ BIASmean_avg_since_4YearBefore+distance +gear_code + day_of_month + avg_year_temp +
         CPUE_Abborre_25andabove_avg_since_4YearBefore + cyprinids_avg_since_4YearBefore + gobies_avg_since_4YearBefore, 
       na.action = na.omit, data=length_age12_age4)
vif(M0)
# remove gear code

M1<-lme(total_length ~ BIASmean_avg_since_4YearBefore*distance  + day_of_month + avg_year_temp +
          CPUE_Abborre_25andabove_avg_since_4YearBefore + cyprinids_avg_since_4YearBefore + gobies_avg_since_4YearBefore, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age4)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# and with conspecifics below 25:
M1<-lme(total_length ~ BIASmean_avg_since_4YearBefore*distance  + day_of_month + avg_year_temp +
          CPUE_Abborre_less25_avg_since_4YearBefore + cyprinids_avg_since_4YearBefore + gobies_avg_since_4YearBefore, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age4)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

#####
# statistical analysis - age 5
#####
# using the random str from the best preliminary model from age 2:
hist(length_age12_age5$total_length)
# replace conspecifics below 25 with above. 

# collinearity: 
M0<-lm(total_length ~ BIASmean_avg_since_5YearBefore+distance + day_of_month + avg_year_temp +
         CPUE_Abborre_25andabove_avg_since_5YearBefore  + gobies_avg_since_5YearBefore, 
       na.action = na.omit, data=length_age12_age5)
vif(M0)
# remove year (collinear with stsp), gear code, cyprinids collinear with abbo

M1<-lme(total_length ~ BIASmean_avg_since_5YearBefore*distance  + day_of_month + avg_year_temp +
          CPUE_Abborre_25andabove_avg_since_5YearBefore  + gobies_avg_since_4YearBefore, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_age5)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# plotting intearction with ggpredict from lmer: no convergence
M2<-lmer(total_length ~ BIASmean_avg_since_5YearBefore*distance  + day_of_month + avg_year_temp +
           CPUE_Abborre_25andabove_avg_since_5YearBefore  + gobies_avg_since_4YearBefore +
           (1|location/sub.location),
         na.action = na.omit,data=length_age12_age5)
pred <- ggpredict(M2, terms = c("BIASmean_avg_since_3YearBefore", "distance"))
plot(pred)


#####
# statistical analysis - all ages - LINEAR MODEL
#####
table(length_age12$age, length_age12$location)

# distributional properties
hist(length_age12_stack$total_length)
hist(length_age12_stack$BIASmean_avg_lifespan)
hist(length_age12_stack$cyprinids_avg_lifespan) # c
hist(length_age12_stack$totCPUE_Mört_avg_lifespan) # 
hist(length_age12_stack$all_prey_avg_lifespan) # 
hist(length_age12_stack$totCPUE_Abborre_avg_lifespan)
hist(length_age12_stack$CPUE_Abbo_samesize_avg_lifespan)

# collinearity: correlation matrix:
df <- data.frame(length_age12_stack$total_length, length_age12_stack$avg_year_temp, length_age12_stack$BIASmean_avg_lifespan,
                 length_age12_stack$cyprinids_avg_lifespan,length_age12_stack$totCPUE_Mört_avg_lifespan,
                 length_age12_stack$all_prey_avg_lifespan,length_age12_stack$age,
                 length_age12_stack$totCPUE_Abborre_avg_lifespan,length_age12_stack$CPUE_Abbo_samesize_avg_lifespan)
# plot pariwise scatterplots of covariates: takes forever
# pairs(df)


# collinearity: 
M0<-lm(total_length ~ age + BIASmean_avg_lifespan +distance +gear_code + day_of_month + avg_year_temp + #year +
         CPUE_Abbo_samesize_avg_lifespan + cyprinids_avg_lifespan, 
       na.action = na.omit, data=length_age12_stack)
vif(M0)
# plot stsp vs year:
plot(length_age12_stack$year, length_age12_stack$BIASmean_avg_lifespan)
# even though vif is not too bad (2.7 max), scatterplot of stsp and year is worrisome, I'd remove year

# use random str from the best preliminary model from age 2:
M1<-lme(total_length ~ age*BIASmean_avg_lifespan * distance + gear_code + day_of_month + age*avg_year_temp + #year +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# explore three way interaction:
ggeffect(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
  #labs(x = "Age", y = "Total length (cm)", title = "Effect of age and BIASmean_avg_lifespan on total length") +
  #theme_minimal(base_size = 14) +
  #theme(legend.position = "bottom") 
ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() # same output
ggpredict(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() # not working
ggaverage(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() # stuck

# exploring different options:
ggeffect(M1, terms = ~ BIASmean_avg_lifespan:distance:age) %>%
  plot() # same
ggeffect(M1, terms = c("BIASmean_avg_lifespan[1:2]", "distance", "age")) %>%
  plot() # plot for specifcc ranges of focal terms

myp<-ggeffect(M1, terms = c("BIASmean_avg_lifespan", "age","distance"))
ggplot(myp, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)
print(myp, collapse_tables = TRUE)

# plotting interaction with ggpredict from lmer: same
M2<-lmer(total_length ~ age*BIASmean_avg_lifespan * distance + gear_code + day_of_month + age*avg_year_temp + #year +
           age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan +
           (1|location/sub.location),
         na.action = na.omit,data=length_age12_stack)
pred <- ggpredict(M2, terms = c("BIASmean_avg_lifespan", "distance","age"))
plot(pred)

# explore 2 way interactions:
ggeffect(M1, terms = c("BIASmean_avg_lifespan", "distance")) %>%
  plot() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") 
ggeffect(M1, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "bottom") 

ggeffect(M1, terms = c("CPUE_Abbo_samesize_avg_lifespan", "age")) %>%
  plot() 
ggeffect(M1, terms = c("cyprinids_avg_lifespan", "age")) %>%
  plot() 
ggeffect(M1, terms = c("avg_year_temp", "age")) %>%
  plot()
ggeffect(M1, terms = c("distance", "age")) %>%
  plot()

ggeffect(M1, terms = c("BIASmean_avg_lifespan")) %>%
  plot()


# use LRT tests to text significance of three way interaction:
M1a<-lme(total_length ~ age*BIASmean_avg_lifespan * distance + gear_code + day_of_month + age*avg_year_temp + #year +
           age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_stack)
M1b<-lme(total_length ~ age*BIASmean_avg_lifespan + BIASmean_avg_lifespan* distance + age* distance +
           gear_code + day_of_month + age*avg_year_temp + #year +
           age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_stack)
anova(M1a,M1b) # 3 way interaction is very signif

##### adding temperature variables to the model: ####
# predictors:
# species: BIASmean_avg_lifespan, cyprinids_avg_lifespan, totCPUE_Mört_avg_lifespan,all_prey_avg_lifespan,clupeids_avg_lifespan
# totCPUE_Abborre_avg_lifespan, CPUE_Abbo_samesize_avg_lifespan
# temperature:temp_year_avg_lifespan,temp_summer_avg_lifespan,temp_winter_avg_lifespan,temp_exceeding_10_year_avg_lifespan,
# dd_year_sum_lifespan,dd_year_avg_lifespan, n_days_exceeding_10_year_avg_lifespan, n_days_exceeding_10_year_sum_lifespan,
# first_day_exceeding_10_julian_avg_lifespan
# others: distance, day_of_month, gear_code, year, age, sub.location, location

###### non standardized variables:#####

# collinearity: 
M0<-lm(total_length ~ age + BIASmean_avg_lifespan +distance +gear_code + day_of_month + temp_year_avg_lifespan + #year +
         CPUE_Abbo_samesize_avg_lifespan + cyprinids_avg_lifespan, 
       na.action = na.omit, data=length_age12_stack)
vif(M0)
# plot stsp vs year:
plot(length_age12_stack$year, length_age12_stack$BIASmean_avg_lifespan)
plot(length_age12_stack$temp_year_avg_lifespan, length_age12_stack$BIASmean_avg_lifespan)
# even though vif is not too bad (2.7 max), scatterplot of stsp and year is worrisome, I'd remove year

# use random str from the best preliminary model from age 2:
M1<-lme(total_length ~ age*BIASmean_avg_lifespan * distance + gear_code + day_of_month + age*temp_year_avg_lifespan + #year +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# explore three way interaction:
ggeffect(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
#labs(x = "Age", y = "Total length (cm)", title = "Effect of age and BIASmean_avg_lifespan on total length") +
#theme_minimal(base_size = 14) +
#theme(legend.position = "bottom") 
ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() # same output
ggpredict(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() # not working

myp<-ggeffect(M1, terms = c("BIASmean_avg_lifespan", "age","distance"))
ggplot(myp, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)
print(myp, collapse_tables = TRUE)

# Test two way interactions: age*stsp + distance*stsp
M1<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_year_avg_lifespan + #year +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance")) %>%
  plot() 
ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() 
ggemmeans(M1, terms = c("temp_year_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M1, terms = c("CPUE_Abbo_samesize_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M1, terms = c("cyprinids_avg_lifespan", "age")) %>%
  plot()

# test an interaction cyprinids_avg_lifespan*distance
M1<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

ggemmeans(M1, terms = c("cyprinids_avg_lifespan", "distance")) %>%
  plot()

# removing distance:
M1<-lme(total_length ~ age*BIASmean_avg_lifespan + gear_code + day_of_month + 
          age*temp_year_avg_lifespan + #year +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() 
ggemmeans(M1, terms = c("temp_year_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M1, terms = c("CPUE_Abbo_samesize_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M1, terms = c("cyprinids_avg_lifespan", "age")) %>%
  plot()


# comparing fit for differnet temp variables:
# temp_year_avg_lifespan,temp_summer_avg_lifespan,temp_winter_avg_lifespan,temp_exceeding_10_year_avg_lifespan,
# dd_year_sum_lifespan,dd_year_avg_lifespan, n_days_exceeding_10_year_avg_lifespan, n_days_exceeding_10_year_sum_lifespan,
# first_day_exceeding_10_julian_avg_lifespan

M1<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M2<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_summer_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M3<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_winter_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M4<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*temp_exceeding_10_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M5<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*dd_year_sum_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M6<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*dd_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M7<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*n_days_exceeding_10_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M8<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*n_days_exceeding_10_year_sum_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

M9<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*first_day_exceeding_10_julian_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)

rsquared(M1)
rsquared(M2)
rsquared(M3) # good
rsquared(M4)
rsquared(M5)
rsquared(M6) # excellent
rsquared(M7) # excellent
rsquared(M8)
rsquared(M9)

# check if any temp var can coestist: nope
cor.test(length_age12_stack$dd_year_avg_lifespan, length_age12_stack$first_day_exceeding_10_julian_avg_lifespan)

### one of the final three best models (now including three way interaction)
M6a<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age*distance +
          cyprinids_avg_lifespan*age*distance, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M6a, type = "marginal", adjustSigma = F) 
rsquared(M6a)
summary(M6a)
plot(M6a)

ggemmeans(M6a, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6a, terms = c("cyprinids_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6a, terms = c("dd_year_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M6a, terms = c("CPUE_Abbo_samesize_avg_lifespan", "distance", "age")) %>%
  plot() 

myp<-ggeffect(M6a, terms = c("BIASmean_avg_lifespan", "age","distance"))
ggplot(myp, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)
print(myp, collapse_tables = TRUE)

summary(length_age12_stack$dd_year_avg_lifespan)

# test if extreme warm temp have more (positive) effect on older perch
M6a<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
           dd_year_sum_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age*distance +
           cyprinids_avg_lifespan*age*distance, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M6a, type = "marginal", adjustSigma = F) 
rsquared(M6a)
summary(M6a)
plot(M6a)

ggemmeans(M6a, terms = c("dd_year_sum_lifespan", "age")) %>%
  plot()


###### exploring relationship between distance and spp:
# are abbo and cyprinids varying with distance?
plot(length_age12_stack$distance, length_age12_stack$CPUE_Abbo_samesize_avg_lifespan)
plot(length_age12_stack$distance, length_age12_stack$cyprinids_avg_lifespan)

cor.test(length_age12_stack$distance, length_age12_stack$CPUE_Abbo_samesize_avg_lifespan, method ="spearman")
cor.test(length_age12_stack$distance, length_age12_stack$cyprinids_avg_lifespan, method ="spearman")

ggplot(length_age12_stack, aes(x = distance, y = CPUE_Abbo_samesize_avg_lifespan)) +
  geom_point(size=3)+
  geom_smooth(method = "loess")

plot(length_age12_stack$distance, length_age12_stack$CPUE_Abborre_25andabove)


summary(length_age12_stack_f$cyprinids_avg_lifespan)

length_age12_stack$distance_f<-as.factor(length_age12_stack$distance)
# remove NA from CPUE_Abbo_samesize_avg_lifespan:
length_age12_stack_f <- length_age12_stack %>%
  filter(!is.na(CPUE_Abbo_samesize_avg_lifespan))

avg<-tapply(length_age12_stack_f$CPUE_Abborre_25andabove,list(length_age12_stack_f$distance_f),mean)
sdpl<-tapply(length_age12_stack_f$CPUE_Abborre_25andabove,list(length_age12_stack_f$distance_f),sd)
l<-tapply(length_age12_stack_f$CPUE_Abborre_25andabove,list(length_age12_stack_f$distance_f),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=F,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5) 

# model with 1 three way interaction:
M1<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M1, terms = c("cyprinids_avg_lifespan", "age")) %>%
  plot() 
ggemmeans(M1, terms = c("dd_year_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M1, terms = c("CPUE_Abbo_samesize_avg_lifespan", "age")) %>%
  plot() 

myp<-ggeffect(M1, terms = c("BIASmean_avg_lifespan", "age","distance"))
ggplot(myp, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)
print(myp, collapse_tables = TRUE)

# the interaction abbo*age doesn't seem very strong. I f i remove it: higher R2!
M1a<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M1a, type = "marginal", adjustSigma = F) 
rsquared(M1a)
summary(M1a)
plot(M1a)

vif(M1a)

ggemmeans(M1a, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 

# use LRT tests to text significance of terms: all signif
M2<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack)
M3<-lme(total_length ~ BIASmean_avg_lifespan*distance+ BIASmean_avg_lifespan*age+distance*age
        +gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack)
anova(M2,M3)

M4<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack)
anova(M2,M4)

###### standardized variables: #####

# standardize variables:
length_age12_stack_std <- length_age12_stack %>%
  mutate(across(c(BIASmean_avg_lifespan, distance, age, CPUE_Abbo_samesize_avg_lifespan, 
                  cyprinids_avg_lifespan, day_of_month, dd_year_avg_lifespan), 
                ~ scale(.) %>% as.vector()))

# best two-way interactions model from above:
M6<-lme(total_length ~ age*BIASmean_avg_lifespan +BIASmean_avg_lifespan*distance + gear_code + day_of_month + 
          age*dd_year_avg_lifespan + #year +
          distance* cyprinids_avg_lifespan +
          age*CPUE_Abbo_samesize_avg_lifespan + age*cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack_std)

anova.lme(M6, type = "marginal", adjustSigma = F) 
rsquared(M6)
summary(M6)
plot(M6)

ggemmeans(M6, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() 
ggemmeans(M6, terms = c("dd_year_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M6, terms = c("CPUE_Abbo_samesize_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M6, terms = c("cyprinids_avg_lifespan", "age")) %>%
  plot()
ggemmeans(M6, terms = c("BIASmean_avg_lifespan", "distance")) %>%
  plot() 

# testing 2 three way interactions
M6a<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month + 
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + 
          cyprinids_avg_lifespan*age*distance, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova.lme(M6a, type = "marginal", adjustSigma = F) 
rsquared(M6a)
summary(M6a)
plot(M6a)

ggemmeans(M6a, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6a, terms = c("cyprinids_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6a, terms = c("dd_year_avg_lifespan", "age")) %>%
  plot()


# and 3 three way interactions 
M6b<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month + 
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age*distance + 
           cyprinids_avg_lifespan*age*distance, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova.lme(M6b, type = "marginal", adjustSigma = F) 
rsquared(M6b)
summary(M6b)
plot(M6b)

ggemmeans(M6b, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6b, terms = c("cyprinids_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6b, terms = c("CPUE_Abbo_samesize_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M6b, terms = c("dd_year_avg_lifespan", "age")) %>%
  plot()

myp<-ggeffect(M6b, terms = c("BIASmean_avg_lifespan", "age","distance"))
ggplot(myp, aes(x, predicted, colour = group)) +
  geom_line() +
  facet_wrap(~facet)
print(myp, collapse_tables = TRUE)


plot(length_age12_stack_std$distance, length_age12_stack_std$CPUE_Abbo_samesize_avg_lifespan)
plot(length_age12_stack_std$distance, length_age12_stack_std$cyprinids_avg_lifespan)



##### comparing models using cross-validated RMSE or MAE:

# I am doing this bc, at least for the model with non standardized variables, the R2 seem to increase slightly despite both F and LRT
# test were significant. May it be a problem of overfitting? Run cross-validation

# Load required libraries
library(nlme)
library(rsample)
library(purrr)
library(yardstick)
library(dplyr)

# Set seed for reproducibility
set.seed(123)

# Remove missing values
data_std <- data.frame(length_age12_stack_std[,c("sub.location", "location", "year", "total_length", "BIASmean_avg_lifespan",
                                                 "distance", "age","gear_code","CPUE_Abbo_samesize_avg_lifespan",
                                                 "cyprinids_avg_lifespan", "day_of_month","dd_year_avg_lifespan")])
data_std_clean <- na.omit(data_std)

# Create 10-fold cross-validation splits
folds <- vfold_cv(data_std_clean, v = 10)

# Define model fitting and prediction function
fit_lme_model <- function(split, formula) {
  train_data <- analysis(split)
  test_data <- assessment(split)
  
  model <- lme(
    fixed = formula,
    random = ~1 | location/sub.location,
    weights = varIdent(form = ~1 | sub.location),
    control = lmc,
    na.action = na.omit,
    method = "REML",
    data = train_data
  )
  
  preds <- predict(model, newdata = test_data, allow.new.levels = TRUE)
  
  tibble(
    truth = test_data$total_length,
    estimate = preds
  )
}

# Define formulas
formula_full <- total_length ~ BIASmean_avg_lifespan * distance * age + gear_code + day_of_month +
  dd_year_avg_lifespan * age + CPUE_Abbo_samesize_avg_lifespan * age + cyprinids_avg_lifespan * age
formula_reduced <- total_length ~ BIASmean_avg_lifespan * distance + BIASmean_avg_lifespan * age + 
  distance * age + gear_code + day_of_month +
  dd_year_avg_lifespan * age + CPUE_Abbo_samesize_avg_lifespan + cyprinids_avg_lifespan * age

# Run cross-validation for full model
library(tidyr)
results_full <- folds %>%
  mutate(metrics = map(splits, ~fit_lme_model(.x, formula_full))) %>%
  unnest(metrics)

# Run cross-validation for reduced model
results_reduced <- folds %>%
  mutate(metrics = map(splits, ~fit_lme_model(.x, formula_reduced))) %>%
  unnest(metrics)

# Compare RMSE and MAE
cat("Full Model:\n")
print(rmse(results_full, truth = truth, estimate = estimate))
print(mae(results_full, truth = truth, estimate = estimate))

cat("\nReduced Model:\n")
print(rmse(results_reduced, truth = truth, estimate = estimate))
print(mae(results_reduced, truth = truth, estimate = estimate))

### similar values. look deeper:

# model with 1 three way interaction
M1<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova.lme(M1, type = "marginal", adjustSigma = F) 
rsquared(M1)
summary(M1)
plot(M1)

# model with no three way interaction (but no dist*age)
M1a<-lme(total_length ~ BIASmean_avg_lifespan*distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova.lme(M1a, type = "marginal", adjustSigma = F) 
rsquared(M1a)
summary(M1a)
plot(M1a)

AIC(M1,M1a)

ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance", "age")) %>%
  plot() 
ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "distance")) %>%
  plot() 
ggemmeans(M1a, terms = c("BIASmean_avg_lifespan", "distance")) %>%
  plot() 
ggemmeans(M1, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() 
ggemmeans(M1a, terms = c("BIASmean_avg_lifespan", "age")) %>%
  plot() 

# Compare predictions directly: Look at the predicted values from both models on the same validation sets.
# Check model coefficients: Are they different between the full and reduced models?
# Consider R² or adjusted R² for more insight.
# Visualize residuals: Plot residuals for both models to see if there's any pattern or difference.


# check signif using LRT:
#####
M2<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
M3<-lme(total_length ~ BIASmean_avg_lifespan*distance+ BIASmean_avg_lifespan*age+distance*age
        +gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M3)

M4<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan +
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M4)

M5<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan + 
          cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M5)

M6<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + 
          cyprinids_avg_lifespan, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M6)

M7<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M7)

M8<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + 
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "ML",data=length_age12_stack_std)
anova(M2,M8)
#####
# and the two way interactions?
M1a<-lme(total_length ~ BIASmean_avg_lifespan*distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova.lme(M1a, type = "marginal", adjustSigma = F) 
rsquared(M1a)
summary(M1a)
plot(M1a)

M1b<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
M1c<-lme(total_length ~ BIASmean_avg_lifespan*distance + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)

M1d<-lme(total_length ~ BIASmean_avg_lifespan*distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
M1e<-lme(total_length ~ BIASmean_avg_lifespan*distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan+cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
M1f<-lme(total_length ~ BIASmean_avg_lifespan*distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age+cyprinids_avg_lifespan,
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)

rsquared(M1a)
rsquared(M1b)
rsquared(M1c)
rsquared(M1d)
rsquared(M1e)
rsquared(M1f)

AIC(M1,M1a,M1b,M1c,M1d,M1e,M1f)

plot(M1a)
plot(M1b)
plot(M1c)
plot(M1d)
plot(M1e)
plot(M1f)

# if I remove distance
M1g<-lme(total_length ~ BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
rsquared(M1g)

# If I delete distance, are cyprinids gaining more explanatory power 
# (meaning that distance affect the amount of cyprinids)?
anova.lme(M1g, type = "marginal", adjustSigma = F)
M1gg<-lme(total_length ~ BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
rsquared(M1gg)


# if I remove stsp
M1h<-lme(total_length ~ distance+ gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)

# if I remove temp
M1i<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)

# if I remove abbo
M1l<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
# if I remove cypr
M1m<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + gear_code + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
# if I remove gear code
M1n<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + day_of_month +
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
# if I remove  day
M1o<-lme(total_length ~ distance+BIASmean_avg_lifespan*age + gear_code + 
           dd_year_avg_lifespan*age+
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)

rsquared(M1b)
rsquared(M1g)
rsquared(M1h)
rsquared(M1i)
rsquared(M1l)
rsquared(M1m)
rsquared(M1n)
rsquared(M1o)

# check outliers:
dotchart(length_age12_stack_std$BIASmean_avg_lifespan) # ok
dotchart(length_age12_stack_std$distance)
dotchart(length_age12_stack_std$dd_year_avg_lifespan)
dotchart(length_age12_stack_std$CPUE_Abbo_samesize_avg_lifespan)
dotchart(length_age12_stack_std$cyprinids_avg_lifespan)
dotchart(length_age12_stack_std$distance)


# remove outliers, i.e. all values above 3 for the predictors: 400 obs less
length_age12_stack_std_outliers <- length_age12_stack_std %>%
  filter(BIASmean_avg_lifespan < 3, distance < 3, dd_year_avg_lifespan < 3,
         CPUE_Abbo_samesize_avg_lifespan < 3, cyprinids_avg_lifespan < 3)

M1_out<-lme(total_length ~ BIASmean_avg_lifespan*distance*age + gear_code + day_of_month +
          dd_year_avg_lifespan*age+
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack_std_outliers)
anova.lme(M1_out, type = "marginal", adjustSigma = F) 
rsquared(M1_out)
summary(M1_out)
plot(M1_out)

# after looking at all R2, model that could be the most conservative:
M<-lme(total_length ~ distance+BIASmean_avg_lifespan+age + gear_code + 
           dd_year_avg_lifespan+
           CPUE_Abbo_samesize_avg_lifespan, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "REML",data=length_age12_stack_std)
anova(M)
rsquared(M)
summary(M)
plot(M)



###### checking a 4 way interaction: TO DO or skip ######
M4<-lme(total_length ~ BIASmean_avg_lifespan*distance*age*dd_year_avg_lifespan + gear_code + day_of_month +
          CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
        random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
        na.action = na.omit, method = "REML",data=length_age12_stack)
anova.lme(M4, type = "marginal", adjustSigma = F) 
rsquared(M4)
summary(M4)
plot(M4)
# with LRT
M4a<-lme(total_length ~ BIASmean_avg_lifespan*distance*age*dd_year_avg_lifespan + gear_code + day_of_month +
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_stack)
M4b<-lme(total_length ~ BIASmean_avg_lifespan*distance*age*dd_year_avg_lifespan + 
           BIASmean_avg_lifespan*distance*age +
           BIASmean_avg_lifespan*distance*dd_year_avg_lifespan+
           BIASmean_avg_lifespan*age*dd_year_avg_lifespan+
           distance*age*dd_year_avg_lifespan+
           gear_code + day_of_month +
           CPUE_Abbo_samesize_avg_lifespan*age + cyprinids_avg_lifespan*age, 
         random=~1|location/sub.location,weights = varIdent(form =~ 1|sub.location), control = lmc,
         na.action = na.omit, method = "ML",data=length_age12_stack)



#####
# statistical analysis - all ages - GAMM
#####

f <- formula(total_length ~ s(age,BIASmean_avg_lifespan, distance) + gear_code + day_of_month + 
               s(avg_year_temp, by = as.numeric(age == "2"))+
               s(avg_year_temp, by = as.numeric(age == "3"))+
               s(avg_year_temp, by = as.numeric(age == "4"))+
               s(avg_year_temp, by = as.numeric(age == "5"))+
               s(CPUE_Abbo_samesize_avg_lifespan,fx = FALSE, k = -1, bs = "cr")+
               s(cyprinids_avg_lifespan, by = as.numeric(age == "2"))+
               s(cyprinids_avg_lifespan, by = as.numeric(age == "3"))+
               s(cyprinids_avg_lifespan, by = as.numeric(age == "4"))+
               s(cyprinids_avg_lifespan, by = as.numeric(age == "5")))
# doesn't converge. Simplify:
f <- formula(total_length ~ s(BIASmean_avg_lifespan, distance) + gear_code + day_of_month + 
               s(avg_year_temp,fx = FALSE, k = -1, bs = "cr")+
               s(CPUE_Abbo_samesize_avg_lifespan,fx = FALSE, k = -1, bs = "cr")+
               s(cyprinids_avg_lifespan,fx = FALSE, k = -1, bs = "cr"))
M10<-gamm(f,random = list(location =~ 1),weights = varIdent(form =~ 1|location),
          control = lmc, method = "REML",data=length_age12_stack)
summary(M10$gam) # R2 is 
anova(M10$gam)
plot(M10$gam)
plot(M10$lme)
summary(M10$lme)
plot(M10$lme, resid(., type = "n") ~ fitted(.),abline = 0, col = 1)
plot(M10$lme, resid(., type = "n") ~ CPUE_Abbo_samesize_avg_lifespan,abline = 0, col = 1)
plot.gam(M10$gam,  shade=TRUE, residuals=TRUE, rug=T,pers=F, all.terms=T,shade.col = 2,by.resids=T,  scheme=3) 
plot.gam(M10$gam,  shade=TRUE, residuals=F, rug=T,pers=F, all.terms=T,shade.col = 2, scheme=2)
plot.gam(M10$gam, select=2, shade=TRUE, residuals=F, rug=T,pers=F, all.terms=F,shade.col=2, scheme=0, 
         seWithMean=T, pages=0) # change scheme and shade.col for different colors
vis.gam(M10$gam, theta = 120, color = "heat")
vis.gam(M10$gam, view=c("BIASmean_avg_lifespan","distance"), plot.type="contour", color="cm", n.grid=60) # color="bw", for no color
vis.gam(M10$gam, view=c("BIASmean_avg_lifespan","distance"),color = "heat")

# with family gamma instead of variance str:
M11<-gamm(f,random = list(location =~ 1), family = "Gamma", 
          control = lmc, method = "REML",data=length_age12_stack)
summary(M11$gam) 
anova(M11$gam)
plot(M11$lme)
vis.gam(M11$gam, view=c("BIASmean_avg_lifespan","distance"),color = "heat")


#####
# time series analysis
#####

# count n of sampled years per sublocation, split by gear_code, and first and last year of sampling
range_years<-length_age12_stack %>%
  group_by(location,sub.location,gear_code) %>%
  summarise(unique_years_sublocat = n_distinct(year),
            first_year = min(year),
            last_year = max(year)) %>%
  print(n=41)

table(range_years$unique_years_sublocat, range_years$gear_code)

# select only sublocation*gear with at least 7 years of sampling and sampled with k064
range_years_subset7<-range_years %>%
  filter(unique_years_sublocat > 6, gear_code == "K064")

# select only sublocation*gear with at least 10 years of sampling and sampled with k064
range_years_subset10<-range_years %>%
  filter(unique_years_sublocat > 9, gear_code == "K064")

# reason to select only gear K064:
# in few cases fishes in the same sublocation have been collected with different gear.
# I tried first to include gear in the temporal trend models but I couldn't use the loop function as some 
# models would not run due to only one level in the factor gear. Hence I excluded gear others than K064


# subset of length dataset with sublocation*gear with at least 10 and 7 years of data
# select a subset with gear k064, and whose sublocation match those in the range_years_subset10:
length_age12_stack_time_series7<-length_age12_stack %>%
  filter(gear_code == "K064") %>%
  filter(sub.location %in% range_years_subset7$sub.location)

length_age12_stack_time_series10<-length_age12_stack %>%
  filter(gear_code == "K064") %>%
  filter(sub.location %in% range_years_subset10$sub.location)


# add a variable indicating sublocation and age:
length_age12_stack_time_series10$sub.location_age<-paste(length_age12_stack_time_series10$sub.location, 
                                                         length_age12_stack_time_series10$age, sep = "_")
length_age12_stack_time_series7$sub.location_age<-paste(length_age12_stack_time_series7$sub.location, 
                                                         length_age12_stack_time_series7$age, sep = "_")
table(length_age12_stack_time_series10$sub.location_age)

# plot times series  for each sublocation*age
ggplot(length_age12_stack_time_series7, aes(x = year, y = total_length, colour = age)) +
  geom_point(size=2)+ 
  facet_wrap(~sub.location)+
  geom_smooth(aes(group = age), method = "loess")
  theme_bw(base_size=15)


##### calculate slopes, SE and p values for length at age for each site ##########

# calculate slopes for length at age and include Julian date (day of month)
# fish are not the same individuals in different years, hence corAR random str makes no sense,
# rather consider year as random, to account for non independence of values taken in the same year

# check model for single site
my_site_age1 <- length_age12_stack_time_series7[length_age12_stack_time_series7$sub.location_age == "Asköfjärden_2", ]
M1 <- lme(total_length ~ year+day_of_month, random =~1|year,
          na.action=na.omit, control = lmc, method = "REML", data=my_site_age1) 
# determine significance via marginal anova
anova.lme(M1, type = "marginal", adjustSigma = F)
summary(M1)
plot(M1)
# determine significance via LRT test (model comparison)
M1a <- lme(total_length ~ year+day_of_month, random =~1|year,
          na.action=na.omit, control = lmc, method = "ML", data=my_site_age1) 
M1b <- lme(total_length ~ day_of_month, random =~1|year,
          na.action=na.omit, control = lmc, method = "ML", data=my_site_age1) 
anova(M1a, M1b)

output_comparison<-anova(M1a, M1b)
output_comparison[2,"p-value"]
M3 <- lme(total_length ~ year+day_of_month, random =~1|year,
          na.action=na.omit, control = lmc, method = "REML", data=my_site_age1)
summary(M3)
coef_value<-summary(M3)$coefficients
coef_value[["fixed"]][["year"]]

# To extract stand error of slope:
summary(M3)$tTable[2,2] 

# calculate increase rate for specific site
# extract fitted values for the first and last year of sampling
F1<-fitted(M3)
F1
(234.8402-163.8301)/163.8301*100 # 43% increase from 2005 to 2022  :0
# visual check:
visreg(M3)

# 1)extract LRT test pvalue for all sites*age:
result_LRT <- vector("list")
for (sub.location_age in unique(length_age12_stack_time_series7$sub.location_age)) {
  my_site_age <- length_age12_stack_time_series7[length_age12_stack_time_series7$sub.location_age == sub.location_age, ]
  M1 <- lme(total_length ~ year+day_of_month, random =~1|year,
            na.action=na.omit, control = lmc, method = "ML", data=my_site_age) 
  M2 <- lme(total_length ~ day_of_month, random =~1|year,
            na.action=na.omit, control = lmc, method = "ML", data=my_site_age)
  anova(M1, M2)
  output_comparison<-anova(M1, M2)
  result_LRT[[sub.location_age]] <-output_comparison[2,"p-value"] # I need to index whatever object I m storing the value 
}

result_LRT[[1]] #print the first object
head(result_LRT)

# convert it into a dataframe:
#head(do.call(rbind, result_LRT))
library(plyr); library(dplyr)
#head(rbind.fill(result_LRT))
#head(rbindlist(result_LRT))
LRT_matrix<-ldply(result_LRT, rbind)#best way
# rename variables in columns:
library(data.table)
setnames(LRT_matrix, old = c('.id','1'), new = c('sub.location_age','pvalue_LRT'))
head(LRT_matrix)

# check how many p values are signif
check<-filter(LRT_matrix,pvalue_LRT<0.05) # 30 out of 52 

# 2) extract slope
result_slope <- vector("list")
for (sub.location_age in unique(length_age12_stack_time_series7$sub.location_age)) {
  my_site_age <- length_age12_stack_time_series7[length_age12_stack_time_series7$sub.location_age == sub.location_age, ]
  M3 <- lme(total_length ~ year+day_of_month, random =~1|year,
  na.action=na.omit, control = lmc, method = "REML", data=my_site_age)
  coef_value<-summary(M3)$coefficients
  result_slope[[sub.location_age]]<-coef_value[["fixed"]][["year"]]
}

result_slope[[1]] #print the first object
head(result_slope)

# convert it into a dataframe:
Slope_matrix<-ldply(result_slope, rbind) 
# rename variables in columns:
setnames(Slope_matrix, old = c('.id','1'), new = c('sub.location_age','slope_year'))
head(Slope_matrix)
# if ERROR, use:
#Slope_matrix<-do.call(rbind, result_slope) # not ideal but I found the way
#slope_year<-as.numeric(Slope_matrix[1:268])
#Site_ID_COORD<-names(result_slope)
#head(Site_ID_COORD)
#table_slopes<-cbind.data.frame(Site_ID_COORD,slope_year)
#head(table_slopes) # halleluja

# 3) extract se of the slopes:
result_SE <- vector("list")
for (sub.location_age in unique(length_age12_stack_time_series7$sub.location_age)) {
  my_site_age <- length_age12_stack_time_series7[length_age12_stack_time_series7$sub.location_age == sub.location_age, ]
  M3 <- lme(total_length ~ year+day_of_month, random =~1|year,
            na.action=na.omit, control = lmc, method = "REML", data=my_site_age)
  result_SE[[sub.location_age]]<-summary(M3)$tTable[2,2] 
}

result_SE[[1]] #print the first object
head(result_SE)

# convert it into a dataframe:
SE_matrix<-ldply(result_SE, rbind) 
# rename variables in columns:
setnames(SE_matrix, old = c('.id','1'), new = c('sub.location_age','SE_slope'))
head(SE_matrix)
# if ERROR, use:
#Slope_matrix<-do.call(rbind, result_slope) # not ideal but I found the way
#slope_year<-as.numeric(Slope_matrix[1:268])
#Site_ID_COORD<-names(result_slope)
#head(Site_ID_COORD)
#table_slopes<-cbind.data.frame(Site_ID_COORD,slope_year)
#head(table_slopes) # halleluja

# sort all as descending by site name:
detach(package:plyr)
table_LRT_pvalues<-LRT_matrix %>% arrange(desc(sub.location_age))
table_coeff<-Slope_matrix %>% arrange(desc(sub.location_age))
table_SE<-SE_matrix %>% arrange(desc(sub.location_age))
head(table_LRT_pvalues)
head(table_coeff)
head(table_SE)

# 4. merge slopes, pvalues, SE 
table_final<- inner_join(table_LRT_pvalues,table_coeff, by = "sub.location_age")
head(table_final)

table_final1<- inner_join(table_final,table_SE, by = "sub.location_age")
head(table_final1)

# add categorical variable for slope with pos/neg/nonsignif levels:
attach(table_final1)
table_final1$trend[pvalue_LRT > 0.05] <- "No trend"
table_final1$trend[pvalue_LRT < 0.05 & slope_year < 0] <- "Decline"
table_final1$trend[pvalue_LRT < 0.05 & slope_year > 0] <- "Increase"
table_final1$all_trends[slope_year < 0] <- "all_declines"
table_final1$all_trends[slope_year > 0] <- "all_increases"
detach(table_final1)

table(table_final1$trend)
table(table_final1$all_trends)

table(length_age12_stack_time_series7$sub.location,length_age12_stack_time_series7$distance)

##### calculate avg stsp and conspecifics over time for each site and age #####

# Extract here the means for each sublocation and age, of "perceived" stsp and conspecifics 
# (avg over whole life span) for individual fish:
avg_time_series<-length_age12_stack_time_series7 %>%
  group_by(sub.location_age, age, sub.location) %>%
  summarise(avg_BIASmean_avg_lifespan = mean(BIASmean_avg_lifespan, na.rm = TRUE),
            # avg_BIASmean = mean(BIASmean, na.rm = TRUE), # wrong, delete eventually
            distance = mean(distance, na.rm = TRUE),
            avg_temp_year_avg_lifespan = mean(temp_year_avg_lifespan, na.rm = TRUE),
            avg_temp_summer_avg_lifespan = mean(temp_summer_avg_lifespan, na.rm = TRUE),
            avg_temp_winter_avg_lifespan = mean(temp_winter_avg_lifespan, na.rm = TRUE),
            avg_temp_exceeding_10_year_avg_lifespan = mean(temp_exceeding_10_year_avg_lifespan, na.rm = TRUE),
            avg_dd_year_sum_lifespan = mean(dd_year_sum_lifespan, na.rm = TRUE),
            avg_dd_year_avg_lifespan = mean(dd_year_avg_lifespan, na.rm = TRUE),
            avg_n_days_exceeding_10_year_avg_lifespan = mean(n_days_exceeding_10_year_avg_lifespan, na.rm = TRUE),
            avg_first_day_exceeding_10_julian_avg_lifespan = mean(first_day_exceeding_10_julian_avg_lifespan, na.rm = TRUE),
            avg_CPUE_Abbo_samesize_avg_lifespan = mean(CPUE_Abbo_samesize_avg_lifespan, na.rm = TRUE),
            # avg_totCPUE_Abborre = mean(totCPUE_Abborre, na.rm = TRUE),
            avg_cyprinids_avg_lifespan = mean(cyprinids_avg_lifespan, na.rm = TRUE))
            #avg_cyprinids = mean(cyprinids, na.rm = TRUE))

# merge:
table_final2<- inner_join(table_final1,avg_time_series, by = "sub.location_age")


# Extract mean stsp desnities in each sublocation during the years covered by the time series dataset, shall I
# choose the same interval fo all (typ 2002-2023), or better tailored for each sublocation, that is, taking
# the first and last year of data available for each sublocation. see range_years_subsets. 

# Step 1: exclude gear k064 and Join the year range info from range_years_subset7 into stsp
stsp_with_years <- stsp %>%
  filter(gear_code == "K064") %>%
  left_join(range_years_subset7 %>% select(sub.location, first_year, last_year), by = c("sub.location","location"))

# Step 2: Filter stsp based on the year range for each sub.location. Sublocation not included in the 
# range_years_subset7 are automatically excluded
filtered_stsp <- stsp_with_years %>%
  filter(year >= first_year & year <= last_year)

# Step 3: Calculate the average for each sub.location
stsp_time_series <- filtered_stsp %>%
  group_by(sub.location) %>%
  summarise(stsp_avg_time_series = mean(BIASmean, na.rm = TRUE))

# merge:
table_final3<- left_join(table_final2,stsp_time_series, by = "sub.location")

# same for conspecifics and other spp:
# Step 1: exclude gear k064 and Join the year range info from range_years_subset7 into stsp
length_age12_stack_with_years <- length_age12_stack %>%
  filter(gear_code == "K064") %>%
  left_join(range_years_subset7 %>% select(sub.location, first_year, last_year), by = c("sub.location","location"))

# Step 2: Filter  based on the year range for each sub.location. Sublocations not included in the 
# range_years_subset7 are automatically excluded
filtered_length_age12_stack <- length_age12_stack_with_years %>%
  filter(year >= first_year & year <= last_year)

# Step 3: Calculate the average for each sub.location
ABBOsamesize_time_series <- filtered_length_age12_stack %>%
  group_by(sub.location) %>%
  summarise(Abbo_samesize_avg_time_series = mean(CPUE_Abbo_samesize, na.rm = TRUE),
            cyprinids_avg_time_series = mean(cyprinids, na.rm = TRUE),
            all_prey_avg_time_series = mean(all_prey, na.rm = TRUE),
            clupeids_avg_time_series = mean(clupeids, na.rm = TRUE),
            year_temp_avg_time_series = mean(avg_temp_year, na.rm = TRUE),
            dd_year_avg_time_series = mean(dd_year, na.rm = TRUE),
            summer_temp_avg_time_series = mean(avg_temp_summer, na.rm = TRUE),
            winter_temp_avg_time_series = mean(avg_temp_winter, na.rm = TRUE),
            year_temp_exceeding_10_avg_time_series = mean(avg_temp_exceeding_10_year, na.rm = TRUE),
            n_days_exceeding_10_year_avg_time_series = mean(n_days_exceeding_10_year, na.rm = TRUE),
            first_day_exceeding_10_julian_avg_time_series = mean(first_day_exceeding_10_julian, na.rm = TRUE))

# merge:
table_final4<- left_join(table_final3,ABBOsamesize_time_series, by = "sub.location")
# and only signif trend:
table_final4_signif<- filter(table_final4, trend != "No trend")

##### exploratory plots #####
# visualize slope by site
ggplot(table_final4, aes(x = reorder(sub.location_age, slope_year), y = slope_year)) +
  geom_bar(stat = "identity", aes(fill = trend)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Site", y = "Slope of length at age") +
  scale_fill_manual(values = c("No trend" = "grey", "Decline" = "red", "Increase" = "blue")) +
  theme(legend.position = "bottom")

# barchart by age:
library(gplots)
avg<-tapply(table_final2$slope_year,list(table_final2$age,table_final2$trend),mean)
sdpl<-tapply(table_final2$slope_year,list(table_final2$age,table_final2$trend),sd)
l<-tapply(table_final2$slope_year,list(table_final2$age,table_final2$trend),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=T,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "slope") 

avg<-tapply(table_final2_signif$slope_year,list(table_final2_signif$age),mean)
sdpl<-tapply(table_final2_signif$slope_year,list(table_final2_signif$age),sd)
l<-tapply(table_final2_signif$slope_year,list(table_final2_signif$age),length)
ci<-sdpl/sqrt(l)
barplot2(avg, beside=T,legend=F,plot.ci=T,ci.l=avg-ci,ci.u=avg+ci, ci.lwd=1,cex.axis=1.5,main = "slope") 

# better plot:
# Calculate means and standard deviations
table_final2_avg_slope <- table_final2 %>%
  group_by(sub.location, trend) %>%
  summarise(
    mean_value = mean(slope_year),
    sd_value = sd(slope_year)
  )
# Create the bar chart
ggplot(table_final2_avg_slope, aes(x = sub.location, y = mean_value, col = trend)) +
  geom_bar(stat = "identity", position = position_dodge(), width = 0.7) +
  geom_errorbar(aes(ymin = mean_value - sd_value, ymax = mean_value + sd_value), 
                width = 0.2, position = position_dodge(0.7)) +
  #facet_wrap(~location)+
  labs(title = "Bar Chart with Means and Standard Deviations",
       x = "sub.location",
       y = "slope (mean)") +
  theme(legend.position="bottom")

# scatterplot of slope vs experienced STSP :
ggplot(table_final2_signif, aes(x = avg_BIASmean_avg_lifespan, y = slope_year, col = sub.location)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="bottom")

ggplot(table_final2_signif, aes(x = avg_BIASmean_avg_lifespan, y = slope_year)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position="bottom")

ggplot(table_final2, aes(x = avg_BIASmean_avg_lifespan, y = slope_year, col = trend)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position="bottom")

# scatterplot of slope vs experienced conspecifics :
ggplot(table_final2_signif, aes(x = avg_CPUE_Abbo_samesize_avg_lifespan, y = slope_year, col = sub.location)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="bottom")

ggplot(table_final2, aes(x = avg_CPUE_Abbo_samesize_avg_lifespan, y = slope_year, col = trend)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position="bottom")

# scatterplot of slope vs experienced cyprinids :
ggplot(table_final2_signif, aes(x = avg_cyprinids_avg_lifespan, y = slope_year, col = sub.location)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="bottom")

ggplot(table_final2, aes(x = avg_cyprinids_avg_lifespan, y = slope_year, col = trend)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position="bottom")

# scatterplot of slope vs ..:
ggplot(table_final5_signif, aes(x = stsp_avg_time_series, y = slope_year, col = sub.location)) +
  geom_point() +
  #geom_smooth(method = "lm", se = FALSE) +
  theme(legend.position="bottom")

ggplot(table_final5, aes(x = stsp_avg_time_series, y = slope_year, col = trend)) +
  geom_point() +
  geom_smooth(method = "loess", se = FALSE) +
  theme(legend.position="bottom")


##### preliminary model with only these covariates: TO REDO ####


# when considering avg of spp and temp over the life span: full model
M1<-lm(slope_year ~ age + avg_BIASmean_avg_lifespan + avg_dd_year_avg_lifespan +
         avg_CPUE_Abbo_samesize_avg_lifespan +
         avg_cyprinids_avg_lifespan+
         distance,
        weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
vif(M1)
summary(M1)
anova(M1)
plot(M1)

# reducing explanatory variables due to low sample size: however I reduce, stsp is the only signif
M1<-lm(slope_year ~ age + avg_BIASmean_avg_lifespan + avg_dd_year_avg_lifespan+
         avg_CPUE_Abbo_samesize_avg_lifespan,
         #avg_cyprinids_avg_lifespan+
         #distance,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
vif(M1)
summary(M1)
anova(M1)
plot(M1)
visreg(M1)

# delete cyprinid and distance. include an interaction: 
M1<-lm(slope_year ~ age * avg_BIASmean_avg_lifespan + 
         avg_dd_year_avg_lifespan +
         avg_CPUE_Abbo_samesize_avg_lifespan,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
summary(M1)
anova(M1)
visreg(M1, xvar = "avg_BIASmean_avg_lifespan", by = "age")

# try different temp variables
colnames(table_final4_signif)
M1<-lm(slope_year ~ age * avg_BIASmean_avg_lifespan + 
         avg_dd_year_avg_lifespan +
         avg_CPUE_Abbo_samesize_avg_lifespan,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
summary(M1)
anova(M1)

# avg_dd_year_avg_lifespan: Radj = 0.63. temp ns - BEST
# avg_temp_year_avg_lifespan: Radj = 0.62. temp ns
# avg_temp_summer_avg_lifespan: Radj = 0.63. temp ns - BEST
# avg_temp_winter_avg_lifespan: Radj = 0.62. temp ns
# avg_temp_exceeding_10_year_avg_lifespan: Radj = 0.62. temp ns
# avg_dd_year_sum_lifespan: Radj = 0.62. temp ns
# avg_n_days_exceeding_10_year_avg_lifespan: Radj = 0.62. temp ns
# avg_first_day_exceeding_10_julian_avg_lifespan: Radj = 0.64. temp ns - BEST

# introducing random factor (and deleting weight): ns
M3a<-gls(slope_year ~ age + avg_BIASmean_avg_lifespan + 
           avg_dd_year_avg_lifespan +
           avg_CPUE_Abbo_samesize_avg_lifespan, method = "REML",
         na.action = na.omit,data=table_final4_signif)
M3b<-lme(slope_year ~ age + avg_BIASmean_avg_lifespan + 
           avg_dd_year_avg_lifespan +
           avg_CPUE_Abbo_samesize_avg_lifespan, random = ~1|sub.location,method = "REML",
         na.action = na.omit,data=table_final4_signif)
anova(M3a,M3b)

### final:
M1<-lm(slope_year ~ age + avg_BIASmean_avg_lifespan + 
         avg_dd_year_avg_lifespan +
         avg_CPUE_Abbo_samesize_avg_lifespan,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
summary(M1)
anova(M1)
# with or without interaction:  r is 0.63 vs 0.49!


# compare to the model with avg current densities of spp as covariates:
# however I reduce, stsp and abbo are the only signif.  CLUPEIDS  COLLINEAR WITH TEMP    
M2<-lm(slope_year ~ age + stsp_avg_time_series + 
         # cyprinids_avg_time_series +
         # clupeids_avg_time_series +
         dd_year_avg_time_series +
         Abbo_samesize_avg_time_series,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
vif(M2)
summary(M2)
anova(M2)
plot(M2)
# with or without interaction:  r is 0.53 vs 0.52. reduce



# try different temp variables
colnames(table_final4_signif)
M2<-lm(slope_year ~ age + stsp_avg_time_series + 
         first_day_exceeding_10_julian_avg_time_series +
         Abbo_samesize_avg_time_series,
       weights = 1/SE_slope, na.action = na.omit,data=table_final4_signif)
summary(M2)
anova(M2)
plot(M2)

# dd_year_avg_time_series: Radj = 0.52. temp ns - 
# year_temp_avg_time_series: Radj = 0.53. temp ns - 
# summer_temp_avg_time_series: Radj = 0.52. temp ns
# winter_temp_avg_time_series: Radj = 0.53. temp ns
# year_temp_exceeding_10_avg_time_series: Radj = 0.49. temp ns
# n_days_exceeding_10_year_avg_time_series: Radj = 0.53. temp ns
# first_day_exceeding_10_julian_avg_time_series: Radj = 0.52. temp ns - 

# quite similar

# introducing random factor (and deleting weight): ns
M3a<-gls(slope_year ~ age + stsp_avg_time_series + 
           first_day_exceeding_10_julian_avg_time_series +
           Abbo_samesize_avg_time_series, method = "REML",
         na.action = na.omit,data=table_final4_signif)
M3b<-lme(slope_year ~ age + stsp_avg_time_series + 
           first_day_exceeding_10_julian_avg_time_series +
           Abbo_samesize_avg_time_series, random = ~1|sub.location,method = "REML",
         na.action = na.omit,data=table_final4_signif)
anova(M3a,M3b)









##### calculate slopes and p values of temporal trends also for stsp and conspecific and temp:####

# check if I need to remove gear other than k064: no need if I use stsp_with_years and filtered_stsp
table(stsp$sub.location,stsp$gear_code)
table(stsp_with_years$sub.location,stsp_with_years$gear_code)
table(filtered_stsp$sub.location,filtered_stsp$gear_code)
# I can use the dataset stsp_with_years if I want the slope over the whole period, while filtered_stsp if I 
# want the to calculate the slope only for those specifc years for which I have time series

### FOR SLOPES OVER THE WHOLE PERIOD (2002-2023): to do

### FOR SLOPES OVER THE YEARS OF EACH TIME SERIE:
# check model for single site
my_site_age1 <- filtered_stsp[filtered_stsp$sub.location == "Asköfjärden", ]
M1<-gls(BIASmean ~ year, correlation=corAR1(form=~year),
    na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site_age1) 
M2<-gls(BIASmean ~ 1, correlation=corAR1(form=~year),
        na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site_age1) 
# determine significance via LRT test (model comparison)
anova(M1, M2)
output_comparison<-anova(M1, M2)
output_comparison[2,"p-value"]
M3 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
          na.action=na.omit,control = list(singular.ok = TRUE),method = "REML", data=my_site_age1) 
summary(M3)
coef_value<-summary(M3)$coefficients
coef_value[["year"]]

# To extract stand error of slope:
summary(M3)$tTable[2,2] 

# calculate increase rate for specific site
# extract fitted values for the first and last year of sampling
F1<-fitted(M3)
F1
(3.1472869-0.3086581 )/0.3086581 *100 # 920% increase from 2005 to 2022  :0
# visual check:
visreg(M3)
plot(M3)

# to avoid lack of convergence I have to remove NA:
filtered_stsp_NA<-na.omit(filtered_stsp) 

# 1)extract LRT test pvalue for all sites:
result_LRT <- vector("list")
for (sub.location in unique(filtered_stsp_NA$sub.location)) {
  my_site <- filtered_stsp_NA[filtered_stsp_NA$sub.location == sub.location, ]
  M1 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site) 
  M2 <- gls(BIASmean ~ 1, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site)
  anova(M1, M2)
  output_comparison<-anova(M1, M2)
  result_LRT[[sub.location]] <-output_comparison[2,"p-value"] # I need to index whatever object I m storing the value 
}

result_LRT[[1]] #print the first object
head(result_LRT)

# convert it into a dataframe:
#head(do.call(rbind, result_LRT))
library(plyr); library(dplyr)
#head(rbind.fill(result_LRT))
#head(rbindlist(result_LRT))
LRT_matrix<-ldply(result_LRT, rbind)#best way
# rename variables in columns:
library(data.table)
setnames(LRT_matrix, old = c('.id','1'), new = c('sub.location','pvalue_LRT_stsp'))
head(LRT_matrix)

# check how many p values are signif
check<-filter(LRT_matrix,pvalue_LRT_stsp<0.05) # 9 out of 11

# 2) extract slope
result_slope <- vector("list")
for (sub.location in unique(filtered_stsp_NA$sub.location)) {
  my_site <- filtered_stsp_NA[filtered_stsp_NA$sub.location == sub.location, ]
  M3 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "REML", data=my_site)
  coef_value<-summary(M3)$coefficients
  result_slope[[sub.location]]<-coef_value[["year"]]
}

result_slope[[1]] #print the first object
head(result_slope)

# convert it into a dataframe:
Slope_matrix<-ldply(result_slope, rbind) 
# rename variables in columns:
setnames(Slope_matrix, old = c('.id','1'), new = c('sub.location','slope_year_stsp'))
head(Slope_matrix)
# if ERROR, use:
#Slope_matrix<-do.call(rbind, result_slope) # not ideal but I found the way
#slope_year<-as.numeric(Slope_matrix[1:268])
#Site_ID_COORD<-names(result_slope)
#head(Site_ID_COORD)
#table_slopes<-cbind.data.frame(Site_ID_COORD,slope_year)
#head(table_slopes) # halleluja

# sort all as descending by site name:
detach(package:plyr)
table_LRT_pvalues<-LRT_matrix %>% arrange(desc(sub.location))
table_coeff<-Slope_matrix %>% arrange(desc(sub.location))
head(table_LRT_pvalues)
head(table_coeff)

# 4. merge slopes, pvalues, SE 
table_final_stsp<- inner_join(table_LRT_pvalues,table_coeff, by = "sub.location")
head(table_final_stsp)


# add categorical variable for slope with pos/neg/nonsignif levels:
attach(table_final_stsp)
table_final_stsp$trend_stsp[pvalue_LRT_stsp > 0.05] <- "No trend stsp"
table_final_stsp$trend_stsp[pvalue_LRT_stsp < 0.05 & slope_year_stsp < 0] <- "Decline stsp"
table_final_stsp$trend_stsp[pvalue_LRT_stsp < 0.05 & slope_year_stsp > 0] <- "Increase stsp"
table_final_stsp$all_trends_stsp[slope_year_stsp < 0] <- "all_declines stsp"
table_final_stsp$all_trends_stsp[slope_year_stsp > 0] <- "all_increases stsp"
detach(table_final_stsp)

table(table_final_stsp$trend_stsp)
table(table_final_stsp$all_trends_stsp)

ggplot(table_final_stsp, aes(x = reorder(sub.location, slope_year_stsp), y = slope_year_stsp)) +
  geom_bar(stat = "identity", aes(fill = trend_stsp)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Site", y = "Slope of stsp") +
  scale_fill_manual(values = c("No trend stsp" = "grey", "Decline stsp" = "red", "Increase stsp" = "blue")) +
  theme(legend.position = "bottom")


### FOR SLOPES OVER THE WHOLE PERIOD (2002-2023): 
# check model for single site
my_site_age1 <- stsp_with_years[stsp_with_years$sub.location == "Asköfjärden", ]
M1<-gls(BIASmean ~ year, correlation=corAR1(form=~year),
        na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site_age1) 
M2<-gls(BIASmean ~ 1, correlation=corAR1(form=~year),
        na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site_age1) 
# determine significance via LRT test (model comparison)
anova(M1, M2)
output_comparison<-anova(M1, M2)
output_comparison[2,"p-value"]
M3 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
          na.action=na.omit,control = list(singular.ok = TRUE),method = "REML", data=my_site_age1) 
summary(M3)
coef_value<-summary(M3)$coefficients
coef_value[["year"]]

# To extract stand error of slope:
summary(M3)$tTable[2,2] 

# calculate increase rate for specific site
# extract fitted values for the first and last year of sampling
F1<-fitted(M3)
F1
(2.7808249-0.2481430 )/0.2481430 *100 # 1021% increase from 2002 to 2023  :0
# visual check:
visreg(M3)
plot(M3)

# to avoid lack of convergence I have to remove NA: (no matters if I have NA in first_year, as I won't need those locations)
stsp_with_years_NA<-na.omit(stsp_with_years) 

# 1)extract LRT test pvalue for all sites:
result_LRT <- vector("list")
for (sub.location in unique(stsp_with_years_NA$sub.location)) {
  my_site <- stsp_with_years_NA[stsp_with_years_NA$sub.location == sub.location, ]
  M1 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site) 
  M2 <- gls(BIASmean ~ 1, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "ML", data=my_site)
  anova(M1, M2)
  output_comparison<-anova(M1, M2)
  result_LRT[[sub.location]] <-output_comparison[2,"p-value"] # I need to index whatever object I m storing the value 
}

result_LRT[[1]] #print the first object
head(result_LRT)

# convert it into a dataframe:
#head(do.call(rbind, result_LRT))
library(plyr); library(dplyr)
#head(rbind.fill(result_LRT))
#head(rbindlist(result_LRT))
LRT_matrix<-ldply(result_LRT, rbind)#best way
# rename variables in columns:
library(data.table)
setnames(LRT_matrix, old = c('.id','1'), new = c('sub.location','pvalue_LRT_stsp_whole_period'))
head(LRT_matrix)

# check how many p values are signif
check<-filter(LRT_matrix,pvalue_LRT_stsp_whole_period<0.05) # 7 out of 11

# 2) extract slope
result_slope <- vector("list")
for (sub.location in unique(stsp_with_years_NA$sub.location)) {
  my_site <- stsp_with_years_NA[stsp_with_years_NA$sub.location == sub.location, ]
  M3 <- gls(BIASmean ~ year, correlation=corAR1(form=~year),
            na.action=na.omit,control = list(singular.ok = TRUE),method = "REML", data=my_site)
  coef_value<-summary(M3)$coefficients
  result_slope[[sub.location]]<-coef_value[["year"]]
}

result_slope[[1]] #print the first object
head(result_slope)

# convert it into a dataframe:
Slope_matrix<-ldply(result_slope, rbind) 
# rename variables in columns:
setnames(Slope_matrix, old = c('.id','1'), new = c('sub.location','slope_year_stsp_whole_period'))
head(Slope_matrix)
# if ERROR, use:
#Slope_matrix<-do.call(rbind, result_slope) # not ideal but I found the way
#slope_year<-as.numeric(Slope_matrix[1:268])
#Site_ID_COORD<-names(result_slope)
#head(Site_ID_COORD)
#table_slopes<-cbind.data.frame(Site_ID_COORD,slope_year)
#head(table_slopes) # halleluja

# sort all as descending by site name:
detach(package:plyr)
table_LRT_pvalues<-LRT_matrix %>% arrange(desc(sub.location))
table_coeff<-Slope_matrix %>% arrange(desc(sub.location))
head(table_LRT_pvalues)
head(table_coeff)

# 4. merge slopes, pvalues, SE 
table_final_stsp_whole_period<- inner_join(table_LRT_pvalues,table_coeff, by = "sub.location")
head(table_final_stsp_whole_period)

# add categorical variable for slope with pos/neg/nonsignif levels:
attach(table_final_stsp_whole_period)
table_final_stsp_whole_period$trend_stsp_whole_period[pvalue_LRT_stsp_whole_period > 0.05] <- "No trend stsp whole period"
table_final_stsp_whole_period$trend_stsp_whole_period[pvalue_LRT_stsp_whole_period < 0.05 & slope_year_stsp_whole_period < 0] <- "Decline stsp whole period"
table_final_stsp_whole_period$trend_stsp_whole_period[pvalue_LRT_stsp_whole_period < 0.05 & slope_year_stsp_whole_period > 0] <- "Increase stsp whole period"
table_final_stsp_whole_period$all_trends_stsp_whole_period[slope_year_stsp_whole_period < 0] <- "all_declines stsp whole period"
table_final_stsp_whole_period$all_trends_stsp_whole_period[slope_year_stsp_whole_period > 0] <- "all_increases stsp whole period"
detach(table_final_stsp_whole_period)

table(table_final_stsp_whole_period$trend_stsp_whole_period)
table(table_final_stsp_whole_period$all_trends_stsp_whole_period)

ggplot(table_final_stsp_whole_period, aes(x = reorder(sub.location, slope_year_stsp_whole_period), y = slope_year_stsp_whole_period)) +
  geom_bar(stat = "identity", aes(fill = trend_stsp_whole_period)) +
  coord_flip() +
  theme_minimal() +
  labs(x = "Site", y = "Slope of stsp whole period") +
  scale_fill_manual(values = c("No trend stsp whole period" = "grey", "Decline stsp whole period" = "red", "Increase stsp whole period" = "blue")) +
  theme(legend.position = "bottom")

# merge
table_final6<- left_join(table_final5,table_final_stsp, by = "sub.location")
table_final7<- left_join(table_final6,table_final_stsp_whole_period, by = "sub.location")

# subset with only significant trends:
table_final7_signif<-table_final7 %>%
  filter(pvalue_LRT < 0.05)

colnames(table_final7)
