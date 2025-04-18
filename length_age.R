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

library(ExcelFunctionsR)
#library(plyr)


#####
# Read Datasets
#####

# read the whole dataset with Swedish cHaracters 
# if ANSI doesn't work, try: encoding = "UTF-8", or encoding ="ISO-8859-1", or "latin1"

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("perch-length-age.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) length age dataset
length_age <- read.csv2("perch-length-age.csv",fileEncoding ="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

head(length_age)

# make column with only month
head(length_age$catch_date)
length_age$month<-as.numeric(LEFT(RIGHT(length_age$catch_date,7),2))
summary(length_age$month)
hist(length_age$month)

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

#####
# check frozen samples: revise after Noora answers - skip
#####
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

# set the format for date:
length_age10$catch_date<-as.Date(length_age10$catch_date, "%d/%m/%y")

head(length_age10)
table(length_age10$location,length_age10$sub.location)
Forsmark<-length_age10 %>%
  filter(location =="Forsmark")
table(Forsmark$sub.location)
Forsmark %>%
  filter(sub.location =="Väst Biotestsjön")

# extract datasets for Ingrid and for Agnes
#####
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


#####
# merge now as soon as I get the data from Ingrid, before next steps


# check differences in F vs M:
#####
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
# merge with gillnets data with temp and CPUE of spp
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
#####
# merge and keep all records in left dataset, and only matching record in right dataset
length_age11<-left_join(length_age10, gillnets_pool, by = c("year","location")) 
head(length_age11)

# remove unnecessary columns:
length_age12<-length_age11 %>%
  select(-c(species,program,survey, gear.code, gear, ID, aging_method,somatic_weight_type,sex, comments,approved,
            sampling_method))

summary(length_age12)

length_age12_age2<-length_age12 %>% 
  filter(age == 2)
length_age12_age3<-length_age12 %>% 
  filter(age == 3)
length_age12_age4<-length_age12 %>% 
  filter(age == 4)
length_age12_age2to4<-length_age12 %>% 
  filter(age < 5 & age >1)

unique(sort(length_age12$location))
table(length_age12$gear_code)

#####
# exploration plots
#####
# length at age vs temp:
ggplot(subset(length_age12, age %in% "2"), aes(x = avg_year_temp , y = total_length)) +
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
ggplot(length_age7, aes(x = avg_year_temp , y = lat)) +
  geom_point()

# length at age vs year
ggplot(length_age7_age2to4, aes(x = year , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~age)+
  labs(title="")+
  theme_classic(base_size=13)

# length at age vs locations and year
ggplot(length_age12_age4, aes(x = year , y = total_length)) +
  geom_point()+
  geom_smooth(method = "lm")+ 
  facet_wrap(~location)+
  labs(title="age 4")+
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
