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

# however, in some cases, even though I have the location in both datasets, the year is not the same, so I get no
# covariates for the length at age data analysis

length_age10 %>%
  filter(location == "Aspöja")
gillnets_pool %>%
  filter(location == "Aspöja")

#####
# merge and keep all records in left dataset, and only matching record in right dataset
length_age11<-left_join(length_age10, gillnets_pool_lag, by = c("year","location")) 
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
length_age12_age5<-length_age12 %>% 
  filter(age == 5)
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
head(length_age12_age2)

# covariates: date as covariate. maybe transform to ordinal number. Conspecific densities as total or split by
# size classes (pooled somehow).  Temp variables: now I have only avg_year_temp, calculate and import others.
# stsp densities: to come. Densities of other food, split by spp (mört and Löja) or pooled. Ddnsities of competitors
# consider lags: for avg year temp, competitors, conspecific, preys, stsp. just 1 year?

# OBS: as a second step, consider effects of temp (possibly a different variables) on predictors, maybe SEM

# OBS: since I have multiple values per location and year, I can't model a temporal correlation structure
# more complex than a symmetrical one unless I pool the values, i.e. take the mean length per location and 
# year. I can try and compare both approach, i.e. (1) all values retained with a simple corr str, and (2) the 
# means with more complex corr str

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
          catch_date, # account for extra growth in august until catch
        data = length_age12_age2)
vif(M0)

# beyond optimal model: M0

# test for temporal corr str: 
# random=~1|location,correlation=corAR1(form=~year). Use script in stat in gillnets for differnt str
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
          catch_date, # account for extra growth in august until catch
        method="REML",na.action=na.omit, data=length_age12_age2)

M1<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M2<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        random=~1|location,correlation=corCompSymm(form=~year),method="REML",na.action=na.omit, data=length_age12_age2)
AIC(M0,M1,M2)
# best is M1

# check variance str:
M1<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M3<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varFixed(~ avg_year_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M4<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varFixed(~ totCPUE_Abborre),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
#M5<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
#        weights=varFixed(~ competitors),
#        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M6<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varFixed(~ all_prey),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M7<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        weights=varFixed(~ field_temp),
        random=~1|location,method="REML",na.action=na.omit, data=length_age12_age2)
M8<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
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
# final
M3<-lme(total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + field_temp + catch_date,
        random=~1|location,weights=varFixed(~ avg_year_temp), 
        method="REML",na.action=na.omit, data=length_age12_age2)

anova.lme(M3, type = "marginal", adjustSigma = F) 
rsquared(M3)
summary(M3)
plot(M3)

# I can't run the usual script for figure, fix it
library(ggeffects)
predict_response(M3, terms = "avg_year_temp")

ggpredict(M3)
pred <- ggmean(M3, "totCPUE_Abborre")
plot(pred)

levels(length_age12_age2$totCPUE_Abborre) # what???


###### approach 2: means and more complex corr str####

# pool values per location and year and calculate the mean
# bring along also the number of samples per location and year to use as weight
# n_=n_distinct()

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
            avg_field_temp = mean(field_temp, na.rm = TRUE),
            avg_catch_date = mean(catch_date, na.rm = TRUE),
            n_samples = n()) 

summary(length_age12_age2_pooled)

#####
# show me where I have NAs
length_age12_age2_pooled %>%
  filter(is.na(totCPUE_Abborre) | is.na(competitors) | 
           is.na(all_prey) | is.na(avg_field_temp) | is.na(avg_catch_date)) %>%
  print(n=19)

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

##### if I want to remove locations that were sampled only 1 or 2 years: ####
# run later if needed
table (length_age12_age2$location, length_age12_age2$year)

length_age12_age2a<-length_age12_age2 %>%
  group_by(location) %>%
  mutate(unique_years = n_distinct(year)) %>%
  ungroup()

table (length_age12_age2a$unique_years)

length_age12_age2a<-length_age12_age2a %>%
  filter(unique_years > 2)
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
          avg_field_temp + # account for different catchability of gillnets with temp (if not collinear, otherwise test on residuals)
          avg_catch_date, # account for extra growth in august until catch
        data = length_age12_age2_pooled)
vif(M0)

# try weighted regression on n samples - beyond optimal model
M1<-lm(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
       weights= n_samples,data=length_age12_age2_pooled)
# compare:
summary(M0)
summary(M1)
# similar results. However, if I use weight, not sure how to incorporate corr str

# without weight:

# useful random str for later. remove samples if it doesn't run (see above)
M2<-gls(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        method="REML",na.action=na.omit,,data=length_age12_age2_pooled)
M3<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corExp(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M4<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M6<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corGaus(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M7<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corSpher(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
AIC(M2,M3,M4,M5,M6,M7)
# best M3 and M4 and M5

# variance str:
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M8<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        weights=varFixed(~ avg_year_temp),
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M9<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        weights=varFixed(~ totCPUE_Abborre),
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M10<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
         weights=varFixed(~ competitors),
         random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M11<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
         weights=varFixed(~ all_prey),
         random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
M12<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
         weights=varFixed(~ avg_field_temp),
         random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
AIC(M5,M8,M9,M11,M12)

# final
M5<-lme(avg_total_length~avg_year_temp+totCPUE_Abborre + competitors + all_prey + avg_field_temp + avg_catch_date,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=length_age12_age2_pooled)
anova.lme(M5, type = "marginal", adjustSigma = F) 
rsquared(M5)
summary(M5)
plot(M5)

