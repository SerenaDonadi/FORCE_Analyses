rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------

library(tidyverse)
library(ggplot2)
#library(dplyr)
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
# however, if I use the following script, it will remove NEJ but also NAs (hence ingen fångst)
#gillnets3<-gillnets2[!gillnets2$GODKAND=="NEJ",]
#unique(gillnets3$GODKAND) # not here
#table(gillnets2$Art,gillnets2$GODKAND)
#table(gillnets3$Art,gillnets3$GODKAND) # but here
# so I trY
gillnets3a<-subset(gillnets2, GODKAND=="JA "| is.na(GODKAND)) # | is or
unique(gillnets3a$GODKAND) 
table(gillnets3a$Art,gillnets3a$GODKAND) # ok!

## TO DO: maybe remove some values of Redskapsdetaljnummer, I didn't get which ones and why they are there

# remove fish < 12 cm, which are poorly sampled by gillnets:
gillnets3<-gillnets3a %>% 
  filter(LANGDGRUPP_LANGD >= 12 | is.na(LANGDGRUPP_LANGD)) # to keep also values "ingen fångst"

# keep only Störning  "NEJ"?
unique(gillnets3$Störning) # there are no NAs
table(gillnets3$Störning,gillnets3$month)
gillnets4<-gillnets3 %>% 
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

summary(gillnets4$Ansträngning)
summary(gillnets5$Ansträngning)
summary(gillnets6$Ansträngning) # why? I have here >100 records, with NAs for all variables. but 2 rows less than before
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
#library(plyr)
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
gillnets7$length_group<-round_any(gillnets7$LANGDGRUPP_LANGD, 1, ceiling) # I have to subtract 0.5 to obtain original values

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

library(datawizard)
##library(dplyr)

# calulate mean, median and L90 and skewenss, kurtosis for Abborre, for each location and year:
gillnets_length_indexes<- gillnets_indiv %>%
  filter(Art=="Abborre") %>%
  group_by(location, year) %>%
  summarise(mean_length=mean(length_group ,na.rm=TRUE),
            median_length=median(length_group ,na.rm=TRUE),
            L90=quantile(length_group ,0.90,na.rm=TRUE),
            sk1=skewness(length_group ,remove_na = TRUE, type = "1", iterations = 100),
            sk2=skewness(length_group ,remove_na = TRUE, type = "2", iterations = 100),
            ku1=kurtosis(length_group ,remove_na = TRUE, type = "1", iterations = 100),
            ku2=skewness(length_group ,remove_na = TRUE, type = "2", iterations = 100)
            ) 

head(gillnets_length_indexes)

# From R help(skeweness) in R: there are three different methods for estimating skewness, as discussed in Joanes and Gill (1988):
# Type "1" is the "classical" method, which is g1 = (sum((x - mean(x))^3) / n) / (sum((x - mean(x))^2) / n)^1.5
# Type "2" first calculates the type-1 skewness, then adjusts the result: G1 = g1 * sqrt(n * (n - 1)) / (n - 2). This is what SAS and SPSS usually return.
# Type "3" first calculates the type-1 skewness, then adjusts the result: b1 = g1 * ((1 - 1 / n))^1.5. This is what Minitab usually returns.apparently there are severalk ways to calculate skeweness based on the type of distribution of my data. check:
# same for kurtosis. I calculate type 1 and 2. They are very similar for skewness, but larger differences for kurtosis


### calculate CPUE per size categories, using dataset with ingen fångst
# first, calculate sum of indiv per size category
gillnets_freq<-gillnets7 %>% 
  group_by(location, year, Art, length_group) %>%
  summarise(number_indiv=sum(LANGDGRUPP_ANTAL ,na.rm=TRUE)
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

# third, calculate CPUE
gillnets_CPUE$CPUE<-gillnets_CPUE$number_indiv/gillnets_CPUE$number_nets
hist(gillnets_CPUE$CPUE)

head(gillnets_CPUE)


### calculate tot CPUE (pooled across size categories). Bring along number indiv and number of nets and avg temp
gillnets_totCPUE<-gillnets_CPUE %>% 
  group_by(location, year, Art) %>%
  summarise(totCPUE=sum(CPUE ,na.rm=TRUE),
            tot_number=sum(number_indiv ,na.rm=TRUE),
            number_nets=mean(number_nets ,na.rm=TRUE),
            avg_year_temp=mean(avg_year_temp ,na.rm=TRUE)
  ) 

summary(gillnets_totCPUE)

# SUMMARY of key datasets
# gillnets_CPUE: replicated at level of location, year, size categories (and spp) - useful for plotting
# gillnets_totCPUE: replicated at level of location, year (and spp)
# gillnets_length_indexes: replicated at level of location, year but only for Abborre - useful for stat. 

# merge/combine as you like!
# combine gillnets_length_indexes with totCPUE, and put totCPUE of different spp in columns:

# spread rows into columns to obtain CPUE of spp as predictors and retain zeros
gillnets_totCPUE_wide<-pivot_wider(gillnets_totCPUE, names_from = Art, values_from = c(totCPUE, tot_number))
gillnets_totCPUE_wide[is.na(gillnets_totCPUE_wide)] <- 0 # wait, replace with 0 only for spp, not temp!
gillnets_totCPUE_wide$avg_year_temp[gillnets_totCPUE_wide$avg_year_temp==0] <- NA

head(gillnets_totCPUE_wide)
summary(gillnets_totCPUE_wide)

# check how many location*year had zero abborre: ok, 4 cases
gillnets_totCPUE_wide %>%
  filter(totCPUE_Abborre == 0)

# select spp to use as predictors: Stensimpa and småpigg are not included as they are all below 12 cm
# run this if it can't find function" select"(caused by name clash in package MASS):
#find("select")
#select <- dplyr::select
gillnets_totCPUE_wide_select<-gillnets_totCPUE_wide %>%
  select(c(location,year,avg_year_temp, number_nets,tot_number_Abborre, totCPUE_Abborre,totCPUE_Mört,totCPUE_Gädda,totCPUE_Storspigg,
           totCPUE_Rötsimpa, totCPUE_Bergsimpa)) 

# merge gillnets_length_indexes table with CPUE of spp:
gillnets_pool<-left_join(gillnets_length_indexes, gillnets_totCPUE_wide_select, by = c("location","year")) # 

# ready for analyses! 
#OBS: check how many fish are used to calculate the indexes and compare with Örjan guidelines
# also consider lag? In that case check function "lag" in dplyr

# SUMMARY of key datasets
# gillnets_CPUE: include CPUE separated for size categories. Replicated at level of location, year, size categories (and spp) - useful for plotting
# gillnets_pool: include length indexes for Abborre and tot CPUE of Abborre and few other spp. Replicated at level of location, year - useful for stat. 
# (gillnets_totCPUE: include tot CPUE. Replicated at level of location, year (and spp))


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

#####
# stats on length indexes
#####

# check spatial and temporal replication:
summary(gillnets_pool)
table(gillnets_pool$location,gillnets_pool$year)
table(gillnets_pool$year,gillnets_pool$location)
count(gillnets_pool, 'location') 

# add n = n years of sampling for each location
gillnets_pool_time<-gillnets_pool %>%
  add_count(location)
gillnets_pool_time$n
hist(gillnets_pool_time$n)
# cut off..?

table(gillnets_pool_time$year)
# 2022 has 24 locations

# make one (or two) subset for time series analyses and one with only spatial replication
gillnets_pool # all replicates: 375
gillnets_pool_time1<-filter(gillnets_pool_time, n>10) # only time series with more than 10 years sampling: 275
gillnets_pool_time2<-filter(gillnets_pool_time, n>2) # all replicates except location with less than 2 sampling years: 343
gillnets_pool_space2021<-filter(gillnets_pool_space, year==2021) # only the year with most sampled locations, 2022: 27

# PS: consider spatial corr based on lat and long, but for tha I need to bring/average them from the original dataset

# responseS: mean_length, median_length, L90, sk1$Skewness, sk2$Skewness, ku1$Kurtosis, ku2$Skewness
# drivers: avg_year_temp, totCPUE_Abborre, totCPUE_Mört, totCPUE_Gädda, totCPUE_Storspigg, totCPUE_Rötsimpa, totCPUE_Bergsimpa
# random: location, year

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
hist(gillnets_pool$mean_length)
hist(gillnets_pool$avg_year_temp)
hist(gillnets_pool$totCPUE_Abborre)

# collinearity
plot(gillnets_pool$avg_year_temp,gillnets_pool$totCPUE_Abborre)
plot(gillnets_pool$avg_year_temp,gillnets_pool$totCPUE_Mört)
plot(gillnets_pool$totCPUE_Abborre,gillnets_pool$totCPUE_Mört)

# time series analyses

# inlcude correlation STR using long time series (gillnets_pool1)
M0<-gls(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        method="REML",na.action=na.omit, data=gillnets_pool1)
vif(M0)
M1<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,method="REML",na.action=na.omit, data=gillnets_pool1)
M2<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corCompSymm(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M3<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corExp(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M4<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M5<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corLin(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M6<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corGaus(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M7<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corSpher(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
AIC(M0,M1,M2,M3,M4,M5,M6,M7)
# best M3 and M4

# check variance str:
M4<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),method="REML",na.action=na.omit, data=gillnets_pool1)
M8<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool1)
M9<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ totCPUE_Abborre),
        method="REML",na.action=na.omit, data=gillnets_pool1)
M10<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ totCPUE_Mört),
        method="REML",na.action=na.omit, data=gillnets_pool1)
M11<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool1)
M12<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ totCPUE_Abborre),
        method="REML",na.action=na.omit, data=gillnets_pool1)
M13<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varPower(~ totCPUE_Mört),
         method="REML",na.action=na.omit, data=gillnets_pool1)
M14<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ avg_year_temp),
         method="REML",na.action=na.omit, data=gillnets_pool1)
M15<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ totCPUE_Abborre),
         method="REML",na.action=na.omit, data=gillnets_pool1)
M16<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varConstPower(~ totCPUE_Mört),
         method="REML",na.action=na.omit, data=gillnets_pool1)
M17<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
         random=~1|location,correlation=corAR1(form=~year),weights=varIdent(form =~ 1|location),
         method="REML",na.action=na.omit, data=gillnets_pool1)
AIC(M4,M8,M9)
# best M8

# final
M8<-lme(mean_length~avg_year_temp+totCPUE_Abborre+totCPUE_Mört,
        random=~1|location,correlation=corAR1(form=~year),weights=varFixed(~ avg_year_temp),
        method="REML",na.action=na.omit, data=gillnets_pool1)
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
        method="REML",na.action=na.omit, data=gillnets_pool1)
vif(M8a)
anova.lme(M8a, type = "marginal", adjustSigma = F) 
rsquared(M8a)
summary(M8a)
