rm(list=ls())
setwd("G:/My Drive/FORCE/Data")
setwd("C:/Users/sedi0002/Google Drive/FORCE/Data")


# Libraries ---------------------------------------------------------------

#library(tidyverse)
library(ggplot2)
library(dplyr)
library(tidyr)
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
guess_encoding("detonation-data.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### new detonation dataset (but received before!)
deto2 <- read.csv2("detonation-data.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(deto2)
summary(deto2$year)

# make column with only year
head(deto2$Fiskedatum)
deto2$year<-as.numeric(RIGHT(deto2$Fiskedatum,4))
summary(deto2$year)
hist(deto2$year)

#### cleaning ####
# check read me file, email and "detonation_old_script_for_conversion"

# keep only approved obs
table(deto2$GODKAND)
unique(deto2$GODKAND) # ok, there is no NAs, but anyways:
deto3 = subset(deto2, GODKAND == "JA " | is.na(GODKAND))  

# keep only not disturbed occasions:
table(deto3$Störning)
unique(deto3$Störning)
deto4 = subset(deto3, Störning == "NEJ" | is.na(Störning))  

# exclude Simpevarp and Biotest Forsmark (but not "Forsmark"), as here water is almost 10 degrees warmer
deto4 %>%
  filter(Lokal =="Simpevarp") # ca 740 obs
deto4 %>%
  filter(Lokal =="Biotestsjön, Forsmark") #ca 3300 obs
deto5 = subset(deto4, !(Lokal == "Simpevarp"))
deto6 = subset(deto5, !(Lokal == "Biotestsjön, Forsmark"))

# exclude Gotland? maybe no need, see read me file 

# consider only some spp:
unique(deto6$Artbestämning)
deto7<-deto6 %>%
  filter(Artbestämning == "Storspigg" | Artbestämning == "Abborre" | Artbestämning == "Gädda" | Artbestämning == "Mört")

# more messy than I thought. what's antal vs LANGDGRUPP_ANTAL? doesn't seem to match what previously said, at least when it comes to
# the obs "provfiskad". I will remove it until I know. 

table(deto7$Fångsttyp)
deto8 = subset(deto7, !(Fångsttyp == "Provfiskad"))
head(deto8)

# Antal is the tot number (all size classes) of indiv for a certain spp, cohort and yta/bottom
# LANGDGRUPP_LANGD is the number of indiv for a certain spp, cohort and yta/bottom for a specific size classes
# there are cases where LANGDGRUPP_LANGD is NA and no size classes were taken. 
# there are cases where only a subsample of the indiv were measured, and the tot is listed under Antal (LANGDGRUPP_LANGD = NA is not 
# repreated for the indiv not measured)

unique(deto8$Antal) # non ci sono NA
# quindi potrei usare Antal per calcolare le CPUE, corrette per bottom/yta e per la dinamite.
# poi uso le info sul subsample, quando c'e, per calcolare how many per size class (i.e. trasponendo la percentuale di una certa classe 
# sui totali contati a livello di Antal)

# try: split up the dataset to calculate tot corrected CPUE, while keeping separate the info on size classes:

#####: calculate CPUE by correcting for missing bottom data and different grams of dynamite
# delete info on size classes
detoCPUE<-deto8 %>%
  select(-c(LANGDGRUPP_LANGD ,LANGDGRUPP_ANTAL ,GODKAND,STORNINGAR, Ansträngning_enhet,Störning)) 
# remove duplicates
duplicated(detoCPUE)
detoCPUE1 = detoCPUE[!duplicated(detoCPUE),]
head(detoCPUE1)
# or: same number of rows removed
# detoCPUE2 = distinct(detoCPUE)
which(duplicated(detoCPUE1)) # i should not have duplicates, even if I remove FANG_ID in the "select" above ..boh

# change names of levels that would give me problems once converted into variable names:
unique(detoCPUE1$Fångsttyp)
detoCPUE1$Sortering [detoCPUE1$Sortering =="Juvenil/Adult"] <- "juvad"
detoCPUE1$Sortering [detoCPUE1$Sortering =="Årsyngel"] <- "noll"
detoCPUE1$Artbestämning [detoCPUE1$Artbestämning =="Gädda"] <- "pike"
detoCPUE1$Artbestämning [detoCPUE1$Artbestämning =="Storspigg"] <- "stsp"
detoCPUE1$Artbestämning [detoCPUE1$Artbestämning =="Mört"] <- "roach"
detoCPUE1$Artbestämning [detoCPUE1$Artbestämning =="Abborre"] <- "perch"
detoCPUE1$Fångsttyp [detoCPUE1$Fångsttyp =="Insamling botten"] <- "bott"
detoCPUE1$Fångsttyp [detoCPUE1$Fångsttyp =="Insamling yta"] <- "surf"

# transpose in wide format:
detoCPUE1_wide<-pivot_wider(detoCPUE1, names_from = c(Artbestämning, Sortering, Fångsttyp), values_from = Antal, values_fill = 0)
head(detoCPUE1_wide) 
# this is the same, so go with the above
#####
# warning msg, trying with concatenate first:
#library(crayon)
#detach("package:ggplot2", unload=TRUE)
#detoCPUE1$sp_cohort_FS<-detoCPUE1$Artbestämning %+% "_" %+% detoCPUE1$Sortering %+% "_" %+% detoCPUE1$Fångsttyp
# remove redondant columns:
#detoCPUE2<-detoCPUE1 %>%
#  select(-c(Artbestämning ,Sortering, Fångsttyp))
# transpose in wide format:
#detoCPUE2_wide<-pivot_wider(detoCPUE2, names_from = c(sp_cohort_FS), values_from = c(Antal))
#####
# I had to keep FANG_ID, that uniquely identifies the obs, for pivot_wider to work well (otherwise it would give me a warning and a series of lists)
# however now I have to summarize the dataframe, so to have all CPUEs from a sampling occasion in the same row:
detoCPUE1_wide2<-detoCPUE1_wide %>% 
  group_by(Fiskedatum, Lokal, Lat_grader,Long_grader, Ansträngning, year) %>%
  summarise(pike_juvad_bott=sum(pike_juvad_bott ,na.rm=TRUE),
            stsp_juvad_surf =sum(stsp_juvad_surf  ,na.rm=TRUE),
            stsp_noll_surf =sum(stsp_noll_surf  ,na.rm=TRUE),
            stsp_juvad_bott =sum(stsp_juvad_bott  ,na.rm=TRUE),
            stsp_noll_bott=sum(stsp_noll_bott ,na.rm=TRUE),
            roach_juvad_surf  =sum(roach_juvad_surf   ,na.rm=TRUE),
            roach_juvad_bott  =sum(roach_juvad_bott   ,na.rm=TRUE),
            perch_juvad_bott  =sum(perch_juvad_bott   ,na.rm=TRUE),
            pike_juvad_surf =sum(pike_juvad_surf  ,na.rm=TRUE),
            pike_noll_bott  =sum(pike_noll_bott   ,na.rm=TRUE),
            roach_noll_surf  =sum(roach_noll_surf   ,na.rm=TRUE),
            perch_noll_surf  =sum(perch_noll_surf   ,na.rm=TRUE),
            perch_noll_bott =sum(perch_noll_bott  ,na.rm=TRUE),
            roach_noll_bott   =sum(roach_noll_bott    ,na.rm=TRUE),
            pike_noll_surf   =sum(pike_noll_surf    ,na.rm=TRUE),
            perch_juvad_surf   =sum(perch_juvad_surf    ,na.rm=TRUE),
            roach_NA_surf    =sum(roach_NA_surf     ,na.rm=TRUE),
            stsp_NA_surf    =sum(stsp_NA_surf     ,na.rm=TRUE),
            perch_NA_bott    =sum(perch_NA_bott     ,na.rm=TRUE)
  ) 

head(detoCPUE1_wide2)


# check if obs for which Sortering is NA refer to juv or ad, if length was measured
#####
deto8 %>%
  filter(is.na(Sortering)) # 70 obs, most are stsp. add them to either juvad or noll
deto8 %>%
  filter(is.na(Sortering) & Artbestämning == "Mört") # length not measured. what to do?
deto8 %>%
  filter(is.na(Sortering) & Artbestämning == "Abborre") # length measured. all obs in Uppsala län in 2020

ggplot(subset(deto8, Artbestämning %in% "Abborre"), aes(x=LANGDGRUPP_LANGD, y=LANGDGRUPP_ANTAL)) +
  geom_bar(stat="identity")+
  facet_wrap(~Sortering)+
  theme_bw(base_size=15)
# hard to say. Let's check if other data exist in that location and year
ggplot(subset(deto8, Lokal %in% "Uppsala län" & year %in% 2020 & Artbestämning %in% "Abborre"), aes(x=LANGDGRUPP_LANGD, y=LANGDGRUPP_ANTAL)) +
  geom_bar(stat="identity")+
  facet_wrap(~Sortering)+
  theme_bw(base_size=15)
# no juvad register that year in that place. the range of values is well between the range for årsyngel, so let't put them in the noll group

ggplot(subset(deto8, Lokal %in% "Stockholms län" & Artbestämning %in% "Mört"), aes(x=LANGDGRUPP_LANGD, y=LANGDGRUPP_ANTAL)) +
  geom_bar(stat="identity")+
  facet_wrap(~Sortering)+
  theme_bw(base_size=15)
# no juvad register in that place in any year. put them with the årsyngel

#####
# incorporate obs for which Sortering is NA to the other variables, and calculate the sum of juvad and noll for stsp at the surface and bottom:
detoCPUE1_wide2$perch_noll_bott2<-detoCPUE1_wide2$perch_noll_bott + detoCPUE1_wide2$perch_NA_bott
detoCPUE1_wide2$roach_noll_surf2<-detoCPUE1_wide2$roach_noll_surf + detoCPUE1_wide2$roach_NA_surf
detoCPUE1_wide2$stsp_all_surf<-detoCPUE1_wide2$stsp_juvad_surf + detoCPUE1_wide2$stsp_NA_surf + detoCPUE1_wide2$stsp_noll_surf
detoCPUE1_wide2$stsp_all_bott<-detoCPUE1_wide2$stsp_juvad_bott + detoCPUE1_wide2$stsp_noll_bott

# check:
summary(detoCPUE1_wide2$roach_noll_surf2)
summary(detoCPUE1_wide2$roach_noll_surf)
summary(detoCPUE1_wide2$perch_noll_bott2)
summary(detoCPUE1_wide2$perch_noll_bott)

# delete columns to avoid using them by mistake:
detoCPUE1_wide3<-detoCPUE1_wide2 %>%
  select(-c(perch_NA_bott ,perch_noll_bott ,roach_NA_surf, roach_noll_surf, stsp_juvad_surf,stsp_NA_surf,stsp_noll_surf,stsp_juvad_bott,stsp_noll_bott)) 

# add for each spp the tot number yta + bottom:
detoCPUE1_wide3$pike_juvad_tot<-detoCPUE1_wide3$pike_juvad_bott+detoCPUE1_wide3$pike_juvad_surf
detoCPUE1_wide3$perch_juvad_tot<-detoCPUE1_wide3$perch_juvad_bott+detoCPUE1_wide3$perch_juvad_surf
detoCPUE1_wide3$roach_juvad_tot<-detoCPUE1_wide3$roach_juvad_bott+detoCPUE1_wide3$roach_juvad_surf

detoCPUE1_wide3$pike_noll_tot<-detoCPUE1_wide3$pike_noll_bott+detoCPUE1_wide3$pike_noll_surf
detoCPUE1_wide3$perch_noll_tot<-detoCPUE1_wide3$perch_noll_bott2+detoCPUE1_wide3$perch_noll_surf
detoCPUE1_wide3$roach_noll_tot<-detoCPUE1_wide3$roach_noll_bott+detoCPUE1_wide3$roach_noll_surf2

detoCPUE1_wide3$stsp_all_tot<-detoCPUE1_wide3$stsp_all_bott+detoCPUE1_wide3$stsp_all_surf

# from Agnes file: check if stsp have been counted in the bottom in Kalmar: not before 2007, that is 2003 and 2006. 
# Weird that in 2005 no stsp was observed either at the bottom or at the surface. To be on the safe side I would also put NA
# for stsp in 2005 at the bottom but also at the surface - check with Agnes
detoCPUE1 %>%
  filter(Lokal == "Kalmar län" & Fångsttyp == "bott")
ggplot(subset(detoCPUE1, Lokal %in% "Kalmar län" & Artbestämning %in% "stsp"), aes(x=Fångsttyp, y=Antal)) +
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme_bw(base_size=15)
# set, in the wide-layout format, stickleback at the bottom and tot in Kalmar län before 2007, and stsp at the surf in Kalmar län in 2005, to NA
detoCPUE1_wide3[detoCPUE1_wide3$Lokal=="Kalmar län" & detoCPUE1_wide3$year < 2007,]
detoCPUE1_wide3[detoCPUE1_wide3$Lokal=="Kalmar län" & detoCPUE1_wide3$year < 2007, which(names(detoCPUE1_wide3) == "stsp_all_bott") ] = NA
detoCPUE1_wide3[detoCPUE1_wide3$Lokal=="Kalmar län" & detoCPUE1_wide3$year < 2007, which(names(detoCPUE1_wide3) == "stsp_all_tot") ] = NA
detoCPUE1_wide3[detoCPUE1_wide3$Lokal=="Kalmar län" & detoCPUE1_wide3$year == 2005, which(names(detoCPUE1_wide3) == "stsp_all_surf") ] = NA
check<-subset(detoCPUE1_wide3, Lokal %in% "Kalmar län" & year %in% c(2003:2006))

# check if I have repeated data in 2019: all good
check = detoCPUE1_wide3[detoCPUE1_wide3$year == 2019, ]


#### calculate conversion factors: ####
# now I need to understand where they did not measure at the bottom (false zeros), where they did but did not record it, and when they did 
# but did not find any (true zeros) - this latter not likely, given that the previous conversion factors are 0.2-0.7, but it could be if I 
# have very low numbers

# they should always (when grams were 10gr) have looked at both bottom and surface (except for Forsmark and Simpevarp), 
# If there is not bottom value is zeros but they did not (always) record zeros.But this applies to the lst 15-20 years
# the zeros are added automaticcaly when switching to the wide format. What I have to do is just don't
# correct for bottom/surface, but apply correction for dynamite (it may be 1 though)
detoCPUE1%>%
  filter(Ansträngning  == 10 & Fångsttyp == "bott" & Antal == 0)

# how to select for sampling occasions where they counted only at the surface or both:
# start by subsetting the data by location and plot for different years Antal at the bottom vs surface. double check with the table
# check groups of location, year, Fångsttyp. and when both surf and bott were counted, check Ansträngning (sometimes different methods in the same year):
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Lokal)
table(detoCPUE1$Ansträngning,detoCPUE1$year,detoCPUE1$Lokal)
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Ansträngning,detoCPUE1$Lokal)

ggplot(subset(detoCPUE1, Lokal %in% "Västernorrlands län" & year %in% "2014"), aes(x=Fångsttyp, y=Antal)) +
  geom_bar(stat="identity")+
  facet_wrap(~Ansträngning)+
  theme_bw(base_size=15)
# OBS: If Ansträngning = 10 and it happened in the last 15-20 years do not correct nor use for calculation of the conversion factor

# Summary:
## Blekinge län: both always.use for calculation of the conversion factor (and do not convert)
## Forsmark: only surf. But it's recent years and Ansträngning = 10. do not convert nor use for calculation of the conversion factor
## Gävleborgs län: exclude <=2004, and 2005 but only when Ansträngning was 1, and 2006-2009
#     1979 1983 2001 2002 2003 2004 2005 2006 2007 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020
#bott    0    0    0    0    0    0   15    0    0    0    0   80   76   39   57  100    0    0    0    0    0    0
#surf    8    6    0   74   35   39  114   64   46   38   95   78   96  108  114  111    0    0    0    0    0    0
## Gävlebukten: both always, one value of Ansträngning
## ICES 29:4967 Åbo: both always, one value of Ansträngning
## Kalmar län: exclude 2003 and 2006. The others have both for all type of Ansträngning (2005, 2007-2010, 2014)
## Norrbottens län: both always for all type of Ansträngning
## Östergötlands län: exclude 2003 (only surf). 2012, 2014, 2020 both for all type of Ansträngning
## Södermanlands län: exclude 2004-2005, 2007-2008 (only surface). exclude 2006 with Ansträngning=1. Inlcude 2006 with Ansträngning=8, 2012, 2014 both
## Stockholms län: exclude 2001-2003 (only surface).In 2004-2020 both for all type of Ansträngning
## Uppsala län: exclude 2002-2003 (only surf), and [2008-2009, 2011-2013] with Ansträngning=10 (also, do not convert these as they
# counted at the bottom but did not register it probably!). 
## Västerbottens län: both always  for all type of Ansträngning
## Västernorrlands län: exclude 2010 (Only surf). 2008-2009, 2011, 2014 both for all type of Ansträngning

# select subset in the wide-layout dataset by excluding sampling occasions where only surf were counted
# then convert these that were excluded, except for those with Ansträngning = 10 of the last 15-20 years
detoCPUE1_wide3 %>%
  filter(Lokal  == "Västernorrlands län" & year == 2008)

sub1<-subset(detoCPUE1_wide3, !(Lokal == "Forsmark"))
sub2<-subset(sub1, !(Lokal == "Gävleborgs län" & year < 2005))
sub3<-subset(sub2, !(Lokal == "Gävleborgs län" & year == 2005 & Ansträngning == 1))
sub4<-subset(sub3, !(Lokal %in% "Gävleborgs län" & year %in% 2006:2009))
sub5<-subset(sub4, !(Lokal %in% "Kalmar län" & year %in% c(2003, 2006)))
sub6<-subset(sub5, !(Lokal %in% "Östergötlands län" & year %in% 2003))
sub7<-subset(sub6, !(Lokal %in% "Södermanlands län" & year %in% c(2004:2005, 2007:2008)))
sub8<-subset(sub7, !(Lokal %in% "Södermanlands län" & year %in% 2006 & Ansträngning %in% 1))
sub9<-subset(sub8, !(Lokal %in% "Stockholms län" & year %in% c(2001:2003)))
sub10<-subset(sub9, !(Lokal %in% "Uppsala län" & year %in% c(2002:2003)))
sub11<-subset(sub10, !(Lokal %in% "Uppsala län" & year %in% c(2008:2009, 2011:2013) & Ansträngning %in% 10))
sub12<-subset(sub11, !(Lokal %in% "Västernorrlands län" & year %in% 2010))
# check: Ok
sub6 %>%
  filter(Lokal  == "Södermanlands län" & year == 2007 ) 
# sub12 is the subset where both surf and bott were counted

# make sure all fish abundances are numeric - not working, 
head(sub12)
for( i in which(names(sub12) == "pike_juvad_bott"):which(names(sub12) == "stsp_all_tot")){
  sub12[,i] = as.numeric(sub12[,i])} 

# for 0+ abborre
ratio_abbo = sub12$perch_noll_surf/
  sub12$perch_noll_tot
sum(!is.nan(ratio_abbo) & !is.infinite(ratio_abbo)) # 2089 non defined or infinite values out of 5664
FS_abbo = round(mean(ratio_abbo[!is.nan(ratio_abbo) & !is.infinite(ratio_abbo)], na.rm = T), digits = 2) 
# 0.45 vs 0.42 in Agnes old data (0.39 in file)

# for 0+ gadda
ratio_gadd = sub12$pike_noll_surf/
  sub12$pike_noll_tot
sum(!is.nan(ratio_gadd) & !is.infinite(ratio_gadd)) # 1391 non defined or infinite values out of 5664
FS_gadd = round(mean(ratio_gadd[!is.nan(ratio_gadd) & !is.infinite(ratio_gadd)], na.rm = T), digits = 2)  
# 0.68 vs 0.64 in Agnes old data (0.64 in file)

# for 0+ roach
ratio_roach = sub12$roach_noll_surf2/
  sub12$roach_noll_tot
sum(!is.nan(ratio_roach) & !is.infinite(ratio_roach)) # 1074 non defined or infinite values out of 5664
FS_roach = round(mean(ratio_roach[!is.nan(ratio_roach) & !is.infinite(ratio_roach)], na.rm = T), digits = 2)  
# 0.84

# for ALL (both juv and noll) spigg
ratio_stsp = sub12$stsp_all_surf/
  sub12$stsp_all_bott
sum(!is.nan(ratio_stsp) & !is.infinite(ratio_stsp))# 2105 non defined or infinite values out of 5664
FS_stsp = round(mean(ratio_stsp[!is.nan(ratio_stsp) & !is.infinite(ratio_stsp)], na.rm = T), digits = 2) 
# 1.56 vs 0.17 in Agnes old data (0.19 in file) 
### OOOBBBBSSS. Likely they did not count stsp at the bottom in many occasions. Find out otherwise I'll have wrong estimates

unique(detoCPUE1$Lokal)
# kalmar: corrected
# Västerbottens län: both surf and bott
# Stockholms län: stsp not counted at the bott in 2001, 2003, but I did exclude it in sub 12. BUT in 2016-2018 no stsp???
# Gävleborgs län: no stsp at the bott before 2010 except 2005 - check that all these are excluded from sub12
# Uppsala län ... 
# ... 
## after talking to Agnes: try to understand which obs included stsp counts in both surface and bottom
## she will send me etsimates of stsp from her model, for the detonation dataset amnd fot the gillnets dataset, we can compare the two
# OBS: Agnes stsp estimates will only cover 2001 onwards!


ggplot(subset(detoCPUE1, Lokal %in% "Stockholms län" & Artbestämning %in% "stsp"), aes(x=Fångsttyp, y=Antal)) +
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme_bw(base_size=15)



# standardize by different gram of dynamite

# check read me file, email and "detonation_old_script_for_conversion"

# check how many samples for each size classes and location and year

