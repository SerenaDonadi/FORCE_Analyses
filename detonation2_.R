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

#####
# cleaning deto2: check read me file, email and "detonation_old_script_for_conversion"

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



# now I need to understand where they did not measure at the bottom (false zeros), where they did but did not record it, and when they did 
# but did not find any (true zeros) - this latter not likely, given that the previous correction factors are 0.2-0.7, but it could be if I 
# have very low numbers

# they should always (when grams were 10gr) have looked at both bottom and surface (except for Forsmark and Simpevarp), 
# If there is not bottom value is zeros but they did not record zeros.But this applies to the lst 15-20 years

head(detoCPUE1_wide3)
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Ansträngning)
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Lokal)



# harmonize bottom/yta estimates
head(deto6)
table(deto6$Fångsttyp)
unique(deto6$Fångsttyp)

table(deto6$Ansträngning,deto6$year)
table(deto6$Fångsttyp,deto6$year)
table(deto6$Ansträngning,deto6$year,deto6$Fångsttyp)


deto6_Ansträngning1<- deto6 %>%
  filter(Ansträngning == "1") 
table(deto6_Ansträngning1$Lokal,deto6_Ansträngning1$year)
table(deto6_Ansträngning1$Fångsttyp,deto6_Ansträngning1$year,deto6_Ansträngning1$Lokal)



# standardize by different gram of dynamite

