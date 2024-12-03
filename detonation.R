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
"Biotestsjön, Forsmark") #ca 3300 obs
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

# when LANGDGRUPP_LANGD is NA, I can replace the NA found under LANGDGRUPP_ANTAL with Antal, which is the tot number of a species 
# and cohort and collected at the surface or bottom. But check if there is any case where they only measured the length for a subsample
# and where they hence wrote under Antal both the measured and the unmeasured samples. Indeed they did so, and there is no extra
# row where LANGDGRUPP_LANGD is NA

unique(deto8$Antal) # non ci sono NA
# quindi potrei usare Antal per calcolare le CPUE, corrette per bottom/yta e per la dinamite.
# poi uso le info sul subsample, quando c'e, per calcolare how many per size class (i.e. trasponendo la percentuale di una certa classe sui totali contati a livello di Antal)

# try: split up the dataset to calculate tot corrected CPUE, while keeping separate the info on size classes:
# delete info on size classes
detoCPUE<-deto8 %>%
  select(-c(FANG_ID,LANGDGRUPP_LANGD ,LANGDGRUPP_ANTAL ,GODKAND,STORNINGAR, Ansträngning_enhet,Störning)) 
# remove duplicates
duplicated(detoCPUE)
detoCPUE1 = detoCPUE[!duplicated(detoCPUE),]
head(detoCPUE1)

# transpose in wide format:
detoCPUE1_wide<-pivot_wider(detoCPUE1, names_from = c(Artbestämning, Sortering, Fångsttyp), values_from = c(Antal), values_fn = list)
head(detoCPUE1_wide) 

# this is the same, so go with the above
#####
# warning msg, trying with concatenate first:
library(crayon)
detach("package:ggplot2", unload=TRUE)
detoCPUE1$sp_cohort_FS<-detoCPUE1$Artbestämning %+% "_" %+% detoCPUE1$Sortering %+% "_" %+% detoCPUE1$Fångsttyp
# remove redondant columns:
detoCPUE2<-detoCPUE1 %>%
  select(-c(Artbestämning ,Sortering, Fångsttyp))
# transpose in wide format:
detoCPUE2_wide<-pivot_wider(detoCPUE2, names_from = c(sp_cohort_FS), values_from = c(Antal))
#####

# OBS: # I would also need to change the nme of the column varibles, they give problems
# do it before that the next step, it may solve the problem, bc now it is creating lists

# replace NULL values with zeros
# check that there is no NA in other variables except the spp variables:
unique(detoCPUE1_wide$year)
detoCPUE1_wide[is.null(detoCPUE1_wide)] 
detoCPUE1_wide[is.null(detoCPUE1_wide)] <- 0
# no, it doesn't work, 
is.null(detoCPUE1_wide)
head(detoCPUE1_wide)

# now I need to understand where they did not measure at the surface (falso zeros) or when they did but did not find any (true zeros):
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Ansträngning)
table(detoCPUE1$Fångsttyp,detoCPUE1$year,detoCPUE1$Lokal)







# consider only Årsyngel for all spp except stsp
table(deto7$Sortering)
unique(deto7$Sortering) # there are NA, typ 9000
subset(deto7, is.na(Sortering))


# also sometimes, given that zero under Fångsttyp are not reported, the row is missing. should be fine to put it into horizontal format



table(deto7$LANGDGRUPP_ANTAL)


# harmonize bottom/yta estimates
head(deto6)
table(deto6$Fångsttyp)
unique(deto6$Fångsttyp)
# they should always (when grams were 10gr) have looked at both bottom and surface (except for Forsmark and Simpevarp), 
# If there is not bottom value is zeros but they did not record zeros.But this applies to the lst 15-20 years
table(deto6$Ansträngning,deto6$year)
table(deto6$Fångsttyp,deto6$year)
table(deto6$Ansträngning,deto6$year,deto6$Fångsttyp)


deto6_Ansträngning1<- deto6 %>%
  filter(Ansträngning == "1") 
table(deto6_Ansträngning1$Lokal,deto6_Ansträngning1$year)
table(deto6_Ansträngning1$Fångsttyp,deto6_Ansträngning1$year,deto6_Ansträngning1$Lokal)

deto6_Ansträngning8<- deto6 %>%
  filter(Ansträngning == "8") 
table(deto6_Ansträngning8$Lokal,deto6_Ansträngning8$year)
table(deto6_Ansträngning8$Fångsttyp,deto6_Ansträngning8$year,deto6_Ansträngning8$Lokal)

deto6_Ansträngning20<- deto6 %>%
  filter(Ansträngning == "20") 
table(deto6_Ansträngning20$Lokal,deto6_Ansträngning20$year)
table(deto6_Ansträngning20$Fångsttyp,deto6_Ansträngning20$year,deto6_Ansträngning20$Lokal)

deto6_Ansträngning200<- deto6 %>%
  filter(Ansträngning == "200") 
table(deto6_Ansträngning200$Lokal,deto6_Ansträngning200$year)
table(deto6_Ansträngning200$Fångsttyp,deto6_Ansträngning200$year,deto6_Ansträngning200$Lokal)




# standardize by different gram of dynamite

table(deto2a$Ansträngning)
unique(deto2a$Ansträngning)


# put spp on columns - do it later, after harmonizing,correcting and pooling the number per spp
deto4_wide<-pivot_wider(deto4, names_from = Artbestämning, values_from = c(LANGDGRUPP_ANTAL, Sortering))

#gillnets_totCPUE_wide[is.na(gillnets_totCPUE_wide)] <- 0 # wait, replace with 0 only for spp, not temp!
#gillnets_totCPUE_wide$avg_year_temp[gillnets_totCPUE_wide$avg_year_temp==0] <- NA

deto4_wide[is.na(deto4_wide), which(names(deto4_wide) == "..."):which(names(deto4_wide) == "....")] = 0 # to test. or look at old script


