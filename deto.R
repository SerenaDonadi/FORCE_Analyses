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
guess_encoding("YNGELDATA_20210202.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) old detonation dataset (but received later!!!)
deto1 <- read.csv2("YNGELDATA_20210202.csv",fileEncoding ="UTF-8",  header=TRUE, sep=";", dec=".") 
head(deto1)
summary(deto1$year)

### 1) new detonation dataset (but received before!)
deto2 <- read.csv2("detonation-data.csv",encoding="ANSI",  header=TRUE, sep=",", dec=".")
head(deto2)
summary(deto2$year)

# make column with only year
head(deto2$Fiskedatum)
deto2$year<-as.numeric(RIGHT(deto2$Fiskedatum,4))
summary(deto2$year)
hist(deto2$year)

#### cleaning up deto1 (old detonation dataset) ####

# remove Gävlebukten missing info
summary(deto1[deto1$bay == "GB",])
df = deto1[deto1$bay != "GB",]

# assume that inventories in Kalmar late 80s and 90s take place end of September (based on Karås reports)
df$date = as.Date(df$date)
df$date[is.na(df$date)] = as.Date(paste0(df$year[is.na(df$date)], "-09-30"))

# set stickleback to NA when FS-delvis as not clear whether S counted or not (abborre and gädda both counted floating and sunken)
df[df$FloatSink == "FS-delvis", which(names(df) == "noll_stsp_tot"):which(names(df) == "stsp_totalt")] = NA


#### calculating FS conversion factors ####
# to estimate tot number also when fish at the bottom were not counted

for( i in which(names(df) == "noll_abbo_tot"):which(names(df) == "noll_gadd_sjunk")){
  df[,i] = as.numeric(df[,i])} # make sure all fish abundances are numeric

table(df$FloatSink) # sometimes counted only floating, sometimes both
df_FS = df[df$FloatSink == "FS",]

# for 0+ abborre
ratio_abbo = df_FS$noll_abbo_flyt/
  df_FS$noll_abbo_tot
sum(!is.nan(ratio_abbo) & !is.infinite(ratio_abbo)) # 2735 non defined or infinite values out of 9920
FS_abbo = round(mean(ratio_abbo[!is.nan(ratio_abbo) & !is.infinite(ratio_abbo)], na.rm = T), digits = 2) # 0.42 (0.39 in file)

# for 0+ gadda
ratio_gadd = df_FS$noll_gadd_flyt/
  df_FS$noll_gadd_tot
sum(!is.nan(ratio_gadd) & !is.infinite(ratio_gadd)) # 2291 non defined or infinite values out of 9920
FS_gadd = round(mean(ratio_gadd[!is.nan(ratio_gadd) & !is.infinite(ratio_gadd)], na.rm = T), digits = 2)  # 0.64 (0.64 in file)

# for ALL spigg
ratio_stsp = (df_FS$noll_stsp_flyt + df_FS$juvad_stsp_flyt)/
  (df_FS$noll_stsp_tot + df_FS$juvad_stsp_tot)
sum(!is.nan(ratio_stsp) & !is.infinite(ratio_stsp))# 3844 non defined or infinite values out of 9920
FS_stsp = round(mean(ratio_stsp[!is.nan(ratio_stsp) & !is.infinite(ratio_stsp)], na.rm = T), digits = 2) #0.17 (0.19 in file)

#### apply correction factors ####

table(df$Conversion.factor.Pentex25.10g)
# OBS: the conversion factors listed in the file may be outdated, see sheets "Konvert gammal" and "Konvertering ny" in the excel file
# "Konvertering sprängstyrkor 20170901" received from Ulf (in subfolder in Data folder)
# Question: Keep or change?

# abborre
df$noll_abbo_corrected = df$noll_abbo_tot
df$noll_abbo_corrected[df$FloatSink == "F"] = df$noll_abbo_tot[df$FloatSink == "F"]/FS_abbo
df$noll_abbo_corrected = df$noll_abbo_corrected*df$Conversion.factor.Pentex25.10g

# gadda
df$noll_gadd_corrected = df$noll_gadd_tot
df$noll_gadd_corrected[df$FloatSink == "F"] = df$noll_gadd_tot[df$FloatSink == "F"]/FS_gadd
df$noll_gadd_corrected = df$noll_gadd_corrected*df$Conversion.factor.Pentex25.10g

# spigg
df$tot_stsp_corrected = df$noll_stsp_tot + df$juvad_stsp_tot
df$tot_stsp_corrected[df$FloatSink == "F"] = (df$noll_stsp_tot + df$juvad_stsp_tot)[df$FloatSink == "F"]/FS_stsp
df$tot_stsp_corrected = df$tot_stsp_corrected*df$Conversion.factor.Pentex25.10g


# remove repeated data from 2019
df2019 = df[df$year == 2019, ] # subset
#df2019 = df2019[order(df2019$DD_N, df2019$DD_E, df2019$tot_stsp_corrected, df2019$noll_abbo_corrected, df2019$noll_gadd_corrected), ]
#df2019 = df2019[!duplicated(cbind(df2019$DD_N, df2019$DD_E, df2019$tot_stsp_corrected, df2019$noll_abbo_corrected, df2019$noll_gadd_corrected)),]
#df = df[df$year != 2019,]
#df = rbind(df, df2019)

# SD: wait, I am not sure, there are some values slightly different, e.g. 500 and 501, 10 and 12.
# Björnöfjärden and other 3-4 locations are fine (see after ordering after lat)
# I'd rather take the mean of obs with the same lat and long, not knowing which row of the almost-equal cases is correct

# I don't know a quick way to do it. If I use summarize I have to bring long all the variables to be able to merge the subset 
# with the rest of the data. Maybe wait to see what I need to do with deto2, which contains the most recent data: if I select
# only some variables then I don't need to bring all columns with summarize 
# does deto 2 contain the sample in deto 1?

df %>% 
  filter(bay == "Nedre Yxnö Glo") 
  
deto2 %>% 
    filter(Lokal == "Nedre Yxnö Glo")
# nope  

df %>% 
  filter(bay == "11 Bäckviken") 

deto2 %>% 
  filter(Lokal == "11 Bäckviken")
# nope  


# chekc with U & A
# look also at the read me file to clean the data