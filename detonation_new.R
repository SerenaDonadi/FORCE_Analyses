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
guess_encoding("detonation-data-new.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### last version (december 2024) of detonation dataset 
detonew <- read.csv2("detonation-data-new.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".")
head(detonew)

# make column with only year
head(detonew$Fiskedatum)
detonew$year<-as.numeric(RIGHT(detonew$Fiskedatum,4))
summary(detonew$year)
hist(detonew$year)

#### cleaning ####

# keep only approved obs
table(detonew$GODKAND)
unique(detonew$GODKAND) # ok, there is no NAs, but anyways:
detonew2 = subset(detonew, GODKAND == "JA " | is.na(GODKAND))  

# keep only not disturbed occasions:
table(detonew2$Störning)
unique(detonew2$Störning)
detonew3 = subset(detonew2, Störning == "NEJ" | is.na(Störning))  

# exclude Simpevarp and Biotest Forsmark (but not "Forsmark"), as here water is almost 10 degrees warmer
detonew3 %>%
  filter(Lokal =="Simpevarp") # ca 740 obs
detonew3 %>%
  filter(Lokal =="Biotestsjön, Forsmark") #ca 3700 obs
detonew4 = subset(detonew3, !(Lokal %in% c("Simpevarp","Biotestsjön, Forsmark")))

# exclude Gotland? maybe no need, see read me file 

# consider only some spp:
unique(detonew4$Artbestämning)
detonew5<-detonew4 %>%
  filter(Artbestämning == "Storspigg" | Artbestämning == "Abborre" | Artbestämning == "Gädda" | Artbestämning == "Mört")

# check comments: GO THROUGH all! also where they wrote 1 for 0+, and 999 or 99,9 for adults. Even if they were provfiskad. i would suggest not to 
# work on this anuy longer until I know what provfiskad means, since we may have many more replicates this way
unique(detonew4$Info_publik)
#"Information om sjunkande fisk endast för abborre och gädda. Resterande arter endast uppgift om flytande individer."
