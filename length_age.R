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
guess_encoding("perch-length-age.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### 1) gillnets
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
# what does it mean "båda kön"?? Comments: "Hermafrodit - underutvecklade gonader". Exclude it?
# what do gonad status levels correspond to? relevant for us? I don't think so but better to confirm
# many fish have been frozen, see comments. could that influence length measurements?
# are differnt method for aging comparable?
# in "sampling method" I find "Stratifierat på honor i 2,5 cm-klasser", "Stratifierat cm-klasser enl. blankett 80"..relevant?

#####
# Subset
#####

# exclude obs with missing age and missing length:
length_age1<-length_age[!is.na(length_age$age),]
length_age2<-length_age1[!is.na(length_age1$total_length),]

# exclude obs not approved
length_age3<-subset(length_age2, approved != "NEJ")

table(length_age2$sampling_method)

summary(length_age3)

# check/exclude weird stuff based on "comments": TO DO
"Längden i originalprotokoll 152, ändrad till 252 baserat på ålder och somatisk vikt (samt totalvikt på originalprotokollet)."
"längd lite osäker" 
"OBS! Ändra tot.längd till 241mm\n"  
"Längden var 294, togs bort som orimlig. NM2020-12-03"  
"Antingen felaktig vikt eller längd, NM 2018-11-12" 
"Antingen felaktig vikt eller längd, NM 2018-11-20" 
"Längd 287 borttagen, fiskens längd stämmer inte överens med åldersprovet."  
"Sektion 1/längden eller vikten stämmer ej"                                                                                   

length_age3 %>% 
  filter(comments == "Längden i originalprotokoll 152, ändrad till 252 baserat på ålder och somatisk vikt (samt totalvikt på originalprotokollet).")

