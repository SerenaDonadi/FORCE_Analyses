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
library(mgcv)
library(ggeffects)

# to calculate the probability of a file of being encoded in several encodings
library(readr)
guess_encoding("temperature-data.csv", n_max = 1000)
# try both encoding = "" and fileEncoding = ""

### read data
temp_satellite <- read.csv2("temperature-data.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(temp_satellite,10)
summary(temp_satellite)
table(temp_satellite$gear_code)

# make column with only month
library(ExcelFunctionsR)
head(temp_satellite$date)
temp_satellite$month<-as.numeric(LEFT(RIGHT(temp_satellite$date,5),2))

# make column with only year
temp_satellite$year<-as.numeric(LEFT(temp_satellite$date,4))

# convert gear to factor, rename it and rename its levels:
temp_satellite$gear_code <- as.factor(temp_satellite$gear_code)
# Check the current levels
levels(temp_satellite$gear_code)
# Rename the levels
levels(temp_satellite$gear_code) <- c("K009","K053", "K059","K064")

# add for each value the degrees exceeding 10:
temp_satellite1<- temp_satellite %>%
  mutate(dd_temp = ifelse(temp > 10, temp - 10, 0)) # calculate degree days exceeding 10
head(temp_satellite1,10)


# calculate avg day temp and dd for each sub.location, gear code and year:
temp_satellite_year <- temp_satellite1 %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_year = mean(temp, na.rm = TRUE),
            #avg_temp_exceeding_10 = ifelse(temp > 10, mean(temp), 0), # give problems
            dd_year = sum(dd_temp, na.rm = TRUE)) 

# calculate average day temperature and  dd for januari-july: (to consider for the year when sampling occurred)
temp_satellite_jan_jul <- temp_satellite1 %>%
  filter(month %in% c(1,2,3,4,5,6,7)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_jan_jul = mean(temp, na.rm = TRUE),
            dd_jan_jul = sum(dd_temp, na.rm = TRUE))

# calculate average day temperature of summer months for each sub.location, gear code and year:
temp_satellite_summer <- temp_satellite1 %>%
  filter(month %in% c(4,5,6,7,8,9,10)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_summer = mean(temp, na.rm = TRUE))

# calculate average day temperature april-july:
temp_satellite_april_jul <- temp_satellite1 %>%
  filter(month %in% c(4,5,6,7)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_april_jul = mean(temp, na.rm = TRUE),
            dd_april_jul = sum(dd_temp, na.rm = TRUE))

# calculate average day temperature of winter months for each sub.location, gear code and year:
temp_satellite_winter <- temp_satellite1 %>%
  filter(month %in% c(11,12,1,2,3)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_winter = mean(temp, na.rm = TRUE))

# calculate average day temperature januari-march:
temp_satellite_jan_mar <- temp_satellite1 %>%
  filter(month %in% c(1,2,3)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_jan_mar = mean(temp, na.rm = TRUE),
            dd_jan_mar = sum(dd_temp, na.rm = TRUE))

# calculate average day temperature for temperature exceeding 10 grader for each sub.location, gear code and year:
temp_satellite_exceeding_10_year <- temp_satellite1 %>%
  filter(temp > 10) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_exceeding_10_year = mean(temp, na.rm = TRUE)) 

# calculate average day temperature for temperature exceeding 10 grader for each sub.location, gear code and year for jan-july
temp_satellite_exceeding_10_jan_jul <- temp_satellite1 %>%
  filter(temp > 10) %>%
  filter(month %in% c(1,2,3,4,5,6,7)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(avg_temp_exceeding_10_jan_jul = mean(temp, na.rm = TRUE)) 

# calculate N days with temp > 10 
temp_satellite_n_days_exceeding_10 <- temp_satellite1 %>%
  filter(temp > 10) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(n_days_exceeding_10 = n())

# calculate N days with temp > 10 until July:
temp_satellite_n_days_exceeding_10_jan_jul <- temp_satellite1 %>%
  filter(temp > 10) %>%
  filter(month %in% c(1,2,3,4,5,6,7)) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(n_days_exceeding_10_jan_jul = n())

# day of the first day with temp > 10 expressed as julian date: - CHECK
temp_satellite_first_day_exceeding_10 <- temp_satellite1 %>%
  filter(temp > 10) %>%
  group_by(location,sub.location, gear_code, year) %>%
  summarise(first_day_exceeding_10 = min(as.Date(date, format="%Y-%m-%d"))) %>%
  mutate(first_day_exceeding_10_julian = as.numeric(first_day_exceeding_10 - as.Date(paste0(year, "-01-01")) + 1))

# NB: it always occur before august

# merge all dataframes together:
temp_satellite_all <- temp_satellite_year %>%
  left_join(temp_satellite_jan_jul, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_summer, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_april_jul, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_winter, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_jan_mar, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_exceeding_10_year, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_exceeding_10_jan_jul, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_n_days_exceeding_10, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_n_days_exceeding_10_jan_jul, by = c("location", "sub.location", "gear_code", "year")) %>%
  left_join(temp_satellite_first_day_exceeding_10, by = c("location", "sub.location", "gear_code", "year"))

head(temp_satellite_all,10)

# check correlation:
cor(temp_satellite_all[,c("avg_temp_year","dd_year","avg_temp_summer","avg_temp_winter",
                           "avg_temp_exceeding_10_year","n_days_exceeding_10",
                           "first_day_exceeding_10_julian")], use = "pairwise.complete.obs")
df <- data.frame(temp_satellite_all[,c("avg_temp_year","dd_year","avg_temp_summer","avg_temp_winter",
                           "avg_temp_exceeding_10_year","n_days_exceeding_10",
                           "first_day_exceeding_10_julian")])
# plot pairwise scatterplots of covariates:
pairs(df)
