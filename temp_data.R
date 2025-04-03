
# I have three types of temp measurement:
#1) satellite
#2) loggers
#3) From the field

# 1) temp from satellite (script copied from the gillnet dataset analyses) 
# the data are attached to the gillnet dataset, the match has been done based on "location"
# avg year temp from satellite: from 1982 to 2023, all locations of the gillnet dataset
# avg day temp from satellite: all locations of the gillnet dataset but only until 2020
# PS: we don't know whether is daily avg or one time, how many time the satellite passed by, and at what time
# Björnöfjärden and Älgöfjärden are not in the gillnet dataset, but they are in the temp dataset

# 2) temp from loggers
# read all files received from Ingrid and extract temp variables, such as:
#####
# avg_day_temp_logger (to compare with day temp from satellite and with temp at the time of sampling) - if available (depending on loggers sampling frequency)

# avg_temp_logger_April_July (summer temp before sampling)
# avg_temp_logger_April_catch_date?(summer temp before sampling v2)
# avg_temp_logger_April_Sept (summer temp)
# avg_temp_logger_October_March (winter temp)
# avg_temp_logger_year (annual temp)

#  if available (depending on loggers sampling frequency):
# min_avg_day_temp_logger_April_July (min summer temp before sampling) - if available
# min_avg_day_temp_logger_April_catch_date?(min summer temp before sampling v2)
# min_avg_day_temp_logger_April_Sept (min summer temp)
# min_avg_day_temp_logger_October_March (min winter temp)
# min_avg_day_temp_logger_year (min annual temp)

# max_avg_day_temp_logger_April_July (max summer temp before sampling) - if available
# max_avg_day_temp_logger_April_catch_date?(max summer temp before sampling v2)
# max_avg_day_temp_logger_April_Sept (max summer temp)
# max_avg_day_temp_logger_October_March (max winter temp)
# max_avg_day_temp_logger_year (max annual temp)

# maybe some measures of variation, such as the average standard deviation of day temperature, or week if frequency of sampling is lower
# avg_sd_day/week?_temp_logger_April_July (variation summer temp before sampling)
# avg_sd_day/week?_temp_logger_April_catch_date? (variation summer temp before sampling v2)
# avg_sd_day/week?_temp_logger_April_Sept (variation summer temp)
# avg_sd_day/week?_temp_logger_October_March (variation winter temp)
# avg_sd_day/week?_temp_logger_year (variation annual temp)

# Measure of cumulative temp over years:
# avg_temp_logger_April1yearbefore_July 
# avg_temp_logger_April2yearbefore_July 
# avg_temp_logger_April3yearbefore_July 
# avg_temp_logger_April4yearbefore_July 

# lagged variables for previous years:
# avg_temp_logger_year_lag1year (annual temp year before)
# avg_temp_logger_April_Sept (summer temp year before)
# avg_temp_logger_October_March (winter temp year before)

# Note: loggers did not measured with the same time intervals, hence I can't sum up the values, unless they have the same levels of aggregation
# check what the lower sampling frequency(once a day? or once a week?) and revise if needed the following.
# frequency is every week for: "JMT8 häxvassen manuell", "JMT3 hamnö manuell", "JMT1 trollholmen manuell". For these do not calculate variables aggregated
# at day level (are they included in any of my datasets? check)

# length of growing season:
# degree-day sums over a threshold of 10C  
# sums_degree_over_10C_till_July  #OBS: degrees of daily avg? or week avg??? check what is the minimum frequency
# sums_degree_over_10C_till_catch_date?
# sums_degree_over_10C_from1yearbefore_July 
# sums_degree_over_10C_from2yearbefore_July 
# sums_degree_over_10C_from3yearbefore_July
# sums_degree_over_10C_from4yearbefore_July

# sums_degree_April_July  #OBS: degrees of daily avg? or week avg??? check what is the minimum frequency
# sums_degree_April_catch_date?
# sums_degree_April1yearbefore_July 
# sums_degree_April2yearbefore_July 
# sums_degree_April3yearbefore_July
# sums_degree_April4yearbefore_July

# n_days_withavg/week?_temp_over_10C_till_July
# n_days_withavg/week?_temp_over_10C_till_catch_date?
# n_days_withavg/week?_temp_over_10C_April1yearbefore_July
# n_days_withavg/week?_temp_over_10C_April2yearbefore_July
# n_days_withavg/week?_temp_over_10C_April3yearbefore_July
# n_days_withavg/week?_temp_over_10C_April4yearbefore_July

# timing of growing season (expressed as ordinal number):
# first_day_withavg/week?_temp_over_10C 
# first_day_withavg/week?_temp_over_10C_lag1year
# first_day_withavg/week?_temp_over_10C_lag2year
# first_day_withavg/week?_temp_over_10C_lag3year
# first_day_withavg/week?_temp_over_10C_lag4year
# last_day_withavg/week?_temp_over_10C_lag1year # or alternatively the number of days when temperature fell below 10 ◦C after the first observation. 
# last_day_withavg/week?_temp_over_10C_lag2year
# last_day_withavg/week?_temp_over_10C_lag3year
# last_day_withavg/week?_temp_over_10C_lag4year




#PS: some of these variables can also be calculated for satellite data!
#####

install.packages("climwin")
library(climwin)
vignette("climwin", package = "climwin")

# check location match
#####
unique(length_age10$location) # 
# locations in length age dataset for which we don't have temp data from loggers (but we have from satellite and field)
# Askrikefjärden, Blekinge län,Bulleröfjärden,Gaviksfjärden... many

# check för Holmön
length_age10 %>%
  filter(location %in% c("Holmön")) %>%
  select(c(long, lat)) %>%
  unique()
# that is: lat and long are  and 63.66783, 20.81528
# in temp logg data I see two loggers in "Holmöarna", with lat and long corresponding, after transformation, to:
# 63.6891666667, 20.8725 ok, use this 
# 63.7033333333, 20.8916666667 this instead seem to be on land. Indeed it says"Sörfjärden,insjö", it's a lake 

length_age10 %>%
  filter(location %in% c("Stockholms skärgårds m kustvatten")) %>%
  select(c(long, lat)) %>%
  unique()
# data from Gålö (2 locations received), should correspond to location Stockholms skärgårds m kustvatten in gillnets dataset.
# however, the fishing has been conducted out in the archipelago, not sure I should use the data from Gålö.
# I could check deviations from satellite data as well
#####

#read all files with temp log data and add a column with the corresponding location in the length_age dataset
# and fix lay out if needed

library(readr)
setwd("G:/My Drive/FORCE/Data/temp logger data")
# to calculate the probability of a file of being encoded in several encodings
guess_encoding("finbo brändö.csv", n_max = 1000)
aspoja <- read.csv2("aspöja birkösundet.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
aland <- read.csv2("finbo brändö.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

aland <- read.csv2("FM ArtSub Borgarna.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

aland <- read.csv2("FM glåbodarna.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

aland <- read.csv2("FM ön.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

aland <- read.csv2("FM trollgrund inner.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

aland <- read.csv2("FM trollgrund ytter.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 


head(aland)


