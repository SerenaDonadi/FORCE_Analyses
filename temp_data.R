
# I have three types of temp measurement:
#1) satellite
#2) loggers
#3) From the field

######
# ecologically significant temp variables.
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

# n_days_withavg_temp_over_10C_till_July
# n_days_withavg_temp_over_10C_till_catch_date?
# n_days_withavg_temp_over_10C_April1yearbefore_July
# n_days_withavg_temp_over_10C_April2yearbefore_July
# n_days_withavg_temp_over_10C_April3yearbefore_July
# n_days_withavg_temp_over_10C_April4yearbefore_July

# timing of growing season (expressed as ordinal number):
# first_day_withavg_temp_over_10C 
# first_day_withavg_temp_over_10C_lag1year
# first_day_withavg_temp_over_10C_lag2year
# first_day_withavg_temp_over_10C_lag3year
# first_day_withavg_temp_over_10C_lag4year
# last_day_withavg_temp_over_10C_lag1year # or alternatively the number of days when temperature fell below 10 ◦C after the first observation. 
# last_day_withavg_temp_over_10C_lag2year
# last_day_withavg_temp_over_10C_lag3year
# last_day_withavg_temp_over_10C_lag4year

# PS: some of these variables can be calculated for satellite data with day temp and with Ingrid data.
# or, if I manage to combine temp variables according to Max approach, with that temp variable


##### 
# temp from satellite
#####
# 1) temp from satellite (script copied from the gillnet dataset analyses) 
# the data are attached to the gillnet dataset, the match has been done based on "location"
# avg year temp from satellite: from 1982 to 2023, all locations of the gillnet dataset
# avg day temp from satellite: all locations of the gillnet dataset but only until 2020
# PS: we don't know whether is daily avg or one time, how many time the satellite passed by, and at what time
# Björnöfjärden and Älgöfjärden are not in the gillnet dataset, but they are in the temp dataset

#####
# temp from loggers
#####
# read all files received from Ingrid and extract ecologically important temp variables

install.packages("climwin")
library(climwin)
vignette("climwin", package = "climwin")

# check location match

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

#read all files with temp log data and add a column with the corresponding location in the length_age dataset
# and fix lay out if needed
unique(length_age10$location) # 
# Askrikefjärden:nope
# Asköfjärden: nope
# Aspöja: 1 logger. Use it also för  Kärrfjärden?                      
# Blekinge län: 1 out of 5 sites close to Torhamn, Karlskrona Ö skärgård. Use that logger?
# Bulleröfjärden: there are loggers, but nothing received from them. Check if any could work                
# Finbo, Åland: yes, 1 logger receeived out of three. check if others are needed           
# Forsmark: I’d consider FM glåbodarna and FM ön, check       
# Gaviksfjärden: yes, 1 logger  
# Holmön: 1 logger received out of two. Don't know which. Both seem on land          
# Kinnbäcksfjärden: yes, 1 logger
# Kumlinge, Åland: yes, 1 logger
# Kvädöfjärden (ccordinates from gillnets seem to be wrong): received 5 or 6 (kvädö eköfjärden?)loggers. Use all??
# Kärrfjärden: use the logger from Aspöja?
# Lagnö: 1 logger close, "Stockholms skg,	Ljusterö Smedsholmen ". Others received (from Ljusterö, south) but maybe skip?
# Långvindsfjärden: yes 1 logger
# Muskö: yes 1 logger
# Mönsterås: nope      
# Norrbyn: yes 1 logger
# Risöfjärden: nope
# Råneå: yes 1 logger  
# Seskaröfjärden: nope
# Stockholms skärgårds m kustvatten: which one? Gålö maybe better for Bulleröfjärden
# Torhamn, Karlskrona Ö skärgård: YES  1 LOGGER 
# Torsås kustvatten: NOPE     
# Valjeviken: nope   
# Vinö: nope              
# Västerbottens län: nope         
# Östra Gotlands m kustvatten: nope

setwd("G:/My Drive/FORCE/Data/temp logger data")

Aland_fin <- read.csv2("finbo brändö.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(Aland_fin)
Aland_fin$location <- "Finbo, Åland"
# rename column År: maybe no need if I use climwin, check later
# Aland_fin <- rename(Aland_fin, year = 'År')
Aland_kum <- read.csv2("kumlinge.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(Aland_kum)
Aland_kum$location <- "Kumlinge, Åland"

Aspoja <- read.csv2("aspöja birkösundet.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(Aspoja)
Aspoja$location <- "Aspöja"

Forsmark1 <- read.csv2("FM ArtSub Borgarna.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".")
head(Forsmark1) # not sure where this is
Forsmark1$location <- "Forsmark"

Forsmark2 <- read.csv2("FM glåbodarna.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(Forsmark2)
Forsmark2$location <- "Forsmark"

Forsmark3 <- read.csv2("FM ön.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
head(Forsmark3)
Forsmark3$location <- "Forsmark"

# maybe skipe these? far away from nets
#Forsmark4 <- read.csv2("FM trollgrund inner.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
#Forsmark5 <- read.csv2("FM trollgrund ytter.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

# which one to consider? Gålö better for Bulleröfjärden?
Stockholm1 <- read.csv2("gålö askviken",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Stockholm2 <- read.csv2("gålö bilsta.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Stockholm3 <- read.csv2("ljusterö smedsholmen.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Stockholm4 <- read.csv2("LjusteröBlötviken2001.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 


Gavik <- read.csv2("gavik.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Holmon <- read.csv2("holmön.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado1 <- read.csv2("JMT1 trollholmen manuell.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado2 <- read.csv2("JMT3 hamnö manuell.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado3 <- read.csv2("JMT8 häxvassen manuell.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado4 <- read.csv2("JMT9 skogsholmen manuell.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado5 <- read.csv2("JMT10 arnö TT.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kinnback <- read.csv2("kinnbäcksfj.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Kvado6 <- read.csv2("kvädö eköfjärden.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 
Långvi <- read.csv2("långvindsfj.csv",fileEncoding="ISO-8859-1",  header=TRUE, sep=",", dec=".") 

# before going on, check where the temop logg are located vs gillnets. maybe no need to import all these
head(aland)

### stop here until I know how to proceed with Max and Co.



