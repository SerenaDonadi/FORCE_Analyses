R scripts of analyses for FORCE project - Serena Donadi

##### 
Length-age dataset
#####

It should be a subset of the gillnets, however, some site* location were not found in this latter one. This is because the gillnets dataset was filtered based on gear K064, while the lnegth-age dataset, provided by Ronny via email to Agnes and then to me, include samples collected with various gear. This is not a problem for us, as the gear will affect the catchability (and not the length), hence the CPUE, for which we are using that data from the gillnet.

- I see under "sub.lokation": "Väst Biotestsjön" and "Syd Biotestsjön". Lokation is "Forsmark". Keep it or remove it? probably remove
No, keep them, they are outside the basin.

- we decided to use sublocation*year, rather than location per year, as our replicates, as they can be quite far apart and be represenattive of different populations.

- note that Råneå 2008, which had a comment of disturbed nets in the gillnets (for the covariates) and was initially excluded, was reincluded. Potentially test with and without it.

- the matching between length-age dataset and stsp and distance from offshore datasets was done based on location, sub.location, year and gear. This is because the fishing was conducted in different places with different gear, even within the same location and sublocation, so we can use finer coordinates to match where the fish was caught and the distance to offshore and the stsp estimates.
The matching with the gillnets dataset, hence all covariate, is not based on gear, as we retained only k064 samples. Hence there will be a slight mismatch in the exact point where the length of fish was taken and the covariates, e.g. CPUE of other species and conspecifics, temperature (except for stsp and distance from offshore). Agnes suggests to include gear as a random factor, to account for the variation produced by such mismatch, as well as differences in catchability of different gear. however, I don't think it helps, as gear may cause variation in the CPUEs (covariates) but not in hte response variable (length), unless it alters the relationship between length and age. No harm to test it as random though.

-about the data on stsp and dist offshore. Stsp come from BIAS. The spatial unit is ICES rectangles, but Agnes will extract estimates for our location (typ a buffer integrated measure). In the model use stsp*dist offshore. The unit for distance to the open sea is in meters. Gotland maybe has no data on dist from offhsore, but it is open, so I can use zeros. I also have some other places with negative distance from offshore, and substituted them with zeros.
The northern areas may be missing now (no data before 2007), Agnes and Ulf will see what to do, and also dist fromn offshore from Åland is missing now, but will come later. If I get more data in the future, use a variable to distinguish stsp estimates obtained with different methods. 


###
Gillnets dataset:
###

To do:
- compare satellite temp values extracted from Agnes to the measured temperatures from the gillnet data to see how well they correspond. Also to temp log values. Use mabne Max approach to calculate more accurate estimates of temp. Matilda (Ultuna) working on a project with Anna G and Magnus H, might use Max method, check with her

- Check climwin R package to find the best temp data as covariate

- Include also Löja (bleak) as a potential covariate (food for perch)

Questions:

- how to calculate CPUE? in a previous project where I used data from KUL, Peter told me that I should divide the abundance per station (and spp) by the product of ansträngning (number of nets) and fisketid (number of days). Also, about fisketid: dygn and natt are not much different, as they don't report the exact start and end time, so no need to keep them separated. However, I don't have fisketid in the current dataset. We thought to start by considering only Ansträngning = 1, but if this is the number of nets rather than the effort, we still need the number of days (or nights, no difference). Check also what to do with Ansträngning_enhet = ..*timmarna.

Update1: I have received a new file with fisketid. It has always the same value of Ansträngning, so we assume that the number of days is already contained in Ansträngning, and they are the same as the net is 1. An exception is found for some values of Ansträngning equal to 35 or 10, where fisketid is 1 and Ansträngning_enhet is nätt*natt. To be on the safe side, until I verify with Peter L., I consider only Ansträngning = 1.
Check also with P why there are Ansträngning = NA but Fisketid 1 (che sarebbero papabili)

Remove all ansträngning that are not 1. Maybe save same after Agnes send me new info from Matilda

Update2: after talking to Peter, Matilda, Ronny, Agnes suggests to remove fishing where effort is expressed in hours, correct some typos, remove weird fishing with Ansträngning = 2 (see cleaning script) - check that all issues mentioned above are solved (e.g. do we still have Ansträngning equal to 35 or 10?)

Update3: in the new file, all Ansträngning are 1, Fisketid_enhet is dygn or natt, and Ansträngning_enhet doe not contain "timmarna", so all good. However, Fisketid is 12 instead of 1 in 2525 cases. I remove it

- it would be good to have temp at the time of fishing (check values as before I found 999, which are NAs), to account for potential artifact (i.e. different catchability with temp) by regressing residual variation after accounting for effects of avg year temp (and interactions).

Done. Plus depth. But is it the depth of the net or the water column? I assume the second because I have a max value of 52 but doublecheck

- the gear have different catchability, check that current data are collected consistently

K064

- shall I consider only data GODkänd yes? yes, but keep also NA

To be on the safe side yes, until I verify with Peter L. But I see that when ingen fångst is reported for some section of a net, godkand is NA. I better keep that, so I would exclude only godkänd NEJ

- Do we need zeros? Yes, they will influence CPUE, so check that zeros are included. Before I found "inge_fångst" written for each station and date

Inge fångst were added by Agnes in the last extraction. I see that they are reported for each section of a net where no spp were caught
When counting the number of nets to calculate the effort, I still should be able to include the nets with nothing. But don't remove NAs in LANGDGRUPP_LANGD or LANGDGRUPP_LANGD.

- What is "Ok nd" under "Art"? I guess another version of Okänd

- What's LANGDGRUPP_ANTAL vs ANTAL? 

Antal is the overall catch PER SECTION of a net (and per spp), LANGDGRUPP_ANTAL is the  number per size class (what I want)

- langdrupp_langd is often but not always rounded to 0.5 cm. Consider size classes? 

Yes, but how:
makes classes of 1 cm as 0.5-1.5, 1.6-2.5,2.6-3.5 etc etc if measurements were rounded up to *.5 value, or 1-2,2-3,4-5 etc etc if measurements were taken as closest *.5 value
Check with Peter L. Ronny will check it

- HIERARCHY (TO BE CONFIRMED): I believe that FANGD_ID refer to different mesh size (section) of the same net, which is identified by specific lat and long (update: now also by OBS_ID). Multiple nets are deployed in the same Lokal, but sometimes in different (but close) dates for the same year. If this is confirmed I would sum the number of perch per size class per Lokal per YEAR (to extract from Fisketdatum). To calculate CPUE, find out how to calculate effort at the same hierarchical level (i.e. number of nets * number of days per Lokal and year). Make sure zero catch are included, as they will influence the CPUE.

Given that I consider only Ansträngning = 1, i.e. 1 net per day or night, I count how many nets per lokal and year to extract the effort. Revise if consider other values of Ansträngning

- Redskapsdetaljnummer = mesh size (there are few mesh sizes that should not be included in this type of net [which include 10, 12, 15, 19, 24, 30, 38, 48, 60] so this is perhaps something to check. FANGST_ID = species within mesh section, OBS_ID = net.

What? should I exclude or include the other values? 

- age info for a subset of the current data is contained in the  "perch_length_age.csv". Back-calculated individual length-at-age for a subset of this latter is contained in "perch-growth-back-calculated.csv"

- exclude data "restricted", that can not be published. Also data collected within the invasive goby program, where the gear was the same but the methods may be different, i.e. nets not set out randomly.
Agnes will add a new variable.


Questions to Peter:

1) to calculate CPUE of species: if I recall correctly, when we discussed the data for the anadromous trout study (analyses still up and running), you told me that to obtain CPUE I had to divide the abundance of each species per station by the product of ansträngning (number of nets) and fisketid (number of days). However, in the dataset that I received, the value of ansträngning is often the same as the value of fisketid, so we assumed that fisketid was contained in ansträngning (often the value is 1, i.e. one day or night and 1 net). An exception is found for some values of Ansträngning that are equal to 35 or 10, where fisketid is 1 and Ansträngning_enhet is nätt*natt. How should I interpret these? 

Also, there are many cases where Ansträngning = NA but Fisketid 1. Is there any way to calculate CPUE or should I discard these records?

Sometimes Ansträngning_enhet = ..*timmarna. Can I convert the corresponding value of  Ansträngning in days, e.g. diving it by 14 for example?

2) I don't recall how bad is "godkänd" = NEJ...better to discard those records? yes

3) "Ok nd" under the variable "Art" is another version of "Okänd"?

4) langdrupp_langd is often but not always rounded to 0.5 cm. I guess people have used different methods (precision) to measure the length of fish. Do you know whether the values have been rounded up to *.5 cm or if values were recorded as the closest *.5 value?

####
Temperature data
###

Agnes extracted satellite measures (from Copericus) of daily temperatures for all the gillnet locations from 1982 to 2023. 
we don't know whether is daily avg or one time, how many time the satellite passed by, and at what time.
OBS: when there is ice the temp remain constant

- compare these values to the measured temperatures from the gillnet data to see how well they correspond. Not well. explore

- Ask also Ingrid Bergman about temp from loggers for all sites and years of the analyses, to compare. Done, waiting for a reply

- compare satellite temp values extracted from Agnes to the measured temperatures from the gillnet data to see how well they correspond. Also to temp log values. Use mayne Max approach to calculate more accurate estimates of temp. Matilda (Ultuna) working on a project with Anna G and Magnus H, might use Max method, check with her

- Check climwin R package to find the best temp data as covariate


###
Questions about detonation dataset:
###

- how to standardize by different gram of dynamite (hence area?)?
Only Gotland was sampled with 1g. Exclude it or recalculate it based on Göran and Ulf conversiuon factors. Wait for new data that are alsready scaled by the effort (where the most recent data should be added, plus Blekinge data, and Gotland maybe)
Update: I talked to Göran S. who said to use conversion factors from the table receive from Ulf listed under the sheet "Konvertering ny".About conversion of ansträngning that are not in the table, use:
1->0.94
12 and 13 -> 12.5
20->25
200: check. not possible. maybe typos
Since the methods changed so much over the year and different areas, no worries if the conversion factor are not exactly corresponding to the ansträngning, the errors/variation in the dataset are many and larger than this.

- bottom and yta: did they register zeros at the bottom? How to treat them, pool or not? Agnes has estimates of fish sinking at the bottom for three spp

they should always (when grams were 10gr) look at both bottom and surface except for Forsmark and Simpevarp (double check! which Formsakr, the biotest or outside??? because now I have not corrected the values of Forsmark - while I excluded biotest values)
If there is not bottom value is zeros but they did not record zeros. this happened for the last 15-20 years, before that however they only counted fish at the surface, and then there was a mixed method period. Göran calculate estimates to make data comparable for floating and sinking, Agnes will send me the info -> see file "detonation_old_scripts_for_conversion"

Update: after selecting obs were both floating and sinking were counted, I recalculated the conversion factors. These match well the ones previously calculated from Agnes with old data for perch and pike, but not for stsp. It seems as they were often not counted, even if perch and pike were counted. What to do: try to understand which obs included stsp counts in both surface and bottom, and use that data to calculate conversion factor (and understand which data need conversion). Agnes will send me etsimates of stsp from her model, for the detonation dataset amnd fot the gillnets dataset, we can compare the two. If they don't match well, I probably can not trust stsp data from detonation dataset. (OBS: Agnes stsp estimates will only cover 2001 onwards!)
Update: I give up. Even within the same location and year, how/if stsp were counted seem to avry with the date (day). It's hard to know when/where stsp have been counted properly, so I won't use these stsp data.

Update: About stsp unreliable count at the bottom and surface. Cross-check with the older file from Agnes where there is a column saying if they were counted at the surface or bottom or both, and see if you can make sense of the high variability. >Ulf says that is some cases (Västrebottom län, where the way of recording changed by day) the field guy was not reliable, but in other cases, such as Stockholm län in 2016-2018, the zeros are ok, as they sampled in very enclosed bays. It may be sensible however to use Agnes correction factor for stsp, but I still need to know where to apply it 

OBS: "provfiskad" is listed under the wrong variable, because the changed on the way where/how they input the data in the protocol. Agnes will send me a new version of the dataset with this extra column, so that I can include the provfiskad obs as well and figure out id fish were counted at the bottom or yta. Also, in the new file, which is now in Teams (16/01/2025, named the same as the old one), there is one or more column indicating spatial units, potentially useful



- exclude Simpevarp and Biotest Forsmark (but not "Forsmark"), as here water is almost 10 degrees warmer

- pool bottom and yta estimates? yes - but see above

- Consider only Godkänt yes and störning nej as for the gillnets? yes 

- How to filter out adults? Under Sortering values are NA, Årsyngel, and Juv/adult. Maybe consider only Årsyngel? I checked for all NA if size were measured, and could pool them into one of the cohort

0.1 means 0+, 99.9 means adults. The other values are actual measurements. We have size limits for spp to decide adults vs juv, but we need to adjust/correct ourselves

UPDATE: Agnes sent me a new version of the dataset, including comments and sub-location. Rerun all the scripts (I started and got to the point where I am checking the comments) and once I have a clean dataset, give an updated list of locations, sublocations and years to Ingrid (to whom I told now to wait for the detonation dataset)

- about the final datasets with lengths: use the site level replication (instead of lokal). Now it is 298 obs, it will increase after I get the provfiskad data), and use the number of fish measured as weight for the response variable

- Ulf has some extra data about perch length before 2014 (while in KUL length values are availablöe only from 2104). I should get this data from him and merge them, but check for duplicates






