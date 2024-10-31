R scripts of analyses for FORCE project - Serena Donadi

###
Questions about gillnets dataset:
###

- how to calculate CPUE? in a previous project where I used data from KUL, Peter told me that I should divide the abundance per station (and spp) by the product of ansträngning (number of nets) and fisketid (number of days). Also, about fisketid: dygn and natt are not much different, as they don't report the exact start and end time, so no need to keep them separated. However, I don't have fisketid in the current dataset. We thought to start by considering only Ansträngning = 1, but if this is the number of nets rather than the effort, we still need the number of days (or nights, no difference). Check also what to do with Ansträngning_enhet = ..*timmarna.

Update1: I have received a new file with fisketid. It has always the same value of Ansträngning, so we assume that the number of days is already contained in Ansträngning, and they are the same as the net is 1. An exception is found for some values of Ansträngning equal to 35 or 10, where fisketid is 1 and Ansträngning_enhet is nätt*natt. To be on the safe side, until I verify with Peter L., I consider only Ansträngning = 1.
Check also with P why there are Ansträngning = NA but Fisketid 1 (che sarebbero papabili)

Remove all ansträngning that are not 1. Maybe save same after Agnes send me new info from Matilda

Update2: after talking to Peter, Matilda, Ronny, Agnes suggests to remove fishing where effort is expressed in hours, correct some typos, remove weird fishing with Ansträngning = 2 (see cleaning script) - check that all issues mentioned above are solved (e.g. do we still have Ansträngning equal to 35 or 10?)

- it would be good to have temp at the time of fishing (check values as before I found 999, which are NAs), to account for potential artifact (i.e. different catchability with temp) by regressing residual variation after accounting for effects of avg year temp (and interactions)

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

Antal is the overall catch of a net, LANGDGRUPP_ANTAL is the  number per size class (what I want)

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


###
Questions to Peter:
###

1) to calculate CPUE of species: if I recall correctly, when we discussed the data for the anadromous trout study (analyses still up and running), you told me that to obtain CPUE I had to divide the abundance of each species per station by the product of ansträngning (number of nets) and fisketid (number of days). However, in the dataset that I received, the value of ansträngning is often the same as the value of fisketid, so we assumed that fisketid was contained in ansträngning (often the value is 1, i.e. one day or night and 1 net). An exception is found for some values of Ansträngning that are equal to 35 or 10, where fisketid is 1 and Ansträngning_enhet is nätt*natt. How should I interpret these? 

Also, there are many cases where Ansträngning = NA but Fisketid 1. Is there any way to calculate CPUE or should I discard these records?

Sometimes Ansträngning_enhet = ..*timmarna. Can I convert the corresponding value of  Ansträngning in days, e.g. diving it by 14 for example?

2) I don't recall how bad is "godkänd" = NEJ...better to discard those records? yes

3) "Ok nd" under the variable "Art" is another version of "Okänd"?

4) langdrupp_langd is often but not always rounded to 0.5 cm. I guess people have used different methods (precision) to measure the length of fish. Do you know whether the values have been rounded up to *.5 cm or if values were recorded as the closest *.5 value?

###
Questions about detonation dataset:
###

- how to standardize by different gram of dynamite (hence area?)?
Only Gotland was sampled with 1g. Exclude it or recalculate it based on Göran and Ulf conversiuon factors. Wait for new data that are alsready scaled by the effort (where the most recent data should be added, plus Blekinge data, and Gotland maybe)

- bottom and yta: did they register zeros at the bottom? How to treat them, pool or not? Agnes has estimates of fish sinking at the bottom for three spp

they should always (when grams were 10gr) look at both bottom and surface except for Forsmark and Simpevarp
If there is not bottom value is zeros but they did not record zeros. this happened for the last 15-20 years, before that however they only counted fish at the surface, and then there was a mixed method period. Göran calculate estimates to make data comparable for floating and sinking, Agnes will send me the info

- exclude Simpevarp and Biotest Forsmark (but not "Forsmark"), as here water is almost 10 degrees warmer

- pool bottom and yta estimates? yes - but see above

- Consider only Godkänt yes and störning nej as for the gillnets? yes - wait for cleaned dataset

- How to filter out adults? Under Sortering values are NA, Årsyngel, and Juv/adult. Maybe consider only Årsyngel?

0.1 means 0+, 99.9 means adults. The other values are actual measurements. We have size limits for spp to decide adults vs juv, but we need to adjust/correct ourselves
  
