R scripts of analyses for FORCE project - Serena Donadi

###
Questions about gillnets dataset:
###

- how to calculate CPUE? in a previous project where I used data from KUL, Peter told me that I should divide the abundance per station (and spp) by the product of ansträngning (number of nets) and fisketid (number of days). Also, about fisketid: dygn and natt are not much different, as they don't report the time, so no need to keep them separated. However, I miss fisketid in the current dataset. We thought to start by considering only Ansträngning = 1, but if this is the number of nets rather than the effort, we still need the number of days (or nights, no difference). Check also what to do with Ansträngning_enhet = ..*timmarna.
Check and implement script for extraction or check with Agnes

- it would be good to have temp at the time of fishing (check values as before I found 999, which are NAs), to account for potential artifact (i.e. different catchability with temp) by regressing residual variation after accounting for effects of avg year temp (and interactions)

- the gear have different catchability, check that current data are collected consistently

- shall I consider only data GODkänd yes?

- Do we need zeros? Yes, they will influence CPUE, so check that zeros are included. Before I found "inge_fångst" written for each station and date

- age info for a subset of the current data is contained in the  "perch_length_age.csv". Backcalculated individual length-at-age for a subset of this latter is contained in "perch-growth-back-calculated.csv"

- What's LANGDGRUPP_ANTAL vs ANTAL? Antal is the overall catch of a net, LANGDGRUPP_ANTAL is the  number per size class (what I want)

- langdrupp_langd is often but not always rounded to 0.5 cm. Consider size classes?

- HIERARCHY (TO BE CONFIRMED): I believe that FANGD_ID refer to different mesh size (section) of the same net, which is identified by specific lat and long. Multiple nets are deployed in the same Lokal, but sometimes in different (but close) dates. If this is confirmed I would sum the number of perch per size class per Lokal per YEAR (to extract from Fisketdatum). To calculate CPUE, find out how to calculate effort at the same hierarchical level (i.e. number of nets * number of days per Lokal and year). Make sure zero catch are included, as they will influence the CPUE.


Agnes check with Matilda, otherwise contact Peter L.

Agnes reply:
1) I did speak to Matilda and she did not know the answer but agreed that for “timmar” it probably makes sense to translate this into nätter/dygn (e.g. 14 hrs = en natt). We might want to confirm this with Peter though, and alert him to the occasions where there seems to have been a mix-up with the units. I think that the Fisketid is already included in the Ansträngning (it is time*net). I added Fisketid so you can have a look at it yourself, but they are usually the same (since we’re always dealing with one net at the time), except for some of those odd ones where it says e.g. Anstränging = 35, and enhet is e.g. nät*natt (not nät*timme).
2)    I have added temperature and also depth.
3)    I would also have expected “_ingen fångst”. Went back and realised I made a mistake in the merging of the catch dataset and the length dataset, as FANGST_ID is NA if there is _ingen fångst. I have now fixed this.
4)    Again, I spoke to Matilda and she did not know but thought, like we did, that the sizes that were not .5 were probably collected by some other programme that used more precise measurements (so these could probably be sorted into the same .5-categories). Again, we might want to confirm this with Peter.
5)    Yes you’re right. I’ve added a few more columns to clarify the structure. Redskapsdetaljnummer = mesh size (there are few mesh sizes that should not be included in this type of net [which include 10, 12, 15, 19, 24, 30, 38, 48, 60] so this is perhaps something to check. FANGST_ID = species within mesh section, OBS_ID = net.
 

###
Questions about detonation dataset:
###

- how to standardize by different gram of dynamite (hence area?)?

- bottom and yta: did they register zeros at the bottom? How to treat them, pool or not? Agnes has estimates of fish sinking at the bottom for three spp

- what is provfiskad?

- Consider only Godkänt yes and störning nej as for the gillnets?

- How to filter out adults? Under Sortering values are NA, Årsyngel, and Juv/adult. Maybe consider only Årsyngel?
  
check with Ronny, meeting set in August
