R scripts of analyses for FORCE project - Serena Donadi

###
Questions about gillnets dataset:
###

- how to calculate CPUE? in a previous project where I used data from KUL, Peter told me that I should divide the abundance per station (and spp) by the product of ansträngning (number of nets) and fisketid (number of days). Also, about fisketid: dygn and natt are not much different, as they don't report the time, so no need to keep them separated. However, I miss fisketid in the current dataset. We thought to start by looking considering only Ansträngning = 1, but if this is the number of nets rather than the effort, we still need the number of days (or nights, no difference). Check also what to do with Ansträngning_enhet = ..*timmarna.
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

###
Questions about detonation dataset:
###

- how to standardized by different gram of dynamite (hence area?)?

- bottom and yta: did they register zeros at the bottom? How to treat them, pool or not? Agnes has estimates of fish sinking at the bottom for three spp

- what is provfiskad?

- Consider only Godkänt yes and störning nej as for the gillnets?

- How to filter our adults? Under Sortering values are NA, Årsyngel, and Juv/adult. Maybe consider only Årsyngel?
check with Ronny, meeting set in August
