# FORCE_Analyses
R scripts of analyses for FORCE project - Serena Donadi

Questions about gillnets dataset:

- how to calculate CPUE? in a previous project where I used data from KUL, Petr told me that I should divide the abundance per station (and spp) by the product of ansträngning (number of nets) and fisketid (number of days). Also, about fisketid: dygn and natt are not much different, as they don't report the time, so no need to keep them separated. However, I miss fisketid in the current dataset. Check and implement script for extraction or check with Agnes

- it would be good to have temp at the time of fishing (check values as before I found 999, which are NAs), to account for potential artifact (i.e. different catchability with temp) by regressing residual variation after accounting for effects of avg year temp (and interactions)

- the gear have different catchability, check that current data are collected consistently

- shall I consider only data GODkänd yes?

- Do we need zeros? If so, check that zeros are included. Before I found "inge_fångst" written for each station and date
