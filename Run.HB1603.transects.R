

### FILENAME: Run.HB1603.transects.R

### Create program to run function (turn lastest script into new function - provide output)
### Run one at a time for now, check input and output bathymetry first
###  (Once get through first round of this, could use list.files on csv data  
###   input to get all unique transect-date combos and loop through to run)


# Remove everything but functions
rm(list = setdiff(ls(), lsf.str()))


###############################################
### Odd warnings that should be ok:

# lubridate message provided the first time it runs. Should be ok, just a warning about working with lubridate periods
# Note: method with signature 'Period#ANY' chosen for function '-',
# target signature 'Period#Period'.
# "ANY#Period" would also be valid

# Warning message such as: 22 failed to parse
# warning message is ok. It has to do with some missing depth values, but plenty of good values for depth data

# For when there are no marine mammals... Copy mm dataset, take first row, make all NA, copy rows to match existing dataset & merge

###############################################

# Function to quickly check marine mammal output per transect

check.mm = function(data=out1) {
  sightings = sum(!is.na(data$CUMUSIGHTID))
  paCount = sum(ceiling(data$density), na.rm=T)
  print(paste("sightings with a proper density estimate and without"))
  print(paste(paCount, sightings, sep=" and "))
}

out1 = combo.f(cruise = 'HB1603', transect = '18', transect_date = '070316'); check.mm(out1) # "27 and 59"
out2 = combo.f(cruise = 'HB1603', transect = '17', transect_date = '070316'); check.mm(out2) # "26 and 51"
out3 = combo.f(cruise = 'HB1603', transect = '16', transect_date = '062916'); check.mm(out3) # "7 and 13"

out4 = combo.f(cruise = 'HB1603', transect = '15', transect_date = '062816'); check.mm(out4) # "31 and 53"
out5 = combo.f(cruise = 'HB1603', transect = '15', transect_date = '062916'); check.mm(out5) # "0 and 0"

out6 = combo.f(cruise = 'HB1603', transect = '14', transect_date = '062816'); check.mm(out6) # "29 and 58"

out7 = combo.f(cruise = 'HB1603', transect = '13', transect_date = '071916'); check.mm(out7) # "5 and 9"
out8 = combo.f(cruise = 'HB1603', transect = '12', transect_date = '071916'); check.mm(out8) # "3 and 7"

out9 = combo.f(cruise = 'HB1603', transect = '09', transect_date = '072116'); check.mm(out9) # "19 and 32"
out10 = combo.f(cruise = 'HB1603', transect = '08', transect_date = '072116'); check.mm(out10) # "86 and 122"   
out11 = combo.f(cruise = 'HB1603', transect = '07', transect_date = '072116'); check.mm(out11) # "110 and 135"

out12 = combo.f(cruise = 'HB1603', transect = '04', transect_date = '072516'); check.mm(out12) # "41 and 66"
out13 = combo.f(cruise = 'HB1603', transect = '03', transect_date = '072516'); check.mm(out13) # "58 and 86"
out14 = combo.f(cruise = 'HB1603', transect = '02', transect_date = '072516'); check.mm(out14) # "10 and 19"

out15 = combo.f(cruise = 'HB1603', transect = '01', transect_date = '072716'); check.mm(out15) # "8 and 17"

out16 = combo.f(cruise = 'HB1603', transect = '116', transect_date = '081616'); check.mm(out16) # "6 and 40"
out17 = combo.f(cruise = 'HB1603', transect = '116', transect_date = '080216'); check.mm(out17) # "2 and 3" <<<<< Re-did file. 

out18 = combo.f(cruise = 'HB1603', transect = '117', transect_date = '081616'); check.mm(out18) # "18 and 43"

out19 = combo.f(cruise = 'HB1603', transect = '118', transect_date = '080216'); check.mm(out19) # "1 and 1"
# out20 = combo.f(cruise = 'HB1603', transect = '118', transect_date = '082316'); check.mm(out20) # This one is passive

out20 = combo.f(cruise = 'HB1603', transect = '119', transect_date = '082316'); check.mm(out20) # "2 and 4"




## Depending on depths etc, these outputs may not each have the same number of 
## columns, so used dplyr bind_rows to retain all potential columns

library(dplyr)
View(bind_rows(out18, out19))
all = bind_rows(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20)

# Warning messages:
#   1: In bind_rows_(x, .id) :
#   Vectorizing 'Interval' elements may not preserve their attributes
# 2: In bind_rows_(x, .id) :
#   Vectorizing 'Interval' elements may not preserve their attributes

# test = rbind(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20,
#            out21)

length(all) #1078
# Some duplicate dts and ints created, not going to chase them down now, not worth it... remove duplicated data (that has a new name)
all[, duplicated(colnames(all))]
all  = select(all, -(starts_with("dt..")))
all  = select(all, -(starts_with("int..")))
length(all) # 1074


write.csv(all, file= 'C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\combined\\HB1603.base.all.04.23.21.csv')   #
saveRDS(all, file='C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\combined\\HB1603.base.all.04.23.21.rds')




