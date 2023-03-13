

### FILENAME: Run.HB1103.transects.R

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

### out2 DIDN'T WORK!! - CHECK!!!!

out1 = combo.f(cruise = 'HB1103', transect = '21', transect_date = '060811'); check.mm(out1) # "0 and 0" <<< Fixed transducers
out2 = combo.f(cruise = 'HB1103', transect = '21', transect_date = '061611'); check.mm(out2) # "13 and 19"   <<< Fixed transducers
out3 = combo.f(cruise = 'HB1103', transect = '21', transect_date = '061811'); check.mm(out3) # "5 and 7"

out4 = combo.f(cruise = 'HB1103', transect = '20', transect_date = '061611'); check.mm(out4) # "15 and 21"

out5 = combo.f(cruise = 'HB1103', transect = '19', transect_date = '061611'); check.mm(out5) # "10 and 14"

out6 = combo.f(cruise = 'HB1103', transect = '18', transect_date = '061411'); check.mm(out6) # "3 and 3" 

out7 = combo.f(cruise = 'HB1103', transect = '17', transect_date = '061411'); check.mm(out7) # "10 and 15"
out8 = combo.f(cruise = 'HB1103', transect = '17', transect_date = '062911'); check.mm(out8) # "2 and 5"

out9 = combo.f(cruise = 'HB1103', transect = '16', transect_date = '060611'); check.mm(out9) # "12 and 22"
out10 = combo.f(cruise = 'HB1103', transect = '16', transect_date = '062911'); check.mm(out10) # "13 and 16"

out11 = combo.f(cruise = 'HB1103', transect = '15', transect_date = '060611'); check.mm(out11) # "4 and 10"
out12 = combo.f(cruise = 'HB1103', transect = '15', transect_date = '062911'); check.mm(out12) # "4 and 5"

out13 = combo.f(cruise = 'HB1103', transect = '14', transect_date = '060611'); check.mm(out13) # "5 and 8"

out14 = combo.f(cruise = 'HB1103', transect = '11', transect_date = '060411'); check.mm(out14) # "14 and 29"
out15 = combo.f(cruise = 'HB1103', transect = '11', transect_date = '072211'); check.mm(out15) # "3 and 3"

out16 = combo.f(cruise = 'HB1103', transect = '10', transect_date = '060411'); check.mm(out16) # "14 and 65"

out17 = combo.f(cruise = 'HB1103', transect = '08', transect_date = '061211'); check.mm(out17) # "2 and 2"

out18 = combo.f(cruise = 'HB1103', transect = '07', transect_date = '061211'); check.mm(out18) # "1 and 1"
out19 = combo.f(cruise = 'HB1103', transect = '07', transect_date = '073011'); check.mm(out19) # "8 and 8"

out20 = combo.f(cruise = 'HB1103', transect = '04', transect_date = '072911'); check.mm(out20) # "11 and 19"

out21 = combo.f(cruise = 'HB1103', transect = '03', transect_date = '072811'); check.mm(out21) # "8 and 22"
out22 = combo.f(cruise = 'HB1103', transect = '03', transect_date = '072911'); check.mm(out22) # "30 and 43"

out23 = combo.f(cruise = 'HB1103', transect = '02', transect_date = '072811'); check.mm(out23) # "12 and 17"

out24 = combo.f(cruise = 'HB1103', transect = '01', transect_date = '072811'); check.mm(out24) # "1 and 4"


## Depending on depths etc, these outputs may not each have the same number of 
## columns, so used dplyr bind_rows to retain all potential columns


library(dplyr)
View(bind_rows(out18, out19))
# all = bind_rows(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20,
#                  out21, out22, out23, out24)
all = bind_rows(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20,
                        out21, out22, out23, out24)

length(all) #1078
# Some duplicate dts and ints created, not going to chase them down now, not worth it... remove duplicated data (that has a new name)
all[, duplicated(colnames(all))]
all  = select(all, -(starts_with("dt..")))
all  = select(all, -(starts_with("int..")))
length(all) #1074

write.csv(all, file= 'C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\combined\\HB1103.base.all.04.23.21.csv')   #
saveRDS(all, file='C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\combined\\HB1103.base.all.04.23.21.rds')




