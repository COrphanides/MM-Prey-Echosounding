

### FILENAME: Run.HB1303.transects.R

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

out1 = combo.f(cruise = 'HB1303', transect = '21', transect_date = '070413'); check.mm(out1) # "6 and 7"
out2 = combo.f(cruise = 'HB1303', transect = '22', transect_date = '070413'); check.mm(out2) # "2 and 8"
#Transect 22 east - too far off shelf 
# Transect 20 - Passive
# Transect 19 in July - Passive
out3 = combo.f(cruise = 'HB1303', transect = '19', transect_date = '081213'); check.mm(out3) # "7 and 9"
# Transect 18 - 7/5 - Passive
out4 = combo.f(cruise = 'HB1303', transect = '18', transect_date = '070813'); check.mm(out4) # "30 and 57"
out5 = combo.f(cruise = 'HB1303', transect = '18', transect_date = '081213'); check.mm(out5)  # 10 and 18" 
out6 = combo.f(cruise = 'HB1303', transect = '17', transect_date = '070813'); check.mm(out6)  # "15 and 44" 
out7 = combo.f(cruise = 'HB1303', transect = '17', transect_date = '081213'); check.mm(out7)  # "3 and 3" 
out8 = combo.f(cruise = 'HB1303', transect = '17', transect_date = '081513'); check.mm(out8)  # "1 and 3" 
out9 = combo.f(cruise = 'HB1303', transect = '16', transect_date = '070813'); check.mm(out9)  # "3 and 6"
# Transect 16 - July 9 - Passive; #Transect 16 - AUg 15 (below) - very small section good
out10 = combo.f(cruise = 'HB1303', transect = '16', transect_date = '081513'); check.mm(out10)  # "5 and 6" 
# Transect 15 - July 9 Passive
# Transect 15 - Aug 15 - Passive
# Transect 14 - July 9 - Passive
out11 = combo.f(cruise = 'HB1303', transect = '14', transect_date = '071013'); check.mm(out11)  #"5 and 7"
out12 = combo.f(cruise = 'HB1303', transect = '13', transect_date = '071013'); check.mm(out12)  # "9 and 15"
out13 = combo.f(cruise = 'HB1303', transect = '12', transect_date = '071213'); check.mm(out13)  # "4 and 6"

out14 = combo.f(cruise = 'HB1303', transect = '11', transect_date = '071213'); check.mm(out14)  # "8 and 13"
# Transect 10 -July 21 - Passive
# Transect 9 - July 21 - Passive; Transect 9 - July 22 - Only a small stretch, but not passive
out15 = combo.f(cruise = 'HB1303', transect = '09', transect_date = '072213'); check.mm(out15)  # "0 and 0"
out16 = combo.f(cruise = 'HB1303', transect = '09', transect_date = '080213'); check.mm(out16)  # "9 and 18"
out17 = combo.f(cruise = 'HB1303', transect = '08', transect_date = '072213'); check.mm(out17)  # "1 and 1"
out18 = combo.f(cruise = 'HB1303', transect = '08', transect_date = '080213'); check.mm(out18)  # "4 and 6"
# Transect 7 - July 18 - Passive
# Transect 7 - Aug 3 - Passive; Transect 7 - Aug 2 - only a little bit of time
out19 = combo.f(cruise = 'HB1303', transect = '07', transect_date = '080213'); check.mm(out19)  # "0 and 1"
# Transect 6 - Aug3 - Passive
out20 = combo.f(cruise = 'HB1303', transect = '06', transect_date = '080413'); check.mm(out20)  # "7 and 11"
# Transect 6 - Aug 17 - No echosounding data corresponding with effort
out21 = combo.f(cruise = 'HB1303', transect = '04', transect_date = '080413'); check.mm(out21)  # "4 and 8"
out22 = combo.f(cruise = 'HB1303', transect = '04', transect_date = '080813'); check.mm(out22)  # "26 and 51"
out23 = combo.f(cruise = 'HB1303', transect = '03', transect_date = '080813'); check.mm(out23)  # "38 and 69"
out24 = combo.f(cruise = 'HB1303', transect = '02', transect_date = '080813'); check.mm(out24)  # "52 and 80"
# Transect 1 - Aug 7 - Passive
out25 = combo.f(cruise = 'HB1303', transect = '63', transect_date = '080413'); check.mm(out25)  # "18 and 19"
# Transect  72 - Aug 17 - Passive


#### UGH --- will need to align columns, they may all not have the same number of columns if it was a really shallow transect for example
####         it will not have the deeper values included!!!!!!!!!!11


library(dplyr)
View(bind_rows(out18, out19))
all = bind_rows(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20,
                 out21, out22, out23, out24, out25)

# Warning messages:
#   1: In bind_rows_(x, .id) :
#   Vectorizing 'Interval' elements may not preserve their attributes
# 2: In bind_rows_(x, .id) :
#   Vectorizing 'Interval' elements may not preserve their attributes

# test = rbind(out1, out2, out3, out4, out5, out6, out7, out8, out9, out10, out11, out12, out13, out14, out15, out16, out17, out18, out19, out20,
#            out21, out22, out23, out24, out25)

write.csv(all, file= 'C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\combined\\HB1303.base.all.04.23.21.csv')   #
saveRDS(all, file='C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\combined\\HB1303.base.all.04.23.21.rds')




