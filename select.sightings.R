
### FILENAME: select.sightings.R
### PURPOSE: Trim sightings data to match acoustic data processed
###############################################################################


# Start fresh
rm(list=ls())

# Load packages
library(lubridate)
library(dplyr)
library(zoo)

#######################################

# 1) Collect complete echosounding datafiles 

# just collect one file per transect, so select the same file everywhere
# example file: HB1603_transect01_072716_0050x01000_018kHz_RawNS_CellExport

hb11.files = list.files(path = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\50x1000\\", 
                        pattern = "018kHz_RawNS_CellExport", include.dirs = TRUE, full.names=TRUE)

hb13.files = list.files(path = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data\\50x1000\\", 
                        pattern = "018kHz_RawNS_CellExport", include.dirs = TRUE, full.names=TRUE)


hb16.files = list.files(path = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\50x1000\\", 
                        pattern = "018kHz_RawNS_CellExport", include.dirs = TRUE, full.names=TRUE)

hb.files = c(hb11.files, hb13.files, hb16.files)

#######################################



for ( k in 1:length(hb.files)){
  mfi.cells = read.csv(hb.files[k], header= T)
  (current_file = hb.files[k]) #print current filename
  
  # Add cruise and transect columns
  bname = basename(hb.files[k])
  cruise = substr(bname, 1,6)
  
  mfi.cells$cruise = substr(bname, 1,6)
  
  if (cruise == "HB1103") { 
    mfi.cells$transect = substr(bname, 17,18)
    mfi.cells$transect.date = substr(bname, 20,25)
    } else if (cruise == "HB1303"){
      mfi.cells$transect = substr(bname, 16,17)
      mfi.cells$transect.date = substr(bname, 19,24) 
      #within HB1603, adjust for the possibilty of 3 character transect names
      } else if (cruise == "HB1603" & substr(bname, 18,18) == "_") {
        mfi.cells$transect = substr(bname, 16,17)
        mfi.cells$transect.date = substr(bname, 19,24) 
        } else if (cruise == "HB1603" & substr(bname, 18,18) != "_") {
          mfi.cells$transect = substr(bname, 16,18)
          mfi.cells$transect.date = substr(bname, 20,25) 
          } else {
            print("something f-ed up")
          }
  
  if (mfi.cells$cruise == "HB1303" & mfi.cells$transect == "19"){
    print("check from here")
  }
  
  # Create date-time variables from acoustic data
  mfi.cells$dt = ymd_hms(paste(trimws(as.character(mfi.cells$Date_S)), trimws(as.character(mfi.cells$Time_S)), sep=":"))
  mfi.cells$dt.e = ymd_hms(paste(trimws(as.character(mfi.cells$Date_E)), trimws(as.character(mfi.cells$Time_E)), sep=":"))
  
  # Select variables to carry over/keep... in future iterations add more acoustic variables 
  mfi.data.ac.tempA = mfi.cells[ ,c("Interval", "Layer", 
                                    "cruise", "transect", "transect.date", 
                                    "Lat_M", "Lon_M", "dt", "dt.e")]
  mfi.data.ac.tempB = reshape(mfi.data.ac.tempA, idvar = "dt", timevar = "Layer", direction = "wide")
  #View(mfi.data.ac.tempB)
  
  # clean up and delete some column names. Adjust if not all depth categories are present
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.e.1'] = 'dt.e'
  # if("dt.e.2" %in% colnames(mfi.data.ac.tempB)) {
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.e.")))
  
  #Clean up Interval variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Interval.1'] = 'Interval'
  mfi.data.ac.tempB<- mfi.data.ac.tempB %>% select("Interval", everything()) # Move interval to 1st column
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Interval.")))
  
  # clean up Lat_M and Lon_M
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lat_M.1'] = 'Lat_M'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lon_M.1'] = 'Lon_M'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lat_M.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lon_M.")))
  
  # clean up cruise
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'cruise.1'] = 'cruise'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'cruise.1'] = 'cruise'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("cruise.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("cruise.")))
  
  # clean up transect.date
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'transect.date.1'] = 'transect_date'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'transect.date.1'] = 'transect_date'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("transect.date.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("transect.date.")))
  
  # clean up transect
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'transect.1'] = 'transect'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'transect.1'] = 'transect'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("transect.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("transect.")))
  

  ##########################################################################
  # Create bottom file here??
  
  # Follow example like in Process.Data4.R
  # Only need to get time difference on first pass for each transect, assume  
  #  time difference doesn't change across one transect
  # 1) Read in bottom files
  # 2) Get time, take difference and apply (read in depth and slope while at it?)
  # NOTES: set up if statement for year (2016 doesn't need any adjustment)
  
  mfi.data.ac.tempB$int = interval(mfi.data.ac.tempB$dt, mfi.data.ac.tempB$dt.e)
  
  # Kept 2016 in here to add bottom data, but setting time "diff" to zero, no need for an offset
  
    #if (cruise == "HB1303" | cruise == "HB1103") {
      ##  Set up bottom file
      if (cruise == "HB1303") { 
        setwd("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csv\\") 
        path.btm.csv = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csv\\" }
      if (cruise == "HB1103") { 
        setwd("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Bottom Lines\\csv\\") 
        path.btm.csv = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Bottom Lines\\csv\\" }
      if (cruise == "HB1603") { 
        setwd("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Bottom Lines\\csv\\") 
        path.btm.csv = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Bottom Lines\\csv\\" }
      transect = mfi.data.ac.tempB$transect[1]
      transect_date = mfi.data.ac.tempB$transect_date[1]
      # accidentally added an additional "_" for 2011 file names
      if (cruise == "HB1103") {bottom.file = list.files(pattern = paste("bottom_t_", transect, '_', transect_date, ".line.csv", sep =''))}
      if (cruise == "HB1303" | cruise == "HB1603") {bottom.file = list.files(pattern = paste("bottom_t", transect, '_', transect_date, ".line.csv", sep =''))}
      print(paste(cruise, transect, transect_date, sep =" "))
      bottom = read.csv(paste(path.btm.csv, bottom.file, sep=""), header= T)
      names(bottom)[1]<-"Ping_date"
      
      if (cruise == "HB1603" & transect == "18"){
        print("check from here")
      }
      
      
      if (cruise == "HB1103" & transect == "04" & transect_date == "072911"){
        print("check from here")
      }
      
      
      #bottom$GPS_UTC_time_hms = hms(bottom$GPS_UTC_time, origin = lubridate::origin, tz = "UTC")
      bottom$GPS_UTC_time_hms = hms(bottom$GPS_UTC_time)
      bottom$GPS_UTC_time_hms = hms::as_hms(bottom$GPS_UTC_time)
      #bottom$GPS_UTC_time_hms = force_tz(bottom$GPS_UTC_time_hms, tzone = "UTC")
      
      
   

      bottom$dt = ymd_hms(paste(trimws(as.character(bottom$Ping_date)), trimws(as.character(bottom$Ping_time)), sep=":"))
      
      
      ## Add depth variables to acoustic data... shouldn't need this here, but set it up so can copy to Process file
      # Use GPS_UTC_time because that matches with sightings data
      bottom$GPS_dt = ymd_hms(paste(bottom$Ping_date, bottom$GPS_UTC_time, sep = ''))
      # Gives Warning message. About 1%  of GPS times are missing (in this test dataset), results in NA values.
      # I am with that, OK if those are dropped, will have almost no impact
      #bottom$GPS_dt = force_tz(bottom$dt, tzone = "UTC") #set it to UTC
      
      
    
    
      bottom$GPS_dt = force_tz(bottom$GPS_dt, tzone = "UTC") #set it to UTC
      mfi.data.ac.tempB$bt_mean = NA
      mfi.data.ac.tempB$bt_sd = NA
      mfi.data.ac.tempB$bt_slope = NA
      
      
      for (indx in 1:nrow(mfi.data.ac.tempB)){
        bt_samp = bottom[which(bottom$GPS_dt >= int_start(mfi.data.ac.tempB$int[indx]) &  bottom$GPS_dt <= int_end(mfi.data.ac.tempB$int[indx])), "Depth"]
        if (length(bt_samp)!=0) {
          mfi.data.ac.tempB$bt_mean[indx] = mean(bt_samp)
          mfi.data.ac.tempB$bt_sd[indx] = sd(bt_samp)
          mfi.data.ac.tempB$bt_slope[indx] = (bt_samp[1] - bt_samp[length(bt_samp)])/1000
        }
      }
      
      ## STILL NEED TO ADJUST FOR TIME SHIFT - GPS VS COMPUTER
      # ping/acoustic time is faster than marine mammal recording time FOR 2013.
      # Move mm time forward to match acoustic time
      # Difference between GPS & computer time same throughout the transect, so can use 1st good record
      sub_good = which(bottom$GPS_UTC_time != "-1")[1]  # Make sure have a  good time, not missing.
      mm_t = seconds(hms(substr(bottom$GPS_dt[sub_good], 12,20)))
      # ac_t = seconds(hms(bottom$GPS_UTC_time[sub_good]))
      # ac_t = seconds(hms(bottom$dt[sub_good]))
      ac_t = seconds(hms(substr(bottom$dt[sub_good], 12,20)))
      diff = mm_t - ac_t #total seconds between two times
      
      if (cruise == "HB1603") { diff = seconds(0)}
      
      # Keep "diff" value for checking later
      mfi.data.ac.tempB$diff = diff
      
      mfi.data.ac.tempB$dt = mfi.data.ac.tempB$dt + diff  
      mfi.data.ac.tempB$dt.e = mfi.data.ac.tempB$dt.e + diff
      mfi.data.ac.tempB$int = interval(mfi.data.ac.tempB$dt, mfi.data.ac.tempB$dt.e)
      
      
   # }
  
  
  #########################################################################
  
  
  if (k==1) {mfi.data.ac.50 = mfi.data.ac.tempB}
  
  if (k>1) {
    # Only add on new columns with measures of acoustic returns
    #dropvars <- names(mfi.data.ac.tempB) %in% c("Interval", "dt", "Lat_M", "Lon_M", "dt.e")
    #mfi.data.ac.tempB <- mfi.data.ac.tempB[!dropvars]
    mfi.data.ac.50 = rbind(mfi.data.ac.50, mfi.data.ac.tempB)
  }
  
  
  
}  # for ( k in 1:length(hb.files))


#####################


## Make date-time interval
# mfi.data.ac.50$int = interval(mfi.data.ac.50$dt, mfi.data.ac$dt.e)
# rename
mfi.data.ac = mfi.data.ac.50




# 2) Read a marine mammal sightings data for all years

# first read distance dataset
hb1103 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1103/HB1103distoutforChris.csv")
hb1303 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1303/HB1303 sightings/HB1303distoutforChris.csv")
hb1603 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1603/HB1603distoutforChris.csv")
# 
hb = rbind(hb1103, hb1303, hb1603)
# write.csv(hb, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\HB1103-HB1303-HB1603_distoutforChris_rbind.csv")

mm.11 = read.csv("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\sightings\\hb1103_sightings&effort.csv")
mm.13 = read.csv("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\HB1303 sightings\\hb1303_sightings&effort_v2.csv")
mm.16 = read.csv("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Sightings\\hb1603_sightings&effort.csv")
mm = rbind(mm.11, mm.13, mm.16)


# 2) Set up marine mammal data... do it for each year
#    (copied below is stuff for 2013, do/adjust for all years)



####################################################################################################
## Add Marine mammal data
#mm = read.csv(mm.data)

#Before used SIGHTDATE and converted to UTC. However, DATETIMEUTC is the same 
# thing, but already in UTC. Also SIGHTDATE was dropped in 2016. So, use DATETIMEUTC
# for all and just convert it to proper date format

mm$DATETIMEUTC.dt = dmy_hms(as.character(mm$DATETIMEUTC))
mm = mm[order(mm$DATETIMEUTC.dt),]
mm$dt = mm$DATETIMEUTC.dt
# mm$dt = force_tz(mm$dt, tzone = "EDT") # sightings data is in EDT, lubridate had assumed UTC. Force to EDT
# Had it as EST, but that doesn't incorporate daylight savings time
mm$dt = with_tz(mm$dt, tzone = "UTC")
names(mm)


# limit columns to join
mm.t = mm[c("ID", "SPEED", "TEAM", "SIGHT_TYPE", "SIGHTNUM", "CUMUSIGHTID", "USEDINEST", "SIGHTLAT", "SIGHTLON", "SWIMDIR", "PERPDIST", "RADIALDIST", "BEARING", "SIZEBEST","dt", "BEHAVIOR", "COMNAME", "NESPP4", "transect_eff", "SST", "DEPTH", "SALINITY",  "leg_eff")]

# Remove records not used in estimate (USEDINEST)
nrow(mm.t) #10254
#mm.t = mm.t[mm.t$USEDINEST == 'yes',]
nrow(mm.t) #8945

# Limit mm data to one sighting id. Sort by radial distance to get closest record to keep
mm.t = mm.t[order(mm.t$CUMUSIGHTID, -mm.t$RADIALDIST),]

# keep last unique RADIALDIST
sum(duplicated(mm.t$RADIALDIST)) # 8553

mm.t = mm.t[ !duplicated(mm.t[, c("CUMUSIGHTID")], fromLast=T),]
sum(duplicated(mm.t$CUMUSIGHTID)) # 0
nrow(mm.t) # 5669
#View(mm.t)



## Two objectives below, depending on if want dataset for p(a) distance analysis, 
## or if want dataset for main combo program linking acoustic & marine mammal data
## Much of this code will presumably be copied over to ProcessData4.R



# Read in Distance dataset, trim variables, 
#  remove duplicate sightings, and merge/replace SIZEBEST with one from above mm dataset
#hb.dist = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1303/HB1303 sightings/HB1303distoutforChris.csv")
hb.dist = hb
table(hb.dist$USEDINEST)
nrow(hb.dist) #13132
# hb.dist = hb.dist[hb.dist$USEDINEST == 'yes',]
nrow(hb.dist) # 11482
# limit columns
# No keep columns
#hb.dist = hb.dist[c("TRIPID", "CUMUSIGHTID", "SIZEBEST", "SPECIES")]
hb.dist = hb.dist[c("TRIPID", "CUMUSIGHTID", "SIZEBEST", "SPECIES")]
nrow(hb.dist) #11482
# don't want to do this (distinct with above limited variables) if using for the distance dataset
hb.dist = distinct(hb.dist)
nrow(hb.dist) #5480

nrow(mm.t) # 5669
mm.t = left_join(mm.t, hb.dist, by = "CUMUSIGHTID")
nrow(mm.t) #5669
summary(mm.t$SIZEBEST.y) #194 NAs
summary(hb.dist$SIZEBEST) # 5 NA
summary(mm.t$SIZEBEST.x) # 4 NAs
tapply(mm.t$SIZEBEST.y, mm.t$COMNAME, summary)
# remove NAs, many in NK species, otherwise mostly handful of animals per species
# except for sperm whale which has 5
mm.t = mm.t[!is.na(mm.t$SIZEBEST.y),]
nrow(mm.t) #5475
# Drop if missing in distance dataset
mm.t = mm.t[!is.na(mm.t$SIZEBEST.y),]
nrow(mm.t) #5475
# gt rid of sizebest.x and rename sizebest.y
drop = names(mm.t) %in% c("SIZEBEST.x")
mm.t = mm.t[!drop]
names(mm.t)[names(mm.t) == "SIZEBEST.y"] <- "SIZEBEST"



# Get Angle of sighting
mm.t$rel.direction = "forward"
for (l in 1:nrow(mm.t)) {
  if (!is.na(mm.t$BEARING[l])) {
    if (mm.t$BEARING[l] <= 90) {mm.t$angle[l] = mm.t$BEARING[l]}
    if (mm.t$BEARING[l] >= 270) {mm.t$angle[l] = 360 - mm.t$BEARING[l]}
    # Are a handful between 90 * 270
    if (mm.t$BEARING[l] > 90 & mm.t$BEARING[l] < 270) {
      mm.t$rel.direction[l] = "backward"
      if (mm.t$BEARING[l] > 90 & mm.t$BEARING[l] < 180) {mm.t$angle[l] = 180 - mm.t$BEARING[l]}
      if (mm.t$BEARING[l] >= 180 & mm.t$BEARING[l] < 270) {mm.t$angle[l] = mm.t$BEARING[l] - 270}
    } #if (mm.t$BEARING[l] > 90 & mm.t$BEARING[l] < 270) {
  } else {    #if (!is.na(mm.t$BEARING[l]) {
    mm.t$rel.direction[l] = "missing"
    mm.t$angle[l] = NA
  } #else  
} #for (l in 1:nrow(mm.t)) {


# Calculate distance along trackline
for (m in 1:nrow(mm.t)) {
  if(mm.t$PERPDIST[m]==0 & !is.na(mm.t$PERPDIST[m])) {mm.t$track.dist[m] = mm.t$RADIALDIST[m]} 
  if(mm.t$RADIALDIST[m]==0 & !is.na(mm.t$RADIALDIST[m])) {mm.t$track.dist[m] = 0} 
  if(mm.t$PERPDIST[m] > 0 & mm.t$RADIALDIST[m]!=0 & !is.na(mm.t$RADIALDIST[m])) {
    mm.t$track.dist[m] = cos(mm.t$angle[m]*pi/180) * mm.t$RADIALDIST[m]
  }
} #for (m in 1:nrow(mm.t)) {

head(mm.t)
summary(mm.t$track.dist)
# hist(mm.t$track.dist)
# hist(mm.t$PERPDIST)



####################################
### Shift marine mammal sightings in time based on sighting distance along trackline and ship speed
# Round distance along trackline for amounts to shift
mm.t$track.round =  round(mm.t$track.dist/1000)
table(mm.t$track.round)



### Create mm time ($dt.mam) based on location of animal X m away
#mm.t$sog.mps = mm.t$sog_mean * 0.514444  #convert speed over ground from knots to meters per second
mm.t$sog.mps = mm.t$SPEED * 0.514444  #convert speed over ground from knots to meters per second
mm.t$sec.away = mm.t$track.dist/mm.t$sog.mps
for(i2 in 1:nrow(mm.t)){
  if(mm.t$rel.direction[i2] == "forward"){mm.t$dt.mam[i2] = mm.t$dt[i2] + seconds(mm.t$sec.away[i2])}
  if(mm.t$rel.direction[i2] == "backward"){mm.t$dt.mam[i2] = mm.t$dt[i2] - seconds(mm.t$sec.away[i2])}
  if(mm.t$rel.direction[i2] == "missing" & mm.t$transect_eff[i2] == transect){mm.t$dt.mam[i2] = mm.t$dt[i2] }
  # for this last one, if transect wasn't there, it  would fill in times since seconds isn't involved.
  # in first two portions, the seconds is missing, so dt.mam ends up NA
}
mm.t$dt.mam = as_datetime(mm.t$dt.mam) # During loop that included missing values datetime format go changed to numerical
####################################



### THE PART THAT ACTUALLY DOES THE MATCHING... 



####################################
# Limit sightings to those occurring during acoustic transect
# Subset by time interval
# mfi.data.ac$dt = mfi.data.ac$dt.x   #name got changed on merge (fixed above)
nrow(mm.t) #5475

# Think actually need a loop here... loop through mm.t data and check each record to see if there is an acoustic record in that time window

# work with one of the below types of loops

# ...if mm date is within acoustic data INTERVAL, output row, save and bind

# (also should check more into "failed to parse") >>> appears to be inconsequential, also challenging to work with times to provide proper value

for (n in 1:nrow(mfi.data.ac)){
  
  # Check if acoustic data is missing and drop?
  # Theoretically this could be done by reading in a sample 200 m acoustic dataset
  # and looking at the Sv_mean where Layer==1, and if it is -999, could drop the 
  # sighting. However, a marine mammal shouldn't be sighted when they are off effort,
  # which is what is dropped from the acoustics, so we should be good. The exported
  # acoustics may not export bad areas anyway, so it might be non-issue for that reason,
  # I'm not actually sure, would have to go back and check setings, etc.
  
  if(mfi.data.ac$transect[n] == "116" & mfi.data.ac$transect_date[n] == '080216') {
    print("stop here and check") # cruise = 'HB1603', transect = '116', transect_date = '080216'
  }
  
  if(mfi.data.ac$transect[n] == "15" & mfi.data.ac$transect_date[n] == '062816') {
    print("stop here and check") # cruise = 'HB1603', transect = '15', transect_date = '062816'
  }
  

  # these both shouldn't be <= or >=, one shouldn't have the equal to or could get duplicates if were exact matches
  mmrow = mm.t[which(mm.t$dt.mam >= int_start(mfi.data.ac$int[n]) &  mm.t$dt.mam < int_end(mfi.data.ac$int[n])), ]
  
  if(nrow(mmrow>=1)){ # if there is something to match...
    
    #print("check this")
    
    if(exists("mmrows")=="FALSE") { # if mmrows dataset doesn't exist, then create it
      mmrows= mmrow
    } else {  #if the mmrows datsaet does exist, then add to it with new records
      mmrows= rbind(mmrows, mmrow)
    }
  } # end of first if 
    
} # end of for loop

# if stuck in situation with repeated first rows
# data_frame = data_frame[-1,]
nrow(mmrows) # 1539
table(mmrows$TRIPID)
# HB1103 HB1303 HB1603 
#    291    332    916 
table(mm.t$TRIPID)  
# HB1103 HB1303 HB1603 
#   1398   1076   3001 
table(mmrows$TRIPID) / table(mm.t$TRIPID)
# HB1103    HB1303    HB1603 
# 0.2081545 0.3085502 0.3052316 
# Seems about right, about half transects passive, and a bunch offshore


# Write out sightings file
write.csv(mmrows, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\hb11.13.16.sightings.csv")



### Setup distance/p(a) dataset (need record from each team for each sighting)
### For p(a)/distance analysis, add back in distance data needed for modeling:
### 
# distance (PERPDIST)
# observer (TEAM)
# size (SIZEBEST)
# SIGHTTIME (time of day)
# GLAREMAG_WAVG
# SEASTATE_WAVG (sea)
# SWELLHEIGHT (swell)

names(hb)
hb.dist2 = hb
hb.dist2 = hb.dist2[c("TRIPID", "CUMUSIGHTID", "DETECTED", "PERPDIST", "TEAM", 
                      "SIZEBEST", "SIGHTTIME", "GLAREMAG_WAVG", "SEASTATE_WAVG",
                      "SWELLHEIGHT",  "SPECIES")]


# limit mmrows variables
names(mmrows)
mmrows2 = mmrows[c("CUMUSIGHTID", "COMNAME")]
nrow(mmrows2) # 1539
nrow(hb.dist2) # 13132
# merge hb.dist2 with mmrows, use mmrows for species selection, but allow two row per species
dist.out = left_join(mmrows2, hb.dist2, by = "CUMUSIGHTID")
nrow(dist.out) #3078, perfect
# Adjusting the column order for the output dataset
dist.out = dist.out %>% relocate(SPECIES, .before = COMNAME)
dist.out = dist.out %>% relocate(TEAM, .after = COMNAME)
dist.out = dist.out %>% relocate(TRIPID)

write.csv(dist.out, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\hb11.13.16.for.pa.csv")

