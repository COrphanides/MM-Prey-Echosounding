

# Starting as same as Process.Data5.R, just without function loop (testing with one transect at a time)


################################################################################################################
### FILENAME: test.process.R
### DIRECTORY: C:\chris\PhD\Dissertation\prey & habitat\R\prey_characteization3
### PURPOSE: Create function to read in marine mammal and echosounder data, merge them, and output data.
###          NOTES FROM PHD VERSION: 
###           Built off of Process.Data2.R. Changes made include:
###            1) Only use "upper" team sightings - DONE
###            2) Do sight distance calculations, then move to proper acoustic bin - DONE
###            3) Adjust ABC calculations based on depth (compensate for truncated bins) - NOT NECESSARY, done in Echoview
###            4) Carry over lat & lon from acoustics for mapping - DONE
###            5) Carry over some other acoustic variables for possible future analysis - DONE
###            6) Add in p(0) to calculate density (rather than counts) for each 1000 m cell - DONE
###            7) Add in SCS to acoustics data so that can carryover temp and SOG (needed for moving sightings) - DONE
###            8) Drop duplicates caused by follows - DONE
###          NOTES APPLICABLE TO THIS VERSION:
###           Changes to be applied here:
###            A) Use both upper and lower teams and create density using both distance and mark-recapture
###               for each individual sighting
###            B) Bring in echometrics (Urmy et al 2012)
###                 - For adding in additional acoustic variables later, do it in the code with this line 
###                   (once for 200 and once for 50):
###                      mfi.data.ac.tempA = mfi.cells[ ,c("Interval", "Layer", "ABC", "Sv_mean", "Kurtosis", "Skewness", "Lat_M", "Lon_M", "dt")]
###                   and then rename variables in sections having lines like this one:
###                      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.1'] = paste('ABC', sp.group, y.res, '000-200', sep='.')
###            C) Potentially bring in other acoustic variables
### AUTHORSHIP: Chris Orphanides, 12.23.20
################################################################################################################


## Checks
# 1) Open each transect and check before running (t12 might need a start region cutoff)
# 2) Check that regions are "nodata"
# 3) Export bottom lines for each transect
# 4) Make list of transects to run


#########################################

rm(list=ls())

# Load packages
library(lubridate)
library(dplyr)
library(zoo)

#########################################


# Create over-arching function
combo.f = function(cruise = cruise, transect = transect, transect_date = transect_date) {
## EXAMPLE:   combo.f(cruise = 'HB1303', transect = '12', transect_date = '071213')

# #### REVISE FUNCTION INPUTS -- USE THESE AS TEST DATES BEFORE RUNNING AS A FUNCTION

#cruise = 'HB1603'; transect = '08'; transect_date = '072116'
# cruise = 'HB1603'; transect = '116'; transect_date = '081616'
# cruise = 'HB1603'; transect = '116'; transect_date = '080216' -- Need to create EV file if an going to use it
#cruise = 'HB1603'; transect = '16'; transect_date = '070316'
# cruise = 'HB1103'; transect = '21'; transect_date = '061611'
#cruise = 'HB1103'; transect = '21'; transect_date = '060811'  # (NO SIGHTINGS THIS DAY!)
#  cruise = 'HB1603'; transect = '18'; transect_date = '070316' #T18    
# cruise = 'HB1303'; transect = '18'; transect_date = '070813'
# cruise = 'HB1303'; transect = '19'; transect_date = '081213'
# cruise = 'HB1303'; transect = '18'; transect_date = '070813'
# cruise = 'HB1303'; transect = '04'; transect_date = '080813'
#  cruise = 'HB1303'; transect = '12'; transect_date = '071213'
# cruise = 'HB1303'
# transect = '12'
# transect_date = '071213'
# # Add switch(es) for outputting csv & rds files?

# Use cruise to set base folders
if (cruise =="HB1303") {
  # modified input variable below ("*_v2*) to align with variables pulled for other years
  mm.data = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\HB1303 sightings\\hb1303_sightings&effort_v2.csv"
  ac.200.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\200x1000\\"
  ac.50.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\50x1000\\"
  path.btm.csv = "C://chris//PhD//Dissertation//prey & habitat//HB1303//Bottom Lines//csv//"
  outdir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\combined\\"
}

if (cruise =="HB1103") {
  # modified input variable below ("*_v2*) to align with variables pulled for other years
  mm.data = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\sightings\\hb1103_sightings&effort.csv"
  ac.200.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\200x1000\\"
  ac.50.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\50x1000\\"
  path.btm.csv = "C://chris//PhD//Dissertation//prey & habitat//HB1103//Bottom Lines//csv//"
  outdir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\combined\\"
}

if (cruise =="HB1603") {
  # modified input variable below ("*_v2*) to align with variables pulled for other years
  mm.data = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Sightings\\hb1603_sightings&effort.csv"
  ac.200.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\200x1000\\"
  ac.50.folder = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\50x1000\\"
  path.btm.csv = "C://chris//PhD//Dissertation//prey & habitat//HB1603//Bottom Lines//csv//"
  outdir = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\combined\\"
}
####



### Take specifications from "select_sightings" for year specific folders, etc.
### DECIDE LATER WHETHER TO USE SAME PROGRAM FOR DIFFERENT CRUISES


## Potential 200x1000 csv inputs
## Have those with schools and those without (NS),also have Raw data (Raw),
## and additional variables within those excel files for echometrics 

## Original MFI
# HB1303_transect12_071213_0200x01000_ESB_038kHz_CellExport.csv
# HB1303_transect12_071213_0200x01000_NSB_200kHz_CellExport.csv
# HB1303_transect12_071213_0200x01000_PHY_038kHz_CellExport.csv
# HB1303_transect12_071213_0200x01000_SBF_038kHz_CellExport.csv
# HB1303_transect12_071213_0200x01000_ZOO_120kHz_CellExport.csv

## Extra NSB
# HB1303_transect12_071213_0200x01000_NSB_120kHz_CellExport.csv
# HB1303_transect12_071213_0200x01000_NSB_038kHz_CellExport.csv

## MFI No Schools
# HB1303_transect12_071213_0200x01000_ESB_038kHz_NS_CellExport.csv
# HB1303_transect12_071213_0200x01000_NSB_200kHz_NS_CellExport.csv
# HB1303_transect12_071213_0200x01000_PHY_038kHz_NS_CellExport.csv
# HB1303_transect12_071213_0200x01000_SBF_038kHz_NS_CellExport.csv
# HB1303_transect12_071213_0200x01000_ZOO_120kHz_NS_CellExport.csv

## MFI No Schools Extra NSB
# HB1303_transect12_071213_0200x01000_NSB_120kHz_NS_CellExport.csv
# HB1303_transect12_071213_0200x01000_NSB_038kHz_NS_CellExport.csv

## Raw exports with no-school
# HB1303_transect02_080813_0200x01000_038kHz_RawNS_CellExport.csv
# HB1303_transect02_080813_0200x01000_038kHz_RawNS1200_CellExport.csv
# HB1303_transect02_080813_0200x01000_070kHz_RawNS_CellExport
# HB1303_transect02_080813_0200x01000_1200kHz_RawNS_CellExport
# HB1303_transect02_080813_0200x01000_200kHz_RawNS_CellExport





# Get base names Echoview output

#  ac.base.name = "HB1303_transect12_071213_*.csv"
# e.g. "HB1303_transect12_071213_0200x01000_ESB_038kHz_CellExport.csv"


# Read in each file
# First one, process like have been, just assign proper ABC name
# Next ones, see if can skip dt steps, and output columns of ABCs
# Merge columns on end
# Do this for all 200s and then repeat for 50s
# Assemble full acoustic file before going on to add bottom and mammals, etc.

setwd(ac.200.folder)
#ac.200.files = list.files(pattern = "^HB1303_transect12")
# ac.200.files = list.files(pattern = paste("^HB1303_transect", transect,"_", transect_date, sep=''))
ac.200.files = list.files(pattern = paste(cruise, "_transect", transect, "_", transect_date, sep=''))
if(cruise == 'HB1103') { 
  ac.200.files = list.files(pattern = paste(cruise, "_transect_", transect, "_", transect_date, sep=''))
} # added an extra underscore in the 2011 filenames unfortunately
path.200 = ac.200.folder
# path.200 = "C://chris//PhD//Dissertation//prey & habitat//HB1303/Data//200x1000//"

setwd(ac.50.folder)
#ac.50.files = list.files(pattern = paste("^HB1303_transect", transect,"_", transect_date, sep=''))
ac.50.files = list.files(pattern =  paste(cruise, "_transect", transect, "_", transect_date, sep=''))
if(cruise == 'HB1103') { 
  ac.50.files = list.files(pattern = paste(cruise, "_transect_", transect, "_", transect_date, sep=''))
} # added an extra underscore in the 2011 filenames unfortunately
path.50 = ac.50.folder
#path.50 = "C://chris//PhD//Dissertation//prey & habitat//HB1303/Data//50x1000//"

# Certainly could streamline the below - half written for doing 50 & 200 in one loop
# Going to leave it as is for now

# Process 200 res files

for ( j in 1:length(ac.200.files)){
  #if (j==1) {  # Read in first file
  mfi.cells = read.csv(paste(path.200, ac.200.files[j], sep=""), header= T)
  current_file = ac.200.files[j]
  
  # get species group 
  # Need to adjust for some transects with a three digit transects (in 2016)
  # and new "RawNS" data names - should have named them slightly differently
  
  test1 = substr(ac.200.files[j], 18,18) # "_"
  test2 = substr(ac.200.files[j], 37,39) # "018"
  test3 = substr(ac.200.files[j], 44,48) # "RawNS"
  test4 = substr(ac.200.files[j], 45,49)  # awNS_" (for 3 digit transects)
  
  # Add layer for 2011 ac.200.files[j] where I mistakenly added an extra "_"
  test5 = substr(ac.200.files[j], 16,16) # "_"
  
  if (test5 != "_"){ #if the data is not from 2011
    
    # If the 2013 or 2016 transect is two digits
    if (test1 == "_") { #is an underscore where the third digit would be
      
      # If the file contains MFI output
      # (example filenames:
      # HB1303_transect12_071213_0050x01000_ZOO_120kHz_NS_CellExport.csv 
      # HB1303_transect12_071213_0050x01000_ZOO_120kHz_CellExport.csv 
      if (test3 != "RawNS") { 
        sp.group = substr(ac.200.files[j], 37,39)
        raw.freq = substr(ac.200.files[j], 41,43)
        sc = substr(ac.200.files[j], 48,49)
        if(sc!="NS") {sc='WS'}  # WS = with schools, NS = No schools
        
        # if it does not contain MFI output, make sp.group = "RAW"
        # (example filename: HB1303_transect12_071213_0050x01000_200kHz_RawNS_CellExport.csv)
      } else { #END FOR: if (test3 != "RawNS")
        sp.group = "RAW" 
        raw.freq = substr(ac.200.files[j], 37,39) # "018"
        sc= "NS"  # WS = with schools, NS = No schools
      }
      
      # If the transect is from a 3-digit transect  
    } else { #END FOR:  if (test1 == "_")...[start of if 3-digit transect]
      
      #if it is an MFI output
      if (test4 != "RawNS") { 
        sp.group = substr(ac.200.files[j], 38,40)
        raw.freq = substr(ac.200.files[j], 42,44)
        sc = substr(ac.200.files[j], 49,50)
        if(sc!="NS") {sc='WS'}
      } else  { #END FOR: if (test4 != "RawNS")
        sp.group = "RAW" 
        raw.freq = substr(ac.200.files[j], 38,40) # "018"
        sc="NS"
      } # end for else for sp.group "RAW"
    } # end for else testing for a 3-digit transect: else { #END FOR:  if (test1 == "_")
    
  } else if (test5 == "_") {  # END OF: if (test5 != "_"){ #if the data is not from 2011  
    # START OF: if data is from 2011 (test5 == "_")
    # example ac.200.files[j]: HB1103_transect_01_072811_0200x01000_018kHz_RawNS_CellExport.csv
    #                HB1103_transect_01_072811_0200x01000_ESB_038kHz_CellExport.csv
    #   
    #   # Adjust/redo for 2011 ac.200.files[j] where an extra "_" was inserted
    # } else (test5 == "_") { #is an underscore where the third digit would be
    #   # END  if (test5 != "_"){ #if the data is not from 2011 
    #     # example ac.200.files[j]: HB1103_transect_01_072811_0200x01000_018kHz_RawNS_CellExport.csv
    #     #                HB1103_transect_01_072811_0200x01000_ESB_038kHz_CellExport.csv
    #     
    test6 = substr(ac.200.files[j], 45,49) # "RawNS" if raw data, not if MFI
    
    # If the file contains MFI output
    # (example filenames:
    # HB1303_transect12_071213_0050x01000_ZOO_120kHz_NS_CellExport.csv
    # HB1303_transect12_071213_0050x01000_ZOO_120kHz_CellExport.csv
    if (test6 != "RawNS") {
      sp.group = substr(ac.200.files[j], 38,40)
      raw.freq = substr(ac.200.files[j], 42,44)
      sc = substr(ac.200.files[j], 49,50)
      if(sc!="NS") {sc='WS'}
      
      # if it does not contain MFI output, make sp.group = "RAW"
      # (example filename: HB1303_transect12_071213_0050x01000_200kHz_RawNS_CellExport.csv)
    } else { #END FOR: if (test6 != "RawNS")
      sp.group = "RAW"
      raw.freq = substr(ac.200.files[j], 38,40) # "018"
      sc= "NS"
    } # end for else, for sp.group = "RAW"
    
  } # end of test5 for 2011 data
  
  
  
  
  
  
  
  
  # REPLICATE (But differently) FOR 50 FILES!
  
  # Delete rows that would be too deep (Layer >= X) based on RAW, MFI, and 50 or 200 bins
  if (sp.group == "RAW") {
    # This loop is only for 200 bins, so don't do it differently for bins here
    if (raw.freq == '200' | raw.freq == '120' | raw.freq == '070'){
      #DROP LAYERS >=4
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <4), ]
    }
    if (raw.freq == '018' | raw.freq == '038'){
      #DROP LAYERS >10
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <=10), ]
    }
  }
  
  # If MFI
  if (sp.group != "RAW"){ mfi.cells <- mfi.cells[which(mfi.cells$Layer <=4), ]}
  #DROP LAYERS >4}
  
  
  # get species group x & y resolution in file - the same spot for all fn
  y.res = substr(ac.200.files[j], 26, 29)
  x.res = substr(ac.200.files[j], 31, 35) # use later in output filename?
  
  # if cruise is in 2011 (named with extra "_") or cruise has a 3 digit transect
  if (cruise == "HB1103" | (cruise == "HB1603" & test1 != "_")) { # extra "_" in filename
    y.res = substr(ac.200.files[j], 27, 30)
    x.res = substr(ac.200.files[j], 32, 36) # use later in output filename?
  }
  
  if (y.res == '0200') {y.res = '200'} else if (y.res == '0050') {y.res ='50'} else {print("not 50 or 200 res"); break}
  # Create base datetime variable (based on pinger/computer time - does not match GPS/marine mammal time)
  mfi.cells$dt = ymd_hms(paste(trimws(as.character(mfi.cells$Date_S)), trimws(as.character(mfi.cells$Time_S)), sep=":"))
  mfi.cells$dt.e = ymd_hms(paste(trimws(as.character(mfi.cells$Date_E)), trimws(as.character(mfi.cells$Time_E)), sep=":"))
  
  # Select variables to carry over/keep... in future iterations add more acoustic variables 
  # mfi.data.ac.tempA = mfi.cells[ ,c("Interval", "Layer", "ABC", "Sv_mean",
  #                                   "Lat_M", "Lon_M", "dt", "dt.e",
  #                                   "Center_of_mass", "Inertia", "Proportion_occupied", "Equivalent_area", "Aggregation_index")]
  mfi.data.ac.tempA = mfi.cells[ ,c("Interval", "Date_S", "Date_E", "Time_S", "Time_E",
                                    "Layer", "ABC", "Sv_mean",
                                    "Lat_M", "Lon_M", "dt", "dt.e",
                                    "Center_of_mass", "Inertia", "Proportion_occupied", "Equivalent_area", "Aggregation_index")]
  mfi.data.ac.tempB = reshape(mfi.data.ac.tempA, idvar = "dt", timevar = "Layer", direction = "wide")
  #View(mfi.data.ac.tempB)
  
  # If carry over Date_S, Date_E and Time_S and Time_E, can re-do dt &dt.E with the next value. Need loop?
  
  # clean up Time_M and Date_M
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Time_S.1'] = 'Time_S'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Date_S.1'] = 'Date_S'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Time_S.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Date_S.")))
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Time_E.1'] = 'Time_E'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Date_E.1'] = 'Date_E'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Time_E.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Date_E.")))
  
  
  # clean up and delete some column names. Adjust if not all depth categories are present
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.e.1'] = 'dt.e'
  # if("dt.e.2" %in% colnames(mfi.data.ac.tempB)) {
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.e.")))
  
  
  ########################
  ### Replace dt.e with first value of next time rather than last of current time,
  ###  but can't have it jump bad data/off effort regions, so put time limit in
  ###  Also, would have NA as last value, so fix that
  ###  Will nee to do this for 50 m section (vs 200)
  mfi.data.ac.tempB$dt.lead = lead(mfi.data.ac.tempB$dt)
  mfi.data.ac.tempB$dt.e2 = mfi.data.ac.tempB$dt.lead
  for (zz in 1:nrow(mfi.data.ac.tempB)){
    if(!is.na(mfi.data.ac.tempB$dt.e2[zz]) & (as.numeric(difftime(mfi.data.ac.tempB$dt.e2[zz], mfi.data.ac.tempB$dt.e[zz], units="secs")) <=15)) {
      mfi.data.ac.tempB$dt.e[zz] = mfi.data.ac.tempB$dt.e2[zz] -1
    }   # leaves dt.e as original if dt.e2 is NA
  }
  # Will run into trouble on last record... fix for that (if row = length(mfi.data.ac.tempB...)) .... use nrow for subscript
  # Also, check how interval works, could use the below ot back it up a second:
  #   mfi.data.ac.tempB$dt.e2[1] -1
  ########################
  
  #Clean up dt.lead variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.lead.1'] = 'dt.lead'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.lead.")))
  
  #Clean up dt.e2 variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.e2.1'] = 'dt.e2'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.e2.")))
  
  
  #Clean up Interval variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Interval.1'] = 'Interval'
  mfi.data.ac.tempB<- mfi.data.ac.tempB %>% select("Interval", everything()) # Move interval to 1st column
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Interval.")))
  
  # clean up Lat_M and Lon_M
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lat_M.1'] = 'Lat_M'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lon_M.1'] = 'Lon_M'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lat_M.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lon_M.")))
  
  # For the raw data, potentially as many as 15 bins for 200m depth bins 
  #  (5 bins per 1000m to 3000m) and 60 for 50m depth bins.
  # The below setup should work for MFI data, but should make different
  #  limits for "RAW" data. May want to add additional limits based on frequency
  
  if (sp.group != "RAW") { #i.e., if MFI data (PHY, ZOO, NSB, etc.)
    
    if (y.res == 200) {
      
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.1'] = paste('ABC', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.2'] = paste('ABC', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.3'] = paste('ABC', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.1'] = paste('Sv_mean', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.2'] = paste('Sv_mean', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.3'] = paste('Sv_mean', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.1'] = paste('COM', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.2'] = paste('COM', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.3'] = paste('COM', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.1'] = paste('Inertia', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.2'] = paste('Inertia', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.3'] = paste('Inertia', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.1'] = paste('PO', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.2'] = paste('PO', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.3'] = paste('PO', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.1'] = paste('EA', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.2'] = paste('EA', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.3'] = paste('EA', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.1'] = paste('AI', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.2'] = paste('AI', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.3'] = paste('AI', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
    } #if (y.res == 200) {
    
  } #if (sp.group != "RAW") {
  
  
  if (sp.group == "RAW") {
    
    if (y.res == 200) {
      
      #If equals x freq, do down to Y depth
      # do to 600 m for all frequencies . although, will only show up with a really strong signal in 200 & 120 in deeper categories
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.1'] = paste('ABC', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.2'] = paste('ABC', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.3'] = paste('ABC', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.1'] = paste('Sv_mean', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.2'] = paste('Sv_mean', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.3'] = paste('Sv_mean', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.1'] = paste('COM', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.2'] = paste('COM', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.3'] = paste('COM', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.1'] = paste('Inertia', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.2'] = paste('Inertia', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.3'] = paste('Inertia', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.1'] = paste('PO', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.2'] = paste('PO', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.3'] = paste('PO', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.1'] = paste('EA', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.2'] = paste('EA', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.3'] = paste('EA', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.1'] = paste('AI', sp.group, sc, y.res, '000.200', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.2'] = paste('AI', sp.group, sc, y.res, '200.400', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.3'] = paste('AI', sp.group, sc, y.res, '400.600', raw.freq, sep='.')
      
      #TO ADD: If not 38 or 18, drop columns with .4, .5, .6, etc. ... maybe here, maybe further up
      
      if (raw.freq == "038" | raw.freq == "018" ) {
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.4'] = paste('ABC', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.5'] = paste('ABC', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.6'] = paste('ABC', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.7'] = paste('ABC', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.8'] = paste('ABC', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.9'] = paste('ABC', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.10'] = paste('ABC', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.4'] = paste('Sv_mean', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.5'] = paste('Sv_mean', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.6'] = paste('Sv_mean', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.7'] = paste('Sv_mean', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.8'] = paste('Sv_mean', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.9'] = paste('Sv_mean', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.10'] = paste('Sv_mean', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.4'] = paste('COM', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.5'] = paste('COM', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.6'] = paste('COM', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.7'] = paste('COM', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.8'] = paste('COM', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.9'] = paste('COM', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.10'] = paste('COM', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.4'] = paste('Inertia', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.5'] = paste('Inertia', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.6'] = paste('Inertia', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.7'] = paste('Inertia', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.8'] = paste('Inertia', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.9'] = paste('Inertia', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.10'] = paste('Inertia', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.4'] = paste('PO', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.5'] = paste('PO', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.6'] = paste('PO', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.7'] = paste('PO', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.8'] = paste('PO', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.9'] = paste('PO', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.10'] = paste('PO', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.4'] = paste('EA', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.5'] = paste('EA', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.6'] = paste('EA', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.7'] = paste('EA', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.8'] = paste('EA', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.9'] = paste('EA', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.10'] = paste('EA', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.4'] = paste('AI', sp.group, sc, y.res, '600.800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.5'] = paste('AI', sp.group, sc, y.res, '800.1000', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.6'] = paste('AI', sp.group, sc, y.res, '1000.1200', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.7'] = paste('AI', sp.group, sc, y.res, '1200.1400', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.8'] = paste('AI', sp.group, sc, y.res, '1400.1600', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.9'] = paste('AI', sp.group, sc, y.res, '1600.1800', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.10'] = paste('AI', sp.group, sc, y.res, '1800.2000', raw.freq, sep='.')
        
      } #if (raw.freq == "038" | raw.freq == "18" ) {
      
      
      
    } #if (y.res == 200) {
    
  } #if (sp.group != "RAW") {
  
  # Create base dataset
  if (j==1) {mfi.data.ac = mfi.data.ac.tempB}
  
  # if (j==6) {
  #   print("check from here")}
  
  if (j>1) {
    # Only add on new columns with measures of acoustic returns
    dropvars <- names(mfi.data.ac.tempB) %in% c("Interval", "dt", "Lat_M", "Lon_M", 
                                                "dt.e", "Time_S", "Time_E", 
                                                "Date_S", "Date_E", "dt.lead", "dt.e2")
    mfi.data.ac.tempB <- mfi.data.ac.tempB[!dropvars]
    mfi.data.ac = cbind(mfi.data.ac, mfi.data.ac.tempB)
  }
  
  # if (j==19) {
  #   print("j=19")
  # }
  
} #for ( j in 1:length(ac.200.files)){




for ( k in 1:length(ac.50.files)){
  #if (k==1) {  # Read in first file
  mfi.cells = read.csv(paste(path.50, ac.50.files[k], sep=""), header= T)
  (current_file = ac.50.files[k]) #print current filename
  # get species group & y resolution in file
  #sp.group = substr(ac.50.files[k], 37,39)
  
  # get species group 
  # Need to adjust for some transects with a three digit transects (in 2016)
  # and new "RawNS" data names - should have named them slightly differently
  
  test1 = substr(ac.50.files[k], 18,18) # "_"
  test2 = substr(ac.50.files[k], 37,39) # "018"
  test3 = substr(ac.50.files[k], 44,48) # "RawNS"
  test4 = substr(ac.50.files[k], 45,49)  # awNS_" (for 3 digit transects)
  
  ## After test4 creation
  
  # Add layer for 2011 ac.50.files[k] where I mistakenly added an extra "_"
  test5 = substr(ac.50.files[k], 16,16) # "_"
  
  if (test5 != "_"){ #if the data is not from 2011
    
    # If the 2013 or 2016 transect is two digits
    if (test1 == "_") { #is an underscore where the third digit would be
      
      # If the file contains MFI output
      # (example filenames:
      # HB1303_transect12_071213_0050x01000_ZOO_120kHz_NS_CellExport.csv 
      # HB1303_transect12_071213_0050x01000_ZOO_120kHz_CellExport.csv 
      if (test3 != "RawNS") { 
        sp.group = substr(ac.50.files[k], 37,39)
        raw.freq = substr(ac.50.files[k], 41,43)
        sc = substr(ac.50.files[k], 48,49)
        if(sc!="NS") {sc='WS'}  # WS = with schools, NS = No schools
        
        # if it does not contain MFI output, make sp.group = "RAW"
        # (example filename: HB1303_transect12_071213_0050x01000_200kHz_RawNS_CellExport.csv)
      } else { #END FOR: if (test3 != "RawNS")
        sp.group = "RAW" 
        raw.freq = substr(ac.50.files[k], 37,39) # "018"
        sc= "NS"  # WS = with schools, NS = No schools
      }
      
      # If the transect is from a 3-digit transect  
    } else { #END FOR:  if (test1 == "_")...[start of if 3-digit transect]
      
      #if it is an MFI output
      if (test4 != "RawNS") { 
        sp.group = substr(ac.50.files[k], 38,40)
        raw.freq = substr(ac.50.files[k], 42,44)
        sc = substr(ac.50.files[k], 49,50)
        if(sc!="NS") {sc='WS'}
      } else  { #END FOR: if (test4 != "RawNS")
        sp.group = "RAW" 
        raw.freq = substr(ac.50.files[k], 38,40) # "018"
        sc="NS"
      } # end for else for sp.group "RAW"
    } # end for else testing for a 3-digit transect: else { #END FOR:  if (test1 == "_")
    
  } else if (test5 == "_") {  # END OF: if (test5 != "_"){ #if the data is not from 2011  
    # START OF: if data is from 2011 (test5 == "_")
    # example ac.50.files[k]: HB1103_transect_01_072811_0200x01000_018kHz_RawNS_CellExport.csv
    #                HB1103_transect_01_072811_0200x01000_ESB_038kHz_CellExport.csv
    #   
    #   # Adjust/redo for 2011 ac.50.files[k] where an extra "_" was inserted
    # } else (test5 == "_") { #is an underscore where the third digit would be
    #   # END  if (test5 != "_"){ #if the data is not from 2011 
    #     # example ac.50.files[k]: HB1103_transect_01_072811_0200x01000_018kHz_RawNS_CellExport.csv
    #     #                HB1103_transect_01_072811_0200x01000_ESB_038kHz_CellExport.csv
    #     
    test6 = substr(ac.50.files[k], 45,49) # "RawNS" if raw data, not if MFI
    
    # If the file contains MFI output
    # (example filenames:
    # HB1303_transect12_071213_0050x01000_ZOO_120kHz_NS_CellExport.csv
    # HB1303_transect12_071213_0050x01000_ZOO_120kHz_CellExport.csv
    if (test6 != "RawNS") {
      sp.group = substr(ac.50.files[k], 38,40)
      raw.freq = substr(ac.50.files[k], 42,44)
      sc = substr(ac.50.files[k], 49,50)
      if(sc!="NS") {sc='WS'}
      
      # if it does not contain MFI output, make sp.group = "RAW"
      # (example filename: HB1303_transect12_071213_0050x01000_200kHz_RawNS_CellExport.csv)
    } else { #END FOR: if (test6 != "RawNS")
      sp.group = "RAW"
      raw.freq = substr(ac.50.files[k], 38,40) # "018"
      sc= "NS"
    } # end for else, for sp.group = "RAW"
    
  } # end of test5 for 2011 data
  
  
  
  # REPLICATE (But differently) FOR 50 FILES!
  
  # Delete rows that would be too deep (Layer >= X) based on RAW, MFI, and 50 or 200 bins
  if (sp.group == "RAW") {
    # This loop is only for 50 bins, so don't do it differently for bins here
    if (raw.freq == '200' ){
      #DROP LAYERS >4, limit to top 200 m
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <=4), ]
    }
    if (raw.freq == '120' ){
      #DROP LAYERS >6, limit to top 300 m
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <=6), ]
    }
    if (raw.freq == '070'){
      #DROP LAYERS >8, limit to top 400 m
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <=8), ]
    }
    if (raw.freq == '018' | raw.freq == '038'){
      #DROP LAYERS >10, limit to top 500 m (probably too deep, but keeps things symmetrical)
      mfi.cells <- mfi.cells[which(mfi.cells$Layer <=10), ]
    }
  }
  
  # If MFI
  if (sp.group != "RAW"){ mfi.cells <- mfi.cells[which(mfi.cells$Layer <=4), ]}
  #DROP LAYERS >4 , limit to top 200 m (different than for 200m, < 4 works there)
  
  
  y.res = substr(ac.50.files[k], 26, 29)
  
  
  # if cruise is in 2011 (named with extra "_") or cruise has a 3 digit transect
  if (cruise == "HB1103" | (cruise == "HB1603" & test1 != "_")) { # extra "_" in filename
    #if (cruise == "HB1103") { # extra "_" in filename
    y.res = substr(ac.50.files[k], 27, 30)
  }
  
  if (y.res == '0200') {y.res = '200'} else if (y.res == '0050') {y.res ='50'} else {print("not 50 or 200 res"); break}
  # Create base datetime variable (based on pinger/computer time - does not match GPS/marine mammal time)
  mfi.cells$dt = ymd_hms(paste(trimws(as.character(mfi.cells$Date_S)), trimws(as.character(mfi.cells$Time_S)), sep=":"))
  mfi.cells$dt.e = ymd_hms(paste(trimws(as.character(mfi.cells$Date_E)), trimws(as.character(mfi.cells$Time_E)), sep=":"))
  
  # Select variables to carry over/keep... in future iterations add more acoustic variables 
  mfi.data.ac.tempA = mfi.cells[ ,c("Interval", "Layer", "ABC", "Sv_mean",
                                    "Lat_M", "Lon_M", "dt", "dt.e",
                                    "Center_of_mass", "Inertia", "Proportion_occupied", "Equivalent_area", "Aggregation_index")]
  mfi.data.ac.tempB = reshape(mfi.data.ac.tempA, idvar = "dt", timevar = "Layer", direction = "wide")
  #View(mfi.data.ac.tempB)
  
  # clean up Time_M and Date_M
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Time_S.1'] = 'Time_S'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Date_S.1'] = 'Date_S'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Time_S.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Date_S.")))
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Time_E.1'] = 'Time_E'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Date_E.1'] = 'Date_E'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Time_E.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Date_E.")))
  
  # clean up and delete some column names. Adjust if not all depth categories are present
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.e.1'] = 'dt.e'
  # if("dt.e.2" %in% colnames(mfi.data.ac.tempB)) {
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.e.")))
  
  ########################
  ### Replace dt.e with first value of next time rather than last of current time,
  ###  but can't have it jump bad data/off effort regions, so put time limit in
  ###  Also, would have NA as last value, so fix that
  ###  Will nee to do this for 50 m section (vs 200)
  mfi.data.ac.tempB$dt.lead = lead(mfi.data.ac.tempB$dt)
  mfi.data.ac.tempB$dt.e2 = mfi.data.ac.tempB$dt.lead
  for (zz in 1:nrow(mfi.data.ac.tempB)){
    if(!is.na(mfi.data.ac.tempB$dt.e2[zz]) & (as.numeric(difftime(mfi.data.ac.tempB$dt.e2[zz], mfi.data.ac.tempB$dt.e[zz], units="secs")) <=15)) {
      mfi.data.ac.tempB$dt.e[zz] = mfi.data.ac.tempB$dt.e2[zz] -1
    }   # leaves dt.e as original if dt.e2 is NA
  }
  # Will run into trouble on last record... fix for that (if row = length(mfi.data.ac.tempB...)) .... use nrow for subscript
  # Also, check how interval works, could use the below ot back it up a second:
  #   mfi.data.ac.tempB$dt.e2[1] -1
  ########################
  
  
  #Clean up dt.lead variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.lead.1'] = 'dt.lead'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.lead.")))
  
  #Clean up dt.e2 variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'dt.e2.1'] = 'dt.e2'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("dt.e2.")))
  
  #Clean up Interval variable
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Interval.1'] = 'Interval'
  mfi.data.ac.tempB<- mfi.data.ac.tempB %>% select("Interval", everything()) # Move interval to 1st column
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Interval.")))
  
  # clean up Lat_M and Lon_M
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lat_M.1'] = 'Lat_M'
  names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Lon_M.1'] = 'Lon_M'
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lat_M.")))
  mfi.data.ac.tempB  = select(mfi.data.ac.tempB, -(starts_with("Lon_M.")))
  
  if (sp.group != "RAW") { #i.e., if MFI data (PHY, ZOO, NSB, etc.)
    
    if (y.res == 50) {
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.1'] = paste('ABC', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.2'] = paste('ABC', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.3'] = paste('ABC', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.4'] = paste('ABC', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.1'] = paste('Sv_mean', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.2'] = paste('Sv_mean', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.3'] = paste('Sv_mean', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.4'] = paste('Sv_mean', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.1'] = paste('COM', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.2'] = paste('COM', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.3'] = paste('COM', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.4'] = paste('COM', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.1'] = paste('Inertia', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.2'] = paste('Inertia', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.3'] = paste('Inertia', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.4'] = paste('Inertia', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.1'] = paste('PO', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.2'] = paste('PO', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.3'] = paste('PO', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.4'] = paste('PO', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.1'] = paste('EA', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.2'] = paste('EA', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.3'] = paste('EA', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.4'] = paste('EA', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.1'] = paste('AI', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.2'] = paste('AI', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.3'] = paste('AI', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.4'] = paste('AI', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
    } #if (y.res == 50) {
    
  } #if (sp.group != "RAW") {
  
  if (sp.group == "RAW") {
    
    if (y.res == 50) {
      
      #If equals x freq, do down to Y depth
      # do to 600 m for all frequencies . although, will only show up with a really strong signal in 200 & 120 in deeper categories
      # Do through 4  (200 m) for all freqs
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.1'] = paste('ABC', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.2'] = paste('ABC', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.3'] = paste('ABC', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.4'] = paste('ABC', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.1'] = paste('Sv_mean', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.2'] = paste('Sv_mean', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.3'] = paste('Sv_mean', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.4'] = paste('Sv_mean', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.1'] = paste('COM', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.2'] = paste('COM', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.3'] = paste('COM', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.4'] = paste('COM', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.1'] = paste('Inertia', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.2'] = paste('Inertia', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.3'] = paste('Inertia', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.4'] = paste('Inertia', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.1'] = paste('PO', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.2'] = paste('PO', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.3'] = paste('PO', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.4'] = paste('PO', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.1'] = paste('EA', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.2'] = paste('EA', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.3'] = paste('EA', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.4'] = paste('EA', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.1'] = paste('AI', sp.group, sc, y.res, '000.050', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.2'] = paste('AI', sp.group, sc, y.res, '050.100', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.3'] = paste('AI', sp.group, sc, y.res, '100.150', raw.freq, sep='.')
      names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.4'] = paste('AI', sp.group, sc, y.res, '150.200', raw.freq, sep='.')
      
      #ADJUST PER FREQUENCY!!!!!!!!!!!!!!!!!!!! >>>>>>  300 (120), 400 (70), 500 (38 & 18)
      #ADJUST DEPTH RANGES IN NAMES!
      
      if (raw.freq == "018" | raw.freq == "038" | raw.freq == "070" | raw.freq == "120") {
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.5'] = paste('ABC', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.6'] = paste('ABC', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.5'] = paste('Sv_mean', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.6'] = paste('Sv_mean', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.5'] = paste('COM', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.6'] = paste('COM', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.5'] = paste('Inertia', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.6'] = paste('Inertia', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.5'] = paste('PO', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.6'] = paste('PO', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.5'] = paste('EA', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.6'] = paste('EA', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.5'] = paste('AI', sp.group, sc, y.res, '200.250', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.6'] = paste('AI', sp.group, sc, y.res, '250.300', raw.freq, sep='.')
        
      } #if (raw.freq == "018" | raw.freq == "038" | raw.freq == "070" raw.freq == "120") {
      
      
      if (raw.freq == "018" | raw.freq == "038" | raw.freq == "070" ) {
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.7'] = paste('ABC', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.8'] = paste('ABC', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.7'] = paste('Sv_mean', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.8'] = paste('Sv_mean', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.7'] = paste('COM', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.8'] = paste('COM', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.7'] = paste('Inertia', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.8'] = paste('Inertia', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.7'] = paste('PO', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.8'] = paste('PO', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.7'] = paste('EA', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.8'] = paste('EA', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.7'] = paste('AI', sp.group, sc, y.res, '300.350', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.8'] = paste('AI', sp.group, sc, y.res, '350.400', raw.freq, sep='.')
        
      } #if (raw.freq == "018" | raw.freq == "038" | raw.freq == "070" ) {
      
      
      
      if (raw.freq == "018" | raw.freq == "038" ) {
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.9'] = paste('ABC', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'ABC.10'] = paste('ABC', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.9'] = paste('Sv_mean', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Sv_mean.10'] = paste('Sv_mean', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.9'] = paste('COM', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Center_of_mass.10'] = paste('COM', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.9'] = paste('Inertia', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Inertia.10'] = paste('Inertia', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.9'] = paste('PO', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Proportion_occupied.10'] = paste('PO', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.9'] = paste('EA', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Equivalent_area.10'] = paste('EA', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.9'] = paste('AI', sp.group, sc, y.res, '400.450', raw.freq, sep='.')
        names(mfi.data.ac.tempB)[names(mfi.data.ac.tempB) == 'Aggregation_index.10'] = paste('AI', sp.group, sc, y.res, '450.500', raw.freq, sep='.')
        
      } #if (raw.freq == "018" | raw.freq == "038"  ) {
      
    } #if (y.res == 500) {
    
  } #if (sp.group != "RAW") {
  
  # Create base dataset
  if (k==1) {mfi.data.ac.50 = mfi.data.ac.tempB}
  
  if (k>1) {
    # Only add on new columns with measures of acoustic returns
    dropvars <- names(mfi.data.ac.tempB) %in% c("Interval", "dt", "Lat_M", "Lon_M", 
                                                "dt.e", "Time_S", "Time_E", 
                                                "Date_S", "Date_E", "dt.lead", "dt.e2")
    mfi.data.ac.tempB <- mfi.data.ac.tempB[!dropvars]
    mfi.data.ac.50 = cbind(mfi.data.ac.50, mfi.data.ac.tempB)
  }
  
} #for ( k in 1:length(ac.50.files)){





#####################

# Put 50 & 200 together
# Only add on new columns with measures of acoustic returns
dropvars <- names(mfi.data.ac.50) %in% c("Interval", "dt", "Lat_M", "Lon_M", 
                                         "dt.e", "dt.lead", "dt.e2") 
mfi.data.ac.50 <- mfi.data.ac.50[!dropvars]
#names(mfi.data.ac.50)
mfi.data.ac = cbind(mfi.data.ac, mfi.data.ac.50)
#names(mfi.data.ac)
#head(mfi.data.ac)


# Add cruise and transect columns
mfi.data.ac$cruise = cruise
mfi.data.ac$transect = transect
mfi.data.ac$transect.date = transect_date
names(mfi.data.ac)

#####################


## Make date-time interval
mfi.data.ac$int = interval(mfi.data.ac$dt, mfi.data.ac$dt.e)


##### OLD
# ##  Set up bottom file
# setwd("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\csv\\")
# # path.btm.csv = "C://chris//PhD//Dissertation//prey & habitat//HB1303//Bottom Lines//csv//"
# # bottom.file =  list.files(pattern = glob2rx(paste("bottom_t", transect, "*csv", sep ='')))
# bottom.file = list.files(pattern = paste("bottom_t", transect, '_', transect_date, ".line.csv", sep =''))
# bottom = read.csv(paste(path.btm.csv, bottom.file, sep=""), header= T)
# # (e.g. "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Bottom Lines\\bottom_t12_07.12.13.line.csv") 
# #Note, first variable (Ping_date) read in with odd characters in front of it "?..Ping_date"
# #names(bottom)[names(bottom) == '?...Ping_date'] = 'Ping_date'
# names(bottom)[1]<-"Ping_date"
# bottom$dt = ymd_hms(paste(trimws(as.character(bottom$Ping_date)), trimws(as.character(bottom$Ping_time)), sep=":"))
####

##########################################################################
# Create bottom file here??


#CHECK TRANSECT VARIABLES TO SEE IF THEY ARE NEEDED BELOW

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
## Transect and transect date are already assigned early in the program
# transect = mfi.data.ac$transect[1]
# transect_date = mfi.data.ac$transect_date[1]
# accidentally added an additional "_" for 2011 file names
if (cruise == "HB1103") {bottom.file = list.files(pattern = paste("bottom_t_", transect, '_', transect_date, ".line.csv", sep =''))}
if (cruise == "HB1303" | cruise == "HB1603") {bottom.file = list.files(pattern = paste("bottom_t", transect, '_', transect_date, ".line.csv", sep =''))}
print(paste(cruise, transect, transect_date, sep =" "))
bottom = read.csv(paste(path.btm.csv, bottom.file, sep=""), header= T)
names(bottom)[1]<-"Ping_date"

# if (cruise == "HB1603" & transect == "18"){
#   print("check from here")
# }
# 
# 
# if (cruise == "HB1103" & transect == "04" & transect_date == "072911"){
#   print("check from here")
# }
# 

#bottom$GPS_UTC_time_hms = hms(bottom$GPS_UTC_time, origin = lubridate::origin, tz = "UTC")
bottom$GPS_UTC_time_hms = hms(bottom$GPS_UTC_time)
bottom$GPS_UTC_time_hms = hms::as_hms(bottom$GPS_UTC_time)
# The above spits an warning indicating some of these are missing. Rare, not an issue, unlikely to impact anything
#bottom$GPS_UTC_time_hms = force_tz(bottom$GPS_UTC_time_hms, tzone = "UTC")


# If try to fix warnings of failed to parse in the future, maybe consider:
#       lagtime = lag(bottom$GPS_UTC_time)
#       leadtime = lead(bottom$GPS_UTC_time)
# and stop trying to split time difference

bottom$dt = ymd_hms(paste(trimws(as.character(bottom$Ping_date)), trimws(as.character(bottom$Ping_time)), sep=":"))


## Add depth variables to acoustic data... shouldn't need this here, but set it up so can copy to Process file
# Use GPS_UTC_time because that matches with sightings data
bottom$GPS_dt = ymd_hms(paste(bottom$Ping_date, bottom$GPS_UTC_time, sep = ''))
# Gives Warning message. About 1%  of GPS times are missing (in this test dataset), results in NA values.
# I am with that, OK if those are dropped, will have almost no impact
#bottom$GPS_dt = force_tz(bottom$dt, tzone = "UTC") #set it to UTC


bottom$GPS_dt = force_tz(bottom$GPS_dt, tzone = "UTC") #set it to UTC
mfi.data.ac$bt_mean = NA
mfi.data.ac$bt_sd = NA
mfi.data.ac$bt_slope = NA


for (indx in 1:nrow(mfi.data.ac)){
  bt_samp = bottom[which(bottom$GPS_dt >= int_start(mfi.data.ac$int[indx]) &  bottom$GPS_dt <= int_end(mfi.data.ac$int[indx])), "Depth"]
  if (length(bt_samp)!=0) {
    mfi.data.ac$bt_mean[indx] = mean(bt_samp)
    mfi.data.ac$bt_sd[indx] = sd(bt_samp)
    mfi.data.ac$bt_slope[indx] = (bt_samp[1] - bt_samp[length(bt_samp)])/1000
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
mfi.data.ac$diff = diff

mfi.data.ac$dt = mfi.data.ac$dt + diff  
mfi.data.ac$dt.e = mfi.data.ac$dt.e + diff
mfi.data.ac$int = interval(mfi.data.ac$dt, mfi.data.ac$dt.e)


#########################################################################


## Adjust ABC (& Sv, Skewness?, Kurtosis?) for cell depth? >> NOT NECESSARY!
##  >>> Checked Echoview online help. Taken care of in Echoview since Apply no bad data regions and Include volume of no data samples were checked
##      (include volume of no data samples sounds incorrect, but it actually "effectively convert no-data samples to the mean of the analysis domain"
##      in other words, it doesn't mess up the ABC. More from the help menu:
##      "Use applied Bad data (no data) regions and effectively reduce Thickness mean calculations and reduce reported volume for NASC"


#DROP so that it will match with other years, assuming "speed" in marine mammal data is good enough to use rather than SCS: 
# temp_mean temp_sd temp_diff sal_mean sal_sd sal_diff lat_med lon_med sog_mean
# mfi.data.ac  = subset(mfi.data.ac, select = -c(temp_mean, temp_sd, temp_diff, sal_mean, sal_sd, sal_diff, lat_med, lon_med ))


## SWITCH TO READ IN DISTANCE FORMAT SIGHTINGS DATA HERE
## POSSIBLY MERGE WITH REGULAR DATA IF MISSING KEY SIGHTINGS VARIABLES


####################################################################################################
## Add Marine mammal data
mm = read.csv(mm.data)
mm$DATETIMEUTC.dt = dmy_hms(as.character(mm$DATETIMEUTC))
mm = mm[order(mm$DATETIMEUTC.dt),]
mm$dt = mm$DATETIMEUTC.dt
# mm$dt = force_tz(mm$dt, tzone = "EDT") # sightings data is in EDT, lubridate had assumed UTC. Force to EDT
# Had it as EST, but that doesn't incorporate daylight savings time
mm$dt = with_tz(mm$dt, tzone = "UTC")
names(mm)

# ###################
# # for 2016 SIGHTDATE is empty and appears to be replaced by "DATETIMEUTC", the only difference is the timezone (EDT vs UTC)
# #hb16dt = which(mm$TRIPID == "HB1603")
# # has got to be a faster way usins subsetting, but... tired of trying to figure it out
# mm$DATETIMEUTC.dt = dmy_hms(as.character(mm$DATETIMEUTC))
# mm$DATETIMEUTC.dt = with_tz(mm$DATETIMEUTC.dt, tzone = "UTC")
# for(zz in nrow(mm) ) {
#   if(mm$TRIPID == "HB1603"){
#     mm$dt[zz] = mm$DATETIMEUTC.dt[zz] 
#   }
# }
#  
# 
# 
# mm$dt[hb16dt] = mm$DATETIMEUTC[hb16dt]  # adjust time later down
# 
# if (mm$TRIPID == "HB1603") {
#   mm$DATETIMEUTC.dt = dmy_hms(as.character(mm$DATETIMEUTC))
#   mm$dt = with_tz(mm$dt, tzone = "UTC")
# }
# #####################

# limit columns to join
mm.t = mm[c("ID", "SPEED", "TEAM", "SIGHT_TYPE", "SIGHTNUM", "CUMUSIGHTID", "USEDINEST", "SIGHTLAT", "SIGHTLON", "SWIMDIR", "PERPDIST", "RADIALDIST", "BEARING", "SIZEBEST","dt", "BEHAVIOR", "COMNAME", "NESPP4", "transect_eff", "SST", "DEPTH", "SALINITY",  "leg_eff")]
nrow(mm.t) #2888

# Limit mm data to one sighting id. Sort by radial distance to get closest record to keep
mm.t = mm.t[order(mm.t$CUMUSIGHTID, -mm.t$RADIALDIST),]

# keep last unique RADIALDIST
sum(duplicated(mm.t$RADIALDIST)) # 2585
mm.t = mm.t[ !duplicated(mm.t[, c("CUMUSIGHTID")], fromLast=T),]
sum(duplicated(mm.t$CUMUSIGHTID)) # 0
nrow(mm.t) # 1840
#View(mm.t)



## Two objectives below, depending on if want dataset for p(a) distance analysis, 
## or if want dataset for main combo program linking acoustic & marine mammal data
## Much of this code will presumably be copied over to ProcessData4.R



# Read in Distance dataset, trim variables, 
#  remove duplicate sightings, and merge/replace SIZEBEST with one from above mm dataset
#hb.dist = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1303/HB1303 sightings/HB1303distoutforChris.csv")

#UPDATE HB DATASET SO CAN WORK WITH MULTIPLE YEARS (COPY FROM OTHER PROGRAM ELSEWHERE)

# read distance dataset
hb1103 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1103/HB1103distoutforChris.csv")
hb1303 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1303/HB1303 sightings/HB1303distoutforChris.csv")
hb1603 = read.csv("C:/chris/PhD/Dissertation/prey & habitat/HB1603/HB1603distoutforChris.csv")
# 
hb = rbind(hb1103, hb1303, hb1603)

hb.dist = hb
table(hb.dist$USEDINEST)
# Their AMAPPS objective was to use the best trackline. If a trackline was done twice, they chose the one with
# the best data (sighting conditions) so they would drop a duplicate transect. For our purposes, those duplicate 
# transects are useful
nrow(hb.dist) #13132
# hb.dist = hb.dist[hb.dist$USEDINEST == 'yes',]
nrow(hb.dist) # 13132
# limit columns
hb.dist = hb.dist[c("TRIPID", "CUMUSIGHTID", "SIZEBEST", "SPECIES")]
nrow(hb.dist) #13132
# don't want to do this (distinct with above limited variables) if using for the distance dataset
hb.dist = distinct(hb.dist)
nrow(hb.dist) #6307

nrow(mm.t) # 1840
mm.t = left_join(mm.t, hb.dist, by = "CUMUSIGHTID")
nrow(mm.t) #1840
summary(mm.t$SIZEBEST.y) #107 NAs
summary(hb.dist$SIZEBEST) # 7 NA
summary(mm.t$SIZEBEST.x) # 4 NAs
tapply(mm.t$SIZEBEST.y, mm.t$COMNAME, summary)
# remove NAs, many in NK species, otherwise mostly handful of animals per species
# except for sperm whale which has 5
# Rely on distance dataset for SIZEBEST, needs to align with distance dataset for density estimation later
mm.t = mm.t[!is.na(mm.t$SIZEBEST.y),]
nrow(mm.t) #1733
# Drop if missing in distance dataset
mm.t = mm.t[!is.na(mm.t$SIZEBEST.y),]
nrow(mm.t) #1733
# get rid of sizebest.x and rename sizebest.y
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


mm.t$track.dist = NA

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

#####################
### Add sog to marine mammal dataset - going to try to use "speed" variable in mm dataset instead
# sog.df = mfi.data.ac[c("sog_mean", "int", "dt", "dt.e")]
# head(sog.df)
# 
# mm.t$sog_mean = NA
# # loop through acoustics (contains sog), pull out where date interval matches marine mammal sighting time
# for (n in 1:nrow(mm.t)){
#   sog_samp = sog.df[which(mm.t$dt[n] >= int_start(sog.df$int) &  mm.t$dt[n] <= int_end(sog.df$int)), "sog_mean"]
#   if (length(sog_samp)==1) {mm.t$sog_mean[n] = sog_samp}
#   if (length(sog_samp)>1) {mm.t$sog_mean[n] = mean(sog_samp)
#   }
# }
# # backup, in case one is missing - for some reason ties are not getting filled, not seen as equal
# for (o in 1:nrow(mm.t)){
#   if(mm.t$transect_eff[o] == transect & is.na(mm.t$sog_mean[o])) {mm.t$sog_mean[o] = mean(mm.t$sog_mean[which(mm.t$transect_eff == transect)], na.rm=T)}
# }
#####################

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




####################################
# Limit sightings to those occurring during acoustic transect
# Subset by time interval
# mfi.data.ac$dt = mfi.data.ac$dt.x   #name got changed on merge (fixed above)

# Drop any records missing distances, can't get time/location without those and shouldn't have p(a) either
mm.t =mm.t[!is.na(mm.t$PERPDIST) | !is.na(mm.t$RADIALDIST), ]  # drops 1 record for 2013


mm.match = mm.t[which(mm.t$dt.mam >= min(mfi.data.ac$dt) &  mm.t$dt.mam <= max(mfi.data.ac$dt.e)),]
nrow(mm.match) 

# CHECK IDs - ANY DUPLICATES?


### IF TROUBLESHOOTING, RUNNING MANUALLY, SKIP THIS NEXT IF STATEMENT ON nrow(mm.match)


### If there are no marine mammals, nothing to match...
### Add columns? Tricky to combine later
if (nrow(mm.match) != 0) {   
  
  ## Line up sightings data with acoustics
  # Changing to use CUMUSIGHTID instead of "ID" to work across years
  # mfi.data.ac$ID = NA
  mfi.data.ac$CUMUSIGHTID = NA
  # 3) Loop on sightings data (trimmed to match acoustics transect window), and add mm ID to acoustic data
  for (indx in 1:nrow(mm.match)){
    
    # this works, for those within...
    #   mfi.data.ac$CUMUSIGHTID[which(mm.match$dt.mam[indx] %within% mfi.data.ac$int[1:nrow(mfi.data.ac)] == TRUE)] = mm.match$CUMUSIGHTID[indx] 
    
    # Check to see if there is a match within the interval, get subscript
    sub = which(mm.match$dt.mam[indx] %within% mfi.data.ac$int[1:nrow(mfi.data.ac)]) 
    
    # if no match within, check for match at endpoints of interval, get subscript
    if (length(sub)==0) {
      sub = grep(mm.match$dt.mam[indx], mfi.data.ac$int[1:nrow(mfi.data.ac)])
      # for HB1603, t08, 072116 there are a bunch of sightings before going on break where the sightings 
      # were too far off to line up the acoustics
    }
    
    # If there is a match, check if ID is already filled (two or more sightings in the same time window).
    #  If not filled, assign value, if filled, do proper join
    if (length(sub)!=0) {  #proceed if have a match
      # print(paste("loop",indx, "length", (length(is.na(mfi.data.ac$CUMUSIGHTID[sub])))))
      sub= sub[1]  # in case there are more than 2 sightings per location, and a row has already been added 
      if (is.na(mfi.data.ac$CUMUSIGHTID[sub])) {  # assign ID if value is not filled
        mfi.data.ac$CUMUSIGHTID[sub] = mm.match$CUMUSIGHTID[indx]
      } else {
        # duplicate record, fill value, and append duplicate row with new ID
        dup = mfi.data.ac[sub,]
        dup$CUMUSIGHTID = mm.match$CUMUSIGHTID[indx]
        mfi.data.ac = rbind(mfi.data.ac, dup)
      }
    }
    
    # Looks good now
    
    
  } # for (indx in 1:nrow(mm.match)){
  
  table(mfi.data.ac$CUMUSIGHTID)
  which(!is.na(mfi.data.ac$CUMUSIGHTID))
  
  mfi.data.ac = mfi.data.ac[order(mfi.data.ac$Interval),] 
  
  
  
  # 4) Add rest of mm data by ID
  #mfi.data.ac.m = select(mfi.data.ac, -int) #dplyr left join can't work with interval type
  mfi.data.ac.m <- subset(mfi.data.ac, select = -c(int))
  names(mm.match)[names(mm.match) == 'dt'] = "dt.sight"  #already is a dt in mm.ac
  #mm.ac = merge(mfi.data.ac, mm.match, by = "ID", all = TRUE)
  #junk.mm.ac = full_join(mfi.data.ac.m, mm.match, by = "ID")
  #junk.full = junk.mm.ac[, c(1, 90, 76, 2, 101, 117, 93, 103)]
  #junk.mm.ac = left_join(mfi.data.ac.m, mm.match, by = "ID")
  #junk.left = junk.mm.ac[, c(1, 90, 76, 2, 101, 117, 93, 103)]
  #View(junk.full)
  #View(junk.left)
  
  # seemed that could  use full join because it is already limited to those that match the ac time period
  # but were some cases where sighting occurred in an area with no interval, no data section
  # mm.ac = full_join(mfi.data.ac.m, mm.match, by = "ID") #full and left join result in the same number of rows, that's good
  mm.ac = left_join(mfi.data.ac.m, mm.match, by = "CUMUSIGHTID") 
  # View(mm.ac) 
  summary(mm.ac$CUMUSIGHTID)
  length(table(mm.ac$CUMUSIGHTID))
  # add interval back in
  mm.ac$int = mfi.data.ac$int
  
  
  ### Lastly, assign densities... need to summarize things per acoustic cell (use Interval variable) 
  ###       4) Incorporate p(0)s to get densities
  
  # Summarize by interval: total number of sightings & avg group size and merge together
  
  library(dplyr)
  mm.ac$sighting = 0
  for(i3 in 1:nrow(mm.ac)) {if(mm.ac$SIZEBEST[i3] > 0 & !is.na(mm.ac$SIZEBEST[i3])) {mm.ac$sighting[i3] = 1} }
  
  # first base dataset...
  # Need to do sorting, merges etc using common name, some species (e.g. pilot whale) are missing an NESPP4
  # Probably best to do this with SPECIES rather than COMNAME, redone below
  
  # # sp.grp.sum = mm.ac %>% group_by(Interval,NESPP4) %>% summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting))
  # sp.grp.sum = mm.ac %>% group_by(Interval,COMNAME) %>% summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting))
  # nrow(sp.grp.sum)  #77 for this run through - could be fewer than mm.ac if there are some repeats
  # #View(test)
  # #nrow(test)
  # # dplyr doesn't support joins with lubridate period and interval classes
  # drop.int <- names(mm.ac) %in% c("int") 
  # mm.ac.j <- mm.ac[!drop.int]
  # # int.df = mm.ac[c("Interval", "NESPP4", "int")]
  # #int.df = mm.ac[c("Interval", "COMNAME", "int")]
  # int.df = mm.ac[c("Interval","int")]
  # # mm.ac.j2 = inner_join(mm.ac.j, sp.grp.sum, by = c("Interval", "NESPP4")) #inner only keeps records in common
  # mm.ac.j2 = inner_join(mm.ac.j, sp.grp.sum, by = c("Interval", "COMNAME")) #inner only keeps records in common
  # junk = anti_join(mm.ac.j, sp.grp.sum, by = c("Interval", "COMNAME"))
  # junk = anti_join(sp.grp.sum, mm.ac.j, by = c("Interval", "COMNAME"))
  # #nrow(check)
  # #View(check)
  # #  View(mm.ac[,c(1,2,174)])
  # # View(mm.ac.j2[,c(1,149,159, 174:176)])
  # 
  
  # sp.grp.sum = mm.ac %>% group_by(Interval,NESPP4) %>% summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting))
  sp.grp.sum = mm.ac %>% group_by(Interval,SPECIES) %>% summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting), .groups = 'drop')
  nrow(sp.grp.sum)  #77 for this run through - could be fewer than mm.ac if there are some repeats
  #View(test)
  #nrow(test)
  # dplyr doesn't support joins with lubridate period and interval classes
  drop.int <- names(mm.ac) %in% c("int") 
  mm.ac.j <- mm.ac[!drop.int]
  # int.df = mm.ac[c("Interval", "NESPP4", "int")]
  #int.df = mm.ac[c("Interval", "SPECIES", "int")]
  int.df = mm.ac[c("Interval","int")]
  # mm.ac.j2 = inner_join(mm.ac.j, sp.grp.sum, by = c("Interval", "NESPP4")) #inner only keeps records in common
  mm.ac.j2 = inner_join(mm.ac.j, sp.grp.sum, by = c("Interval", "SPECIES")) #inner only keeps records in common
  junk = anti_join(mm.ac.j, sp.grp.sum, by = c("Interval", "SPECIES"))
  junk = anti_join(sp.grp.sum, mm.ac.j, by = c("Interval", "SPECIES"))
  #nrow(check)
  #View(check)
  #  View(mm.ac[,c(1,2,174)])
  # View(mm.ac.j2[,c(1,149,159, 174:176)])
  
  
  
  #mm.ac = merge(mm.ac.j2,int.df, by=c("Interval", "NESPP4"), all.x=TRUE)
  # test = merge(mm.ac.j2,int.df, by=c("Interval"), all.x=TRUE)
  # nrow(merge(mm.ac.j2,int.df, by.x=c("Interval")))
  # Run this after merge to see: View(mm.ac[which(mm.ac$CUMUSIGHTID == 12661),])
  
  # Would like to merge int column back in, but merge and joins keeps everything for some reason, resulting in extra rows, 
  # and dplyr join don't work with lubridate values.
  # Solution, change int to character variable instead of lubridate date, then only retain distinct so merge doesn't add more rows
  # (int not used in modelling, but is nice to have if need to check on anything)
  int.df$int = as.character(int.df$int)
  int.df = distinct(int.df)
  mm.ac = left_join(mm.ac.j2, int.df, by = c("Interval"))
  nrow(mm.ac)
  #View(mm.ac[,c(1,149,159,174:177)])
  
  
  
  ### Tested approach first, need to create dataset with more than one sighting per cell
  # junk = mm.ac
  # # first base dataset
  # test = junk %>% group_by(Interval,NESPP4) %>% 
  #   summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting))
  # View(test)
  # nrow(test)
  # # then modfied dataset
  # table(junk$NESPP4)
  # junk[which(junk$NESPP4 == 6997), "NESPP4"] = 6942
  # table(junk$NESPP4)
  # test1 =  junk %>% group_by(Interval,NESPP4) %>% summarize(mean.grp.size =mean(SIZEBEST), sightings.per.cell=sum(sighting))
  # View(test1)
  # nrow(test1)
  # 
  # # dplyr doesn't support joins with lubridate period and interval classes
  # drop.int <- names(mm.ac) %in% c("int") 
  # mm.ac.j <- mm.ac[!drop.int]
  # int.df = mm.ac[c("Interval", "NESPP4", "int")]
  # check = inner_join(mm.ac.j, test1, by = c("Interval", "NESPP4"))
  # nrow(check)
  # View(check)
  # test2 = merge(check,int.df, by=c("Interval", "NESPP4"), all.x=TRUE)
  
  unique(mm.ac$CUMUSIGHTID) 
  length(unique(mm.ac$CUMUSIGHTID) ) -1 # NA will be considered unique
  # Same, good. No duplicates of sight ids
  
  ## For denstiy modelling, should want one row per cell. Remove other species and individual sightings so just have one density value per cell
  ## Droppped modelling scripts I think?  If not, could do it there, do remove other duplicates
  
  
  
  
  ##############################################################################################
  #### USE TO CREATE TIME VARIABLE USED IN P(A) CALCULATIONS (fROM BETH)!
  # result$SIGHTTIME<- round(as.numeric(format(result$DATETIMEUTCF, "%H"))+as.numeric(format(result$DATETIMEUTCF, "%M"))/60+as.numeric(format(result$DATETIMEUTCF, "%S"))/3600, 3)  
  ####
  
  ############################
  
  
  
  ### Now calculate densities with p(a)
  # mm.ac$pA = NA
  mm.ac$density = 0
  # Read in p(a) file
  pA = read.csv("C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\fitted.match.acoutsic.csv")
  
  # Merge may drop some sightings records because of distance truncation distances
  mm.ac2 = left_join(mm.ac, pA, by = "CUMUSIGHTID") 
  mm.ac= mm.ac2
  
  
  # for(i4 in 1:nrow(mm.ac)){
  #   # From AMAPPS I Supplemental Appendix tables
  #   # Mark-Recapture Distance Sampling Analysis for 2010-2013.
  #   # Applied to all species, in future could do model based on sighting by sighting basis
  #   
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6933) {mm.ac$pA[i4] = 0.361} # humpbacks
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6931) {mm.ac$pA[i4] = 0.513} # fin whale
  #   if(!is.na(mm.ac$COMNAME[i4]) & mm.ac$COMNAME[i4] == 'Whale, Fin/Sei') {mm.ac$pA[i4] = 0.513} #sei whale      
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6932) {mm.ac$pA[i4] = 0.513} # sei whale
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6945) {mm.ac$pA[i4] = 0.513} # minke whale
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6948) {mm.ac$pA[i4] = 0.605} # sperm whale
  #   # Giving blue whale p(0) as sei and fin
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6947) {mm.ac$pA[i4] = 0.513} # blue whale
  #   # Giving right whale, baleen, nk, and Unid Lg whale avg of whales
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6946) {mm.ac$pA[i4] = 0.493} # right whale
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6993) {mm.ac$pA[i4] = 0.493} # baleen whale, nk
  #   if(!is.na(mm.ac$COMNAME[i4]) & mm.ac$COMNAME[i4] == 'Whale, Unid. Large') {mm.ac$pA[i4] = 0.493} # Unid Lg whale
  #   
  #   
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6954) {mm.ac$pA[i4] = 0.355} # cuvier's
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6909) {mm.ac$pA[i4] = 0.355} # sowerby's
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6908) {mm.ac$pA[i4] = 0.355} # Blainville
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6953) {mm.ac$pA[i4] = 0.355} # Unid Mesoplondont
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] ==   64) {mm.ac$pA[i4] = 0.355} # Unid Ziphiid
  #   # document had cuviers, sowwerbys and Unid beaked whales, all gettig the same p(0)
  #   # Giving Unid med whale same as beaked whales
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] ==   56) {mm.ac$pA[i4] = 0.355} # Unid med whale
  #   
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6956) {mm.ac$pA[i4] = 0.355} # Pygmy/Dwarf Sperm Whale  
  #   
  #   if(!is.na(mm.ac$COMNAME[i4]) & mm.ac$COMNAME[i4] == 'Whale, Pilot (unid.)') {mm.ac$pA[i4] = 0.740} # Pilot whales
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] ==   63) {mm.ac$pA[i4] = 0.740} # Unid small whale
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6980) {mm.ac$pA[i4] = 0.740} # toothed whale, nk
  #   # Giving Unid small whale and nk toothed whale the same a pilot whale
  #   
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6942) {mm.ac$pA[i4] = 0.674} # Risso's
  #   # WSD only seen in arial survey, none in summer (at least not in 2013 on the shelf break)
  #   # if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6936) {mm.ac$pA[i4] = X.XXX} # White-sided
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6940) {mm.ac$pA[i4] = 0.600} # common dolphin
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6901) {mm.ac$pA[i4] = 0.924} # Atl Spotted dolphin
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6902) {mm.ac$pA[i4] = 0.924} # spotted dolphin, brid
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6943) {mm.ac$pA[i4] = 0.924} # spotted dolphin, nk (Stenella sp)     
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6952) {mm.ac$pA[i4] = 0.764} # striped dolphin
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6941) {mm.ac$pA[i4] = 0.643} # bottlenose dolphin
  #   # None in NE shipboard data, at least not enough to generate a p(0) and not on the shelf break in 2013
  #   # if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6960) {mm.ac$pA[i4] = X.XXX} # harbor porpoise 
  #   
  #   # Assigning mean p(0) for unid dolphin
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 6997) {mm.ac$pA[i4] = 0.733} # unid dolphin   
  #   if(!is.na(mm.ac$NESPP4[i4]) & mm.ac$NESPP4[i4] == 57.5) {mm.ac$pA[i4] = 0.733} # unid lg dolphin
  #   # p(0) for general phocidae is 0.181, none in my dataset
  #  
  #  mm.ac$density[i4] = (mm.ac$sightings.per.cell[i4] * mm.ac$mean.grp.size[i4])/(2*1000*mm.ac$pA[i4])
  #  mm.ac$density.t[i4] = (mm.ac$sightings.per.cell[i4] * mm.ac$mean.grp.size[i4])/(1000)
  # }
  
  
  # re-do density per cell in next script. In some instances this is not summed by species & cell
  mm.ac$density = (mm.ac$sightings.per.cell * mm.ac$mean.grp.size)/(2*1000*mm.ac$pA)
  mm.ac$density.t = (mm.ac$sightings.per.cell * mm.ac$mean.grp.size)/(1000)
  
  
  
  
  
  
  ## Create outputs, set up folder struture
  base.fn = paste(outdir, "mmac_", cruise, "_t", transect, "_", transect_date, sep = '')
  # base.fn = paste(outdir, x.res, "x", y.res , "z", "\\", 
  #                 cruise, "_", transect, "_", out.type, "_", sp.group, "_", freq, "kHz", "_",  
  #                 x.res, "x", y.res, "z", "_", "mm.ac",  sep = '')
  csv.fn = paste(base.fn, ".csv", sep="")
  rds.fn = paste(base.fn, ".rds", sep="")
  write.csv(mm.ac, file=csv.fn)   #
  saveRDS(mm.ac, file=rds.fn )
  # Example of how to read into a different filename
  # test.mm.ac <- readRDS(rds.fn)
  # This would read it in with the original variable name
  # readRDS(rds.fn)
  # Other option, could save multiple R data objects as R.data file if use save(data1, data2, file = ...), 
  # would bring data in with load(filename)
  
  # Could read in these datasets separately for an acf analysis
  
  out = mm.ac
  
} else {    # if (nrow(mm.match) != 0) {  
  
  # if there are no marine mammals on the transect, add empty columns and output so data structure matches all the others
  
  ## ADD sighting	
  # mean.grp.size	
  # sightings.per.cell	
  # int	
  # density	
  # pA	
  # grp.num	
  # grp.sp	
  # density.t
  
  mm.t$mean.grp.size = NA
  mm.t$sightings.per.cell = NA
  mm.t$int = NA
  mm.t$density = NA
  mm.t$pA = NA
  mm.t$grp.num = NA
  mm.t$grp.sp = NA
  mm.t$density.t = NA
  
  mm.out = mm.t[0,]
  mm.out = mm.out[1,]
  mm.out = mm.out[rep(row.names(mm.out),nrow(mfi.data.ac)),]
  out = cbind(mfi.data.ac, mm.out)
  
  ## Create outputs, set up folder struture
  base.fn = paste(outdir, "mmac_", cruise, "_t", transect, "_", transect_date, sep = '')
  csv.fn = paste(base.fn, ".csv", sep="")
  rds.fn = paste(base.fn, ".rds", sep="")
  write.csv(out, file=csv.fn)   #
  saveRDS(out, file=rds.fn )
  
}  # End of else

return(out)

} # End of function

