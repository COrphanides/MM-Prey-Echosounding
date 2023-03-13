## PURPOSE: Example of getting proper detection probabilities P(a)

library(foreign)
library(mrds)
library(dplyr)
library(tidyr)
options(scipen = 7)

###### LOAD DATA IN DISTANCE FORMAT FROM HB1103, HB1303 AND USE THE BEST MODEL  #####
###### DEFINITION  FROM THE AMAPPS I REPORT.                                    #####
###### PDETECT = P(a) = DETECTION PROBABILITY AT A DISTANCE a                   #####
###### dsmodel = Distance Sampling Model ; mrmodel = Mark-Recapture Model       #####

# Clear 
rm(list=ls(all=TRUE))

# Read in base dataset

path <- "C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\"
# path <- "C:\\chris\\PhD\\Dissertation\\prey & habitat\\Sams Pa etc\\NE-ship_distance_example\\"
# path <- "J:\\NOAA\\NOAA Computer\\Documents\\Marine mammals\\Distance analysis\\Data\\Shipboard surveys\\NE Groups\\"
# data.file <- "NE ship group 1 NEW.csv"
#data.file <- "hb11.13.16.for.pa.csv"
data.file <- "HB-11-13-16_Distance_ShelfBreak_Sightings.txt"
data <- read.csv(paste(path, data.file, sep = ""))
data$EID <- 1:nrow(data)
# Remove is species missing
data <- data[!is.na(data$SPECIES),]   # went from 8254 to 7730
# This (Effort) isn't in the distance dataset I have but doesn't come up again, I don't think
# maybe it is "transect_eff" or "leg_eff"?
# data$Effort <- data$EFFORT_M   
data[data$TEAM== "upper", "TEAMNUM"]= 1
data[data$TEAM== "lower", "TEAMNUM"]= 2




### DEFINE THE SPECIES GROUP###
# (leftover from Sam's example code/dataset, don't think it is needed or used)
group.code <- "Group 1"
surveya<- "NE SHIP"
ID <-paste(group.code,  surveya)

# Remove rows with no sightings and rows with sightings
# of stuff we are not interested in (debris, sharks, etc.), 
# distance not recorded for these
sightings.dat <- data[is.na(data$CUMUSIGHTID) == FALSE & 
                        is.na(data$PERPDIST) == FALSE ,]
nrow(sightings.dat)
sightings.dat <- sightings.dat[sightings.dat$PERPDIST != -99 ,]
nrow(sightings.dat) #6732



#### Define availability bias 

# ask Sam about this, assumes everything is seen?
# leftover code from Sam's example
# abiaship = 1


##### RENAME FIELDS AS REQUIRED FOR MRDS #####
sightings.dat$object <- sightings.dat$CUMUSIGHTID
sightings.dat$size <- sightings.dat$SIZEBEST
sightings.dat$distance <- sightings.dat$PERPDIST
sightings.dat$observer <- sightings.dat$TEAMNUM
sightings.dat$detected <- sightings.dat$DETECTED

# And maybe some that don't need renaming...
sightings.dat$time <- sightings.dat$SIGHTTIME 
sightings.dat$glare <- sightings.dat$GLAREMAG_WAVG 
sightings.dat$sea <- sightings.dat$SEASTATE_WAVG  
sightings.dat$swell <- sightings.dat$SWELLHEIGHT  


####  NESHIP MRDS ANALYSIS  ####
#===============================

# neship.sightings<- sightings.dat3
neship.sightings<- sightings.dat


summary(neship.sightings$sea)

###  Make datasets for species Groups
table(neship.sightings$COMNAME)
unique(neship.sightings$COMNAME)

unique(neship.sightings$SPECIES)




# Atlantic White-sided Dolphins, common dolphin, harbor porpoise (no HP in this dataset)
# NE???-shipboard group 5
neship.sightings.g5 <- subset(neship.sightings, neship.sightings$SPECIES == "SADO" |
                                neship.sightings$SPECIES == "UNCW" , ) 
neship.sightings.g5$grp.num <- "5"
neship.sightings.g5$grp.sp <- "Common & WSD"




## Test Sam's model 1st

neship.mrds.g5<- ddf(method="io", data=neship.sightings.g5,
                     mrmodel=~glm(formula=~distance * observer + size),
                     dsmodel=~mcds(key="hr", formula =~swell),
                     meta.data=list(binned=FALSE, width = 3800))
# Gives warning of hazard-rate scale parameter close to zero, which other one didn't
summary(neship.mrds.g5)
#plot(neship.mrds.g5, breaks = c(seq(0,4000, by = 500)))
# plots looks decent
ddf.gof(neship.mrds.g5) # p-value = 0.667525 (better than my data - 0.55)
# gof plots and detection curve plots 

## Set up output
fitted.g5 = data.frame(neship.mrds.g5$fitted)
fitted.g5 = tibble::rownames_to_column(fitted.g5, "CUMUSIGHTID")
colnames(fitted.g5) <- c("CUMUSIGHTID", "pA")
head(fitted.g5)

#output fitted dataset
write.csv(fitted.g5, (paste(path, "wsd.shelf.fitted.g5.csv", sep = "")), row.names = FALSE)

# Should I try to make my own model? Hasn't yielded improvements yet...
