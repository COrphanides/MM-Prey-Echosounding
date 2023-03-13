## FILENAME: hump.shelfbreak.pa.R
## DIRECTORY:C:\chris\PhD\Dissertation\prey & habitat\R\prey_characterization3
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

#### THIS DUPLICATE ISSUES DOESN'T APPEAR TO BE AN ISSUE WITH THIS REVISED DATASET
# # Some records have duplicate records. Check and remove duplicates
# # (count of all objects should be 2)
# tail(sort(table(sightings.dat$object)),10)
# # 48743 48746 48747 48748 47326 47332 47333 47334 47335 47626 
# #     2     2     2     2     4     4     4     4     4     4 
# check = sightings.dat[sightings.dat$object == '47326' |
#                            sightings.dat$object == '47332' |
#                            sightings.dat$object == '47333' |
#                            sightings.dat$object == '47334' |
#                            sightings.dat$object == '47335' | 
#                          sightings.dat$object == '47626',]
# # Manually remove some duplicates, no evident differences in this trimmed down dataset
# sightings.dat2 =  sightings.dat[sightings.dat$X != 1575 & sightings.dat$X != 1576 ,]
# sightings.dat3 =  sightings.dat2[sightings.dat2$X < 2095 | sightings.dat2$X > 2104, ]
# nrow(sightings.dat) # 6732
# nrow(sightings.dat3) # 6707

####  NESHIP MRDS ANALYSIS  ####
#===============================

# neship.sightings<- sightings.dat3
neship.sightings<- sightings.dat


summary(neship.sightings$sea)



#Truncation distance group 1 per AMAPPS I final report
#======================================================

# Leftover from example code - truncation distances applied in formulas
# trunc.dist <- 5850

### BEST MODEL group 1 per AMAPPS I final report
#================================================


## NOTE: "distance" is the same as PERDIST, renamed above
## NOTE: "observer" is made above from TEAMNUM 

###  Make datasets for species Groups
table(neship.sightings$COMNAME)
unique(neship.sightings$COMNAME)

unique(neship.sightings$SPECIES)




# NE - shipboard group 8
# Humpbacks (pg 26) 
neship.sightings.g8 <- subset(neship.sightings, neship.sightings$SPECIES == "HUWH", )  
neship.sightings.g8$grp.num <- "8"
neship.sightings.g8$grp.sp <- "Hump"




## Test Sam's model 1st

neship.mrds.g8<- ddf(method="io", data=neship.sightings.g8,
                     mrmodel=~glm(formula=~distance + time),
                     dsmodel=~mcds(key="hr", formula =~ glare),
                     meta.data=list(binned=FALSE, width = 7000))
summary(neship.mrds.g8) # AIC = 2351.11
#plot(neship.mrds.g8, breaks = c(seq(0,7000, by = 500)))
ddf.gof(neship.mrds.g8) # p-value = 0.977756


## Set up output
fitted.g8 = data.frame(neship.mrds.g8$fitted)
fitted.g8 = tibble::rownames_to_column(fitted.g8, "CUMUSIGHTID")
colnames(fitted.g8) <- c("CUMUSIGHTID", "pA")
head(fitted.g8)

#output fitted dataset
write.csv(fitted.g8, (paste(path, "hump.shelf.fitted.g8.csv", sep = "")), row.names = FALSE)

## Do this step below in pa.calcs.R
# 
# b.input = neship.mrds.g8
# sp.grps = b.input[c("CUMUSIGHTID", "grp.num", "grp.sp")]
# sp.grps$CUMUSIGHTID = as.character(sp.grps$CUMUSIGHTID) #change to character for merging
# # 2 rows per sighting (upper and lower teams), drop to one sighting per team
# sp.grps2 = unique(sp.grps)
# nrow(unique(sp.grps))  #  1078
# which(duplicated(sp.grps2$CUMUSIGHTID))  #None
# nrow(fitted.all) # 942
# # Not all sightings are used in the model because of truncation distances
# fitted.all$CUMUSIGHTID = as.character(fitted.all$CUMUSIGHTID)
# nrow(fitted.all) #942
# fitted.all2 = dplyr::left_join(fitted.all, sp.grps2, by = "CUMUSIGHTID")
# fitted.NO1 = dplyr::anti_join(fitted.all, sp.grps2, by = "CUMUSIGHTID") # 0, everything in fitted.all is in sp.grps
# fitted.NO2 = dplyr::anti_join(sp.grps2, fitted.all, by = "CUMUSIGHTID")  # no match in B (fitted.all) for some stuff in sp.grps
# nrow(fitted.NO2) # 136
# 
# out.file = "fitted.match.acoutsic.csv"
# write.csv(fitted.all2, (paste(path, out.file, sep = "")), row.names = FALSE)


#### SKIP BELOW - Not going to get better than Sam's model!

# 
# 
# #### PROCESS >>>>>>
# 
# # 1) Add more variables into base dataset to test out
# # 2) Use process shown here: https://examples.distancesampling.org/mrds-golftees/mrds-golftees-distill.html
# #    a) Run mr model first by itself (no mr included, cycle through all the options) and compare AICs, then gof of top models
# #    b) then run distance model and cycle through options and compare AICs and gof of top models
# #    c) Compare to current model and to model Sam used in latest distance modeling
# 
# 
# 
# library(knitr)
# 
# # Full independence model
# # Set up list with possible models
# mr.formula <- c( "~distance",
#                  
#                  "~distance + size", 
#                  "~distance + observer", 
#                  "~distance + time", 
#                  "~distance + glare", 
#                  "~distance + sea", 
#                  "~distance + swell", 
#                  
#                  "~distance + size + observer", 
#                  "~distance + size + glare",
#                  "~distance + size + sea", 
#                  "~distance + size + swell",
#                  
#                  "~distance + observer + time", 
#                  "~distance + observer + glare", 
#                  "~distance + observer + sea",
#                  "~distance + observer + swell",
#                  
#                  "~distance + time + glare",
#                  "~distance + time + sea",
#                  "~distance + time + swell",
#                  "~distance + time + size",
#                  
#                  "~distance + glare + sea",
#                  "~distance + glare + swell",
#                  "~distance + glare + sea",
#                  
#                  "~distance + size + observer + time",
#                  "~distance + size + observer + glare", 
#                  "~distance + size + observer + sea", 
#                  "~distance + size + observer + swell",
#                  
#                  "~distance + observer + time + glare",  
#                  "~distance + observer + time + sea", 
#                  "~distance + observer + time + swell",
#                  
#                  "~distance + time + glare + sea", 
#                  "~distance + time + glare + swell", 
#                  
#                  # distance*observer
#                  
#                  "~distance*observer",
#                  "~distance*observer + size",
#                  "~distance*observer + time", 
#                  "~distance*observer + glare", 
#                  "~distance*observer + sea", 
#                  "~distance*observer + swell", 
#                  
#                  "~distance*observer + size + glare",
#                  "~distance*observer + size + sea", 
#                  "~distance*observer + size + swell",
#                  
#                  "~distance*observer + time + glare",
#                  "~distance*observer + time + sea",
#                  "~distance*observer + time + swell",
#                  "~distance*observer + time + size",
#                  
#                  "~distance*observer + glare + sea",
#                  "~distance*observer + glare + swell",
#                  "~distance*observer + glare + sea",
#                  
#                  "~distance*observer + size + time",
#                  "~distance*observer + size + glare", 
#                  "~distance*observer + size + sea", 
#                  "~distance*observer + size + swell",
#                  
#                  "~distance*observer + time + glare",  
#                  "~distance*observer + time + sea", 
#                  "~distance*observer + time + swell",
#                  
#                  "~distance*observer + time + glare + sea", 
#                  "~distance*observer + time + glare + swell" )
# 
# 
# 
# num.mr.models <- length(mr.formula)
# # Create dataframe to store results
# fi.results <- data.frame(MRmodel=mr.formula, AIC=rep(NA,num.mr.models))
# # Loop through all MR models
# for (i in 1:num.mr.models) {
#   fi.model  <- ddf(method='io.fi', 
#                    mrmodel=~glm(formula=as.formula(mr.formula[i])),
#                    data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
#   fi.results$AIC[i] <- summary(fi.model)$aic
# }
# # Calculate delta AIC
# fi.results$deltaAIC <- fi.results$AIC - min(fi.results$AIC)
# # Order by delta AIC
# fi.results <- fi.results[order(fi.results$deltaAIC), ]
# # Print results in pretty way
# knitr::kable(fi.results, digits=2)
# 
# # Spread in AIC is a bit limited
# # 
# # |   |MRmodel                                   |     AIC| deltaAIC|
# #   |:--|:-----------------------------------------|-------:|--------:|
# #   |16 |~distance + time + glare                  | 1420.05|     0.00|
# #   |27 |~distance + observer + time + glare       | 1421.91|     1.86|
# #   |31 |~distance + time + glare + swell          | 1422.15|     2.10|
# #   |30 |~distance + time + glare + sea            | 1422.36|     2.31|
# #   |41 |~distance*observer + time + glare         | 1423.46|     3.41|
# #   |52 |~distance*observer + time + glare         | 1423.46|     3.41|
# #   |4  |~distance + time                          | 1424.54|     4.49|
# #   |17 |~distance + time + sea                    | 1424.84|     4.79|
# #   |56 |~distance*observer + time + glare + swell | 1425.57|     5.51|
# #   |55 |~distance*observer + time + glare + sea   | 1425.83|     5.78|
# #   |12 |~distance + observer + time               | 1426.40|     6.35|
# #   |18 |~distance + time + swell                  | 1426.48|     6.43|
# #   |28 |~distance + observer + time + sea         | 1426.70|     6.65|
# #   |6  |~distance + sea                           | 1427.46|     7.40|
# #   |19 |~distance + time + size                   | 1427.69|     7.64|
# #   |34 |~distance*observer + time                 | 1428.11|     8.06|
# #   |20 |~distance + glare + sea                   | 1428.31|     8.26|
# #   |22 |~distance + glare + sea                   | 1428.31|     8.26|
# #   |29 |~distance + observer + time + swell       | 1428.34|     8.29|
# #   |42 |~distance*observer + time + sea           | 1428.45|     8.39|
# #   |53 |~distance*observer + time + sea           | 1428.45|     8.39|
# #   |14 |~distance + observer + sea                | 1429.31|     9.26|
# #   |23 |~distance + size + observer + time        | 1429.55|     9.50|
# #   |10 |~distance + size + sea                    | 1429.93|     9.88|
# #   |43 |~distance*observer + time + swell         | 1430.05|    10.00|
# #   |54 |~distance*observer + time + swell         | 1430.05|    10.00|
# #   |36 |~distance*observer + sea                  | 1431.24|    11.19|
# #   |5  |~distance + glare                         | 1431.27|    11.22|
# #   |48 |~distance*observer + size + time          | 1431.29|    11.24|
# #   |44 |~distance*observer + time + size          | 1431.29|    11.24|
# #   |25 |~distance + size + observer + sea         | 1431.79|    11.74|
# #   |9  |~distance + size + glare                  | 1431.84|    11.79|
# #   |45 |~distance*observer + glare + sea          | 1432.07|    12.02|
# #   |47 |~distance*observer + glare + sea          | 1432.07|    12.02|
# #   |21 |~distance + glare + swell                 | 1432.68|    12.63|
# #   |1  |~distance                                 | 1432.93|    12.88|
# #   |13 |~distance + observer + glare              | 1433.13|    13.08|
# #   |24 |~distance + size + observer + glare       | 1433.70|    13.65|
# #   |39 |~distance*observer + size + sea           | 1433.70|    13.65|
# #   |50 |~distance*observer + size + sea           | 1433.70|    13.65|
# #   |7  |~distance + swell                         | 1433.92|    13.87|
# #   |2  |~distance + size                          | 1434.64|    14.59|
# #   |3  |~distance + observer                      | 1434.79|    14.74|
# #   |35 |~distance*observer + glare                | 1435.07|    15.02|
# #   |38 |~distance*observer + size + glare         | 1435.59|    15.54|
# #   |49 |~distance*observer + size + glare         | 1435.59|    15.54|
# #   |15 |~distance + observer + swell              | 1435.79|    15.74|
# #   |11 |~distance + size + swell                  | 1436.04|    15.99|
# #   |46 |~distance*observer + glare + swell        | 1436.47|    16.42|
# #   |8  |~distance + size + observer               | 1436.50|    16.45|
# #   |32 |~distance*observer                        | 1436.78|    16.73|
# #   |37 |~distance*observer + swell                | 1437.75|    17.70|
# #   |26 |~distance + size + observer + swell       | 1437.90|    17.85|
# #   |33 |~distance*observer + size                 | 1438.46|    18.41|
# #   |40 |~distance*observer + size + swell         | 1439.84|    19.79|
# #   |51 |~distance*observer + size + swell         | 1439.84|    19.79|
# # ##################
# 
# 
# ##TOP
# #   |16 |~distance + time + glare                  | 1420.05|     0.00|
# #   |27 |~distance + observer + time + glare       | 1421.91|     1.86|
# #   |31 |~distance + time + glare + swell          | 1422.15|     2.10|
# #   |30 |~distance + time + glare + sea            | 1422.36|     2.31|
# #   |41 |~distance*observer + time + glare         | 1423.46|     3.41|
# #   |52 |~distance*observer + time + glare         | 1423.46|     3.41|
# #   |4  |~distance + time                          | 1424.54|     4.49|
# 
# # ~distance+time+glare
# best.mr1  <- ddf(method='io.fi', 
#                 mrmodel=~glm(formula=~distance+time+glare),
#                 data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr1)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr1)
# ddf.gof(best.mr1, main="mr qq")
# # CVM: p-value = p-value = 0.01647
# plot(best.mr1, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # ~distance + observer + time + glare
# best.mr1b  <- ddf(method='io.fi', 
#                  mrmodel=~glm(formula=~distance + observer + time + glare),
#                  data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr1)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr1b)
# ddf.gof(best.mr1b, main="mr qq")
# # CVM: p-value = p-value = 0.0162864
# plot(best.mr1b, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # ~distance
# best.mr2  <- ddf(method='io.fi', 
#                 mrmodel=~glm(formula=~distance),
#                 data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr2)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr2)
# ddf.gof(best.mr2, main="mr qq") # p-value = 0.000374553, shittier
# plot(best.mr2, breaks = c(seq(0,7000, by = 500)))
# 
# # ~distance+time
# best.mr3  <- ddf(method='io.fi', 
#                 mrmodel=~glm(formula=~distance+time),
#                 data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr3)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr3)
# ddf.gof(best.mr3, main="mr qq") #  p-value = 0.00239457
# plot(best.mr3, breaks = c(seq(0,7000, by = 500)))
# 
# # ~distance + time + glare + swell
# best.mr4  <- ddf(method='io.fi', 
#                 mrmodel=~glm(formula=~distance + time + glare + swell),
#                 data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr4)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr4)
# ddf.gof(best.mr4, main="mr qq") # p-value = 0.0156468
# plot(best.mr4, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # ~distance + time + glare + swell
# best.mr5  <- ddf(method='io.fi', 
#                  mrmodel=~glm(formula=~distance + time + glare + sea),
#                  data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr4)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr5)
# ddf.gof(best.mr5, main="mr qq") # pp-value = 0.0120849
# plot(best.mr5, breaks = c(seq(0,7000, by = 500)))
# 
# 
# 
# # ~distance + time + glare + swell
# best.mr6  <- ddf(method='io.fi', 
#                  mrmodel=~glm(formula=~distance*observer + time + glare),
#                  data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# # Check out fit
# #detection.tables <- det.tables(best.mr4)
# #print(detection.tables)
# #plot(detection.tables)
# summary(best.mr6)
# ddf.gof(best.mr6, main="mr qq") # p-value = 0.0111975
# plot(best.mr6, breaks = c(seq(0,7000, by = 500)))
# 
# 
# 
# 
# ##### Example from website - fitting distance model next
# 
# # Point independence model, Include covariates in DS model
# # Use selected MR model, iterate across DS models
# ds.formula <- c(   "~size", 
#                    # "~time", #gave error when calculating summary/AIC: (Error in t(partial) %*% vcov :)
#                    "~glare", 
#                    # "~sea",  # ran but had NAs for AICs starting with sea, but AIC 1292.694 when run separetly
#                    # "~swell",  # run separately, ACI = 1296.591 - gave error when calculating summary/AIC: (Error in t(partial) %*% vcov :)
#                    
#                    "~size + glare",
#                    "~size + sea", 
#                    "~size + swell",
#                    
#                    #"~ time + glare", # same error, run separately, AIC = 1322.559
#                    # "~ time + sea", # same error, 1322.559
#                    # "~ time + swell", # same error, 1322.559
#                    # "~ time + size", # same error, 1322.559
#                    
#                    "~ glare + sea", # 1290.108
#                    "~ glare + swell",
#                    "~ glare + sea",
#                    
#                    "~ time + glare + sea" 
#                    # "~ time + glare + swell"  # 1324.559
# )
# num.ds.models <- length(ds.formula)
# # Create dataframe to store results
# pi.results <- data.frame(DSmodel=ds.formula, AIC=rep(NA,num.ds.models))
# # Loop through ds models - use selected MR model from earlier
# for (i in 1:num.ds.models) {
#   pi.model <- ddf(method='trial', mrmodel=~glm(formula=~distance+time+glare),
#                   dsmodel=~mcds(key='hr',formula=as.formula(ds.formula[i])), 
#                   data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
#   pi.results$AIC[i] <- summary(pi.model)$AIC
# }
# # Calculate delta AIC
# pi.results$deltaAIC <- pi.results$AIC - min(pi.results$AIC)
# # Order by delta AIC
# pi.results <- pi.results[order(pi.results$deltaAIC), ]
# knitr::kable(pi.results, digits = 2)
# 
# # 
# # 
# # |DSmodel              |    AIC| deltaAIC|
# #   |:--------------------|------:|--------:|
# #   |~size                | 879.14|       NA|
# #   |~glare               | 874.55|       NA|
# #   |~size + glare        | 876.49|       NA|
# #   |~size + sea          | 879.05|       NA|
# #   |~size + swell        | 880.62|       NA|
# #   |~ glare + sea        | 874.99|       NA|
# #   |~ glare + swell      | 876.22|       NA|
# #   |~ glare + sea        | 874.99|       NA|
# #   |~ time + glare + sea |     NA|       NA|
# 
# # # The officially best model looks like crap
# best = ddf(method='io', mrmodel=~glm(formula=~distance+time+glare),
#            dsmodel=~mcds(key='hr',formula=~ glare + sea),
#            data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# summary(best)
# AIC(best) # 
# ddf.gof(best) #p-value = 0.419301
# plot(best, breaks = c(seq(0,7000, by = 500)))
#      
#      
# # 
# # This one is the same gof as Sam's - # p-value = 0.589535
# best = ddf(method='io', mrmodel=~glm(formula=~distance+time+glare),
#            dsmodel=~mcds(key='hr',formula=~ glare ),
#            data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# summary(best)
# AIC(best) # 1293.271
# ddf.gof(best) # p-value = 0.429174
# plot(best, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # This one is the same gof as Sam's - # Little better: p-value = 0.619451
#  best = ddf(method='io', mrmodel=~glm(formula=~distance+time+glare),
#            dsmodel=~mcds(key='hr',formula=~ size+glare ),
#             data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
#  summary(best)
#  AIC(best) # 1293.271
#  ddf.gof(best) # p-value = 0.390528
#  plot(best, breaks = c(seq(0,7000, by = 500)))
#  
# 
# # # slightly worse than a couple others: p-value = 0.579359
#  best = ddf(method='io', mrmodel=~glm(formula=~distance+time+glare),
#             dsmodel=~mcds(key='hr',formula=~ glare+swell ),
#             data=neship.sightings.g8, meta.data=list(binned=FALSE, width = 7000))
# summary(best)
# AIC(best) # 1293.271
# ddf.gof(best) #p-value = 0.418102
# plot(best, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # Perhaps stay with Sam's original model - slightly better AIC, slightly worse gof
# best <- ddf(method="io", data=neship.sightings.g8,
#                      mrmodel=~glm(formula=~distance + time),
#                      dsmodel=~mcds(key="hr", formula =~ glare),
#                      meta.data=list(binned=FALSE, width = 7000))
# summary(best)
# AIC(best)  # 1292.814
# ddf.gof(best) # p-value = 0.429174
# plot(best, breaks = c(seq(0,7000, by = 500)))
# 
# 
# # # Try a few other top models,
# # # & review rest of example code from website to look at other fit metrics
# # fi.results
# # pi.results
# 
# 
# 
