## FILENAME: pa.calcs.R
## DIRECTORY: C:\chris\PhD\Dissertation\prey & habitat\R\prey_characterization3
## PURPOSE: Get proper detection probabilities P(a)

library(foreign)
library(mrds)
library(dplyr)
library(tidyr)
options(scipen = 7)

###### LOAD DATA IN DISTANCE FORMAT FROM HB1103, HB1303 AND USE THE BEST MODEL  #####
###### DEFINITION  FROM THE AMAPPS I REPORT.                                    #####
###### PDETECT = P(a) = DETECTION PROBABILITY AT A DISTANCE a                   #####
###### dsmodel = Distance Sampling Model ; mrmodel = Mark-Recapture Model       #####

# Remove everything but functions
rm(list = setdiff(ls(), lsf.str()))

# Read in base dataset

path <- "C:\\chris\\PhD\\Dissertation\\prey & habitat\\combined\\"
data.file <- "hb11.13.16.for.pa.csv"
data <- read.csv(paste(path, data.file, sep = ""))
data$EID <- 1:nrow(data)
data[data$TEAM== "upper", "TEAMNUM"]= 1
data[data$TEAM== "lower", "TEAMNUM"]= 2

check = data[which(data$CUMUSIGHTID == '47994' | data$CUMUSIGHTID == '47993'),]
check

check = data[which(data$CUMUSIGHTID == '45606'),]
check


# Remove rows with no sightings and rows with sightings
# of stuff we are not interested in (debris, sharks, etc.), 
# distance not recorded for these
sightings.dat <- data[is.na(data$CUMUSIGHTID) == FALSE & 
                           is.na(data$PERPDIST) == FALSE ,]
nrow(sightings.dat) # 3494
test = sightings.dat[sightings.dat$PERPDIST == -99 ,]
# View(test)
# -99 are all debris or Fishing Gear (FIGE), Fishing Boat (FIBO) and I debris-balloon (DEBA).
sightings.dat <- sightings.dat[sightings.dat$PERPDIST != -99 ,]
nrow(sightings.dat) # 3192


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

nrow(sightings.dat) # 3192

# Some records have duplicate records. Check and remove duplicates
# (count of all objects should be 2)
tail(sort(table(sightings.dat$object)),12)
# 48747 48748 11823 12186 12790 12794 47326 47332 47333 47334 47335 47626 
#  2     2     4     4     4     4     4     4     4     4     4     4
check = sightings.dat[sightings.dat$object == '11823' |
                        sightings.dat$object == '12186' |
                        sightings.dat$object == '12790' |
                        sightings.dat$object == '12794' | 
                        sightings.dat$object == '47326' |
                        sightings.dat$object == '47332' |
                        sightings.dat$object == '47333' |
                        sightings.dat$object == '47334' |
                        sightings.dat$object == '47335' |
                        sightings.dat$object == '47626' ,]

names(check)
check[, c("X", "object", "CUMUSIGHTID", "SPECIES", "COMNAME") ]
junk = check[, c("X", "object", "CUMUSIGHTID", "SPECIES", "COMNAME") ]
check <- junk[order(check$CUMUSIGHTID),] 
check
#View(check)
# Manually remove some duplicates, no evident differences in this trimmed down dataset
sightings.dat2 =  sightings.dat[sightings.dat$X != 1169 & sightings.dat$X != 1170 ,]
sightings.dat3 =  sightings.dat2[sightings.dat2$X != 1553 & sightings.dat2$X != 1554 ,]
sightings.dat4 =  sightings.dat3[sightings.dat3$X != 1517 & sightings.dat3$X != 1518 ,]
sightings.dat5 = sightings.dat4   #sightings.dat5 =  sightings.dat4[sightings.dat4$X != 1551 & sightings.dat4$X != 1552 ,]
sightings.dat6 =  sightings.dat5[sightings.dat5$X != 1571 & sightings.dat5$X != 1572 ,]
sightings.dat7 = sightings.dat6   # sightings.dat7 =  sightings.dat6[sightings.dat6$X != 2553 & sightings.dat6$X != 2554 ,]
sightings.dat8 = sightings.dat7      #sightings.dat8 =  sightings.dat7[sightings.dat7$X != 1515 & sightings.dat7$X != 1516 ,]
sightings.dat9 =  sightings.dat8[sightings.dat8$X != 2035 & sightings.dat8$X != 2036 ,]
sightings.dat10 =  sightings.dat9[sightings.dat9$X < 2555 | sightings.dat9$X > 2564, ]
nrow(sightings.dat) 3192
nrow(sightings.dat10) # 3172
tail(sort(table(sightings.dat10$object)),10)

####  NESHIP MRDS ANALYSIS  ####
#===============================

neship.sightings<- sightings.dat10

check = neship.sightings[which(neship.sightings$CUMUSIGHTID == '47994' | neship.sightings$CUMUSIGHTID == '47993'),]
check


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
# 
# [1] "Unknown"                         "Dolphin, unid"                   "Whale, Sperm"                    "Dolphin, Risso's"               
# [5] "Dolphin, Atlantic Spotted"       "Dolphin, Striped"                "Whale, Pilot (unid.)"            "Dolphin, Common"                
# [9] "Whale, Cuvier's Beaked"          "Whale, Unid. medium"             "Whale, Sei"                      "Whale, Minke Whale"             
# [13] "Whale, Fin"                      "Whale, Unid. Large"              "Whale, Fin/Sei"                  "Dolphin, Bottlenose"            
# [17] "Shark, unid."                    "Shark, Basking"                  "Fish, Ocean Sunfish (Mola Mola)" "Fish, Tuna sp"                  
# [21] "Dolphin, Pantropical Spotted"    "Dolphin, Stenella sp."           "Whale, Unid. small whale"        "Whale, Sowerby's Beaked"        
# [25] "Whale, Pygmy/Dwarf sperm"        "Whale, Humpback"                 "Whale, Unid. Mesoplondont"       "Whale, Unid. Odontocete"        
# [29] "Whale, Unid. Baleen"             "Whale, Blue"                     "Dolphin, Unid. Large"            "Whale, Right"                   
# [33] "Whale, Pygmy Sperm Whale"        "Whale, True's Beaked"            "Whale, False Killer"             "Whale, Unid. cetacean"          
# [37] "Fish, Billfish"                  "Dolphin, Risso's/Bottlenose"     "Dolphin, Delphinus/Stenella"     "Dolphin, Common/White-sided"    
# [41] "Ray, Manta"                      "Turtle, Unidentified"            "Turtle, Leatherback"             "Debris, Unid."                  
# [45] "Debris, Plastic"  

unique(neship.sightings$SPECIES)
# [1] "UNK"  "UNDO" "SPWH" "GRAM" "ASDO" "PIWH" "STDO" "SADO" "GOBW" "UNMW" "SEWH" "MIWH" "FIWH" "UNLW" "FISE" "BODO" "UNSH" "BASH" "OCSU" "TUNA" "PSDO" "UNST"
# [23] "UNSW" "SWBW" "UNKO" "HUWH" "BEWH" "UNTW" "UNBA" "BLWH" "UNLD" "RIWH" "PSWH" "TRBW" "FKWH" "UNCE" "BIFI" "UNRB" "UNDS" "UNCW" "MARA" "UNTU" "LETU" "UNDE"
# [45] "DEPL" "DEBA"






# NE - shipboard group 8
# Humpbacks (pg 26) 
neship.sightings.g8 <- subset(neship.sightings, neship.sightings$COMNAME == "Whale, Humpback", ) 
neship.sightings.g8$grp.num <- "8"
neship.sightings.g8$grp.sp <- "Hump"


# NE - shipboard group 10
# Fin whales, sei whales, minke whales,
neship.sightings.g10 <- subset(neship.sightings, neship.sightings$COMNAME == "Whale, Fin" |
                                 neship.sightings$COMNAME == "Whale, Sei" |
                                 neship.sightings$COMNAME == "Whale, Fin/Sei" |
                                 neship.sightings$COMNAME == "Whale, Minke Whale" , ) 
neship.sightings.g10$grp.num <- "10"
neship.sightings.g10$grp.sp <- "Fin, Sei, Minke"

# Sperm Whale
# NE - shipboard group 9
neship.sightings.g9 <- subset(neship.sightings, neship.sightings$COMNAME == "Whale, Sperm", ) 
neship.sightings.g9$grp.num <- "9"
neship.sightings.g9$grp.sp <- "Sperm"

# Cuviers Beaked Whale, Sowerby's Beaked Whale, unknown beaked whales, 
#  Kogia/Dwarf Sperm Whale, 
# NE - shipboard group 6
neship.sightings.g6 <- subset(neship.sightings, neship.sightings$COMNAME == "Whale, Cuvier's Beaked" |
                                 neship.sightings$COMNAME == "Whale, Sowerby's Beaked" |
                                 neship.sightings$COMNAME == "Whale, Unid. Mesoplondont" |
                                 neship.sightings$COMNAME == "Whale, Pygmy/Dwarf sperm" , ) 
neship.sightings.g6$grp.num <- "6"
neship.sightings.g6$grp.sp <- "Beaked & Pygmy"

# Short finned pilot whale, long finned pilot whale, 
# NE - shipboard group 7
neship.sightings.g7 <- subset(neship.sightings, neship.sightings$COMNAME == "Whale, Pilot (unid.)"  , ) 
neship.sightings.g7$grp.num <- "7"
neship.sightings.g7$grp.sp <- "Pilot"

# Risso's dolphin
# NE - shipboard group 4
neship.sightings.g4 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Risso's", ) 
neship.sightings.g4$grp.num <- "4"
neship.sightings.g4$grp.sp <- "Risso"

# Atlantic White-sided Dolphins, common dolphin, harbor porpoise (no HP in this dataset)
# NE???-shipboard group 5
# neship.sightings.g5 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Common/White-sided" |
#                                 neship.sightings$COMNAME == "Dolphin, Common" , )
# neship.sightings.g5$grp.num <- "5"
# neship.sightings.g5$grp.sp <- "Common & WSD"
# 
#
# Atlantic spotted dolphin
# NE???-shipboard group 1
# neship.sightings.g1 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Common/White-sided" |
#                                 neship.sightings$COMNAME == "Dolphin, Common" , ) 
# neship.sightings.g1$grp.num <- "1"
# neship.sightings.g1$grp.sp <- "Common/WSD"
#
# # Striped dolphin
# # NE???-shipboard group 2 (one mismatch where a striped dolphin has a ASDO (spotted) dolphing species code)
# neship.sightings.g2 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Striped" , ) 
# 
# 
# # Common BOTTLENOSE Dolphin
# # NE???-shipboard group 3
# neship.sightings.g3 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Bottlenose" , ) 
# 
# # Atlantic White-sided Dolphins, common dolphin, harbor porpoise (no HP in this dataset)
# # NE???-shipboard group 5
# # neship.sightings.g5 <- subset(neship.sightings, neship.sightings$COMNAME == "Dolphin, Common/White-sided" |
# #                                 neship.sightings$COMNAME == "Dolphin, Common" , ) 

# Atlantic White-sided Dolphins, common dolphin, harbor porpoise (no HP in this dataset)
# NE???-shipboard group 5
neship.sightings.g5 <- subset(neship.sightings, neship.sightings$SPECIES == "SADO" |
                                 neship.sightings$SPECIES == "UNCW" , ) 
neship.sightings.g5$grp.num <- "5"
neship.sightings.g5$grp.sp <- "Common & WSD"


# Atlantic spotted dolphin
# NE???-shipboard group 1 -- LIKEY NOT ENOUGH FOR A MODEL!!!
neship.sightings.g1 <- subset(neship.sightings, neship.sightings$SPECIES == "ASDO"  , ) 
neship.sightings.g1$grp.num <- "1"
neship.sightings.g1$grp.sp <- "Spotted Dolphin"

# Striped dolphin
# NE???-shipboard group 2 (one mismatch where a striped dolphin has a ASDO (spotted) dolphing species code)
neship.sightings.g2 <- subset(neship.sightings, neship.sightings$SPECIES == "STDO" , ) 
neship.sightings.g2$grp.num <- "2"
neship.sightings.g2$grp.sp <- "Striped Dolphin"

check = neship.sightings.g2[which(neship.sightings.g2$CUMUSIGHTID == '47994' | neship.sightings.g2$CUMUSIGHTID == '47993'),]
check


# Common BOTTLENOSE Dolphin
# NE???-shipboard group 3
neship.sightings.g3 <- subset(neship.sightings, neship.sightings$SPECIES == "BODO" , ) 
neship.sightings.g3$grp.num <- "3"
neship.sightings.g3$grp.sp <- "Bottlenose"




##########################################
## Getting parameters from: [draft Appendix 1 of AMAPPS II final Report]

# NE - shipboard group 8
# Humpbacks (pg 26) 
# mr = distance + time of day
# mr_truncation = 7000
# ds = distance + glare
# ds_truncation = 7000
# key = HR
# (switched order of mr & ds in formula so it matches from table, limit mistakes)
neship.mrds.g8<- ddf(method="io", data=neship.sightings.g8,
                  mrmodel=~glm(formula=~distance + time),
                  dsmodel=~mcds(key="hr", formula =~ glare),
                  meta.data=list(binned=FALSE, width = 7000))
summary(neship.mrds.g8)
#plot(neship.mrds.g8, breaks = c(seq(0,7000, by = 500)))
ddf.gof(neship.mrds.g8) # p-value = 0.429174





# NE - shipboard group 10
# Fin whales, sei whales, minke whales, 
# mr = distance * observer + group size + sea state
# mr_truncation = 6000
# ds = distance  +  time of day + group size
# ds_truncation = 6000
# key = HR
neship.mrds.g10 <- ddf(method="io", data=neship.sightings.g10,
                  mrmodel=~glm(formula=~distance * observer + size + sea),
                  dsmodel=~mcds(key="hr", formula =~time + size),
                  meta.data=list(binned=FALSE, width = 6000))
summary(neship.mrds.g10)
#plot(neship.mrds.g10)
ddf.gof(neship.mrds.g10) #p-value = 0.920025



# Sperm Whale
# NE - shipboard group 9
# mr = distance *  observer + glare + group size
# mr_trunc = 4600
# ds = distance + swell
# ds_truc = 4600
# key = HR
neship.mrds.g9<- ddf(method="io", data=neship.sightings.g9,
                  mrmodel=~glm(formula=~distance * observer + glare + size),
                  dsmodel=~mcds(key="hr", formula =~swell),
                  meta.data=list(binned=FALSE, width = 4600))
summary(neship.mrds.g9)
#plot(neship.mrds.g9)
ddf.gof(neship.mrds.g9) # p-value = 0.889016


## Keep this model, some others in beaked.pa.R that have higher gof, but with 
## warning of hazard rate scale parameter near zero

# Cuviers Beaked Whale, Sowerby's Beaked Whale, unknown beaked whales, 
#  Kogia/Dwarf Sperm Whale, 
# NE - shipboard group 6
# mr = distance * observer + group size 
# mr_trunc = 3800
# ds = distance + sea state + swell + time of day
# ds_truc = 3800
# key =HR
neship.mrds.g6 <- ddf(method="io", data=neship.sightings.g6,
                  mrmodel=~glm(formula=~distance+glare+sea),
                  dsmodel=~mcds(key="hr", formula =~glare + sea),
                  meta.data=list(binned=FALSE, width = 3800))
summary(neship.mrds.g6)
#plot(neship.mrds.g6)  # These plots don't appear to fit that well
# --- try changing bin widths for histograms
ddf.gof(neship.mrds.g6) # p-value = 0.740458
#plot(neship.mrds.g6, breaks = c(seq(0,4000, by = 250)))



# Debi's 2016 model - ds =  swellheight & glare - used hazard model
# mr = distance, size, visibilty


# Short finned pilot whale, long finned pilot whale, 
# NE - shipboard group 7
# mr = distance * observer + group size + glare
# mr_trunc = 3500
# ds = distance + glare + swell + time of day
# ds_trunc = 3500
# key = HR
neship.mrds.g7 <- ddf(method="io", data=neship.sightings.g7,
                  mrmodel=~glm(formula=~distance * observer + size + glare),
                  dsmodel=~mcds(key="hr", formula =~glare + 
                                  swell + time),
                  meta.data=list(binned=FALSE, width = 3500))
summary(neship.mrds.g7)
#plot(neship.mrds.g7)
ddf.gof(neship.mrds.g7) # p-value = 0.989609



# Risso's dolphin
# NE???-shipboard group 4
# sea state + group size
# 2200
# distance + sea state + swell + group size
# 2200
# HR

neship.mrds.g4 <- ddf(method="io", data=neship.sightings.g4,
                  mrmodel=~glm(formula=~sea + size),
                  dsmodel=~mcds(key="hr", formula =~sea + 
                                  swell + size),
                  meta.data=list(binned=FALSE, width = 2200))
summary(neship.mrds.g4)
#plot(neship.mrds.g4) # these don't look at good as others, general line ok, but more outliers
ddf.gof(neship.mrds.g4)  # p-value = 0.935646




# Atlantic White-sided Dolphins, common dolphin, harbor porpoise
# NE???-shipboard group 5
# mr = distance * observer + group size
# 3800
# ds = distance + swell
# 3800
# key HR
neship.mrds.g5<- ddf(method="io", data=neship.sightings.g5,
                  mrmodel=~glm(formula=~distance * observer + size),
                  dsmodel=~mcds(key="hr", formula =~swell),
                  meta.data=list(binned=FALSE, width = 3800))
summary(neship.mrds.g5)
#plot(neship.mrds.g5, breaks = c(seq(0,4000, by = 500)))
# above plots don't look great - lots above the line
ddf.gof(neship.mrds.g5) # plot looks good, but p-value = 0.557407





# Atlantic spotted dolphin
# NE???-shipboard group 1
# sea state + swell
# 2000
# distance + sea state
# 2000
neship.mrds.g1 <- ddf(method="io", data=neship.sightings.g1 ,
                  mrmodel=~glm(formula=~sea + swell),
                  dsmodel=~mcds(key="hr", formula =~sea),
                  meta.data=list(binned=FALSE, width = 2000))
# ONLY 6 OBSERVATIONS - COMBINE WITH OTHER SPECIES? USE WHOLE SHELF BREAK? OR DROP?
summary(neship.mrds.g1)
#plot(neship.mrds.g1)
ddf.gof(neship.mrds.g1)




# Striped dolphin
# NE???-shipboard group 2
# distance * observer + sea state + group size
# 5000
# distance + sea state
# 5000
# HR
neship.mrds.g2 <- ddf(method="io", data=neship.sightings.g2,
                  mrmodel=~glm(formula=~distance * observer + sea + size),
                  dsmodel=~mcds(key="hr", formula =~sea),
                  meta.data=list(binned=FALSE, width = 5000))
summary(neship.mrds.g2) #(only 41 observations)
#plot(neship.mrds.g2)
ddf.gof(neship.mrds.g2)  # p-value = 0.84297
#plot(neship.mrds.g2, breaks = c(seq(0,5000, by = 250)))







# Common BOTTLENOSE Dolphin
# NE???-shipboard group 3
# distance * observer + group size + sea state
# 4000
# distance + sea state
# 4000
# HR
neship.mrds.g3<- ddf(method="io", data=neship.sightings.g3,
                  mrmodel=~glm(formula=~distance * observer + size + sea),
                  dsmodel=~mcds(key="hr", formula =~sea),
                  meta.data=list(binned=FALSE, width = 5000))
summary(neship.mrds.g3)
#plot(neship.mrds.g3)
ddf.gof(neship.mrds.g3) #p-value = 0.938329



fitted.g1 = data.frame(neship.mrds.g1$fitted)
fitted.g1 = tibble::rownames_to_column(fitted.g1, "CUMUSIGHTID")
colnames(fitted.g1) <- c("CUMUSIGHTID", "pA")
head(fitted.g1)

fitted.g2 = data.frame(neship.mrds.g2$fitted)
fitted.g2 = tibble::rownames_to_column(fitted.g2, "CUMUSIGHTID")
colnames(fitted.g2) <- c("CUMUSIGHTID", "pA")
head(fitted.g2)

check = fitted.g2[which(fitted.g2$CUMUSIGHTID == '47994' | fitted.g2$CUMUSIGHTID == '47993'),]
check


fitted.g3 = data.frame(neship.mrds.g3$fitted)
fitted.g3 = tibble::rownames_to_column(fitted.g3, "CUMUSIGHTID")
colnames(fitted.g3) <- c("CUMUSIGHTID", "pA")
head(fitted.g3)

fitted.g4 = data.frame(neship.mrds.g4$fitted)
fitted.g4 = tibble::rownames_to_column(fitted.g4, "CUMUSIGHTID")
colnames(fitted.g4) <- c("CUMUSIGHTID", "pA")
head(fitted.g4)

fitted.g5 = data.frame(neship.mrds.g5$fitted)
fitted.g5 = tibble::rownames_to_column(fitted.g5, "CUMUSIGHTID")
colnames(fitted.g5) <- c("CUMUSIGHTID", "pA")
head(fitted.g5)

fitted.g6 = data.frame(neship.mrds.g6$fitted)
fitted.g6 = tibble::rownames_to_column(fitted.g6, "CUMUSIGHTID")
colnames(fitted.g6) <- c("CUMUSIGHTID", "pA")
head(fitted.g6)

fitted.g7 = data.frame(neship.mrds.g7$fitted)
fitted.g7 = tibble::rownames_to_column(fitted.g7, "CUMUSIGHTID")
colnames(fitted.g7) <- c("CUMUSIGHTID", "pA")
head(fitted.g7)

fitted.g8 = data.frame(neship.mrds.g8$fitted)
fitted.g8 = tibble::rownames_to_column(fitted.g8, "CUMUSIGHTID")
colnames(fitted.g8) <- c("CUMUSIGHTID", "pA")
head(fitted.g8)

fitted.g9 = data.frame(neship.mrds.g9$fitted)
fitted.g9 = tibble::rownames_to_column(fitted.g9, "CUMUSIGHTID")
colnames(fitted.g9) <- c("CUMUSIGHTID", "pA")
head(fitted.g9)

fitted.g10 = data.frame(neship.mrds.g10$fitted)
fitted.g10 = tibble::rownames_to_column(fitted.g10, "CUMUSIGHTID")
colnames(fitted.g10) <- c("CUMUSIGHTID", "pA")
head(fitted.g10)


### Replace humpback fitted values with values from the full shelf model (hump.shelfbreak.pa.R)
fitted.g8.shelf = read.csv(paste(path, "hump.shelf.fitted.g8.csv", sep = ""))
# drop original p0 (which should be renamed pa by the way)
fitted.g8.orig = fitted.g8
fitted.g8.id = subset(fitted.g8.orig, select = -c(pA))
fitted.g8.shelf$CUMUSIGHTID = as.character(fitted.g8.shelf$CUMUSIGHTID)
nrow(fitted.g8.id); nrow(fitted.g8.shelf)
fitted.g8.new = semi_join(fitted.g8.shelf, fitted.g8.id, by = "CUMUSIGHTID" )
nrow(fitted.g8.new)


### Replace WSD fitted values with values from the full shelf model (wsd.shelfbreak.pa.R)
# Has a better fit (0.55 vs 0.66), gof plot and distance curve fit looks much better, 
# although gives warning about hazard-rate scale parameter close to zero
fitted.g5.shelf = read.csv(paste(path, "wsd.shelf.fitted.g5.csv", sep = ""))
# drop original p0 (which should be renamed pa by the way)
fitted.g5.orig = fitted.g5
fitted.g5.id = subset(fitted.g5.orig, select = -c(pA))
fitted.g5.shelf$CUMUSIGHTID = as.character(fitted.g5.shelf$CUMUSIGHTID)
nrow(fitted.g5.id); nrow(fitted.g5.shelf)
fitted.g5.new = semi_join(fitted.g5.shelf, fitted.g5.id, by = "CUMUSIGHTID" )
nrow(fitted.g5.new)




### DROP spotted dolphin, not enough observations (g1)
fitted.all = rbind(fitted.g2, fitted.g3, fitted.g4, fitted.g5.new, fitted.g6,
                   fitted.g7, fitted.g8.new, fitted.g9, fitted.g10)

# Could add group number and/or species group columns to to get mean per species group
# rbind base data input to each model, pull out those columns & ID and merge
b.input = rbind(neship.sightings.g2, neship.sightings.g3, neship.sightings.g4,
                neship.sightings.g5, neship.sightings.g6, neship.sightings.g7, neship.sightings.g8, 
                neship.sightings.g9, neship.sightings.g10)
sp.grps = b.input[c("CUMUSIGHTID", "grp.num", "grp.sp")]
sp.grps$CUMUSIGHTID = as.character(sp.grps$CUMUSIGHTID) #change to character for merging
# 2 rows per sighting (upper and lower teams), drop to one sighting per team
sp.grps2 = unique(sp.grps)
nrow(unique(sp.grps))  #  1081
which(duplicated(sp.grps2$CUMUSIGHTID))  #None
nrow(fitted.all) # 945
# Not all sightings are used in the model because of truncation distances
fitted.all$CUMUSIGHTID = as.character(fitted.all$CUMUSIGHTID)
nrow(fitted.all) #945
fitted.all2 = dplyr::left_join(fitted.all, sp.grps2, by = "CUMUSIGHTID")
fitted.NO1 = dplyr::anti_join(fitted.all, sp.grps2, by = "CUMUSIGHTID") # 0, everything in fitted.all is in sp.grps
fitted.NO2 = dplyr::anti_join(sp.grps2, fitted.all, by = "CUMUSIGHTID")  # no match in B (fitted.all) for some stuff in sp.grps
nrow(fitted.NO2) # 136

out.file = "fitted.match.acoutsic.csv"
write.csv(fitted.all2, (paste(path, out.file, sep = "")), row.names = FALSE)

