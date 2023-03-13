
#### MODIFY FOR finBACKS...




### PROGRAM NAME: density.models.fin.R
### DIRECTORY: C:\chris\PhD\Dissertation\prey & habitat\R\prey_characterization3
### PURPOSE: Run GAM models for finback whales
### AUTHOR: Chris Orphanides
### NOTES:  Try modeling finback whales
#############################################################################


## Remove everything but functions
rm(list = setdiff(ls(), lsf.str()))

## Add libraries
library(dplyr)
library(mgcv)

## Read and assemble base file
path = "C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\combined\\"
base11 = readRDS("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1103\\Data\\combined\\HB1103.base.all.04.23.21.rds") 
base13 = readRDS("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\combined\\HB1303.base.all.04.23.21.rds") 
#base13 = read.csv("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1303\\Data2\\combined\\HB1303.base.all.04.23.21.manual.dt.int.fix.csv") 

# Fix issues with rare missing dts & ints w/no marine mammals
length(which(is.na(base13$dt))) #16
cond = which(!is.na(base13$dt...2) & is.na(base13$dt))
base13[cond, "dt"] = base13$dt...2[cond]
length(which(is.na(base13$dt)))
base13$dt[cond]
# remove problematic 
base13 = subset(base13, select = -c(dt...2,dt...1132) )


length(which(is.na(base13$int))) #16
cond = which(!is.na(base13$int...1114) & is.na(base13$int))
#base13[cond, "int"] = base13$int...1114[cond]
library(lubridate)
base13[cond, "int"] = as.character(interval(base13$dt[cond], base13$dt.e[cond])   )
base13$int[cond]
length(which(is.na(base13$int)))
# remove problematic 
base13 = subset(base13, select = -c(int...1114,int...1153) )
# test =  bind_rows(base11, base13, base16)
# View(names(test))

length(which(is.na(base11$int))) #
cond = which(is.na(base11$int) & is.na(base11$dt))
length(cond)
#
startingpoint = base11[c("dt", "dt.e", "int")]
#View(startingpoint)
cond2 = (cond +1)[1:length(cond)-1]
cond1 = cond[1:length(cond)-1]
base11[cond2, "dt"] = base11$dt.e[cond1]
base11$dt[1] = base11$dt.e[1] -1
library(lubridate)
base11[cond, "int"] = as.character(interval(base11$dt[cond], base11$dt.e[cond])   )
length(which(is.na(base11$int))) # 0, good


base16 = readRDS("C:\\chris\\PhD\\Dissertation\\prey & habitat\\HB1603\\Data\\combined\\HB1603.base.all.04.23.21.rds") 

nrow(base11) # 1294
nrow(base13) # 1385
nrow(base16) # 1586
sum(nrow(base11), nrow(base13), nrow(base16)) # 4265

base = bind_rows(base11, base13, base16)
nrow(base) # 4265
#View(base[c("dt", "dt.e", "int")])


###########################################################################################3

#  fill in missing depth values,
base <- base[order(base$dt),] 
for (i in 1:nrow(base)) {
  if (is.na(base$bt_mean[i])){ 
    base$bt_mean[i] = mean(c(base$bt_mean[i-2], base$bt_mean[i-1], base$bt_mean[i+1], base$bt_mean[i+2]),na.rm=T) }
  if (is.na(base$bt_slope[i])){ 
    base$bt_slope[i] = mean(c(base$bt_slope[i-2], base$bt_slope[i-1], base$bt_slope[i+1], base$bt_slope[i+2]),na.rm=T)}
  if (is.na(base$bt_sd[i])){ 
    base$bt_sd[i] = mean(c(base$bt_sd[i-2], base$bt_sd[i-1], base$bt_sd[i+1], base$bt_sd[i+2]),na.rm=T)
  }
}


#################################################################
## Explore species counts generally
unique(base$COMNAME)
unique(base$SPECIES)

summary(base$COMNAME)
base$sighting = 0
base[!is.na(base$CUMUSIGHTID), "sighting"] = 1
sum(base$sighting, na.rm=T)   #1641
summary(base$SIZEBEST)
sum(base$SIZEBEST, na.rm =T)  #18980
tapply(base$sighting,base$COMNAME,sum,na.rm=T)
tapply(base$SIZEBEST,base$COMNAME,sum,na.rm=T)



#################################################################
## fin whale specific variables and dataset


# Create dataset with species, fin name, and nespp4 for future reference
sp.vars = c("COMNAME", "SPECIES", "NESPP4")
sp.id = base[sp.vars]
sp.id = sp.id[!is.na(sp.id$SPECIES),]
sp.id = unique(sp.id)
sp.id = sp.id[order(sp.id$COMNAME),]
#View(sp.id)
write.csv(sp.id, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\R\\amapps.species.codes.csv")
sp.id[grep("Fin", sp.id$COMNAME),]
#           COMNAME SPECIES NESPP4
# 690     Whale, Fin    FIWH   6931
# 479 Whale, Fin/Sei    FISE     NAv



base.fin = base
base.fin$fin = 'none'
base.fin[c(which(base.fin$NESPP4 == 6931 )), 'fin'] = 'fin'
nrow(base.fin) #4265
table(base.fin$fin) # 166 sightings

# 1) Create numerical species sightings variable 
base.fin$fin.sight = 0
base.fin[c(which(base.fin$NESPP4 == 6931)), 'fin.sight'] = 1
sum(base.fin$fin.sight) #80
nrow(base); nrow(base.fin) #4265 for both

# 2) Sort dataset by dt and fin numeric variable, fin will be first if multiple sightings
base.fin = base.fin[order(base.fin$dt, -base.fin$fin.sight),] 


########################
### Create individual densities to sum if there are more than one per cell
base.fin$density.i = (1* base.fin$SIZEBEST)/(2*1000*base.fin$pA)
#write.csv(base.fin, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\R\\test\\check.base.fin.csv")

keep.vars = c("int", "dt", "COMNAME", "CUMUSIGHTID", "SIZEBEST", "pA", "mean.grp.size", "sightings.per.cell", "density.t", "density.i")
test.fin = base.fin[keep.vars]
nrow(test.fin) #4265








################################################################################################
### GET SUMS OF SIZEBEST AND GROUPS (WHICH I DON'T REALLY NEED) PRIOR TO THE AGGERGATION STEP
### JUST REMEMBER TO NOT INCLUDE THE ROWS WHERE PA=NA
tst2 = base.fin[which(base.fin$fin == "fin" & base.fin$pA > 0),]
table(tst2$fin) # 154 sightings (groups)
tapply(tst2$SIZEBEST,tst2$fin,sum,na.rm=T) # 251 animals
keep.vars = c("int", "dt", "COMNAME", "fin", "CUMUSIGHTID", "SIZEBEST", "pA", "mean.grp.size", "sightings.per.cell", "density.t", "density.i")
tst2v= tst2[keep.vars]
#View(tst2v)
# Get # of cells occupied further down
################################################################################################



###############################################################################

# Probably should switch out some of these testing names for clarity, 
#  but it works, so I am not going to mess with it

keep.vars = c("int", "dt", "COMNAME", "fin", "CUMUSIGHTID", "SIZEBEST", "pA", "mean.grp.size", "sightings.per.cell", "density.t", "density.i")
test.fin = base.fin[keep.vars]
# View(test.fin)
nrow(test.fin) #4265

# Assign 0s to NAs so aggregate works properly
test.fin$density.i[is.na(test.fin$density.i)] = 0
junk2 = aggregate(x = test.fin$density.i,                # Specify data column
                  # by = list(test.fin$int, test.fin$COMNAME),              # Specify group indicator
                  by = list(test.fin$int, test.fin$fin),              # Specify group indicator
                  # FUN = sum)
                  FUN = function(x) sum(x, na.rm = TRUE)) # Josh's addition, just in case
colnames(junk2) = c("int", "fin", "density.cell")

# Compare the two
require(dplyr)
junk0=junk2


head(diff)  #163 reords
nrow(test.fin) #4265 records
nrow(junk0[junk0$density.cell > 0 & junk0$density.cell > 0 & junk0$fin =="fin", ]) # 104 occupied cells (confirm further down)


# fix with above where 0s are subsituted for NAs
junk = junk0

nrow(base.fin)
nrow(junk)
test = left_join(base.fin, junk, by =c("int", "fin"))
base.fin= test
nrow(base.fin) #4265


nrow(base.fin[base.fin$density.i > 0 & base.fin$density.cell > 0 & base.fin$fin =="fin", ]) 

keep.vars = c("int", "dt", "COMNAME",  "CUMUSIGHTID", "SIZEBEST", "mean.grp.size", "sightings.per.cell",
              "pA",  "fin", "density.t", "density.i", "density.cell")



###############################################################################
#

# 1) Sort by date, 2) sort by fin, 3) keep only one date, keep fin if is one
test.fin = base.fin
# sort by date and fin
test.fin <- test.fin[order(test.fin$int, -test.fin$fin.sight),]
keep.vars.lim = c("int", "dt", "COMNAME", "SIZEBEST",
                  "pA",  "fin", "fin.sight", "density.t", "density.i", "density.cell")
# View(test.fin[keep.vars.lim])
test.fin.lim = test.fin[!duplicated(test.fin$int),]
# View(test.fin.lim[keep.vars.lim])

base.fin = test.fin.lim


## Make species specific density variable for modeling
base.fin$fin.density = 0
#base.fin$fin.density.t = 0
for(i in 1:nrow(base.fin)){
  if(base.fin$fin[i] == "fin") { 
    base.fin$fin.density[i] = base.fin$density.cell[i] # updated to cell-based density #confirmed that if pA=0, cell density is 0
    # base.fin$fin.density.t[i] = base.fin$density.t[i]  # don't think this one is accurate any longer (raw count, doesn't sum)
  }
}

# Number of acoustic cells occupied
nrow(base.fin[base.fin$fin.density > 0  & base.fin$fin =="fin", ]) # Still 104, great!

# Make sure no duplicates of int (one fin density per acoustic cell)
table(duplicated(base.fin$int)) # good, no duplicates


################################################################################################






## Look at depth distribution
hist.f = hist(base[which(base$NESPP4 == 6931), 'bt_mean' ], breaks=c(50, 100, 200, 300, 400 ,500, seq(1000,3000, 500)), main = 'finback whale')
hist.f$breaks
hist.f$counts  #25 out of 166 are in an area with a depth below 200 m
# breaks: 50  100  200  300  400  500 1000 1500 2000 2500 3000
# counts:      71   59   11    7    5   10    3    0    0    0
# hmm...
check.f = base[which(base$NESPP4 == 6931), ]
table(check.f$bt_mean) # 
depths = base[which(base$NESPP4 == 6931), 'bt_mean' ]

# 
fin.only = base.fin[base.fin$NESPP4 == 6931,]
table(fin.only$bt_mean)


##### Reworked variable selection, earlier version was dropping non-ABC MFIs
## Want everything but MFI past 200m,except WS variables and NSB-38kHz

# All names
n = names(base.fin)
length(n) # 1168
# Drop WS
n = n[grepl("\\.NS\\.", n)]
length(n) # 784

# Select 0-200 MFI... above ended up dropping non ABC MFI stuff later on!!!
n.mfi.nsb = n[grepl("*NSB*", n)] #^ is for when it starts with... rest captured in Urmi code below
n.mfi.esb = n[grepl("*ESB*", n)] 
n.mfi.sbf = n[grepl("*SBF*", n)] 
n.mfi.zoo = n[grepl("*ZOO*", n)] 
n.mfi.phy = n[grepl("*PHY*", n)] 
n.mfi.all = c(n.mfi.nsb, n.mfi.esb, n.mfi.sbf, n.mfi.zoo, n.mfi.phy)

# Why was this MFI selection only limited to 100 m for the 50s before?
n.mfi.0.200 = n.mfi.all[grepl("200.000.200*", n.mfi.all)]
n.mfi.0.050 = n.mfi.all[grepl("50.000.050*", n.mfi.all)]
n.mfi.50.100 = n.mfi.all[grepl("50.050.100*", n.mfi.all)]
n.mfi.50.150 = n.mfi.all[grepl("50.100.150*", n.mfi.all)]
n.mfi.50.200 = n.mfi.all[grepl("50.150.200*", n.mfi.all)]

n.mfi = c(n.mfi.0.200, n.mfi.0.050, n.mfi.50.100, n.mfi.50.150, n.mfi.50.200)

# Limit names to variables that could be used in a model
all.vars = n
# get all names needed
toMatch = c("^ABC", "^AI","^COM", "^EA", "^Inertia", "^PO", "^Sv_mean")
toKeep = grep(paste(toMatch, collapse="|"), all.vars, value=TRUE)
toDrop = c("ESB", "SBF", "ZOO", "PHY", "NSB")
# noMFI = grep(paste(toDrop, collapse="|"), toKeep, value=TRUE) # this woudl be values rather than subs
noMFIsubs = grep(paste(toDrop, collapse="|"), toKeep)
noMFI = toKeep[-c(noMFIsubs)]

# Combine and then remove any duplicates
mostVars = c(noMFI, n.mfi); length(mostVars)
mostVars = unique(mostVars)     ;length(mostVars) #686
# View(mostVars)

# Remove 038 NSBs. Favor 200 NSB, but consider 120
NSBtoDrop = mostVars[grepl("NSB", mostVars, fixed=TRUE) & grepl(".038", mostVars, fixed=TRUE)] #35 names
NSB38subs = grep(paste(NSBtoDrop, collapse="|"), mostVars)
mostVars = mostVars[-c(NSB38subs)]
length(mostVars) #651


# Rename to fit loop below
n.all = mostVars



test.c = "Sv_mean.SBF.WS.200.000.200.038"
test.fin = base.fin

# loop on variable list
for (j in 1:length(n.all)) {
  
  if (grepl("^Sv_mean*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -999.00), n.all[j] ] = -105
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = -105
  }
  
  # Don't see how this would be less than zero, but just in case... also won't turn NAs into 0
  if (grepl("^ABC*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) < 0 ), n.all[j] ] = 0
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = 0
  }
  
  # FYI summary(base$ABC.RAW.NS.200.800.1000.018) will look like it is all 0s and NAs, but that is 
  # because the numbers area ll so close to zero
  
  # Set fill-ins for Urmy variables
  if (grepl("^AI*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = 0
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = 0
  }
  
  if (grepl("^EA*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = 0
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = 0
  }
  
  # I guess this could happen if the others were missing 
  # if it does, use substr() to do math for the mid-point of the layer
  
  if (grepl("^COM*", n.all[j])) {
    
    # # check regular values to make sure they stay the sam
    # if(base.fin$COM.ESB.NS.200.400.600.038[j] != -9.9e+37 & !is.na(base.fin$COM.ESB.NS.200.400.600.038[j]) ) {
    #   print("check from here")
    # }
    # 
    # if(base.fin$COM.ESB.NS.200.400.600.038[j] == -9.9e+37 | is.na(base.fin$COM.ESB.NS.200.400.600.038[j]) ) {
    #   print("check from here")
    # }
    
    if (substr(n.all[j], 12,14) == "200") {
      
      if (as.numeric(substr(n.all[j], 16,19)) < 800) {
        mid = as.numeric(substr(n.all[j], 16,19)) + 100
        #Assigned mid, but didn't fill the value with mid?
        # base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
        # base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
      }   
      
      if (as.numeric(substr(n.all[j], 16,19)) >= 800) {
        mid = as.numeric(substr(n.all[j], 16,19)) + 100
        # base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
        # base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
      }
      
      if (as.numeric(substr(n.all[j], 16,19)) >= 1000) {
        mid = as.numeric(substr(n.all[j], 16,19)) + 100
        # base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
        # base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
      }
      
    } else {mid = as.numeric(substr(n.all[j], 19,21)) - 25
    # base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
    # base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
    }  # if is a 50 m resolution...
    
    ## This is where 200 goes after if statments above, no to 50s
    ## Also, apparently doesn't fill in like I thought it would
    
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
    
    if (substr(n.all[j], 12,13) == "50") { # if 50 m bins
      mid = as.numeric(substr(n.all[j], 16,19)) + 25
      base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = mid
      base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = mid
    }
    
  } # end of base COM if statement
  
  # Inertia (should be zero or super high?) May want to use other metrics instead of this one
  if (grepl("^Inertia*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = 0
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = 0
  }
  
  # PO
  if (grepl("^PO*", n.all[j])) {
    base.fin[which(eval(substitute(base.fin$ a, list(a = n.all[j]))) == -9.9e+37 ), n.all[j] ] = 0
    base.fin[which(eval(substitute(is.na(base.fin$ a), list(a = n.all[j]))) ), n.all[j] ] = 0
  }

}


summary(base.fin$fin.density)
# 8 NAs - Missing due to truncation distance
test = base.fin[which(is.na(base.fin$fin.density)),]
test[,c("COMNAME", "fin.density", "SIZEBEST", "SPECIES", "NESPP4", "pA")]



########################
### Re-run with fin density = 0 where currently is NA due to truncation distance!
### See if it makes any difference in variable selection

base.fin.d = base.fin
base.fin.d[c(which(is.na(base.fin.d$fin.density))), 'fin.density'] = 0 #already done, but just in case
summary(base.fin.d$fin.density)
base.fin = base.fin.d

saveRDS(mostVars, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\R\\prey_characterization5\\mostVars.rds")
saveRDS(base.fin, "C:\\chris\\PhD\\Dissertation\\prey & habitat\\R\\prey_characterization5\\base.fin.02.02.23.rds") 


#############################################################
#### Try with full dataset with filled in values


varlist = mostVars

aic.list <- vector(mode = "list", length = length(varlist))
dev.list <- vector(mode = "list", length = length(varlist))

gam.fin.deep <- lapply(varlist, function(x) { 
  gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ s(i, bs='ts', k=20), list(i=as.name(x)))) 
})

# gam.stats <- list(var = varlist,  aic = list(), dev = list())
gam.stats <- list(var = varlist,  aic = aic.list, dev = dev.list)

for (j in 1:length(varlist)) {
  gam.stats$aic[[j]] = AIC(gam.fin.deep[[j]])
  gam.stats$dev[[j]] = (1 - gam.fin.deep[[j]]$deviance/gam.fin.deep[[j]]$null.deviance)*100   # %deviance explained
  # plot.gam(gam.fin.deep[[j]])  # could plot if want to, could remember how to save plots or assemble them together
}

gam.stats
# turn list into dataframe, then sort based on AIC
gam.stats.df = as.data.frame(do.call(cbind, gam.stats))
# then sort on it
gam.stats.df$aic = as.numeric(gam.stats.df$aic)
gam.stats.df$dev = as.numeric(gam.stats.df$dev)

gam.stats.df  <- gam.stats.df[order(-gam.stats.df$dev),] 
gam.stats.df

#                                 var       aic          dev

gam.stats.df  <- gam.stats.df[order(gam.stats.df$aic),] 
gam.stats.df



# this outupt is not ideal, but better than nothing (gave error without unlist)
write.csv(unlist(gam.stats.df), "C:\\chris\\PhD\\Dissertation\\prey & habitat\\species models\\fin.all.gam.step1.csv")


###  SKIPPING PLOTS, ETC. GOING WITH DEPTH LIKE LAST TIME - VARIABLES VERY SIMILAR TO FIRST ROUND

gam.fin.deep[[100]]   # Sv_mean.RAW.NS.200.800.1000.038
plot.gam(gam.fin.deep[[100]])   # tight negative relationship. Most values bottom out at about -95, but -105 has some
#plot.gam(gam.fin.deep[[100]], xlim=c(0, 0.00005)) 

gam.fin.deep[[30]]   # Sv_mean.RAW.NS.200.800.1000.018
plot.gam(gam.fin.deep[[30]])  
# 18 & 38 pattern of 800-1000 m depth are about the same

gam.fin.deep[[32]]   # Inertia.RAW.NS.200.800.1000.018
plot.gam(gam.fin.deep[[32]])  # negative with higher inertia, but weaker relationship

gam.fin.deep[[102]]   # Inertia.RAW.NS.200.800.1000.038
plot.gam(gam.fin.deep[[102]])  # nearly the same


gam.fin.deep[[25]]   # Inertia.RAW.NS.200.600.800.018
plot.gam(gam.fin.deep[[25]])   # Negative with higher inertia at 600-800 m
# Similar to 800-1000 Sv plots. x-axis goes to -4 vs -6
# However, Sv mean has a big gap between on the lower end of the dB scale
# After this variable, AIC jumps about 2 points, so may stop here, although
# Intertia variables at 800-1000 somehow explain more deviance despite having larger confidence intervals 
# (perhaps due to better spread with regard to no data values)
# I'm hesitant to include a variable so deep when only 8 observations are in a depth > 600 and 3 greater than 800.
# Perhaps this is a way to include the depth presence... try with depth alone?

gam.fin.deep[[81]]   # Inertia.RAW.NS.200.200.400.038
# AIC 1586.042 (best AIC among all variables was 1582.202); deviance explained: 19.049579
plot.gam(gam.fin.deep[[81]])  # strong negative relationship, mostly negative past ~1000

gam.fin.deep[[643]]   # EA.SBF.NS.50.150.200.038
# AIC 1586.52 (best AIC among all variables was 1582.202); deviance explained: 18.305287
plot.gam(gam.fin.deep[[643]])  # very similar relationship to above - high evenness = low fin whales


## Variables with best deviance explained: PO.RAW.NS.200.600.800.038, COM.RAW.NS.200.800.1000.038
gam.fin.deep[[96]]   # PO.RAW.NS.200.600.800.038
# AIC 1589.545  (best AIC among all variables was 1582.202); deviance explained: 29.905354
plot.gam(gam.fin.deep[[96]]) # Really wacky plot

gam.fin.deep[[101]]   # PO.RAW.NS.200.600.800.038
# AIC 1590.284  (best AIC among all variables was 1582.202); deviance explained: 27.650072
plot.gam(gam.fin.deep[[101]]) 
plot.gam(gam.fin.deep[[101]], ylim = c(-100, 100) )
# Again, super strange plot, strong effects at particular locations with large confidence intervals,
# peaks at just over 800 and 900 (the default missing value)


test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) ) 
summary(test) #  explain the second most deviance (25%) and more than the lowest AIC
plot.gam(test) # decreasing with a little curve. CIs expand around 2000 m
AIC(test) # 2576.599, not quite the top AIC, but only 2 points away 


fin.deep.k.1 = gam.cv(formula = fin.density ~ s(Sv_mean.RAW.NS.200.800.1000.038, bs='ts', k=20) , 
                         k=10, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.1



fin.deep.k.1 = gam.cv(formula = fin.density ~ s(Inertia.RAW.NS.200.800.1000.018, bs='ts', k=20) , 
                      k=10, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.1



fin.deep.k.1 = gam.cv(formula = fin.density ~ s(Inertia.RAW.NS.200.600.800.018, bs='ts', k=20) , 
                      k=10, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.1


fin.deep.k.1 = gam.cv(formula = fin.density ~ s(bt_mean, bs='ts', k=20) , 
                      k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.1





###########################################
### Run step 2

varlist = mostVars
varlist2 = varlist[varlist != "bt_mean"] 
aic.list2 <- vector(mode = "list", length = length(varlist2))
dev.list2 <- vector(mode = "list", length = length(varlist2))


gam.fin.deep <- lapply(varlist2, function(x) { 
  gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
      s(bt_mean, bs='ts', k=20) +                                                                    
      s(i, bs='ts', k=20), list(i=as.name(x)))) 
})


# gam.stats2 <- list(var = varlist,  aic = list(), dev = list())
gam.stats2 <- list(var = varlist2,  aic = aic.list2, dev = dev.list2)

for (j in 1:length(varlist2)) {
  gam.stats2$aic[[j]] = AIC(gam.fin.deep[[j]])
  gam.stats2$dev[[j]] = (1 - gam.fin.deep[[j]]$deviance/gam.fin.deep[[j]]$null.deviance)*100   # %deviance explained
  # plot.gam(gam.fin.deep[[j]])  # could plot if want to, could remember how to save plots or assemble them together
}



gam.stats2
# turn list into dataframe, then sort based on AIC
gam.stats2.df = as.data.frame(do.call(cbind, gam.stats2))
# then sort on it
gam.stats2.df$aic = as.numeric(gam.stats2.df$aic)
gam.stats2.df$dev = as.numeric(gam.stats2.df$dev)

gam.stats2.df  <- gam.stats2.df[order(-gam.stats2.df$dev),] 
gam.stats2.df


gam.stats2.df  <- gam.stats2.df[order(gam.stats2.df$aic),] 
gam.stats2.df


# this outupt is not ideal, but better than nothing (gave error without unlist)
write.csv(unlist(gam.stats2.df), "C:\\chris\\PhD\\Dissertation\\prey & habitat\\species models\\fin.all.gam.step2.csv")


gam.fin.deep[[204]]   # ABC.RAW.NS.50.000.050.018
summary(gam.fin.deep[[204]])
plot.gam(gam.fin.deep[[204]], scale = 0)   
plot.gam(gam.fin.deep[[204]], scale = 0, select = 2, xlim = c(0, 0.00005), ylim =c(-50,25))
plot.gam(gam.fin.deep[[204]], scale = 0, select = 2, xlim = c(0, 0.00010), ylim =c(-100,25))
#plot.gam(gam.fin.deep[[204]], scale = 0, select = 2, xlim = c(0, 0.00020), ylim =c(-200,50))
plot.gam(gam.fin.deep[[204]], scale = 0, select = 2, xlim = c(0, 2e-05), ylim =c(-20,10))


gam.fin.deep[[1]]   # ABC.RAW.NS.200.000.200.018
summary(gam.fin.deep[[1]])
plot.gam(gam.fin.deep[[1]], scale = 0) # similar negative slope


gam.fin.deep[[205]]   # Sv_mean.RAW.NS.50.000.050.018
summary(gam.fin.deep[[205]]) # AIC a point higher then best AIC model, explained almost as much deviance : 29 vs 31
plot.gam(gam.fin.deep[[205]], scale = 0)  # tight plot, but hard to interpret
plot.gam(gam.fin.deep[[205]], scale = 0, select =2, ylim=c(-5,5))



fin.deep.k.2 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                           s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) , 
                         k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.2

fin.deep.k.2 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(ABC.RAW.NS.200.000.200.018, bs = "ts", k = 20) , 
                      k=10, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.2

#  



fin.deep.k.2 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(Sv_mean.RAW.NS.50.000.050.018, bs = "ts", k = 20) , 
                      k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.2


## Making plots to copy for documentation in Google Doc
test = gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
    s(bt_mean, bs = "ts",k = 20) +  s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) )) 
plot.gam(test, select =2)
summary(test)
plot.gam(test, scale = 0, select = 2, xlim = c(0, 0.00010), ylim =c(-100,25))
plot.gam(test, scale = 0, select = 2, xlim = c(0, 0.00005), ylim =c(-50,25))
plot.gam(test, scale = 0, select = 2, xlim = c(0, 2e-05), ylim =c(-20,10))


test = gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
          s(bt_mean, bs = "ts",k = 20) +  s(Sv_mean.RAW.NS.50.050.100.200, bs = "ts", k = 20) )) 
plot.gam(test, select =2)
summary(test)


test = gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
          s(bt_mean, bs = "ts",k = 20) +  s(Sv_mean.RAW.NS.50.000.050.018, bs = "ts", k = 20) )) 
plot.gam(test, select =2)
summary(test)
plot.gam(test, scale = 0, select =2, ylim=c(-5,5))


###########################################
### Run step 3

varlist = mostVars
varlist3 = varlist[varlist != "bt_mean" & varlist != "EA.RAW.NS.200.000.200.200"] 
aic.list3 <- vector(mode = "list", length = length(varlist3))
dev.list3 <- vector(mode = "list", length = length(varlist3))


gam.fin.deep <- lapply(varlist3, function(x) { 
  gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
      s(bt_mean, bs = "ts",k = 20) +  s(EA.RAW.NS.200.000.200.200, bs = "ts", k = 20) +                                                                 
      s(i, bs='ts', k=20), list(i=as.name(x)))) 
})


# gam.stats3 <- list(var = varlist,  aic = list(), dev = list())
gam.stats3 <- list(var = varlist3,  aic = aic.list3, dev = dev.list3)

for (j in 1:length(varlist3)) {
  gam.stats3$aic[[j]] = AIC(gam.fin.deep[[j]])
  gam.stats3$dev[[j]] = (1 - gam.fin.deep[[j]]$deviance/gam.fin.deep[[j]]$null.deviance)*100   # %deviance explained
  # plot.gam(gam.fin.deep[[j]])  # could plot if want to, could remember how to save plots or assemble them together
}



gam.stats3
# turn list into dataframe, then sort based on AIC
gam.stats3.df = as.data.frame(do.call(cbind, gam.stats3))
# then sort on it
gam.stats3.df$aic = as.numeric(gam.stats3.df$aic)
gam.stats3.df$dev = as.numeric(gam.stats3.df$dev)

gam.stats3.df  <- gam.stats3.df[order(-gam.stats3.df$dev),] 
gam.stats3.df

#                                 var       aic      dev


gam.stats3.df  <- gam.stats3.df[order(gam.stats3.df$aic),] 
gam.stats3.df




# this outupt is not ideal, but better than nothing (gave error without unlist)
write.csv(unlist(gam.stats3.df), "C:\\chris\\PhD\\Dissertation\\prey & habitat\\species models\\fin.all.gam.step3.csv")

#
gam.fin.deep[[188]]   # EA.RAW.NS.200.000.200.200
summary(gam.fin.deep[[188]]) # all very significant, nearly 34% deviance explained
plot.gam(gam.fin.deep[[188]], scale = 0)   
# 

gam.fin.deep[[523]]   # EA.NSB.NS.50.000.050.200
summary(gam.fin.deep[[523]])  # p-value a little better, deviance explained a little highter
plot.gam(gam.fin.deep[[523]], scale = 0)  # extremely similar plot to raw 200
plot.gam(gam.fin.deep[[523]], scale = 0, select = 3, ylim = c(-5,5))



fin.deep.k.3 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                           s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(EA.RAW.NS.200.000.200.200, bs = "ts", k = 20),
                         k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.3

#         MAPE          MAE       RelMAE          RHO         ASPE 
# 8.922199e+01 2.356651e-04 3.622943e-01 2.456870e-01 7.865917e-07

fin.deep.k.3 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20),
                      k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.3



## Test with Sv instead of ABC for second variable
test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(Sv_mean.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) )
summary(test) # all quite significant, 34.5% explained
AIC(test) # 2563.014 (quite a bit higher, best is 2558.high)
plot.gam(test, scale =0) # depth, linear negative, Sv plot is curved downward, EA plot about the same

test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(Sv_mean.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(EA.RAW.NS.200.000.200.200, bs = "ts", k = 20) )
summary(test)  # all quite significant, 34.5% explained (like previous one)
AIC(test) # 2565.033 (quite a bit higher, best is 2558.high)
plot.gam(test, scale=0) # similar to above, but Sv curve fairly flat for much of it, though tight CIs



concurvity(gam.fin.deep[[523]]) # worst case is 0.31

# See if outliers have anything to do with it


# Below concurvity is old, but same principle likely applies
base.fin.out = base.fin[which(base.fin$ABC.RAW.NS.50.000.050.018 < 0.00010),]
nrow(base.fin); nrow(base.fin.out)


# Would the concurvity still be there without outliers?
test = gam(method="REML", select=T, family=tw(), data = base.fin.out, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(Sv_mean.SBF.NS.50.000.050.038, bs = "ts", k = 20) )
concurvity(test) # nope, concurvity still there, not driven by outliers



### Try other top variables and test concurvity
test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(Sv_mean.RAW.NS.50.000.050.038, bs = "ts", k = 20) )
concurvity(test) # this one has concurvity issues as well
concurvity(test, full = FALSE)


#########################################


fin.deep.k.3 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(Sv_mean.SBF.NS.50.000.050.038, bs = "ts", k = 20),
                      k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.3



##########################################################################################

test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(PO.NSB.NS.50.000.050.200, bs = "ts", k = 20) )
concurvity(test) 
AIC(test)  # 
plot.gam(test, scale = 0, select=3)  # h


test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(EA.RAW.NS.50.000.050.200, bs = "ts", k = 20) )
concurvity(test) # 
summary(test) #
AIC(test)  # 
plot.gam(test, scale = 0) # 


test = gam(method="REML", select=T, family=tw(), data = base.fin, 
          fin.density~ s(bt_mean, bs='ts', k=20) +   
            s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
            s(PO.NSB.NS.50.000.050.120, bs = "ts", k = 20) )
concurvity(test) # 
summary(test) #
AIC(test)  # 
plot.gam(test, scale = 0) # 


test = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(COM.RAW.NS.50.000.050.038, bs = "ts", k = 20) )
concurvity(test) # 
summary(test) #
AIC(test)  # 
plot.gam(test, scale = 0) # 


##

###########################################
### Run step 4

varlist = mostVars
varlist4 = varlist[varlist != "bt_mean" & varlist != "ABC.RAW.NS.50.000.050.018" & varlist!= "EA.NSB.NS.50.000.050.200"] 
aic.list4 <- vector(mode = "list", length = length(varlist4))
dev.list4 <- vector(mode = "list", length = length(varlist4))


gam.fin.deep <- lapply(varlist4, function(x) { 
  gam(method="REML", select=T, family=tw(), data = base.fin, substitute(fin.density~ 
      s(bt_mean, bs='ts', k=20) +   s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +                                                                 
      s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) +  s(i, bs='ts', k=20), list(i=as.name(x)))) 
})


# gam.stats4 <- list(var = varlist,  aic = list(), dev = list())
gam.stats4 <- list(var = varlist4,  aic = aic.list4, dev = dev.list4)

for (j in 1:length(varlist4)) {
  gam.stats4$aic[[j]] = AIC(gam.fin.deep[[j]])
  gam.stats4$dev[[j]] = (1 - gam.fin.deep[[j]]$deviance/gam.fin.deep[[j]]$null.deviance)*100   # %deviance explained
  # plot.gam(gam.fin.deep[[j]])  # could plot if want to, could remember how to save plots or assemble them together
}



gam.stats4
# turn list into dataframe, then sort based on AIC
gam.stats4.df = as.data.frame(do.call(cbind, gam.stats4))
# then sort on it
gam.stats4.df$aic = as.numeric(gam.stats4.df$aic)
gam.stats4.df$dev = as.numeric(gam.stats4.df$dev)

gam.stats4.df  <- gam.stats4.df[order(-gam.stats4.df$dev),] 
gam.stats4.df


gam.stats4.df  <- gam.stats4.df[order(gam.stats4.df$aic),] 
gam.stats4.df


# this outupt is not ideal, but better than nothing (gave error without unlist)
write.csv(unlist(gam.stats4.df), "C:\\chris\\PhD\\Dissertation\\prey & habitat\\species models\\fin.all.gam.step4.csv")


gam.fin.deep[[533]]   # COM.SBF.NS.50.000.050.038
summary(gam.fin.deep[[533]]) # 
plot.gam(gam.fin.deep[[533]], scale = 0)
plot.gam(gam.fin.deep[[533]], scale = 0, select=4)  #

gam.fin.deep[[462]]   # ABC.RAW.NS.50.150.200.200
summary(gam.fin.deep[[462]]) # a
plot.gam(gam.fin.deep[[462]], scale = 0) # 
concurvity(gam.fin.deep[[462]]) # 

gam.fin.deep[[420]]   # ABC.RAW.NS.50.150.200.120
summary(gam.fin.deep[[420]]) # 
plot.gam(gam.fin.deep[[420]], scale = 0) # 
concurvity(gam.fin.deep[[420]]) # 0.37



fin.deep.k.4 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                           s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) +
                           s(COM.SBF.NS.50.000.050.038, bs = "ts", k = 20),
                         k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.4

fin.deep.k.4 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) +
                        s(ABC.RAW.NS.50.150.200.200, bs = "ts", k = 20),
                      k=100, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.4




#####################################
### Test some high freq, zoo, & NSB fish variables independently


# Try basic ABC & Sv for 120 and 200
gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
                fin.density~ s(bt_mean, bs='ts', k=20) +   
                  s(ABC.RAW.NS.50.000.050.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2)

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.RAW.NS.50.000.050.200, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2)

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(Sv_mean.RAW.NS.50.000.050.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(Sv_mean.RAW.NS.50.000.050.200, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)

# Try some of the top high freq variables from before


############################
#### PLOT THIS ONE
gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(PO.ZOO.NS.50.050.100.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)


gam.S5 =gam.test
pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.S5.pdf")
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.S5, scale=0, select=2,
         xlab="Zooplankton proportion occupied at 50-100 m", 
         ylab="Relative effect on fin whale abundance",
         cex.lab = 1.5)
dev.off()

gam.S5 =gam.test
tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.S5.tif")
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.S5, scale=0, select=2,
         xlab="Zooplankton proportion occupied at 50-100 m", 
         ylab="Relative effect on fin whale abundance",
         cex.lab = 1.5)
dev.off()
#######################





############

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.NSB.NS.200.000.200.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.RAW.NS.200.000.200.200, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.RAW.NS.50.150.200.200, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0)



# Check NSB and ZOO in first 50

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.NSB.NS.50.000.050.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0) # pretty much no relationship

gam.test = gam(method="REML", select=T, family=tw(), data = base.fin, 
               fin.density~ s(bt_mean, bs='ts', k=20) +   
                 s(ABC.ZOO.NS.50.000.050.120, bs = "ts", k = 20) )
gam.test # 
AIC(gam.test)
plot.gam(gam.test, select=2, scale=0) # pretty much no relationship





####################################

########### FINAL MODEL (for v3) ##########################################################

gam.fin.f = gam(method="REML", select=T, family=tw(), data = base.fin, 
           fin.density~ s(bt_mean, bs='ts', k=20) +   
             s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20) +
             s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) +  
             s(COM.SBF.NS.50.000.050.038, bs='ts', k=20))
gam.fin.f # 5.20
summary(gam.fin.f) # 38.2% deviance explained, all highly significant
plot.gam(gam.fin.f)
AIC(gam.fin.f)  #2553.303
plot.gam(gam.fin.f, scale = 0)
gam.check(gam.fin.f)
concurvity(gam.fin.f) # worst worst case is 0.36

## Try this 1000 times
fin.deep.k.4 = gam.cv(formula = fin.density ~ s(bt_mean, bs = "ts",k = 20) +
                        s(ABC.RAW.NS.50.000.050.018, bs = "ts", k = 20)  +  s(EA.NSB.NS.50.000.050.200, bs = "ts", k = 20) +
                        s(COM.SBF.NS.50.000.050.038, bs = "ts", k = 20),
                      k=1000, method="REML", select=T, family=tw(), data = base.fin  #, control = list(scale.est="robust"),
)
fin.deep.k.4



# Make manuscript plots
## - Making tiffs for inclusion in the word document,
##   but pdfs to submit separately

#----

# png("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v2\\fin.var1.v2.png")
pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var1.v5.pdf")
par(mar=c(4,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=1, 
         # xlab="Ocean Depth (m)", ylab="s(Bottom Depth (m))")
         xlab="Ocean depth (m)", ylab="Relative effect on fin whale abundance",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
dev.off()

# png("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v2\\fin.var1.v2.png")
tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var1.v5.tif")
par(mar=c(4,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=1, 
         # xlab="Bottom Depth (m)", ylab="s(Bottom Depth (m))")
         xlab="Ocean depth (m)", ylab="Relative effect on fin whale abundance",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
dev.off()

#----
# Make truncated image to serve as primary plot
pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.zoom.v5.pdf")
par(mar=c(4,4,1,1)+0.5)
#par(oma=c(1,4,1,1))
#mtext("axis label",outer=TRUE,line=2)
### ADD UNITS! ADD SECOND X-AXIS FOR EVENNESS
plot.gam(gam.fin.f, scale=0, select=2, 
         # xlab="18 kHz ABC at 0-50 m", ylab="s(18 kHz ABC at 0-50 m))")
         # xlab= expression("18 ~ kHz ~ s[a] ~ at ~ 10-50 ~ m"), ylab="Relative effect of 10-50 m 18 kHz S[a] on fin whale abundance",
         xlab= expression(paste("18 kHz ", s[a], " (", m^2*m^-2, ")", " at 10-50 m")), 
         ylab= "",
         cex.lab = 1.5, ylim=c(-8.5,4.2), xlim =c(0,0.00003))
#title(ylab=expression(paste("Relative effect of 10-50 m 18 kHz", s[a], " on fin whale abundance")), line=2.0, cex.lab=1.5)
#title(ylab=expression(atop("Relative effect of 10-50 m 18 kHz ", paste(s[a], "on fin whale abundance"))), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()


# Make truncated image to serve as primary plot
tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.zoom.v5.tiff")
par(mar=c(4,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=2, 
         # xlab="18 kHz ABC at 0-50 m", ylab="s(18 kHz ABC at 0-50 m))")
         # xlab= expression("18 ~ kHz ~ s[a] ~ at ~ 10-50 ~ m"), ylab="Relative effect of 10-50 m 18 kHz S[a] on fin whale abundance",
         xlab= expression(paste("18 kHz ", s[a], " (", m^2*m^-2, ")", " at 10-50 m")), 
         ylab= "",
         cex.lab = 1.5, ylim=c(-8.5,4.2), xlim =c(0,0.00003))
#title(ylab=expression(paste("Relative effect of 10-50 m 18 kHz ", s[a], "on fin whale abundance")), line=2.0, cex.lab=1.5)
#title(ylab=expression(atop("Relative effect of 10-50 m 18 kHz ", paste(s[a], "on fin whale abundance"))), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()

#----

# Make image of full plot (which will be an inset)
pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.v5.pdf")
#Could make EPS file if want to
#setEPS()
#postscript("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.zoom.v5.eps")
par(mar=c(4,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=2, #xlim = c(0,0.00005), 
         #xlab="18 kHz ABC at 0-50 m", 
         #ylab="s(18 kHz ABC at 0-50 m))",
         #main = 'Zoom',
         xlab= expression(paste("18 kHz ", s[a], " (", m^2*m^-2, ")", " at 10-50 m")),  
         ylab= "",
         cex.lab = 1.5)
#title(ylab=expression(paste("Relative effect of 10-50 m 18 kHz ", s[a], "on fin whale abundance")), line=2.0, cex.lab=1.5)
#title(ylab=expression(atop("Relative effect of 10-50 m 18 kHz ", paste(s[a], "on fin whale abundance"))), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()


# Make image of full plot (which will be an inset)
tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.v5.tiff")
#Could make EPS file if want to
#setEPS()
#postscript("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var2.zoom.v5.eps")
par(mar=c(4,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=2, #xlim = c(0,0.00005), 
         #xlab="18 kHz ABC at 0-50 m", 
         #ylab="s(18 kHz ABC at 0-50 m))",
         #main = 'Zoom',
         xlab= expression(paste("18 kHz ", s[a], " (", m^2*m^-2, ")", " at 10-50 m")),  
         ylab= "",
         cex.lab = 1.5)
#title(ylab=expression(paste("Relative effect of 10-50 m 18 kHz ", s[a], "on fin whale abundance")), line=2.0, cex.lab=1.5)
#title(ylab=expression(atop("Relative effect of 10-50 m 18 kHz ", paste(s[a], "on fin whale abundance"))), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()

# Read files into a pdf, added in the inset, and exported as a tiff

#----

pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var3.v5.pdf")
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=3, 
         # xlab="Evenness for non-swim bladder fish at 0-50 m", 
         xlab= "",
         ylab="",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
#title(ylab=expression(paste("Relative effect for evenness of non-swim bladder fish \n          at 10-50 m on fin whale abundance")), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
title(xlab=expression(paste( xlab=" less even                                                      more even ")), line=3.0, cex.lab=1.25)
title(xlab=expression(paste( xlab="Evenness (m) for non-swim bladder fish at 10-50 m")), line=5.0, cex.lab=1.5)
dev.off()

tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var3.v5.tif")
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=3, 
         # xlab="Evenness for non-swim bladder fish at 0-50 m", 
         xlab= "",
         ylab="",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
#title(ylab=expression(paste("Relative effect for evenness of non-swim bladder fish \n          at 10-50 m on fin whale abundance")), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
title(xlab=expression(paste( xlab=" less even                                                       more even ")), line=3.0, cex.lab=1.25)
title(xlab=expression(paste( xlab="Evenness (m) for non-swim bladder fish at 10-50 m")), line=5.0, cex.lab=1.5)
dev.off()

#----


pdf("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var4.v5.pdf")
#par(mar=c(4,4,1,1)+0.5)
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=4,
         xlab="Swimbladder fish center of mass (m) at 10-50 m", 
         ylab="",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
#title(ylab=expression(paste("Relative effect of swimbladder fish center of mass \n           at 10-50 m on fin whale abundance")), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()


tiff("C:\\chris\\PhD\\Dissertation\\prey & habitat\\Manuscript\\Figures\\v5\\fin.var4.v5.tif")
# par(mar=c(4,4,1,1)+0.5)
par(mar=c(7,4,1,1)+0.5)
plot.gam(gam.fin.f, scale=0, select=4,
         xlab="Swimbladder fish center of mass (m) at 10-50 m",
         ylab="",
         cex.lab = 1.5, ylim=c(-8.5,4.2))
#title(ylab=expression(paste("Relative effect of swimbladder fish center of mass \n           at 10-50 m on fin whale abundance")), line=2.0, cex.lab=1.5)
title(ylab="Relative effect on fin whale abundance", line=2.5, cex.lab=1.5)
dev.off()


#----


