# Hall 2019

# Script 2

# This script:
  # estimates plot-level AGB and damage
  # exports plot-level values for canopy height and damage for the island-wide AGB estimate
  # exports paper figures of regressions between CH and AGB, and NPV and AGB lost

# Jazlynn Hall 
# Chapter 2 - PhD Dissertation, columbia University E3B
# August 2019

###################################################
# PRELIMINATIES: random numbers, absolute paths, functionality
###################################################

set.seed(98374584) 

# set path names
user = "Jaz"
#user = "Gabriel"

if(user == "Jaz")
{
  path_scripts <- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Scripts/'
  path_formatteddamage <- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Data/FormattedDamage/'
  path_plotvalues <- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Data/PlotLevel_Values/'
}

if(user == "Gabriel")
{
  path_scripts <- '/Users/gabriela/Dropbox/PROFESIONAL/PROYECTOS/Hall-et-al/DataForMs/Scripts/'
  path_formatteddamage <- '/Users/gabriela/Dropbox/PROFESIONAL/PROYECTOS/Hall-et-al/DataForMs/Data/FormattedDamage/'
  path_plotvalues <- '/Users/gabriela/Dropbox/PROFESIONAL/PROYECTOS/Hall-et-al/DataForMs/Data/PlotLevel_Values/'
}

# Load functionality
library(ggplot2)
library(gridExtra)

# read in the functions for AGB estimates and Damage 
source(paste0(path_scripts, "functionsfordamagecalc.R")) # Jaz's functions
source(paste0(path_scripts, "functions-for-stem-volume-loss-20190826.R")) # Gabriel's functions for truncated cones etc

##############################################################
### GLOBAL PARAMETERS
##############################################################
# Gabriel: this section defines the options for killing or not
# broken trees, etc. The different ways of doing the analysis 
# should be easy to implement by modifying this section.

# Trees included:
big.trees.only = TRUE # use only large trees dbh > 10 cm or not
#big.trees.only = FALSE

# Preferred allometries
# Jaz: I don't think height allometry matters if we are using the BUD estimate, but I specify anyway. 
    # Both BUD and Conservative use Zambrano 
#height.allometry = "Chave"
height.allometry = "Zambrano" # from https://besjournals.onlinelibrary.wiley.com/doi/abs/10.1111/1365-2745.13075


# Jaz: didn't notice much difference between 
#agb.allometry = "Chave" 
agb.allometry = "Scatena" # from Jaz's code -- the PREMON height plays a role in here

overwrite.palms = TRUE # regardless of the chosen allometries, this will over-write the height & AGB for the palms. I use code from Jaz's code.

# Uprooted trees:
agb.loss.in.uprooted = 1  # this is the original implementation (it overestimates AGB loss) 
# Jaz: I decided to include uprooted as lost AGB in both estimates (conservative and BUD)
#agb.loss.in.uprooted = NA # it does nothing. AGB is considered lost if the tree is broken or dead


# Height to break (HTB) 
# Jaz: conservative estimate 
#use.HTB.when.possible = TRUE # this will use HTB in Luquillo even if other plots don't have it. Reccommended because many trees have this
# Jaz: BUD estimate 
use.HTB.when.possible = FALSE
imput.HTB = FALSE # this will imput HTB(%) = f(DBH). It does not add much and it is difficult to justify.
if(imput.HTB) use.HTB.when.possible = TRUE # if imputation happens all trees will have HTB

# Broken trees:
# (This is expressed in terms of broken below and broken above,
# but it includes the original implementation of 'kill all BUD')

# Jaz: BUD estimate 
agb.loss.in.broken.above = 1  # --- this is the original implementation:     
agb.loss.in.broken.below = 1  # anything broken is completely gone ---

#agb.loss.in.broken.above = 0  # --- this is one exploration made by 
#agb.loss.in.broken.below = 1  # Jaz as an alternative to the original ---

#agb.loss.in.broken.above = 0.5  # --- this is the other exploration made by 
#agb.loss.in.broken.below = 1  # Jaz as an alternative to the original ---

# Jaz:conservative estimate 
#agb.loss.in.broken.above = NA # --- this will not assume anything, it will estimate these
#agb.loss.in.broken.below = NA # values from trees with information on height to break ---

kill.broken.palms = TRUE # palms cannot survive without the apical bud, it can make sense to force "broken = dead" for them

# Branches etc:
# We have more or less fine estimates of % of remaining crown.
# We can use them when present or just use midpoints of damage categories.
#use.estimates.of.crown.loss = TRUE # Jaz: conservative
use.estimates.of.crown.loss = FALSE # Jaz: BUD


##############################################################
### MERGE AND FILTER DATASETS
##############################################################

# read in the data (multiple site types with different types of data, columns, etc.)
lfdp <- read.csv(paste0(path_formatteddamage, 'LFDP_190818.csv'))
#lfdp <- read.csv(paste0(path_formatteddamage, 'LFDP_191016_alltrees.csv')) # this version has all stems below 10cm DBH to compare AGV 
el <- read.csv(paste0(path_formatteddamage, 'ElevationGradient_190818.csv'))
cg <- read.csv(paste0(path_formatteddamage, 'ClimateGradient_190818.csv'))
cg <- read.csv(paste0(path_formatteddamage, 'ClimateGradient_190925.csv'))# This version fixed some of the messy break data so broken stems are counted as broken
cs <- read.csv(paste0(path_formatteddamage, 'Chronosequence_190818.csv'))

str(lfdp)
str(el)
str(cg)
str(cs)

# first format all columns to have the same structure 
lfdp <- formatcols(lfdp)
el <- formatcols(el)
cg <- formatcols(cg)
cs <- formatcols(cs)

# merge plots together into one dataframe 
all<- rbind(el,cg,cs,lfdp)
head(all)
unique(all$plot)
str(all)
str(lfdp)

# now only take values greater than 10 cm DBH, and only take trees without NA values for DBH
all <- all[!is.na(all$DIAM),]
if(big.trees.only) all <- all[all$DIAM>=10,]

# read in the plot areas 
plotareas <- read.csv(paste0(path_plotvalues, 'PlotAreas2.csv'))
names(plotareas)[3] <- 'PlotArea_m2'
data <- merge(all, plotareas[,c('plot','PlotArea_m2','lon','lat')], by=c('plot')) # I change the "all" name to "data"
head(data)

# COORDINATES -- necessary for Chave allometries
# NOTE GABRIEL: I DO NOT HAVE THE COORDINATES FOR ALL THE PLOTS,
# THAT'S WHY I AM USING RANDOM COORDINATES FOR ALL OF THEM, ROUGHLY
# IN THE AREAS WHERE FIELD PLOTS WERE LOCATED, BASED ON THE MAPS.
# They could be added to the PlotAreas.csv file. I am assuming
# that they will be there.

if(FALSE) # MAKE THIS 'FALSE' WHEN COORDINATES ARE KNOWN
{
  # a set of reference coordinates
  some.coordinates <- rbind(
    c(18.3077632,-66.6449313),
    c(18.3142895,-66.5938929),
    c(18.2308395,-66.6947505),
    c(18.190264,-66.611907),
    c(18.2373696,-66.1963024)); colnames(some.coordinates) <- c("lat", "lon")
    head(some.coordinates)
    
    # assign coordinates randomly to the plots
    i <- sample(1:nrow(some.coordinates), size = nrow(plotareas), replace = TRUE)
    plotareas$lon <- some.coordinates[i,"lon"]
    plotareas$lat <- some.coordinates[i,"lat"]
    head(plotareas)
}



##############################################################
### DEFINE THE ALLOMETRIES
##############################################################
# Each site may have a different E value for the Chave allometries,
# based on the coordinates. This step defines the allometric functions,
# and calculates the E parameter that corresponds to each stem.

any.chave = (height.allometry == "Chave" | agb.allometry == "Chave")
if(any.chave)
{
  #install.packages("raster")
  #install.packages("ncdf4")
  source("http://chave.ups-tlse.fr/pantropical_allometry/readlayers.r")
  coord <- as.matrix(plotareas[,c("lon", "lat")])
  E <- retrieve_raster("E", coord, plot = FALSE, format="nc")
  E <- data.frame(plot = plotareas$plot, E = E)
  data <- merge(data, E[,c("plot", "E")], by=c('plot'))
}


### AGB allometries:
if(agb.allometry == "Chave")
{
  # (this follows the allometries in Chave et al. 2014, the output is in kg)
  # (the E parameter can be passed as a vector with one value per dbh)
  get_AGB <- function(dbh.mm, E, palms = NULL, density = 0.65)
  {
    warning("PLEASE PROVIDE THE DBH IN mm, and the density in g/cm^3")
    dbh.in.cm = dbh.mm / 10
    TreeAGBkg <- exp(-1.803 - 0.976*E + 0.976*log(density) + 2.673*log(dbh.in.cm) - 0.0299*(log(dbh.in.cm))^2)
     
    if(!is.null(palms))
    {
      b1Ht = 0.8519565
      b2Ht = 0.64
      palms.height <- exp(b1Ht + b2Ht * log(dbh.in.cm[palms]))
      TreeAGBkg[palms] <-  7.7 * palms.height + 6.8
    }
    TreeAGBkg
  }
}


if(agb.allometry == "Scatena") # local allometries, better, Jaz's original option
{
  get_AGB <- function(dbh.mm, palms = NULL)
  {
    # Calculate everything, then overwrite for palms
    warning("PLEASE PROVIDE THE DBH IN mm")
    dbh.in.cm = dbh.mm / 10
    TreeAGBkg <- exp(2.475 *log(dbh.in.cm) - 2.399)
    
    # Calculate height for the palms, and use that for the AGB calculation
    # According to Jaz's note within her code, the height = f(dbh) is taken from María
    # and the agb = f(height) from Scatena. I ignore the original sources
    if(!is.null(palms))
    {
      b1Ht = 0.8519565
      b2Ht = 0.64
      palms.height <- exp(b1Ht + b2Ht * log(dbh.in.cm[palms]))
      TreeAGBkg[palms] <-  7.7 * palms.height + 6.8
    }
    TreeAGBkg
  }
}


### Height allometries:
if(height.allometry == "Zambrano")
{
  # This is a trivial, explicit modification of the original, so the input and output are in mm
  get_total_height <- function(dbh.mm, palms = NULL)
  {
    dbh.cm = dbh.mm / 10
    h.m <- 10^(-0.1318*(log(dbh.cm, base = 10))^2 + 0.8888*log(dbh.cm, base = 10) + 0.2708)
    
    # Over-write palms
    if(!is.null(palms))
    {
      b1Ht = 0.8519565
      b2Ht = 0.64
      h.m[palms] <- exp(b1Ht + b2Ht * log(dbh.cm[palms]))
    }
    h.mm <- h.m * 1000
    h.mm
  }
}


if(height.allometry == "Chave")
{
  # The input is assumed to be in mm, but it will try to change units if not.
  # The output is in mm.
  get_total_height <- function(dbh.mm, E, hmax = NULL, palms = NULL)
  {
    # make sure of numeric input
    dbh <- as.numeric(as.character(dbh.mm))
    E <- as.numeric(as.character(E))
    
    # check for mm input
    if(quantile(dbh, 0.75, na.rm = TRUE) > 20) UNITS.DBH = "mm"
    if(quantile(dbh, 0.75, na.rm = TRUE) < 20) UNITS.DBH = "cm"
    if(UNITS.DBH == "cm")
    {
      warning("It seems that the DBH units are in cm. They have been changed to mm")
      dbh <- dbh * 10
    }
    
    if(!is.null(hmax))
    {
      if(hmax > 10 & hmax < 100) UNITS.H = "m"
      if(hmax > 100 & hmax < 1000) UNITS.H = "dm"
      if(hmax > 1000 & hmax < 10000) UNITS.H = "cm"
      if(hmax > 10000 & hmax < 100000) UNITS.H = "mm"
      if(UNITS.H == "m")
      {
        warning("It seems that the height units are in m. They have been changed to mm")
        hmax = hmax * 1000
      }
      if(UNITS.H == "dm")
      {
        warning("It seems that the height units are in dm. They have been changed to mm")
        hmax = hmax * 100
      }
      if(UNITS.H == "cm")
      {
        warning("It seems that the height units are in cm. They have been changed to mm")
        hmax = hmax * 10
      }
    }
    
    # define the required allometries (functions)
    get_allometry_DH <- function(E)
    {
      f <- function(DBH) exp(0.893 - E + 0.760 * log(DBH) - 0.0340 * (log(DBH))^2)
      attr(f, "E") <- E
      f
    }
    
    get_htop <- get_allometry_DH(E = E)
    
    # do the calculations
    # (Chave et al's 2014 allometries get the dbh in cm and return the height in m)
    dbh.in.cm = dbh / 10
    htop.in.meters <- get_htop(dbh.in.cm)
    
    # Over-write palms
    if(!is.null(palms))
    {
      b1Ht = 0.8519565
      b2Ht = 0.64
      htop.in.meters[palms] <- exp(b1Ht + b2Ht * log(dbh.in.cm[palms]))
    }
    
    # Output
    htop <- htop.in.meters * 1000 # in mm
    if(!is.null(hmax) & length(dbh) > 1)
    {
      require(scales)
      htop <- scales::rescale(htop, to = c(1300, hmax))
    }
    htop
  }
}
  


##############################################################
### HANDLE HEIGHTS AND HEIGHTS TO BREAK
##############################################################
# Gabriel: this implements the height allometries, then adjust the
# observed height to breaks so they are never higher than the estimated
# maximum height. After this general correction, it fits a model
# to imput (relative) height to break as a functin of DBH, if
# that option has been activated in the section of general method options.

# Calculate height
palms = NULL
if(overwrite.palms) palms <- which(data$species %in% c('PREMON', 'ROYBOR'))

if(height.allometry == "Zambrano")
  data$TotalHeight <- get_total_height(dbh.mm = data$DIAM*10, palms = palms)

if(height.allometry == "Chave")
  data$TotalHeight <- get_total_height(dbh.mm = data$DIAM*10, E = data$E, palms = palms, hmax = NULL)


# Crop height to break
data$BreakHeight.mm <- data$BreakHeight * 1000
to.change <- which(data$BreakHeight.mm > data$TotalHeight)
data$BreakHeight.mm[to.change] <- data$TotalHeight[to.change]

# Imput missing heights to break
if(imput.HTB)
{
  # Gabriel: the HTB will be positively correlated with the
  # dbh just because large trees can be broken low or high while
  # short trees can be broken low only. I fit a model of
  # relative height to break, not absolute height to break.
  # There is almost no relationship, so the model-based
  # imputation is basically the same as the imputation
  # using the mean height to break.
  
  plot(data$DIAM, data$BreakHeight.mm, log = "x")
  cor.test(data$DIAM, data$BreakHeight.mm)
  relative.HTB <- data$BreakHeight.mm / data$TotalHeight
  do.with <- which(!is.na(relative.HTB) & !is.na(data$DIAM))
  plot(data$DIAM[do.with], relative.HTB[do.with], log = "x")
  cor.test(data$DIAM[do.with], relative.HTB[do.with])
  
  # Fit a model of relative HTB
  tmp <- data.frame(x = data$DIAM[do.with], y =  relative.HTB[do.with])
  mod <- glm(y ~ x, data = tmp)
  
  # Predict missing relative HTB
  sort(table(data$BU[data$BU2 == 0])) # not broken, not uprooted
  sort(table(data$BU[data$BU2 == 1])) # broken or [broken & uprooted]
  sort(table(data$BU[data$BU2 == 2])) # uprooted but not broken
  sort(table(data$BU[data$BU2 == 3])) # I have no idea
  missing <- which(data$BU2 == 1 & is.na(data$BreakHeight.mm))
  length(missing) # quite few! why?
  relative.HTB.missing <- coef(mod)[1] + coef(mod)[2] * data$DIAM[missing]
  
  # Go back and express in terms of absolute HTB
  data$BreakHeight.mm[missing] <- data$TotalHeight[missing] * relative.HTB.missing
  
  # Update the break-above and break-below codes, after the adjustment and imputation of heights to break.
  # This respects the old value, it does not remove information on broken-below vs. broken-above
  data$Break_BPOM[which(data$BreakHeight.mm <= 1300 | data$Break_BPOM == "B")] <- "B"
  data$Break_APOM[which(data$BreakHeight.mm >  1300 | data$Break_APOM == "A")] <- "A"
  table(data$Break_BPOM)
  table(data$Break_APOM)
  
}


##################################################################
### PRE-HURRICANE BASAL AREA AND AGB, INCLUDING BRANCH & LEAF AGB
##################################################################
# Gabriel: this section adds basal area and AGB per tree,
# as it was before the hurricane. As typical, it assumes
# perfect trees, never broken, never hollow, etc.
# Assuming pre-hurricane perfection is a source of bias
# that we cannot estimate or control in any way. Except
# perhaps by looking at the % of broken trees etc. in
# other forests, like BCI. But we won't do that now.
# Pre-hurricane perfection is bias that will remain.

# Add basal area
data <- calc.ba(data)

# Tree-level AGB
palms = NULL
if(overwrite.palms) palms <- which(data$species %in% c('PREMON', 'ROYBOR'))

if(agb.allometry == "Scatena")
  data$TreeAGBkg <- get_AGB(dbh.mm = data$DIAM * 10, palms = palms)

if(agb.allometry == "Chave")
  data$TreeAGBkg <- get_AGB(dbh.mm = data$DIAM * 10, E = data$E, palms = palms, density = 0.65)

# Do the summary by plot:
bioag<- aggregate(TreeAGBkg~plot, data = data, FUN='sum') 
names(bioag)[2]<- 'PlotAGBkg'  
data<- merge(data, bioag, by='plot')
head(data)


# Branches and leaves -- using Scatena allometries, 
# Chave allometries don't have this level of detail.
# The palms are excluded by over-writing 0 for them.
# The code is taken directly from Jaz's function, using diameter in cm
data$BranchAGBkg <- exp(1.982 *log(data$DIAM) - 3.69)
data$LeafAGBkg <- exp(1.792 * log(data$DIAM) - 3.758)
data$BranchAGBkg[palms] <- 0
data$LeafAGBkg[palms] <- 0  
head(data)


#######################################################################
### POST-HURRICANE AGB, including the loss due to branches and leaves
#######################################################################
# Gabriel: this is where the calculations are done and where the
# decisions have more impact. To implement the options more easily,
# I extracted some of the code from inside the functions written by Jaz.

# First, it tries to calculate the most accurate estimate possible, 
# for those trees with the information on height to break. Then, 
# it uses that information to estimate the % of AGB
# loss in trees broken-below vs broken-above, for those broken
# trees with no information on height to break.

# 1- Try to calculate things as accurate as possible for the broken trees:
# The calculation here is the proportion of volume (= proportion of AGB) above a given point.
# This is the relative loss, in [0, 1]. That column can be updated or complemented later for dead trees, etc.
data$TreeAGBkg.relative.loss <- NA
do <- which(!is.na(data$BreakHeight.mm))

for(i in do)
{
  p = get_p_volume_above_h(dbh = as.numeric(data[i,"DIAM"])*10,
                              h = as.numeric(data[i,"BreakHeight.mm"]),
                              maximum.height = as.numeric(data[i,"TotalHeight"]))
  
  data$TreeAGBkg.relative.loss[i] <- p
  cat(i, "- ")
}

head(data)
summary(data$TreeAGBkg.relative.loss)
table(data$TreeAGBkg.relative.loss > 0)


# 2- Add the proportion of AGB loss in broken-above and broken-below trees.
# (Estimate it first, if it is not available)
# alive vs dead: # NF are most likely dead -- empty spaces are most likely alive
if(is.na(agb.loss.in.broken.above))
{
  if(kill.broken.palms)  living.BA <- which((data$Break_APOM == "A" | data$BreakHeight.mm > 1300) 
                                            & data$StatusAD %in% c("A", "") & !(data$species %in% c("PREMON", "ROYBOR")))
  if(!kill.broken.palms) living.BA <- which((data$Break_APOM == "A" | data$BreakHeight.mm > 1300) 
                                            & data$StatusAD %in% c("A", ""))
  agb.loss.in.broken.above = mean(data[living.BA, "TreeAGBkg.relative.loss"], na.rm = TRUE)
  length(living.BA)
}

if(is.na(agb.loss.in.broken.below))
{
  if(kill.broken.palms)  living.BB <- which((data$Break_BPOM == "B" | data$BreakHeight.mm <= 1300)
                                            & data$StatusAD %in% c("A", "") & !(data$species %in% c("PREMON", "ROYBOR")))
  if(!kill.broken.palms) living.BB <- which((data$Break_BPOM == "B" | data$BreakHeight.mm <= 1300)
                                            & data$StatusAD %in% c("A", ""))
  agb.loss.in.broken.below = mean(data[living.BB, "TreeAGBkg.relative.loss"], na.rm = TRUE)
  length(living.BB) # few!
}

BA.missing.HTB <- which(data$Break_APOM == "A" & is.na(data$BreakHeight.mm))
BB.missing.HTB <- which(data$Break_BPOM == "B" & is.na(data$BreakHeight.mm))

BA.not.missing.HTB <- which(data$Break_APOM == "A" & !is.na(data$BreakHeight.mm))
BB.not.missing.HTB <- which(data$Break_BPOM == "B" & !is.na(data$BreakHeight.mm))

# Note Gabriel -- it does not seem justified to discard the information on
# the height to break and substitute that by mean-value imputation. There 
# are many more trees with information on specific HTB than trees for which
# only BB or BA is known.

length(BA.missing.HTB)
length(BA.not.missing.HTB)
length(BB.missing.HTB)
length(BB.not.missing.HTB)


if(!use.HTB.when.possible) # if we renounce to use the avilable info, it labels everything as "missing HTB"
{
  BA.missing.HTB <- c(BA.missing.HTB, BA.not.missing.HTB)
  BB.missing.HTB <- c(BB.missing.HTB, BB.not.missing.HTB)
}

data$TreeAGBkg.relative.loss[BA.missing.HTB] <- agb.loss.in.broken.above
data$TreeAGBkg.relative.loss[BB.missing.HTB] <- agb.loss.in.broken.below


# 3- Add or over-write losses for dead, uprooted trees (possibly treated as dead), and broken palms (possibly treated as dead)
dead <- which(!data$StatusAD %in% c("A", "")) # NF are most likely dead -- empty spaces are most likely alive
length(dead) / nrow(data) # ~ 10% or so of mortality
data$TreeAGBkg.relative.loss[dead] <- 1 # complete AGB loss in dead trees

if(!is.na(agb.loss.in.uprooted))
{
  if(agb.loss.in.uprooted)
  {
    uprooted <- which(data$BU2 == 2 | grepl("U", data$BU)) # is this enough? just BU2 = 2 does not recover [broken and uprooted]
    length(uprooted) / nrow(data) # this % of uprooted (either broken or not) should make sense
    data$TreeAGBkg.relative.loss[uprooted] <- 1 # complete AGB loss in uprooted trees
  }
}

if(kill.broken.palms)
{
  broken <- which(data$BU2 == 1) # broken, included [broken and uprooted]
  palms <- which(data$species %in% c('PREMON', "ROYBOR"))
  broken.palms <- intersect(broken, palms)
  length(broken.palms) / length(palms)
  data$TreeAGBkg.relative.loss[broken.palms] <- 1 # complete AGB loss in broken palms
}

# The remaining trees are assumed to be complete (except for branches & leaves, calculated later)
data$TreeAGBkg.relative.loss[is.na(data$TreeAGBkg.relative.loss)] <- 0


# 4- Branch and leaf loss
# Gabriel: it seems there are two sources of information here. One is the % of remaining crown (WITHIN THE LIVING LENGTH).
# The other is the none-light-medium-heavy crown damage levels. These seem to follow a prior protocol. The second source
# of information is easier to use. The first source of information requires estimating the amount of crown within
# a given proportion of the main axis. 

# This calculation is done ONLY for those trees that seem complete so far. Anything with TreeAGBkg.relative.loss > 0
# is assumed to have a TREE-LEVEL AGB loss estimate that already includes branches and leaves. This includes broken trees,
# uprooted trees, dead trees, etc. as handled by the global options.

# Clean the Damage column a bit
data$Damage[data$Damage %in% c("Light", "L")] <- "Light"
data$Damage[data$Damage %in% c("Medium", "M")] <- "Medium"
data$Damage[data$Damage %in% c("Heavy", "S")] <- "Heavy"
data$Damage[!data$Damage %in% c("Light", "Medium", "Heavy")] <- "None"

# Calculation based on mid-points of categorical damage levels
data$BranchAGBkg.relative.loss <- 0
do <- which(data$TreeAGBkg.relative.loss == 0) # add this nuance for "apparently complete" trees only
data$BranchAGBkg.relative.loss[intersect(do, data$Damage == "Light")]  <- 0.125 # assume lightly damaged trees lost 12.5% of branch AGB
data$BranchAGBkg.relative.loss[intersect(do, data$Damage == "Medium")] <- 0.500 # assume moderately damaged trees lost 50% of branch AGB
data$BranchAGBkg.relative.loss[intersect(do, data$Damage == "Heavy")]  <- 0.875 # assume heavily damaged trees lost 87.5% of branch AGB

# Over-write if there is a finer % of crown loss that we want to use:
if(use.estimates.of.crown.loss)
{
  # add this nuance for "apparently complete" trees only that have information on % remaining crown
  do <- which(data$TreeAGBkg.relative.loss == 0 & !is.na(data$PCrownRemaining))
  data$BranchAGBkg.relative.loss[do] <- (1 - data$PCrownRemaining/100)[do]
}

# 5- Do the final calculation of absolute AGB loss per tree. It iterates across the levels of AGB loss estimate:
# (a) Using the tree-level estimate -->
data$TreeAGBkg.absolute.loss <- data$TreeAGBkg * data$TreeAGBkg.relative.loss

# (b) --> if it looks complete, use the branch-level estimate --> 
complete <- which(data$TreeAGBkg.absolute.loss == 0)
data$TreeAGBkg.absolute.loss[complete] <- ((data$BranchAGBkg * data$BranchAGBkg.relative.loss) + (data$LeafAGBkg * 1)) [complete]

# Jaz - ALL LEAF AGB IS ASSUMED TO BE LOST WHETHER THERE IS BRANCH DAMAGE OR NOT. 
#       THIS HAS BEEN UPDATED IN THE LINE ABOVE 
# (c) --> if it still looks complete, remove all the leaves (we are assuming 100% leaf loss during the hurricane).
#complete <- which(data$TreeAGBkg.absolute.loss == 0)
#data$TreeAGBkg.absolute.loss[complete] <- (data$LeafAGBkg * 1)[complete]


# 6- Compare with the initial AGB and the "kill all BUD trees" option----
total.initial.agb = sum(data$TreeAGBkg, na.rm = TRUE)
total.loss.agb = sum(data$TreeAGBkg.absolute.loss, na.rm = TRUE)
total.loss.agb / total.initial.agb # this is our estimate
# Jaz: when using BUD estimate, this value is :
    #  when using the conservative estimate, this value is 20.55%

b <- which(data$BU2 == 1)
u <- which(data$BU2 == 2)
d <- which(!data$StatusAD %in% c("A", ""))
bud <- unique(c(b, u, d))
not.bud <- (1:nrow(data))[-bud]
agb.in.buds =  sum(data$TreeAGBkg[bud], na.rm = TRUE)
extra.loss.in.not.buds = sum((data$BranchAGBkg * data$BranchAGBkg.relative.loss)[not.bud] + data$LeafAGBkg[not.bud])
(agb.in.buds + extra.loss.in.not.buds) / total.initial.agb
# Jaz: when using BUD estimate, this value is :
#  when using the conservative estimate, this value is : 0.2969615, higher, why is that? 

###################################################################
# SUMMARIES OR VALUES PER PLOT
###################################################################

# Find basal area damaged in each category 
# (Gabriel: I haven't checked this; it seems this calculation is relevant only under the "all or nothing" scenario)
ba_dam <- calc.damage.ba(data)
head(ba_dam)
ba_dam$BA_BUD<- ba_dam$BA_B + ba_dam$BA_U + ba_dam$BA_D
ba_dam$BApercent_BUD<- (ba_dam$BA_B + ba_dam$BA_U + ba_dam$BA_D) / ba_dam$PlotBA * 100


# Plot-level loss in Mg per hectare
# The AGB loss is in kg: Jaz: "divide kg by 1,000 to get Mg, then multiply m2 by 10,000 to get ha"
# (Note Gabriel -- Jaz wrote 'multiply' and multiplied, but it should be a division... I write code for this from scratch, for comparisons)
plot.ids <- unique(data$plot)
loss.per.plot.Mg.ha <- sapply(plot.ids, function(id) {
  sub <- data[data$plot == id,]
  loss.kg = sum(sub$TreeAGBkg.absolute.loss, na.rm = TRUE)
  area.m2 = sub$PlotArea_m2[1]
  loss.Mg = loss.kg / 1000
  area.ha = area.m2 / 10000
  loss.Mg / area.ha
})

names(loss.per.plot.Mg.ha) <- as.character(plot.ids)
dotchart(sort(loss.per.plot.Mg.ha))
median(loss.per.plot.Mg.ha)


#
#
# GABRIEL -- I HAD TO STOP HERE, SORRY!  
#
#

# JAZ: taken over from Gabriel's updates on Sept 2, 2019
# plot-level AGB estimate in Mg per ha
agb.per.plot.Mg.ha <- sapply(plot.ids, function(id) {
  sub <- data[data$plot == id,]
  agb.kg = sub$PlotAGBkg[1]
  area.m2 = sub$PlotArea_m2[1]
  agb.Mg = agb.kg / 1000
  area.ha = area.m2 / 10000
  agb.Mg / area.ha
})
names(agb.per.plot.Mg.ha) <- as.character(plot.ids)

# Plot-level proportion of AGB lost 
prop.loss.per.plot.Mg.ha <- sapply(plot.ids, function(id) {
  sub <- data[data$plot == id,]
  loss.kg = sum(sub$TreeAGBkg.absolute.loss, na.rm = TRUE)
  preAGB.kg = sub$PlotAGBkg[1]
  loss.kg/preAGB.kg
})
names(prop.loss.per.plot.Mg.ha) <- as.character(plot.ids)

plotloss_Mgha = data.frame(plot = names(loss.per.plot.Mg.ha), AGB_lostMgha = loss.per.plot.Mg.ha)
plotloss_prop = data.frame(plot = names(prop.loss.per.plot.Mg.ha), LossProportion = prop.loss.per.plot.Mg.ha)
plotAGB_Mgha<-  data.frame(plot = names(agb.per.plot.Mg.ha), PlotAGB_Mgha = agb.per.plot.Mg.ha)


# Calculate the number, density, and proportion of stems lost 
stem_dam<- calc.stemstats(data)
# now calculate the final stem damage metrics 
stem_dam$n.bud<- stem_dam$n.b + stem_dam$n.u + stem_dam$n.d
stem_dam$stemdensityBUD_ha <- stem_dam$densityB_ha + stem_dam$densityU_ha + stem_dam$densityD_ha
stem_dam$stempercent_BUD <- (stem_dam$proportion_B + stem_dam$proportion_U + stem_dam$proportion_D) * 100

# Read in the npv and canopy height files
#npv<- read.csv(paste0(path_plotvalues, 'NPV_AreaWeighted_921to1101_190731.csv')) # this version didn't sum to one
npv<- read.csv(paste0(path_plotvalues, 'NPV_AreaWeighted_921to1101_190902.csv')) # this version did
ch<- read.csv(paste0(path_plotvalues, 'CanopyHeight_AWAverage.csv'))


# create a dataframe for all major plot-level damage metrics for plotting with NPV and CH
plotdata<- Reduce(merge, list(npv[,2:4], ch[,2:3],ba_dam[,c('plot','plottype', 'BApercent_BUD')], 
                  stem_dam[,c('plot','n.bud','stemdensityBUD_ha','stempercent_BUD')],  
                  plotAGB_Mgha, plotloss_Mgha, plotloss_prop))
length(unique(plotdata$plot))
head(plotdata)

# remove outliers 
plotdata<- plotdata[plotdata$plot!='SB3',] # SB3 is in a cloud shadow area, skewing results, outlier, remove 
plotdata<- plotdata[plotdata$plot!='Elev300',]

# export list of used plots to be used for maps (figs 1 and 3)
# first reorder so arcGIs doesn't think plot values are a numeric variable
plots<- plotdata[,c('plot','plottype')]
plots<- plots[order(plots$plottype),]
#write.csv(plots, paste0(path_plotvalues, 'plotsused_regression.csv'))

# Export plot-level values for landscape-level AGB analysis ----
#write.csv(plotdata, paste0(path_plotvalues, 'FinalDamageMetrics_NPV_190902.csv'))

# set names for the files to write depending on the global values above 
if(height.allometry == "Chave" & agb.allometry == "Chave" & use.HTB.when.possible == FALSE &
   use.estimates.of.crown.loss == FALSE & agb.loss.in.uprooted == 1 & 
   agb.loss.in.broken.above == 1 & agb.loss.in.broken.below == 1)
{write.csv(plotdata, paste0(path_plotvalues, 'AGBIterations/DamageMetrics190925_HChaveAGBChave_BUD.csv'))}
if(height.allometry == "Chave" & agb.allometry == "Chave" & use.HTB.when.possible == TRUE &
   use.estimates.of.crown.loss == TRUE & is.na(agb.loss.in.uprooted) & 
   agb.loss.in.broken.above == 0.4209094 & agb.loss.in.broken.below == 0.9309261)
{write.csv(plotdata, paste0(path_plotvalues, 'AGBIterations/DamageMetrics190925_HChaveAGBChave_HTBCLossNoU.csv'))}

if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == FALSE &
   use.estimates.of.crown.loss == FALSE & agb.loss.in.uprooted == 1 & 
   agb.loss.in.broken.above == 1 & agb.loss.in.broken.below == 1)
{write.csv(plotdata, paste0(path_plotvalues, 'AGBIterations/DamageMetrics190925_HZambranoAGBScatena_BUD.csv'))}
if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == TRUE &
   use.estimates.of.crown.loss == TRUE & is.na(agb.loss.in.uprooted) & 
   agb.loss.in.broken.above == 0.414422 & agb.loss.in.broken.below == 0.9293611)
{write.csv(plotdata, paste0(path_plotvalues, 'AGBIterations/DamageMetrics190925_AGBCScat_HTBCLossNoU.csv'))}
if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == TRUE &
   use.estimates.of.crown.loss == TRUE & agb.loss.in.uprooted==1)
{write.csv(plotdata, paste0(path_plotvalues, 'AGBIterations/DamageMetrics190925_AGBCScat_HTBCLossWU.csv'))}


###################################################################
# REGRESSIONS WITH NPV
###################################################################
# Gabriel: This section cannot be seen as an assessment of what
# are the good options and the bad options to assess AGB loss.
# The reasoning "lower R2 = worse AGB loss estimate" is flawed.
# The R2 does not inform about the error in the estimation of the response.
# Only if the predictor and the response were perfectly correlated
# one could infer about the error in the estimation of the response.
# E.g. hollow trees are invisible from above. A better estimate of AGB
# could include tree hollowness. That would reduce (not increase) the
# R2 in the regression with NPV, because NPV cannot reflect tree 
# hollowness from above. Variation due to hollowness is unexplained
# variation in such regression. The same with any improvement in AGB
# estimates that is not reflected by NPV.

# create a function to add model equations and R2 values to the plots as labels 
findlabel1<- function(damage, NPV){
  lm1<- lm(damage ~ NPV)  
  lmcoef<- format(round(lm1$coefficients[2], digits = 2), nsmall = 2)
  lmint<- format(round(lm1$coefficients[1], digits = 2),nsmall = 2)
  label1<- paste0("y = ",lmcoef," x + ",lmint)
  return(label1)}

findlabel2<- function(damage, NPV){
  lm1<- lm(damage ~ NPV)
  lmr2<- format(round(summary(lm1)$adj.r.squared, digits = 2), nsmall = 2)
  label2<- paste0("R^2 == ",lmr2)  
  return(label2)
}

# plot proportion of AGB lost ~ NPV ----
# export Fig 2 regression  NPV ~ AGB 
label1<- findlabel1(damage = plotdata$LossProportion, NPV = plotdata$NPV)
label2<- findlabel2(damage = plotdata$LossProportion, NPV = plotdata$NPV)
fig2<- ggplot(data=plotdata,aes(x=NPV, y=LossProportion)) + theme_bw()+  ylab(bquote('Proportion AGB Lost (Mg ha'^-1~')'))+ xlab(expression(paste(Delta ,"NPV")))+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+ 
  scale_x_continuous(limits=c(-1,1.2)) + scale_y_continuous(limits=c(-0.5,1.2)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(0,0.6)) +
  annotate(geom="text", x=0.23, y=(0.6)*.95, label=label1,color="blue", size=3.5)+
  annotate("text", x=0.23, y=(0.6)*.85, label=label2,parse=T,color="blue", size=3.5)
summary(lm(LossProportion ~ NPV, data=plotdata))
fig2
# export figure for manuscript - UPDATE THIS - 190818 is not the most recent figure
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/Regressions/Fig2_AGBproportionlost_NPV_190911.tiff",
#      width = 3, height = 3, units = 'in', res = 300)
# fig2
# dev.off()


# plot other damage metrics ----
# log(AGB lost) ~ NPV 

label1<- findlabel1(damage = log(plotdata$AGB_lostMgha), NPV = plotdata$NPV)
label2<- findlabel2(damage = log(plotdata$AGB_lostMgha), NPV = plotdata$NPV)
dam1<- ggplot(data=plotdata,aes(x=NPV, y=log(AGB_lostMgha))) + theme_bw()+ ylab('log(AGB Lost)')+ xlab(expression(paste(Delta ,"NPV")))+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+ ggtitle('A')+
  scale_x_continuous(limits=c(-1,1.2)) + scale_y_continuous(limits=c(0,10)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(1,8)) +
  annotate(geom="text", x=0.2, y=(8-1)*.95+1, label=label1,color="blue", size=3.5)+
  annotate("text", x=0.2, y=(8-1)*.75+1, label=label2,parse=T,color="blue", size=3.5)
summary(lm(log(AGB_lostMgha) ~ NPV, data=plotdata))
dam1

# % BA ~ NPV
label1<- findlabel1(damage = plotdata$BApercent_BUD, NPV = plotdata$NPV)
label2<- findlabel2(damage = plotdata$BApercent_BUD, NPV = plotdata$NPV)

dam2<- ggplot(data=plotdata,aes(x=NPV, y=BApercent_BUD)) + theme_bw()+  ylab('% Basal Area BUD')+ xlab(expression(paste(Delta ,"NPV")))+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+ ggtitle('B')+
  scale_x_continuous(limits=c(-1,1.2)) + scale_y_continuous(limits=c(-50,120)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(0,100)) +
  annotate(geom="text", x=0.2, y=(100-0)*.95+0, label=label1,color="blue", size=3.5)+
  annotate("text", x=0.2, y=(100-0)*.75+0, label=label2,parse=T,color="blue", size=3.5)
dam2
summary(lm(BApercent_BUD ~ NPV, data=plotdata))

# stem density ~ NPV 
label1<- findlabel1(damage = plotdata$stemdensityBUD_ha, NPV = plotdata$NPV)
label2<- findlabel2(damage = plotdata$stemdensityBUD_ha, NPV = plotdata$NPV)

dam3<- ggplot(data=plotdata,aes(x=NPV, y=stemdensityBUD_ha)) + theme_bw()+  ylab(bquote('Stems BUD ha'^-1))+ xlab(expression(paste(Delta ,"NPV")))+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+ ggtitle('C')+
  scale_x_continuous(limits=c(-1,1.2)) + scale_y_continuous(limits=c(-100,1000)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(0,850)) +
  annotate(geom="text", x=0.25, y=(850-0)*.95+0, label= label1,color="blue", size=3.5)+
  annotate("text", x=0.25, y=(850-0)*.75+0, label= label2,parse=T,color="blue", size=3.5)
dam3
summary(lm(stemdensityBUD_ha ~ NPV, data=plotdata))

# stem % ~ NPV 
label1<- findlabel1(damage = plotdata$stempercent_BUD, NPV = plotdata$NPV)
label2<- findlabel2(damage = plotdata$stempercent_BUD, NPV = plotdata$NPV)
dam4<- ggplot(data=plotdata,aes(x=NPV, y=stempercent_BUD)) + theme_bw()+  ylab('% Stems BUD')+ xlab(expression(paste(Delta ,"NPV")))+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+ ggtitle('D')+
  scale_x_continuous(limits=c(-1,1)) +  scale_y_continuous(limits=c(-10,120)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(0,100)) +
  annotate(geom="text", x=0.2, y=(100-0)*.95+0, label=label1,color="blue", size=3.5)+
  annotate("text", x=0.2, y=(100-0)*.75+0, label=label2,parse=T,color="blue", size=3.5)
dam4
summary(lm(stempercent_BUD ~ NPV, data=plotdata))

grid.arrange(dam1,dam2,dam3,dam4, ncol=2) # it gives warning messages because I extented the geom_smooth to the full plot range rather than the extent of the data 

# export Fig S3 additional regressions for manuscript ----
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/Regressions/Damage_NPV_supplementary_190925.tiff",
#      width = 5, height = 4, units = 'in', res = 300)
# grid.arrange(dam1,dam2,dam3,dam4, ncol=2)
# dev.off()



# plot Fig S8 regression AGB ~ CH----
label1<- findlabel1(damage = log(plotdata$PlotAGB_Mgha), NPV = plotdata$CanopyHeight_m)
label2<- findlabel2(damage = log(plotdata$PlotAGB_Mgha), NPV = plotdata$CanopyHeight_m)

chplot<- ggplot(data=plotdata,aes(x=CanopyHeight_m, y=log(PlotAGB_Mgha))) + theme_bw()+  
  ylab(bquote('log(AGB in Mg ha'^-1~')'))+ xlab('Canopy Height (m)')+
  geom_point(size=1)+ geom_smooth(method=lm,fullrange=T)+
  scale_x_continuous(limits=c(0,50)) +  scale_y_continuous(limits=c(0,10)) +
  coord_cartesian(xlim=c(10,30), ylim=c(4,6.5)) +
  annotate(geom="text", x=15, y=(6.5-4)*.95+4, label=label1,color="blue", size=3.5)+
  annotate("text", x=15, y=(6.5-4)*.85+4, label=label2,parse=T,color="blue", size=3.5)
chplot
summary(lm(log(PlotAGB_Mgha) ~ CanopyHeight_m, data=plotdata))



# export Fig S8 AGB ~ NPV - did not need to update, it didn't change 
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/Regressions/FigS8_CHAGB_regression190925.tiff",
#      width = 3, height = 3, units = 'in', res = 300)
# chplot
# dev.off()


# Read in regression values for the BUD AGB loss estimate and for the conservative estimate
loss1<- read.csv(paste0(path_plotvalues,'AGBIterations/DamageMetrics190925_HZambranoAGBScatena_BUD.csv'))
loss2<- read.csv(paste0(path_plotvalues,'AGBIterations/DamageMetrics190925_AGBCScat_HTBCLossWU.csv'))



# remove the sites where we have no break height data 
unique(data$plot[is.na(data$BreakHeight)])
unique(data$BreakHeight)
bhplots<- unique(data$plot[!is.na(data$BreakHeight) & data$plot!='Elev300' & data$plot!='SB3'])
bhplots
length(bhplots)
#babplots<- unique(data$plot[data$Break_APOM!='A' & data$Break_BPOM!='B'])

extra <- loss1$plot %in% bhplots # remove sites with no break height values 
loss1 <- loss1[extra,] # GUA3 is also not in the data - has no NPV value even though it has break height 
extra2 <- loss2$plot %in% bhplots # remove sites with no break height values 
loss2 <- loss2[extra2,]

label1<- findlabel1(damage = loss1$LossProportion, NPV = loss1$NPV)
label2<- findlabel2(damage = loss1$LossProportion, NPV = loss1$NPV)
label1.2<- findlabel1(damage = loss2$LossProportion, NPV = loss2$NPV)
label2.2<- findlabel2(damage = loss2$LossProportion, NPV = loss2$NPV)

cagb<- ggplot() + theme_bw()+  ylab(bquote('Proportion AGB Lost (Mg ha'^-1~')'))+ xlab(expression(paste(Delta ,"NPV")))+
  geom_smooth(data=loss1,aes(x=NPV, y=LossProportion), method=lm,fullrange=T)+ 
  geom_smooth(data=loss2,aes(x=NPV, y=LossProportion), method=lm,fullrange=T, col='red', fill='salmon1')+ 
  geom_point(data=loss1,aes(x=NPV, y=LossProportion), size=1.3, color= 'blue') +
  geom_point(data=loss2,aes(x=NPV, y=LossProportion), size=1, color='red') +
  scale_x_continuous(limits=c(-1,1.2)) + scale_y_continuous(limits=c(-0.5,1.2)) +
  coord_cartesian(xlim=c(0,0.6), ylim=c(0,0.6)) +
  annotate(geom="text", x=0.23, y=(0.6)*.95, label=label1,color="blue", size=3.5)+
  annotate("text", x=0.23, y=(0.6)*.85, label=label2,parse=T,color="blue", size=3.5)+
  annotate(geom="text", x=0.23, y=(0.6)*.65, label=label1.2,color="red", size=3.5)+
  annotate("text", x=0.23, y=(0.6)*.55, label=label2.2,parse=T,color="red", size=3.5)
cagb
summary(lm(LossProportion ~ NPV, data=plotdata))

# export figure for manuscript 
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/Regressions/CompareAGBs_wuthU_190925.tiff",
#      width = 3, height = 3, units = 'in', res = 300)
# cagb
# dev.off()

# find the percent AGB loss differece between methods 
extra <- data$plot %in% unique(loss1$plot) # remove sites with no break height values 
databh <- data[extra,]
total.initial.agb2 = sum(databh$TreeAGBkg, na.rm = TRUE)
total.loss.agb2 = sum(databh$TreeAGBkg.absolute.loss, na.rm = TRUE)
total.loss.agb2 / total.initial.agb2 # this is our estimate 
# 0.2073024 for conservative when uprooted is included 
# 0.3040036 for BUD 
(0.3040036-0.2073024)*100 # 9.67% difference

# print if using BUD method
if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == FALSE &
   use.estimates.of.crown.loss == FALSE & agb.loss.in.uprooted == 1 & 
   agb.loss.in.broken.above == 1 & agb.loss.in.broken.below == 1)
{total.loss.agb2 / total.initial.agb2}
# print if using most conservative method 
if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == TRUE &
   use.estimates.of.crown.loss == TRUE & is.na(agb.loss.in.uprooted))
{total.loss.agb2 / total.initial.agb2
  write.csv(loss2 , paste0(path_plotvalues,'AGBIterations/DamageMetrics190911_AGBCScat_HTBCLossNoU_BHsites.csv'))
}
# print if using conservative method while including uprooted stems in the total
if(height.allometry == "Zambrano" & agb.allometry == "Scatena" & use.HTB.when.possible == TRUE &
   use.estimates.of.crown.loss == TRUE & agb.loss.in.uprooted==1 )
{total.loss.agb2 / total.initial.agb2
  write.csv(loss2 , paste0(path_plotvalues,'AGBIterations/DamageMetrics190911_AGBCScat_HTBCLossWU_BHsites.csv'))
}

# 0.2942884 for BUD, 0.1714413 for most conservative, and 0.2073024 if uprooted is included 
0.2942884 - 0.1714413
0.2942884 - 0.2073024

# export new regressions for the estimated error in AGB 
