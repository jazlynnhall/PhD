# This script establishes the functions to estimate field AGB loss in plots 

# Functions I will need to include: 

# format the columns of all separate dataframes to ensure they all match when rbinded (I also got rid of the column that gets created after export to csv)----
formatcols<- function(df){
  df<- df[,2:15]
  df$plot<- as.factor(as.character(df$plot))
  df$stem.tag<- as.factor(as.character(df$stem.tag))
  df$quad<- as.factor(as.character(df$quad))
  df$DIAM<- as.numeric(as.character(df$DIAM)) 
  df$PCrownRemaining<- as.numeric(as.character(df$PCrownRemaining))
  df$Damage<- as.character(df$Damage)
  return(df)
}

# test to make sure that DBH is in the same unit for every plot ----
testdbh<- function(dbh){
  if(quantile(dbh, 0.75, na.rm = TRUE) > 20) UNITS.DBH = "mm"
  if(quantile(dbh, 0.75, na.rm = TRUE) < 20) UNITS.DBH = "cm"
  if(UNITS.DBH == "mm")
  {
    warning("It seems that the DBH units are in mm. They have been changed to cm")
    dbh <- dbh / 10
    
  }}

# calculate basal area of tree and the individual tree contribution to the total plot basal area
# calculate indiv tree BA and BA damaged per plot  
calc.ba<- function(df){
  df$TreeBA <- 0.00007854 * df$DIAM^2          # calculate basal area in square meters
  ba.ag<- aggregate(TreeBA ~ plot, data=df, FUN='sum') # aggregate the total BA per plot 
  names(ba.ag)[2] <- 'PlotBA'
  
  df<- merge(df, ba.ag, by='plot')     
  #df$TreeBAplotprop <- df$TreeBA/df$PlotBA  # calculate the indiv tree proportion of plot BA
  return(df)
}

# calculate pre-storm AGB for tree, leaf, and branch----
calc.agb<- function(df){
  # Calculate AGB (kg) for dicots -  using general dicot allometry developed by Scatena in EYNF 
  dicot<- df[df$species!='PREMON',] # subset data to not include palm 
  dicot$TreeAGBkg <- exp(2.475 *log(dicot$DIAM)-2.399)
  
  # Calculate AGB (kg) for palms 
  palm<- df[df$species=='PREMON',] # subset data to only include palm 
  # Allometry for palm uses height to calculate biomass, but I don't have height. # use equation given to me from Maria based on her work with palm DBH/height relationships
  b1Ht <- 0.8519565
  b2Ht <- 0.64
  palm$Height <- exp (b1Ht + b2Ht * log(palm$DIAM) )
  palm$TreeAGBkg <- 7.7 * palm$Height + 6.8 # now use Scatena's allometries for palm AGB 
  extra <- names(palm) %in% c("Height") # remove extra column and rbind palm and non-palm dfs together
  palm <- palm[!extra]
  
  newdata<- rbind(dicot,palm)
  
  # now calculate leaf and branch AGB in kg - again using Scatena allometries, palms excluded
  newdata$LeafAGBkg <- 0  
  newdata$LeafAGBkg[newdata$species!='PREMON'] <- exp(1.792 *log(newdata$DIAM[newdata$species!='PREMON'])-3.758)
  newdata$BranchAGBkg <- 0
  newdata$BranchAGBkg[newdata$species!='PREMON'] <- exp(1.982 *log(newdata$DIAM[newdata$species!='PREMON'])-3.69)
  
  # Find the total plot AGB in kg, sum AGB by plot 
  bioag<- aggregate(TreeAGBkg~plot, data=newdata, FUN='sum') 
  names(bioag)[2]<- 'PlotAGBkg'  
  newdata<- merge(newdata, bioag, by='plot')  # merge into new dataframe
  
  #newdata$TreeAGBplotprop <- newdata$TreeAGBkg/newdata$PlotAGBkg # calculate the indiv tree proportion of Biomass
  return(newdata)
}

# estimate the plot proportion BA and AGB of each tree damage category
calc.damage.ba<- function(df){
  # create a subset of d to only include broken, uprooted, and dead trees and some variation of canopy damage 
  damaged.b<- df[df$StatusAD=='A' & df$BU2!=2 & df$BU2==1 | df$Break_APOM=='A' | df$Break_BPOM=='B',] # for broken
  damaged.u<- df[df$BU2==2 &  df$StatusAD=='A' ,] # for uprooted
  damaged.d<- df[df$StatusAD=='D',] # for dead
  # subset dataframe for stems broken above and below POM
  damaged.bb<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_APOM!='A'  & df$BU2==1 | df$Break_BPOM=='B',] # for broken, uprooted, dead
  damaged.ba<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_BPOM!='B' & df$BU2==1 | df$Break_APOM=='A',] # for broken, uprooted, dead
  
  
  #aggregate the proportion of BA damaged from H. Maria by plot for each type of damage
  dg.ag1<- aggregate(TreeBA ~ plot, data = damaged.b, FUN='sum')
  names(dg.ag1)[2] <- 'BA_B'
  dg.ag2<- aggregate(TreeBA ~ plot, data = damaged.u, FUN='sum')
  names(dg.ag2)[2] <- 'BA_U'
  dg.ag3<- aggregate(TreeBA ~ plot, data = damaged.d, FUN='sum')
  names(dg.ag3)[2] <- 'BA_D'
  dg.ag4<- aggregate(TreeBA ~ plot, data = damaged.bb, FUN='sum')
  names(dg.ag4)[2] <- 'BA_BB'
  dg.ag5<- aggregate(TreeBA ~ plot, data = damaged.ba, FUN='sum')
  names(dg.ag5)[2] <- 'BA_BA'
  
  
  #merge with the rest of the dataframe 
  df<-  merge(df, dg.ag1, by='plot',all=T) # aggregate basal area of all broken stems 
  df<-  merge(df, dg.ag2, by='plot',all=T)
  df<-  merge(df, dg.ag3, by='plot',all=T)
  df<-  merge(df, dg.ag4, by='plot',all=T)
  df<-  merge(df, dg.ag5, by='plot',all=T)
  
  # now set all na values to zero 
  df$BA_B[is.na(df$BA_B)] <- 0
  df$BA_U[is.na(df$BA_U)] <- 0
  df$BA_D[is.na(df$BA_D)] <- 0
  df$BA_BB[is.na(df$BA_BB)] <- 0
  df$BA_BA[is.na(df$BA_BA)] <- 0
  
  # create a dataframe with just plot names
  newdf<- df[!duplicated(df$plot), c('plot','plottype','PlotBA','BA_B','BA_U','BA_D','BA_BB','BA_BA')] 
  
  return(newdf)
}

# estimate the plot proportion od AGB in each tree damage category 
calc.damage.agb<- function(df){
  # create a subset of d to only include broken, uprooted, and dead trees and some variation of canopy damage 
  damaged.b<- df[df$StatusAD=='A' & df$BU2!=2 & df$BU2==1 | df$Break_APOM=='A' | df$Break_BPOM=='B',] # for broken
  damaged.u<- df[df$BU2==2 &  df$StatusAD=='A' ,] # for uprooted
  damaged.d<- df[df$StatusAD=='D',] # for dead
  # subset dataframe for stems broken above and below POM
  damaged.bb<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_APOM!='A'  & df$BU2==1 | df$Break_BPOM=='B',] # for broken, uprooted, dead
  damaged.ba<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_BPOM!='B' & df$BU2==1 | df$Break_APOM=='A',] # for broken, uprooted, dead
  # subset to find leaf AGB - do not include broken, uprooted, or dead trees, they are already accounted for
  non.bud<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_BPOM!='B' & df$Break_APOM!='A'  & df$BU2!=1,] 
  
  
  #aggregate the proportion of AGB damaged from H. Maria by plot for each type of damage
  dg.ag1<- aggregate(TreeAGBkg ~ plot, data = damaged.b, FUN='sum')
  names(dg.ag1)[2] <- 'AGB_B'
  dg.ag2<- aggregate(TreeAGBkg ~ plot, data = damaged.u, FUN='sum')
  names(dg.ag2)[2] <- 'AGB_U'
  dg.ag3<- aggregate(TreeAGBkg ~ plot, data = damaged.d, FUN='sum')
  names(dg.ag3)[2] <- 'AGB_D'
  dg.ag4<- aggregate(TreeAGBkg ~ plot, data = damaged.bb, FUN='sum')
  names(dg.ag4)[2] <- 'AGB_BB'
  dg.ag5<- aggregate(TreeAGBkg ~ plot, data = damaged.ba, FUN='sum')
  names(dg.ag5)[2] <- 'AGB_BA'
  lf.ag<- aggregate(LeafAGBkg ~ plot, data = non.bud, FUN='sum')
  names(lf.ag)[2] <- 'AGB_Leaf'
  
  #merge with the rest of the dataframe 
  df<-  merge(df, dg.ag1, by='plot',all=T) # aggregate basal area of all broken stems 
  df<-  merge(df, dg.ag2, by='plot',all=T)
  df<-  merge(df, dg.ag3, by='plot',all=T)
  df<-  merge(df, dg.ag4, by='plot',all=T)
  df<-  merge(df, dg.ag5, by='plot',all=T)
  df<-  merge(df, lf.ag, by='plot',all=T)
  
  # now set all na values to zero 
  df$AGB_B[is.na(df$AGB_B)] <- 0
  df$AGB_U[is.na(df$AGB_U)] <- 0
  df$AGB_D[is.na(df$AGB_D)] <- 0
  df$AGB_BB[is.na(df$AGB_BB)] <- 0
  df$AGB_BA[is.na(df$AGB_BA)] <- 0
  df$AGB_Leaf[is.na(df$AGB_Leaf)] <- 0
  
  # create a dataframe with just plot names
  newdf<- df[!duplicated(df$plot), c('plot','plottype','PlotAGBkg','PlotAGB_Mgha','AGB_B','AGB_U','AGB_D','AGB_BB','AGB_BA','AGB_Leaf')] #,'Branch_Loss', 'Branch_Loss_BrokenA')] 
  return(newdf)
}

# estimate branch damage 
calc.damage.branchagb<- function(df, areas){
  # format df to not have NA values in the columns we are working with
  df$PCrownRemaining[is.na(df$PCrownRemaining)]<- 9999
  df$Break_APOM<- as.character(df$Break_APOM)
  df$Break_BPOM<- as.character(df$Break_BPOM)
  
  df$Break_APOM[is.na(df$Break_APOM)]<- 'NotBreak'
  df$Break_BPOM[is.na(df$Break_BPOM)]<- 'NotBreak'
  # now subset df to only include living, unbroken, and non-uprooted stems 
  branch<- df[df$StatusAD=='A' & df$BU2!=2 & df$BU2!=1 & df$Break_APOM!='A' & df$Break_BPOM!='B',]
  
  #now further subset to three categories of branch damage
  light<- branch[branch$PCrownRemaining>75 & branch$PCrownRemaining<=100 | branch$Damage=='Light',]
  medium<- branch[branch$PCrownRemaining>25 & branch$PCrownRemaining<=75  | branch$Damage=='Medium',]
  heavy<- branch[branch$PCrownRemaining<=25 | branch$Damage=='Heavy',]
  # calculate branch agb lost
  light$Branch_Loss_tree <- light$BranchAGBkg * 0.125 # assume lightly damaged trees lost 12.5% of AGB
  medium$Branch_Loss_tree <- medium$BranchAGBkg * 0.5 # assume moderately damaged trees lost 50% of AGB
  heavy$Branch_Loss_tree <- heavy$BranchAGBkg * 0.875 # assume heavily damaged trees lost 87.5% of AGB
  
  branch2<- rbind(light,medium,heavy)
  
  # aggregate the branch AGB loss
  dg.ag6<- aggregate(Branch_Loss_tree ~ plot, data = branch2, FUN='sum')
  names(dg.ag6)[2] <- 'Branch_Loss'
  
  # now I need to estimate branch damage for stems broken above point of measure, first exclude all trees that are broken below, uprooted, or dead (the full tree damage for these trees is already accounted for above)
  branch_ba<- df[df$StatusAD=='A' & df$BU2!=2 & df$Break_BPOM!='B' & df$BU2==1 & df$Break_APOM=='A' |  df$BU2!=1 & df$Break_APOM=='A' ,]
  # now further subset to three categories of branch damage
  light_ba<- branch_ba[branch_ba$PCrownRemaining>75 & branch_ba$PCrownRemaining<=100 | branch_ba$Damage=='Light',]
  medium_ba<- branch_ba[branch_ba$PCrownRemaining>25 & branch_ba$PCrownRemaining<=75  | branch_ba$Damage=='Medium',]
  heavy_ba<- branch_ba[branch_ba$PCrownRemaining<=25 | branch_ba$Damage=='Heavy',]
  # calculate branch_ba agb lost
  light_ba$Branch_Loss_BA_tree <- light_ba$BranchAGBkg * 0.125 # assume lightly damaged trees lost 12.5% of AGB
  medium_ba$Branch_Loss_BA_tree <- medium_ba$BranchAGBkg * 0.5 # assume moderately damaged trees lost 50% of AGB
  heavy_ba$Branch_Loss_BA_tree <- heavy_ba$BranchAGBkg * 0.875 # assume heavily damaged trees lost 87.5% of AGB
  
  branch_ba2<- rbind(light_ba,medium_ba,heavy_ba)
  
  #aggregate the branch AGB loss
  dg.ag7<- aggregate(Branch_Loss_BA_tree ~ plot, data = branch_ba2, FUN='sum')
  names(dg.ag7)[2] <- 'Branch_Loss_BrokenA'
  
  df<-  merge(df, dg.ag6, by='plot',all=T) # add the branch loss variables
  df<-  merge(df, dg.ag7, by='plot',all=T)
  # now set all na values to zero
  
  df$Branch_Loss[is.na(df$Branch_Loss)] <- 0
  df$Branch_Loss_BrokenA[is.na(df$Branch_Loss_BrokenA)] <- 0
  # create a dataframe with just plot names
  newdf<- df[!duplicated(df$plot), c('plot','Branch_Loss', 'Branch_Loss_BrokenA')]
  return(newdf)
}


# find the values for the number broken, uprooted or dead 
calc.stemstats<- function(df){
  count.b <- aggregate(data=df[df$StatusAD=='A' & df$BU2!=2 & df$BU2==1 | df$Break_APOM=='A' | df$Break_BPOM=='B',], BU ~ plot, function(x) length(x))
  names(count.b)[2] <- 'n.b'
  count.u <- aggregate(data=df[df$BU2==2 &  df$StatusAD=='A' ,], BU ~ plot, function(x) length(x))
  names(count.u)[2] <- 'n.u'
  count.d <- aggregate(data=df[df$StatusAD=='D',], StatusAD ~ plot, function(x) length(x))
  names(count.d)[2] <- 'n.d'
  count<- aggregate(data=df, DIAM ~ plot, function(x) length(x))
  names(count)[2] <- 'n.stems'
  # merge with original df 
  df<- merge(df,count.b, by='plot', all=T)
  df<- merge(df,count.u, by='plot', all=T)
  df<- merge(df,count.d, by='plot', all=T)
  df<- merge(df,count, by='plot', all=T)
  
  # set all na values to zero 
  df$n.b[is.na(df$n.b)] <- 0
  df$n.u[is.na(df$n.u)] <- 0
  df$n.d[is.na(df$n.d)] <- 0
  
  # find stem density in # stems per hectare
  df$densityB_ha<- df$n.b/(df$PlotArea_m2/ 10000)
  df$densityU_ha<- df$n.u/(df$PlotArea_m2/ 10000)
  df$densityD_ha<- df$n.d/(df$PlotArea_m2/ 10000)
  
  # find % stems BUD 
  df$proportion_B<- df$n.b/df$n.stems
  df$proportion_U<- df$n.u/df$n.stems
  df$proportion_D<- df$n.d/df$n.stems
  
  # create a dataframe with just plot names
  newdf<- df[!duplicated(df$plot), c('plot','n.b','n.u','n.d','densityB_ha','densityU_ha','densityD_ha','proportion_B','proportion_U','proportion_D')]
  return(newdf)
}
