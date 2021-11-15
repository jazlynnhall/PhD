# Hall 2019

# Script 1 - Landscape-level AGB calculation from the NPV pixel values 

# This script find the landscape-level estimates for AGB loss. 

# Jazlynn Hall 
# Chapter 2 - PhD Dissertation, columbia University E3B
# August 2019
# Created: 19-08-06
# updated: 19-09-02 to include updated AGB estimates based on Gabriel's input 
# updated 19-09-11 to add conservative AGB loss estimates using only field plots with break height values

library(raster)
library(ggplot2)
library(rgdal)

# set path names
user = "Jaz"
#user = "" # insert name of user here

# CHOOSE WHICH ESTIMATE TO CALCULATE ---
AGBloss_estimate<- 'BUD'
#AGBloss_estimate<- 'conservative'
#AGBloss_estimate<- 'conservative_with_uprooting'


if(user == "Jaz")
{
  path_plotvalues<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Data/PlotLevel_values/'
  path_npv<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/NPV/'
  path_npv_vector<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/NPV'
  path_ch<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/'
}

if(user == "")
{
  path_plotvalues<- '/DataForMs/Data/PlotLevel_values/AGBIterations/'
  path_npv<- '/DataForMs/GIS/NPV/'
  path_npv_vector<- '/DataForMs/GIS/NPV'
  path_ch<- '/DataForMs/GIS/RiskFactors/'
}



#read in raster dNPV 
# this file is the Sentinel-2 NPV values - pre(9/15-11/01, 2016) and post(9/21-11/01, 2017) Maria
#rastersource<- paste0(path_npv, 'npvmask_wgs') # this npv estimate is the estimate where sumtoone was not implemented
rastersource<- paste0(path_npv, 'npvmskwgs0902') # fractions must sum to one when unmixing in this estimate. 


r<- raster(rastersource)
r@crs # WGS84
r.spgrd<- as(r,'SpatialPointsDataFrame') #convert to spatial points dataframe 
# Read in values for canopy height - this file only includes values between 0 and 40m
canopyheight<- raster(paste0(path_ch, 'ch040_10m'))
# extract canopy height values for dNPV locations 
rasValue1=extract(canopyheight, r.spgrd)
# add canopy height values to npv values 
combinePointValue=cbind(r.spgrd,rasValue1)


# create a df of the NPV and CH values for every pixel in the study area  ----
v<- as.data.frame(combinePointValue@data)
names(v)[1]<- 'NPV'
names(v)[2]<- 'canopyh'
head(v)

# now remove missing values for canopy height 
v<- na.omit(v)
length(v$NPV)

# check study area extent
length(v$NPV)*100/10000 
# 305966.2 - 190818: this changed a bit after I decided to read the NPV values in differently
# 312318.5 - 190902 I realized I didn't force the unmixing fractions to sum to one, 
            # so I got rid of a lot of the high damage areas that had values over 1 
            # and low damage areas with values under -1

# find percent pixels with damage 
length(v$NPV[v$NPV>0]) / length(v$NPV) # 0.9190232

# create a new column with only positive NPV values 
v$NPV_noneg<- v$NPV
v$NPV_noneg[v$NPV<0]<- 0

# export the histogram of the NPV for the total study area 
fs2<- ggplot(v, aes(x=NPV)) + theme_bw()+ 
  labs(x=expression(paste(Delta ,"NPV")), y="% Pixels")+
  geom_histogram(aes(y=stat(count / sum(count))*100),bins = 20,color='black', fill='blue', boundary=0)#+

#fs2


# now create the figure for the Histogram fig supplementary figure S2
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/NPV_Histogram/HistNPV_StudyArea_190911.tiff",
#      width = 3, height = 2, units = 'in', res = 300)
# fs2
# dev.off()



# read in the plot-level AGB and AGB loss estimates to create the linear models
    # Estimate used for the paper:
d<- read.csv(paste0(path_plotvalues,'DamageMetrics190909_HZambranoAGBScatena_BUD.csv')) 
d<- read.csv(paste0(path_plotvalues,'GabJaz_compare/DamageMetrics190925_AGBScat_BUD_Jaz.csv'))
    # Estimate of conservative AGB loss using all field plots used for the paper 
#d2<- read.csv(paste0(path_plotvalues,'DamageMetrics190911_AGBCScat_HTBCLossNoU_BHsites.csv'))
    # Estimate using conservative AGB loss using only field plots with break height (HTB wU)
        # AND including uprooted stems in the total AGB loss 
#d3<- read.csv(paste0(path_plotvalues,'DamageMetrics190911_AGBCScat_HTBCLossWU_BHsites.csv'))
d3<- read.csv(paste0(path_plotvalues,'GabJaz_compare/DamageMetrics190925_AGBCScat_conservJaz_BHsites.csv'))

# find values for tables 
min(d$CanopyHeight_m) 
max(d$CanopyHeight_m)
mean(d$CanopyHeight_m)
sd(d$CanopyHeight_m)

min(d$PlotAGB_Mgha)
max(d$PlotAGB_Mgha)
mean(d$PlotAGB_Mgha)
sd(d$PlotAGB_Mgha)

# create linear models estimating AGB and AGB loss, based on values between field plots and remote sensing
# Models represent: 
    # proportion AGB lost ~ DNPV
    # log(AGB in Mg per ha) ~ Area-weighted Canopy Height (m)


# NPV %AGB Lost 
# DEPENDS ON WHICH METHOD WE ARE CALCULATING
if(AGBloss_estimate== 'BUD'){
  m1<- lm(data=d, LossProportion ~ NPV) }
if(AGBloss_estimate== 'conservative'){
  m1<- lm(data=d2, LossProportion ~ NPV) }
if(AGBloss_estimate== 'conservative_with_uprooting'){
  m1<- lm(data=d3, LossProportion ~ NPV) }


# AGB ~ CH 
m2<- lm(data=d, log(PlotAGB_Mgha) ~ CanopyHeight_m)



# 1. Estimate AGB and the 95% confidence intervals for total AGB across the study area in Mg per ha ----
# equations taken from https://rstudio-pubs-static.s3.amazonaws.com/71339_d0b8346f41314979bc394448c5d60d86.html
# use the linear regression from our field and remote sensing metrics, 
#       and predict values for AGB, and AGB lost for each pixel
conf_interval_m2 <- exp(predict(m2, newdata=data.frame(CanopyHeight_m=v$canopyh), 
                                interval="confidence",level = 0.95))
head(conf_interval_m2)


# find the pre-storm AGB in Mg per ha
sum(conf_interval_m2[,1], na.rm=T)
sum(conf_interval_m2[,2], na.rm=T)
sum(conf_interval_m2[,3], na.rm=T)


# _______________________________________________
# now calculate the AGB in Tg rather than Mg per ha 
# each pixel is 0.01 ha and 1 Tg =  1,000,000 Mg

# AGB Mg    0.01 ha          1 Tg        AGB Tg
# ------  * -------  * -------------- =  -------
#   ha      1 pixel     1,000,000 Mg      pixel
# _______________________________________________

# find the pre-storm AGB in Tg 
sum(conf_interval_m2[,1]*0.01/ 1000000, na.rm=T) # 190912: 45.86585 # all stayed the same for 190925
sum(conf_interval_m2[,2]*0.01/ 1000000, na.rm=T) # 190912: 33.2855
sum(conf_interval_m2[,3]*0.01/ 1000000, na.rm=T) # 190912: 65.26793

head(conf_interval_m2)

# calculate pixel-level ABG in Tg
v$AGB<-conf_interval_m2[,1]*0.01/ 1000000
v$AGB_95low<-  conf_interval_m2[,2]*0.01/ 1000000
v$AGB_95high<-  conf_interval_m2[,3]*0.01/ 1000000


# find average values of AGB across the study area 
conf_interval_m2_mean <- exp(predict(m2, newdata=data.frame(CanopyHeight_m=mean(v$canopyh, na.rm=T)), 
                              interval="confidence", level = 0.95))
conf_interval_m2_mean 
# 190925 : 118.9354 (CI = 86.62448, 163.2983) AGb in Mg per ha






# 2.Estimate the proportion of AGB lost to storm and 95% confidence intervals---- 


# Calculate the % AGB lost from the storm using confidence intervals
conf_interval_m1 <- predict(m1, newdata=data.frame(NPV=v$NPV_noneg), interval="confidence",level = 0.95)


# Calculate the % AGB lost from the storm 
v$proAGBd<-conf_interval_m1[,1]
v$proAGBd_95low<-  conf_interval_m1[,2]
v$proAGBd_95high<-  conf_interval_m1[,3]


# Calculate the AGB lost in Tg
v$AGBd <- v$AGB*v$proAGBd
v$AGBd_95low <- v$AGB*v$proAGBd_95low 
v$AGBd_95high <- v$AGB*v$proAGBd_95high 

# summarize AGB lost in Tg
sum(v$AGBd, na.rm=T) # 190925 : 10.44  BUD, 8.111424 HTB WU
sum(v$AGBd_95low, na.rm=T) # 190925 : 8.112186 BUD, 5.855264 HTB WU
sum(v$AGBd_95high, na.rm=T) # 190925 : 12.76781 BUD, 10.36758 HTB WU

# find percent of AGB lost 
sum(v$AGBd, na.rm=T) / sum(v$AGB, na.rm=T)# 190925 : 0.2276203 BUD, 0.1768511 HTB WU 

# find difference in total AGB lost between methods
# when not including uprooted
10.44 - 8.111424 # 190925 : could overpredict AGB loss by 2.32 Tg. 



# now find the +- values, need to update
sum(v$AGBd, na.rm=T)-sum(v$AGBd_95low, na.rm=T)    # 190925 : 2.32781 BUD, 2.25616 HTB WU
sum(v$AGBd_95high, na.rm=T) - sum(v$AGBd, na.rm=T) # 190925 : 2.32781 BUD, 2.25616 HTB WU


# Now find the Tg C by dividing AGB by 2
sum(v$AGBd)/2 # 190925 : 5.219998 BUD, 4.055712 HTB WU
sum(v$AGBd_95low)/2 # 190925 : 4.056093 BUD, 2.927632 HTB WU
sum(v$AGBd_95high)/2 # 190925 : 6.383903 BUD, 5.183792 HTB WU

# percent of AGB not lost 
(sum(v$AGB)- sum(v$AGBd) ) / sum(v$AGB)
# percent lost
sum(v$AGBd)  / sum(v$AGB)

# BUD
10.44/ 45.86585
# HTB WU
8.111424 / 45.86585
