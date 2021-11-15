# Master script

# Jazlynn Hall PhD Thesis, Chapter 1


# Section 1: Source the data ---- 

# read in relevant libraries
library(tidyverse) # data manipulation
library(ggplot2) # pretty plots 
library(gridExtra) # pretty plots 
library(ggstance) # for coefficient plotting (has a vertical dodge)
library(grid) # for shared legends in grid.arrange
library(cowplot) # for shared legends in grid.arrange


library(dataRetrieval) # download USGS streamflow data
library(streamstats) # determine USGS-calculated watershed characteristics
library(IETD) # identify rainfal and dry events

library(sp) # working with watersheds and chars
library(rgdal) # spatial data manipulation 
library(sf) # spatial data 
library(raster) # spatial data
library(ncdf4) # manipulating nc files - MSWEP rainfall 
library(gstat) # idw for PRISM annual rainfall 

library(leaflet) # quickly plot spatial data 
library(mapview) # pretty maps
library(RColorBrewer) # pretty plots

library(stringr) # detect strings in column values 
library(lubridate) # work with dates and times 

library(zoo) # interpolate streamflow values by minute 

library(hydroTSM) # FDCs

library(corrplot) # plot correlation between runoff and rainfall 

library(lme4)
library(lmerTest)
library(visreg) # contour plots
library(coefplot)
library(sjPlot) # plot interaction line plots
library(sjmisc)
library(ggeffects) # plot interactions - this is MUCH EASIER than the others

library(performance) # test nlme performance
library(coefplot2) # plot nlme models
library(nlme) # run nme lmms




# set directories for the chapter 
fileloc<- "D:/Dropbox/Backup/Chapter1/DataforMs/"

# set directory for the final figure locations 
finalloc<- 'D:/Dropbox/Backup/Chapter1/Manuscript/Hydrological Processes/Submission1/'

# source functions 
source(paste0(fileloc, 'Scripts/Functions.R'))

## Streamflow ----

# set the relevant directories
# source(paste0(fileloc, 'Scripts/Section1_SourceData/Streamflow1_Streamstats.R'))


# what directory do we want the watershed shapefiles to be saved in
wshed_shp_dir<- 'D:/Dropbox/Backup/Chapter1/DataforMs/GIS/Streamstats/Watersheds/Indiv'
# Directory for the merged watershed shapefile
wshed_shp_dir2<- 'D:/Dropbox/Backup/Chapter1/DataforMs/GIS/Streamstats/Watersheds'
# and the df of characteristics
wshed_char_dir<- 'D:/Dropbox/Backup/Chapter1/DataforMs/GIS/Streamstats/Characteristics/'



### Site Selection ----


# location of the geodatabase downloaded from the NHD :
# if this is the same location, no need to run this line, if it is different, change it in Streamflow2 line 45 or so
fgdb <- paste0(fileloc, 'GIS/NHD/NHD_H_Puerto_Rico_State_GDB.gdb')

# No need to run this line, the file has been exported already
# actually should probably run it so I can get the values for pipeline, etc and make a table
source(paste0(fileloc, 'Scripts/Section1_SourceData/Streamflow2_SiteSelection2_keepreservoirs.R'))
# No wplot a map of the selected sites - various other layers as well
# m8+m_humod+m_geo+m_dams2+m_pipelines+m_lakes+m_reservoirs


# read in the selected watersheds shapefile
ws<-  st_read(paste0(wshed_shp_dir2, "/", "Watersheds_selected_20052016_210706.shp"))
sites<- unique(ws$site_no)


# make a table for export to the manuscript for site selection 
nonvolcanic<- data.frame(site_no = ws_volcanic$site_no, Proportion_Volcanic = ws_volcanic$prop_VOlcanic) %>%
  mutate(NonVolcanic = ifelse(Proportion_Volcanic<0.5, 'X','')) %>% dplyr::select(site_no, NonVolcanic)#%>% dplyr::filter(NonVolcanic %in% 'X')
pipelines<- data.frame(site_no = unique(ws_pipe$site_no))%>% mutate(Pipelines = 'X')
lakes<- data.frame(site_no = unique(ws_lakes$site_no))%>% mutate(Lakes = 'X')
dams<- data.frame(site_no = unique(ws_dam$site_no)) %>% mutate(Dams = 'X')
#reservoirs<- data.frame(site_no = unique(ws_res$site_no))%>% mutate(Reservoirs = 'X')
karst<- data.frame(site_no = unique(karst$site_no)) %>% mutate(Karst = 'X')
abnorm<- data.frame(site_no = 50043800) %>% mutate(Abnormal = 'X')
selected<- data.frame(site_no = sites, Selected = 'Selected') 
  
# merge them all together and make a table 
tsites<- merge(nonvolcanic, pipelines, by='site_no', all.x=T) %>% dplyr::mutate(Pipelines = ifelse(is.na(Pipelines), '', Pipelines))
tsites<- merge(tsites, lakes, by='site_no', all.x=T)%>% dplyr::mutate(Lakes = ifelse(is.na(Lakes), '', Lakes))
tsites<- merge(tsites, dams, by='site_no', all.x=T)%>% dplyr::mutate(Dams = ifelse(is.na(Dams), '', Dams))
#tsites<- merge(tsites, reservoirs, by='site_no', all.x=T)%>% dplyr::mutate(Reservoirs = ifelse(is.na(Reservoirs), '', Reservoirs))
tsites<- merge(tsites, karst, by='site_no', all.x=T)%>% dplyr::mutate(Karst = ifelse(is.na(Karst), '', Karst))
tsites<- merge(tsites, abnorm, by='site_no', all.x=T)%>% dplyr::mutate(Abnormal = ifelse(is.na(Abnormal), '', Abnormal))
tsites<- merge(tsites, selected, by='site_no', all.x=T)%>% dplyr::mutate(Selected = ifelse(is.na(Selected), '', Selected))

# I also found a link to the streamstats gagepages and read the descriptions for every station with data 
# https://streamstatsags.cr.usgs.gov/gagepages/html/

regdiv<- read.csv(paste0(fileloc,"/GIS/Streamstats/streamstats_regulation_diversions.csv")) 
tsites2<- merge(tsites, regdiv, all=T)

# sites that need to be removed or adjusted after regdiv: 
remove_sites<- c(50100450, # local resident ag water withdrawal
                 50111500, # dam
                 50047535,# domestic discharges from locals
                 50043197) # water extraction ~ 100m downstream from gage affecting low flow  https://wdr.water.usgs.gov/wy2011/pdfs/50043197.2011.pdf?1625614442133


# adjust_sites<- c(50053025, # about 0.45 ft^3/s (0.01274258 cms) is withdrawn
#                  50071000)# about 10 ft^3/s (0.283168 cms) is withdrawn
# tsites2 <- tsites2 %>% mutate(#Selected = ifelse(site_no %in% remove_sites, '',Selected), 
#                               Selected = ifelse(site_no %in% adjust_sites, 'Adjusted',Selected), )

tsites2$Dams[tsites2$site_no%in%'50111500'] <- 'X'
tsites2$Abnormal[tsites2$site_no%in%'50100450'] <- 'X'
tsites2$Abnormal[tsites2$site_no%in%'50047535'] <- 'X'
tsites2$Abnormal[tsites2$site_no%in%'50043197'] <- 'X'

tsites2$Selected[tsites2$site_no%in%'50111500'] <- ''
tsites2$Selected[tsites2$site_no%in%'50100450'] <- ''
tsites2$Selected[tsites2$site_no%in%'50047535'] <- ''
tsites2$Selected[tsites2$site_no%in%'50043197'] <- ''

tsites2<- tsites2 %>% mutate(Regulation.and.Diversions = ifelse(is.na(Regulation.and.Diversions),'',Regulation.and.Diversions))

# actually, remove the regulations completely and add another abnormality table: 
tsites3<- tsites2 %>% dplyr::filter(Abnormal %in% 'X')

tsites2<- tsites2 %>% dplyr::select(-Regulation.and.Diversions)
# the table is now ready for export
# write.csv(tsites2, paste0(fileloc,"/Tables/SiteSelection.csv"))


# Now add the description found in the water year report for site :50043197 
tsites3<- tsites3 %>% mutate(Regulation.and.Diversions = ifelse(site_no %in% 50043197, '2011 Water Year Report: https://wdr.water.usgs.gov/wy2011/pdfs/50043197.2011.pdf?1625614442133 Water extraction about 500 ft (152.4 m) downstream of gage by Puerto Rico Acueduct and Sewer Authority (PRASA) affecting low flow.', Regulation.and.Diversions))
tsites3<- tsites3 %>% dplyr::select(site_no, Regulation.and.Diversions)
# write.csv(tsites3, paste0(fileloc,"/Tables/SiteSelection_AbnormalSites.csv"))



### Download streamflow for selected sites----
# no need to download this again unless I have to, it takes a long time 
#source(paste0(fileloc, 'Scripts/Section1_SourceData/Streamflow3_Download.R'))

# read in the downloaded streamflow 
s<- read.csv(paste0(fileloc,'Data/Streamflow/Streamflow_SelectedSites_20052016_210707.csv')) %>% 
  dplyr::rename(DateTime=dateTime) %>% 
  mutate(DateTime = force_tz(ymd_hms(DateTime), tzone = "America/Puerto_Rico"), 
         Date = as.Date(Date), site_no = as.factor(site_no)) %>%
  arrange(site_no, DateTime)#%>% 
  #dplyr::filter(!site_no %in% as.factor(remove_sites))
length(unique(s$site_no))

# check the streamflow time series for each of the sites: 
# ggplot(s, aes(x=DateTime, y=log(Flow_Inst+1)))+geom_line()+facet_wrap(~site_no, scales='free_y')


## Rainfall ----

# MSWEP came from GloH20 as a shared google drive folder, which is insanely difficult to work with. 
#I downloaded the 3hourly folder and saved it to the xxxxxxxxxxxx folder 
# process the rainfall data for each watershed and save it to an additional file: 

r3<- read.csv(paste0(fileloc,'Data/Rainfall/MSWEP_3hourly_20052016_210707.csv'))%>%
  mutate(Day = sprintf("%03d", Day), Hour = sprintf("%02d", Hour), DateTime = paste0(Year,Day,Hour), origin = as.Date(paste0(Year,'-01-01')),
         Date = as.Date(as.numeric(Day)-1, origin = origin, tz = "UTC"),  DateTimeUTC = ymd_h(paste0(Date, ' ',Hour)),
         DateTime3 = with_tz(DateTimeUTC, "America/Puerto_Rico"),
         Date = as.Date(format(DateTime3, '%Y-%m-%d')), site_no = as.factor(as.character(site_no))) %>%
  dplyr::select(site_no, Date, DateTime3, Rain3hourly) 
head(r3)
length(unique(r3$site_no))

# find the annual rainfall for the study period
arain<- r3 %>% mutate(Year = format(DateTime3, '%Y'))%>% dplyr::filter(Year >2004) %>% group_by(site_no,Year) %>%
  summarize(Rain_year = sum(Rain3hourly))
# find the average annual rainfall 
arainav<- arain %>% group_by(site_no) %>% summarize(AnnRainMSWEP = mean(Rain_year))
# find the maximum annual rainfall 
arainmax<- arain %>% group_by(site_no) %>% summarize(AnnRainMSWEP_max = max(Rain_year))



## Watershed characteristics ----

# find antecedent rainfall: 
rday<- r3 %>% group_by(site_no, Date) %>% dplyr::summarise(Rain_mmday = sum(Rain3hourly))

# calculate antecedent rainfall 
rday<- rday %>% dplyr::group_by(site_no) %>% 
  mutate(l1 = lag(Rain_mmday,1), l2 = lag(Rain_mmday,2), l3 = lag(Rain_mmday,3), l4 = lag(Rain_mmday,4) , l5 = lag(Rain_mmday,5),
         l6 = lag(Rain_mmday,6), l7 = lag(Rain_mmday,7), l8 = lag(Rain_mmday,8), l9 = lag(Rain_mmday,9) , l10 = lag(Rain_mmday,10),
         l11 = lag(Rain_mmday,11), l12 = lag(Rain_mmday,12), l13 = lag(Rain_mmday,13), l14 = lag(Rain_mmday,14), l15 = lag(Rain_mmday,15),
         l16 = lag(Rain_mmday,16), l17 = lag(Rain_mmday,17), l18 = lag(Rain_mmday,18), l19 = lag(Rain_mmday,19) , l20 = lag(Rain_mmday,20),
         l21 = lag(Rain_mmday,21), l22 = lag(Rain_mmday,22), l23 = lag(Rain_mmday,23), l24 = lag(Rain_mmday,24) , l25 = lag(Rain_mmday,25),
         l26 = lag(Rain_mmday,26), l27 = lag(Rain_mmday,27), l28 = lag(Rain_mmday,28), l29 = lag(Rain_mmday,29), l30 = lag(Rain_mmday,30),
  ) %>% 
  rowwise() %>% 
  mutate(
    ARain7 = sum(l1,l2,l3,l4,l5,l6,l7),
    ARain14 = sum(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14),
    ARain30 = sum(l1,l2,l3,l4,l5,l6,l7,l8,l9,l10,l11,l12,l13,l14,l15,l16,l17,l18,l19,l20,l21,l22,l23,l24,l25,l26,l27,l28,l29,l30)) %>% 
  dplyr::select(site_no, Date, Rain_mmday,ARain7, ARain14,ARain30)
# head(data.frame(rday), 31)

# remove all but out study period 
rday<- rday %>% dplyr::filter(Date>=as.Date('2004-12-31'))

rdf_list<- rday %>% group_by(site_no) %>% group_split()



# apply the function
rdf_list<- lapply(rdf_list, find_ARain_quantiles)
# create a df of all quantiles
rARquant<- bind_rows(rdf_list)

# summarize and find mean quantiles across watersheds 
rARquant<- rARquant %>% dplyr::filter(!sn %in% remove_sites)
rmeanquants<- rARquant %>% summarise(ARain30_33=mean(ARain30_33), ARain30_50= mean(ARain30_50), ARain30_66=mean(ARain30_66), ARain7_33= mean(ARain7_33), ARain7_50 = mean(ARain7_50), ARain7_66=mean(ARain7_66))

# source(paste0(fileloc, 'Scripts/Section1_SourceData/Watershed_characteristics.R'))

# export to a shapefile - overwrite when it gets updated
# ws_chars<- st_read(paste0(wshed_shp_dir2, 'Watersheds_selected_chars_210624.shp'))
ws_chars<- st_read(paste0(wshed_shp_dir2, 'Watersheds_selected_chars_210706.shp'))
# length(unique(ws_chars$site_no))
ws_chars2<- ws_chars %>% filter(!site_no %in% as.factor(remove_sites))
# m8<- mapview(ws_chars2, zcol='site_no', legend=F)
# m8+m_pipelines+m_dams_weirs+m_reservoirs+m_lakes+m_dams2
# try another tactic: gather streamstats site descriptions using th URL
# https://statistics.berkeley.edu/computing/faqs/reading-web-pages-r
# test<- readLines('https://streamstatsags.cr.usgs.gov/gagepages/html/50035000.htm')
# head(test)


watershedchars<- st_drop_geometry(ws_chars2) #%>% dplyr::select(site_no, SSURGOB, ELEV, ROCKDEP, MINBELEV, DRNAREA, PRECIP, Forest_percent, Above700_percent, Slope)
# pre-scale the predictors so they stay the same between models: 
watershedchars<- watershedchars %>% dplyr::filter(!site_no %in% remove_sites)%>%
  mutate( Forest_scaled = scale(ForestP),Above700_scaled = scale(A700P), Slope_scaled = scale(Slope))

# merge with average MSWEP rainfall for the study period
watershedchars<- merge(watershedchars, arainav, by='site_no') 
watershedchars<- merge(watershedchars, arainmax, by='site_no') 
watershedchars<- watershedchars %>% mutate(Area_m2 = DRNAREA*2.59e+6)

# merge with average MSWEP Rainfall from 1980 to 1995
# Read in the average MSWEP values from 1980-1995
msnorm<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/MSWEP_Normals_19801995_210710.csv') %>% dplyr::select(-X)
watershedchars<- merge(watershedchars, msnorm, by='site_no')





# Section 2: Process the data ----



## Total 3hourly streamflow  ----

# Approximate streamflow by the minute to calculate total runoff 
  # This script takes a long time to run. I have exported the file for faster use 
# source(paste0(fileloc, 'Scripts/Section2_ProcessData/Streamflow1_3hourly_Intervals_210707.R'))

# read in the processed 3hourly estimates of streamflow
s3<- read.csv(paste0(fileloc, 'Data/Streamflow/Streamflow_3hourlysummary_intervals_210707.csv')) %>% 
  mutate(DateTime3 = force_tz(ymd_hms(DateTime3), tzone = "America/Puerto_Rico"), site_no = as.factor(site_no)) %>%
  arrange(site_no, DateTime3) %>% # dplyr::select(-X) %>% 
  dplyr::filter(!site_no %in% as.factor(remove_sites))
length(unique(s3$site_no))

# test whether or not this makes sense: 
# test<- s%>% dplyr::filter(site_no %in% unique(s$site_no[1]), DateTime <=  force_tz(ymd_hms("2005-01-10 01:00:00"), tzone = "America/Puerto_Rico"))
# test2<- s3%>% dplyr::filter(site_no %in% unique(s$site_no[1]), DateTime3 <=  force_tz(ymd_hms("2005-01-10 01:00:00"), tzone = "America/Puerto_Rico"))
# test<- process_sf_3hourlytimes(test)
# test3<- merge(test, test2, by=c('site_no','DateTime3'), all=T)
# ggplot(test3, aes(x=DateTime))+geom_line(aes(y=Flow_Inst))+geom_point(aes(y=Ftot3_cm3h/10800))

#Looks pretty good, total flow is a little high because it assumes flow is steady when it is decreasing and vice verse, but I can deal with it


# Fill in missing streamflow values up to 12 missing hours----

sr3<- merge(s3, r3, by=c('site_no','DateTime3'), all.y=T)
# we only need the rain data for the study period: 
sr3 <- sr3 %>% dplyr::filter(DateTime3>= min(s3$DateTime3))
 
# approximate missing s3 values - up to 12 hours 
sr3<- sr3 %>% group_by(site_no) %>% 
  mutate(Ftot3_cm3h = na.approx(Ftot3_cm3h, na.rm=F, maxgap=4), 
         Fmax3_cms = na.approx(Fmax3_cms, na.rm=F, maxgap=4), 
         Fmin3_cms = na.approx(Fmin3_cms, na.rm=F, maxgap=4) )

sr3<- sr3 %>%dplyr::filter(!site_no %in% as.factor(remove_sites))
length(unique(sr3$site_no))


## Rainfall and dry events----
# source(paste0(fileloc, 'Scripts/Section2_ProcessData/Rainfall1_Events.R'))
# source(paste0(fileloc, 'Scripts/Section2_ProcessData/Rainfall2_EventsProcessing.R'))


## Subset the data based on antecedent rain/dry conditions and process specific discharge

# mms = millimeters per second, me = meters per event

rain<- read.csv(paste0(fileloc, 'Data/ModelData/RainfallEvents_total_peak_210723.csv'))  %>%
  mutate(site_no = factor(site_no)) %>% dplyr::filter(!site_no %in% remove_sites) %>%
  # identify the units the DRNAREA watershed characteristic is in: https://pubs.usgs.gov/fs/2017/3046/fs20173046.pdf # square miles, so convert from mile2 to m2: 1 mile 2 = 2.59e+6 m2
  mutate(DRNAREA_m2 = DRNAREA*2.59e+6, Fmax_mms = Fmaxe_cms/DRNAREA_m2*1000, Ftot_me = Ftote_cme/DRNAREA_m2,
         Ftot_mme = Ftot_me*1000,
         logmax = log(Fmax_mms*1000000+1), logtot = log(Ftot_mme+0.00001),
         logvol = log(Volume+1), lograin6 = log(Rain6hourly+1), lograinlag = log(Rainlag+1))
dry<- read.csv(paste0(fileloc, 'Data/ModelData/DryEvents_total_low_210708.csv'))  %>%
  mutate(site_no = factor(site_no))%>%dplyr::filter(!site_no %in% remove_sites) %>%
  mutate(DRNAREA_m2 = DRNAREA*2.59e+6, Fmin_mms = Fmine_cms/DRNAREA_m2*1000, Ftot_me = Ftote_cme/DRNAREA_m2,
         Ftot_mme = Ftot_me*1000,
         logmin = log(Fmin_mms*1000000+1), logtot =  log(Ftot_mme+0.00001),
         logDur = log(Duration+1), logMDur = log(minDuration+1))

length(unique(rain$site_no))

# instead, take the max flow - flow at the beginning of the 6hour rainfall period (see ...EventsProcessing.R)
# rain<- rain %>% mutate(F6_mms = F6hrlag/DRNAREA_m2*1000 ,
#                        Fmax2 = Fmax_mms - F6_mms) %>% 
#   dplyr::filter(Fmax2 >=0) #%>% # remove all events with negative values this indicates that rainfall was not well estimated 
  #mutate(logmax2())
# hist(log(rain$Fmax2*1000000), breaks=100)

#rain[which(rain$Volume %in% max(rain$Volume)),]


## Determine sites that likely have rainfall underestimated ----

# try adding longitude to the predictors, this may help tremendously
wslon<- DataAvailable %>% dplyr::select(site_no, dec_long_va) %>% dplyr::rename(Longitude = dec_long_va) %>% 
  dplyr::filter(site_no %in% unique(rain$site_no))

# merge with rain and dry 
rain<- merge(rain, wslon, by=c('site_no'))
dry<- merge(dry, wslon, by=c('site_no'))

# test the correlation between longitude and 1) forest cover 2) PRISM annual rainfall, and 3) MSWEP Annual Rainfall
test<- rain %>% dplyr::select(site_no, ForestP, AnnRain, AnnRainMSWEP, Longitude) %>% dplyr::distinct()
# ggplot(test, aes(ForestP, Longitude))+geom_point()+ggpubr::stat_cor()
# ggplot(test, aes(AnnRain, Longitude))+geom_point()+ggpubr::stat_cor()
# ggplot(test, aes(AnnRainMSWEP, Longitude))+geom_point()+ggpubr::stat_cor()


source(paste0(fileloc, 'Scripts/Section2_ProcessData/Streamflow2_TotalFlow_StudyPeriod_Annual_210712.R'))

# Now find the site annual average for the study period: 
anav<- dfaf %>% dplyr::group_by(site_no)%>% add_tally()%>% summarise(AnnTotalAv_mm = mean(Flow_mmyr_norm))
anav<- merge(anav, watershedchars, by='site_no')
head(anav)

plot(anav$AnnRain, anav$AnnRainNormals8095)
# create a df removing sites where rainfall is likely underestimated
dfaf2<- dfaf %>% dplyr::filter(AnnRain >2500)

# identify sites that likely have rainfall underestimated
highrainsites<- unique(dfaf2$site_no)

dfaf3<- dfaf %>% dplyr::filter(!site_no %in% highrainsites)




# Code to find appropriate amount to add to the response to make the distribution normal when logged:
# ggplot(dry)+
#   geom_histogram(bins = 100, aes(x = log(Ftot_me*1000000+1))) +
#   labs(x='Total Daily Runoff')
# ggplot(rain)+
#   geom_histogram(bins = 100, aes(x = log(Fmax_mms*1000000+1))) +
#   labs(x='Max Daily Runoff')#+ylim(0,100)

# Not particularly normally distributed. May need to work on that one, but for now test it
# ggplot(rain)+
#   geom_histogram(bins = 100, aes(x = log(dMax_mm+0.1))) +
#   labs(x='Max Daily Runoff')#+ylim(0,100)


# Merge AR with the dfs 
rain<- rain %>% mutate(Starting = force_tz(ymd_hms(Starting), tz='America/Puerto_Rico'), Date = as.Date(format(Starting, '%Y-%m-%d')))
dry<- dry %>% mutate(Starting = force_tz(ymd_hms(Starting), tz='America/Puerto_Rico'), Date = as.Date(format(Starting, '%Y-%m-%d')))
rain<- merge(rain, rday, by=c('site_no','Date'))
dry<- merge(dry, rday, by=c('site_no','Date'))


# calculate the antecedent rainfall conditions for our remaining sites
rdf_list<- rday %>% #dplyr::filter(!site_no %in% highrainsites) %>% 
  group_by(site_no) %>% group_split()
# apply the function
rdf_list<- lapply(rdf_list, find_ARain_quantiles)
# create a df of all quantiles
rARquant<- bind_rows(rdf_list)
# summarize and find mean quantiles across watersheds 
rARquant<- rARquant %>% dplyr::filter(!sn %in% remove_sites)
rmeanquants<- rARquant %>% summarise(ARain30_33=mean(ARain30_33), ARain30_50= mean(ARain30_50), ARain30_66=mean(ARain30_66), ARain7_33= mean(ARain7_33), ARain7_50 = mean(ARain7_50), ARain7_66=mean(ARain7_66))


wetr<- rain %>% dplyr::filter(ARain30 >= rmeanquants$ARain30_66) # greater than the 66% quantile for month and weekly volumes
dryr<- rain %>% dplyr::filter(ARain30 <= rmeanquants$ARain30_33) 
wetd<- dry %>% dplyr::filter(ARain30 >= rmeanquants$ARain30_66) # greater than the 66% quantile for month and weekly volumes
dryd<- dry %>% dplyr::filter(ARain30 <= rmeanquants$ARain30_33) 


# remove the potentially problematic sites
# wetr<- wetr %>% dplyr::filter(!site_no %in% highrainsites)
# dryr<- dryr %>% dplyr::filter(!site_no %in% highrainsites)
# wetd<- wetd %>% dplyr::filter(!site_no %in% highrainsites)
# dryd<- dryd %>% dplyr::filter(!site_no %in% highrainsites)


# create final values to add to the model
wetr<- wetr %>% mutate(Forestprop = ForestP/100,A700prop = A700P/100, Fmax_mmsx105 = Fmax_mms*1000000+1)
dryr<- dryr %>% mutate(Forestprop = ForestP/100,A700prop = A700P/100, Fmax_mmsx105 = Fmax_mms*1000000+1)
wetd<- wetd %>% mutate(Forestprop = ForestP/100,A700prop = A700P/100, Fmin_mmsx105 = Fmin_mms*1000000+1)
dryd<- dryd %>% mutate(Forestprop = ForestP/100,A700prop = A700P/100, Fmin_mmsx105 = Fmin_mms*1000000+1)



# find correlation between watershed chars
# cor(watershedchars[watershedchars$site_no %in% unique(rain$site_no),c('ForestP', 'A700P','Slope')])
# # great, NO REAL CORRELATION 
# cor(rain[,c('Volume','ForestP', 'A700P','Slope')])






# Section 3: FDCs ----

# plot streamflow for smallest and largest watersheds to justify FDCs on a sudaily scale
bigsmallsites<- watershedchars[which(watershedchars$Area_m2 %in% min(watershedchars$Area_m2)|watershedchars$Area_m2 %in% max(watershedchars$Area_m2)),'site_no']

ggplot(s %>% 
         dplyr::filter(site_no %in% bigsmallsites, 
                       DateTime >=  
                         force_tz(ymd_hms('2005-04-11 00:00:00'), 
                                  tzone = "America/Puerto_Rico"),
                       DateTime <=  
                         force_tz(ymd_hms('2005-04-14 00:00:00'), 
                                  tzone = "America/Puerto_Rico")), 
       aes(x=DateTime, y=Flow_Inst)) +
  geom_line()+facet_wrap(~site_no, scales = 'free_y', ncol=1)


# separate the 15-minute flows into wet and dry conditions
s_wet<- rday %>% dplyr::filter(ARain30 >= rmeanquants$ARain30_66)
s_dry<- rday %>% dplyr::filter(ARain30 <= rmeanquants$ARain30_33)

s_wet<- merge(s, s_wet, by=c('site_no','Date'))
s_dry<- merge(s, s_dry, by=c('site_no','Date'))

source(paste0(fileloc, 'Scripts/Section2_ProcessData/FDC.R'))

# fdcval<- read.csv(paste0(fileloc, 'Data/FDC/FDC_0100_15min_210706.csv'))
# qval<- read.csv(paste0(fileloc, 'Data/FDC/FDC_595_15min_210706.csv'))

# ggplot(fdcval, aes(PE,Q))+geom_line()+facet_wrap(~site_no, scales='free_y')


# calculate the site-specific estimated rainfall event volumes

wvols<- rain %>% group_by(site_no) %>% dplyr::summarise(meanVol = mean(Volume))
maxvol<- max(wvols$meanVol)

# merge qval with watershed chars
qval<- merge(qvals, watershedchars, by='site_no')
qval<- merge(qval, wvols, by='site_no')

#qval<- qval %>% dplyr::filter(!site_no %in% highrainsites)
# qval<- qval %>% dplyr::filter(AnnRain<= 2500)

# normalize by Ann Rain 
maxArain = max(qval$AnnRainMSWEP_max)

qval<- qval %>% rowwise()%>%mutate(
  Area_m2 = DRNAREA*2.59e+6, # covert from miles2 to m2
  Q5n = (Q5/Area_m2*1000)*#10000*
    (maxvol/meanVol), 
  Q95n = (Q95/Area_m2*1000)*#10000*
    (maxvol/meanVol)
)
p1<- ggplot(qval, aes(x=ForestP, y=Q5n, color=A700P))+
  geom_smooth(method='lm')+geom_point(size=2.5)+
  ggpubr::stat_cor(size=2.5)+
  labs(title='a',x='% Forest Cover', 
       y =bquote('Q5n'~(mm ~ s^-1))   # bquote('Q5n'~(mm ~ s^-1*'x'~10^3))
       )+
  theme_bw()+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=6), axis.title = element_text(size = 8), title=element_text(size=10))

p2<- ggplot(qval, aes(x=ForestP, y=Q95n, color=A700P))+
  geom_smooth(method='lm')+geom_point(size=2.5)+
  ggpubr::stat_cor(size=2.5)+#+ggrepel::geom_text_repel(aes(label = site_no))
  labs(title='b',x='% Forest Cover', 
       y = bquote('Q95n'~(mm ~ s^-1)) # bquote('Q95n'~(mm ~ s^-1*'x'~10^3))
       )+
  theme_bw()+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=6), axis.title = element_text(size = 8), title=element_text(size=10))
# grid.arrange(p1,p2, ncol=2)


legend_a <- get_legend(p1 + 
                         theme(legend.position="right", 
                               legend.title = element_text(size=8), 
                               legend.text = element_text(size=6)))

# arrange the two plots in two rows
pint <- cowplot::plot_grid( p1 + theme(legend.position="none"),
                            p2 + theme(legend.position="none"),
                            align = 'vh',hjust = -1, 
                            nrow = 1)

# combine the plots with the shared legend
pf <- cowplot::plot_grid(pint, legend_a, ncol = 2, rel_widths =  c(1, .2))
pf

# 50034000,50043197,50044810,50047535,50053025,50055380,50055750,50058350,50061800,50063800,50064200,50065500,50067000,50070900,50071000,50075000,50100450,50106100,50110900,50111500,50113800,50114900,50124200,50025155

# export figure
# jpeg(paste0(fileloc,'Figures/FDCs_20watersheds_normeventVolume_mms.jpg'),
#     width = 5, height = 3, units = 'in', res = 600)
# #grid.arrange(p1,p2, ncol=2)
# pf
# dev.off()


# tiff(paste0(finalloc,'Figures/Figure3_FDCs.tif'),
#     width = 5, height = 2, units = 'in', res = 600)
# #grid.arrange(p1,p2, ncol=2)
# pf
# dev.off()

## Wet and Dry Periods 

# merge qval with watershed chars
qval_wet<- merge(qvals_wet, watershedchars, by='site_no')
qval_wet<- merge(qval_wet, wvols, by='site_no')
# qval_wet<- qval_wet %>% dplyr::filter(AnnRain<= 2500)
# qval_wet<- qval_wet %>% dplyr::filter(!site_no %in% highrainsites)
# determine the average annual rainfall estimate during the study period: 
# normalize by Ann Rain 
#maxArain = max(qval_wet$AnnRainMSWEP)
qval_wet<- qval_wet %>% rowwise()%>%mutate(
  Area_m2 = DRNAREA*2.59e+6, # covert from miles2 to m2
  Q5n = (Q5/Area_m2*1000)*#10000*
    (maxvol/meanVol), 
  Q95n = (Q95/Area_m2*1000)*#10000*
    (maxvol/meanVol),  
)

qval_dry<- merge(qvals_dry, watershedchars, by='site_no')
qval_dry<- merge(qval_dry, wvols, by='site_no')
# qval_dry<- qval_dry %>% dplyr::filter(AnnRain<= 2500)
# qval_dry<- qval_dry %>% dplyr::filter(!site_no %in% highrainsites)
# determine the average annual rainfall estimate during the study period: 
# normalize by Ann Rain 
#maxArain = max(qval_dry$AnnRain)
qval_dry<- qval_dry %>% rowwise()%>%mutate(
  Area_m2 = DRNAREA*2.59e+6, # covert from miles2 to m2
  Q5n = (Q5/Area_m2*1000)*#10000*
    (maxvol/meanVol), 
  Q95n = (Q95/Area_m2*1000)*#10000*
    (maxvol/meanVol), 
)

# could also use bquote 
# bquote('Q5n'~(mm ~ s^-1*'x'~10^6))

p1<- ggplot(qval_wet, aes(x=ForestP, y=Q5n, color=A700P))+
  #geom_smooth(method='lm')+
  geom_point(size=3)+
  ggpubr::stat_cor(size=3)+
  labs(subtitle='a',title='Wet Conditions',x='', 
       y =expression(Q5n~(mm~s^-1)) # expression(Q5n~(mm~s^-1~x~10^-4))
       )+
  theme_bw()+lims(y=c(-0.001,0.0125))+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=8), axis.title = element_text(size = 9), title = element_text(size = 10))
p2<- ggplot(qval_dry, aes(x=ForestP, y=Q5n, color=A700P))+
  geom_smooth(method='lm')+geom_point(size=3)+
  ggpubr::stat_cor(size=3)+
  labs(subtitle='b',title='Dry Conditions',x='', y = '')+
  theme_bw()+lims(y=c(-0.001,0.0125))+#lims(y=c(-10,125))+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=8), axis.title = element_text(size = 9), title = element_text(size = 10))
# grid.arrange(p1,p2)

p3<- ggplot(qval_wet, aes(x=ForestP, y=Q95n, color=A700P))+
  geom_smooth(method='lm')+geom_point(size=3)+
  ggpubr::stat_cor(size=3)+#+ggrepel::geom_text_repel(aes(label = site_no))
  labs(subtitle='c',#title='',
       x='% Forest Cover', 
       y = expression(Q95n~(mm~s^-1)) # expression(Q95n~(mm~s^-1~x~10^-4))
       )+
  theme_bw()+lims(y=c(-0.00025,0.0025))+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=8), axis.title = element_text(size = 9), title = element_text(size = 10))
p4<- ggplot(qval_dry, aes(x=ForestP, y=Q95n, color=A700P))+
  geom_smooth(method='lm')+geom_point(size=3)+
  ggpubr::stat_cor(size=3)+#+ggrepel::geom_text_repel(aes(label = site_no))
  labs(subtitle='d',#title='',
       x='% Forest Cover', y = '')+
  theme_bw()+lims(y=c(-0.00025,0.0025))+#lims(y=c(-2.5,25))+
  scale_colour_viridis_c(name='% Above\n700m')+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  theme(axis.text = element_text(size=8), axis.title = element_text(size = 9), title=element_text(size=10))

grid.arrange(p1,p2,p3,p4, ncol=2)


#legend_a <- get_legend(p1 + theme(legend.position="right"))

# arrange the two plots in two rows
pint <- cowplot::plot_grid( p1 + theme(legend.position="none"),
                            p2 + theme(legend.position="none"),
                            p3 + theme(legend.position="none"),
                            p4 + theme(legend.position="none"),
                            #align = 'vh',#hjust = -1,vjust =0.1,
                            rel_widths = c(1,1,-0.6,-0.6),
                            nrow = 2)
# pint
# combine the plots with the shared legend
pf <- cowplot::plot_grid( pint, legend_a, ncol = 2, rel_widths =  c(1, .2))
pf


# export figure
# jpeg(paste0(fileloc,'Figures/FDCs_antecedent_20watersheds_normeventVolume_mms.jpg'),
#      width = 5, height = 4.5, units = 'in', res = 600)
# # grid.arrange(p1,p2,p3,p4, ncol=2)
# pf
# dev.off()

# export figure
# tiff(paste0(finalloc,'Figures/Figure5_FDCs_Antecedent.tif'),
#      width = 5, height = 4.5, units = 'in', res = 600)
# #grid.arrange(p1,p2, ncol=2)
# pf
# dev.off()


# try something else, run a glm with the FDC values to see if this gives me anything useful 
test<- glm(Q5 ~ meanVol + scale(ForestP) +scale(A700P), family=gaussian, data=qval_dry)
summary(test)
# nope it never worked. nothing is significant. 

# Section 4: Mixed models ----

# check the autocorrelation: 
# SCRIPT Section3_Model/Check_autocorrelation.R
# CAREFUL:: THIS SCRIPT EXPORTS FIGURES BASED ON CURRENT VALUES OF DFAF3, WETR, DRTR ETC

# Remove the high rainfall watersheds where rainfall is likely underestimated
# wetr<- wetr %>% dplyr::filter(AnnRain <=2500)
# dryr<- dryr %>% dplyr::filter(AnnRain <=2500)
# wetd<- wetd %>% dplyr::filter(AnnRain <=2500)
# dryd<- dryd %>% dplyr::filter(AnnRain <=2500)


# remove the outlier responses ----
# hist(rain$Fmax_mms, breaks=100, ylim = c(0,1000))
# hist(dry$Fmin_mms, breaks=100, ylim = c(0,1000))
# 
# max98q<- unname(quantile(rain$Fmax_mms, 0.98))
# min98q<- unname(quantile(dry$Fmin_mms, 0.98))
# 
# # remove the outliers in the final models 
# wetr<- wetr %>% dplyr::filter(Fmax_mms<max98q)
# dryr<- dryr %>% dplyr::filter(Fmax_mms<max98q)
# wetd<- wetd %>% dplyr::filter(Fmin_mms<min98q)
# dryd<- dryd %>% dplyr::filter(Fmin_mms<min98q)


## Peak flow ----



# test collinearity 
# set.seed(1)
# test <- lme(log(Fmax_mmsx105) ~ log(Rain6hourly+1) + Forestprop +scale(A700P),
#             data=wetr, method="REML",correlation=corARMA(p=2),random = ~ 1 | site_no)
# check_collinearity(test)

# change logmax to log(Fmin_mmsx105) and change lograin6 to log(Rain6hourly+1) to plot results in original units, but I am skeptical of how well this actually works

set.seed(1)
wmaxm <- lme(logmax ~ lograin6 + scale(ForestP) +scale(A700P)+#Slope_scaled+
               lograin6* scale(ForestP),
             method="REML",correlation = corARMA(p=2), 
             random = ~ 1 | site_no, data=rain)
set.seed(1)
dmaxm <- lme(logmax ~ lograin6 + scale(ForestP) +scale(A700P)+#Slope_scaled+
               lograin6* scale(ForestP),
             method="REML",correlation = corARMA(p=2), 
             random = ~ 1 | site_no, data=dryr)

# and again with transformed variables 
# set.seed(1)
# wmaxm2 <- lme(logmax~ lograin6 + Forest_scaled +Above700_scaled+#Slope_scaled+
#                 lograin6 * Forest_scaled,
#               method="REML",correlation = corARMA(p=2), 
#               random = ~ 1 | site_no, data=wetr)
# set.seed(1)
# dmaxm2 <- lme(logmax ~ lograin6  + Forest_scaled +Above700_scaled+#Slope_scaled+
#                 lograin6 * Forest_scaled,
#               method="REML",correlation = corARMA(p=2), 
#               random = ~ 1 | site_no, data=dryr)


# check models
# plot(wmaxm)# check out residuals
# plot_model(wmaxm, type='est') # quick coefficient plot

p1<- plot_model(wmaxm2, type='int')
p2<- plot_model(dmaxm2, type='int')
grid.arrange(p1,p2)




## Low Flow ----

# ggplot(wetd, aes(y=log(Fmin_mmsx105), x=log(Duration)))+geom_point()+geom_smooth(method='lm')+facet_wrap(~site_no)


set.seed(1)
wminm <- lme(logmin~ logDur + scale(ForestP) +scale(A700P)+#Slope_scaled+
               logDur * scale(ForestP),
             method="REML",correlation = corARMA(p=2), 
             random = ~ 1 | site_no, data=wetd)
set.seed(1)
dminm <- lme(logmin ~ logDur  + scale(ForestP) +scale(A700P)+#Slope_scaled+
               logDur * scale(ForestP),
             method="REML",correlation = corARMA(p=2), 
             random = ~ 1 | site_no, data=dryd)

# plot the same but this time without transformations in the model 
# set.seed(1)
# wminm2 <- lme(logmin~ logDur + Forest_scaled +Above700_scaled+#Slope_scaled+
#                 logDur * Forest_scaled,
#              method="REML",correlation = corARMA(p=2), 
#              random = ~ 1 | site_no, data=wetd)
# set.seed(1)
# dminm2 <- lme(logmin ~ logDur  + Forest_scaled +Above700_scaled+#Slope_scaled+
#                logDur * Forest_scaled,
#              method="REML",correlation = corARMA(p=2), 
#              random = ~ 1 | site_no, data=dryd)



# check models
# plot(wmaxm)# check out residuals
# plot_model(wminm, type='est') # quick coefficient plot
# plot_model(dminm, type='est') # quick coefficient plot

coefplot2::coefplot2(wminm)
coefplot2::coefplot2(dminm)

summary(dminm)

model_performance(wminm)
model_performance(dminm)

p1<- plot_model(wminm2, type='int')
p2<- plot_model(dminm2, type='int')
grid.arrange(p1,p2)


# plot(ggpredict(wminm, terms = c("logDur")))



# Extract coefficients and calculate CI to determine if results are significant----

wemax<- extractresults(wmaxm, 'Lag 6hr Rainfall','Rainfall') 
wemax<- wemax %>% mutate(Model = 'Wet Conditions',prednames = fct_relevel(prednames,wemax$prednames))
demax<- extractresults(dmaxm, 'Lag 6hr Rainfall','Rainfall') 
demax<- demax %>% mutate(Model = 'Dry Conditions',prednames = fct_relevel(prednames,demax$prednames))

emax2<- rbind(wemax, demax) %>% dplyr::filter(!prednames %in% 'Intercept')%>% 
  mutate(#significant2 = fct_relevel(significant2,c('95% CI','66% CI')),#, 'Non-significant')),
         Model = fct_relevel(Model,c('Wet Conditions','Dry Conditions')))

wemin<- extractresults(wminm, 'Dry Event Duration','Duration') 
wemin<- wemin %>% mutate(Model = 'Wet Conditions',prednames = fct_relevel(prednames,wemin$prednames))
demin<- extractresults(dminm, 'Dry Event Duration','Duration') 
demin<- demin %>% mutate(Model = 'Dry Conditions',prednames = fct_relevel(prednames,demin$prednames))

emin2<- rbind(wemin, demin) %>% dplyr::filter(!prednames %in% 'Intercept')%>% 
  mutate(#significant2 = fct_relevel(significant2,c('95% CI',#'66% CI',
          #                                         'Non-significant')),
         Model = fct_relevel(Model,c('Wet Conditions','Dry Conditions')))



# Model stats ----
mp_tot<- data.frame(model_performance(mtotann))

mp_maxw<- data.frame(model_performance(wmaxm))
mp_maxd<- data.frame(model_performance(dmaxm))

mp_minw<- data.frame(model_performance(wminm))
mp_mind<- data.frame(model_performance(dminm))

# add a model name and bind rows 
mp_tot<- mp_tot %>% mutate(Model = 'Annual Streamflow', n_points = length(dfaf3$site_no), n_sites =20)
mp_maxw<- mp_maxw %>% mutate(Model = 'Peak Streamflow, Wet conditions', n_points = length(wetr$site_no), n_sites =20)
mp_maxd<- mp_maxd %>% mutate(Model = 'Peak Streamflow, Dry conditions', n_points = length(dryr$site_no), n_sites =20)
mp_minw<- mp_minw %>% mutate(Model = 'Low Streamflow, Wet conditions', n_points = length(wetd$site_no), n_sites =20)
mp_mind<- mp_mind %>% mutate(Model = 'Low Streamflow, Dry conditions', n_points = length(dryd$site_no), n_sites =20)

mp<- rbind(mp_maxw,mp_maxd,mp_minw,mp_mind) 
mp<- mp %>% dplyr::select(Model, n_sites, n_points, R2_marginal, R2_conditional, RMSE)%>% 
  mutate(R2_conditional = round(R2_conditional, digits=2),
         R2_marginal = round(R2_marginal, digits=2),
         RMSE = round(RMSE, digits=2))

# export for the manuscript: 
# write.csv(mp,paste0(fileloc,'Tables/ModelPerformance_210719.csv'), row.names=F)


# Plot coefficients----

cp1<- ggplot(emax2, aes(x=est, y=prednames, color = Model#, shape = significant3
                        ))+
  theme_bw()+theme(legend.position = 'none')+
  labs(x='Coefficients', y='', title='a',subtitle = 'Peak Runoff')+
  geom_point(position = position_dodge2v(height=0.5))+
  scale_color_manual(values=c('Wet Conditions' = 'darkblue','Dry Conditions'= 'darkred'))+
  scale_shape_manual(name='Coefficient Significance', #values=c("95% CI" = 15, "66% CI" = 16,"Non-significant" = 17))+
                     values=c("Significant" = 15,"Non-Significant" = 17))+
  geom_pointrange(aes(xmin = lower, xmax = upper), fatten=5, position = position_dodge2v(height=0.5))+
  geom_linerange(aes(y = prednames, xmin = lower2, xmax = upper2), size=1.1, position = position_dodge2v(height=0.5))+
  #guides(color=FALSE)+
  geom_vline(xintercept = 0, linetype = 'dotted')

cp2<- ggplot(emin2, aes(x=est, y=prednames, color = Model#, shape = significant3
                        ))+
  theme_bw()+theme(legend.position = c(0.8,0.8))+
  labs(x='Coefficients', y='', title='b',subtitle = 'Low Runoff')+
  geom_point(position = position_dodge2v(height=0.5))+
  scale_color_manual(values=c('Wet Conditions' = 'darkblue','Dry Conditions'= 'darkred'))+
  scale_shape_manual(name='Coefficient Significance',  #values=c("95% CI" = 15, "66% CI" = 16,"Non-significant" = 17))+
                     values=c("Significant" = 15,"Non-Significant" = 17))+
  geom_pointrange(aes(xmin = lower, xmax = upper), fatten=5, position = position_dodge2v(height=0.5))+
  geom_linerange(aes(y = prednames, xmin = lower2, xmax = upper2), size=1.1, position = position_dodge2v(height=0.5))+
  #guides(color=FALSE)+
  geom_vline(xintercept = 0, linetype = 'dotted')

grid.arrange(cp1,cp2,nrow=1)
# grid_arrange_shared_legend(cp1, cp2)

# plot both coefficient plots and interaction plots in one large graph
# legend_a <- get_legend(cp1 + theme(legend.position="right")+guides(shape=FALSE))
# legend_b <- get_legend(cp1 + theme(legend.position="right")+guides(color=FALSE))
# legend_a<- p1 <- plot_grid( legend_a, legend_b, ncol = 1, rel_widths =  c(1, 1))

legend_a <- get_legend(cp1 + theme(legend.position="right"))

# arrange the two plots in two rows
pcoefs <- cowplot::plot_grid( cp1 + theme(legend.position="none"),
                     cp2 + theme(legend.position="none"),
                     align = 'vh',hjust = -1, nrow = 1)
# combine the plots with the shared legend
pf <- cowplot::plot_grid( pcoefs, legend_a, ncol = 2, rel_widths =  c(1, .3))
pf

# export figure 
# jpeg(paste0(fileloc,'Figures/Coef_wetdry_20watersheds.jpg'),
#     width = 9, height = 4, units = 'in', res = 600)
# pf
# dev.off()

# export figure 
# tiff(paste0(finalloc,'Figures/Figure4_Coef.tif'),
#     width = 9, height = 4, units = 'in', res = 600)
# pf
# dev.off()


# Plot Interactions---- 

# unscale the plots 
# for rain events 
raindf<- data.frame(spline(rain$Rain6hourly, rain$lograin6, xout = c(1,10,100)))
maxdf<- data.frame(spline(wetr$Fmax_mmsx105, wetr$logmax, xout = c(1,10,100,1000,10000)))
maxdf2<- data.frame(spline(dryr$Fmax_mmsx105, dryr$logmax, xout = c(1,10,100,1000,10000)))

# for dry events
drydf<- data.frame(spline(dry$Duration, dry$logDur, xout = c(10,100,500)))
mindf<- data.frame(spline(wetd$Fmin_mmsx105, wetd$logmin, xout = c(1,10,100)))
mindf2<- data.frame(spline(dryd$Fmin_mmsx105, dryd$logmin, xout = c(1,10,100)))





# use if I transform the predictors and response inside of the model 
# pmaxw <- ggpredict(wmaxm, terms = c("Rain6hourly [0:100]","ForestP[50,100]"))
# pmaxd <- ggpredict(dmaxm, terms = c("Rain6hourly [0:100]","ForestP [50,100]"))
# pminw <- ggpredict(wminm, terms = c("Duration [12:1000]","ForestP [50,100]"))
# pmind <- ggpredict(dminm, terms = c("Duration [12:1000]","ForestP [50,100]"))

pmaxw <- ggpredict(wmaxm, terms = c("lograin6","ForestP[50,100]"))
pmaxd <- ggpredict(dmaxm, terms = c("lograin6","ForestP [50,100]"))
pminw <- ggpredict(wminm, terms = c("logDur","ForestP [50,100]"))
pmind <- ggpredict(dminm, terms = c("logDur","ForestP [50,100]"))


ip1<- plot(pmaxw)+theme_bw()+
  #lims(y=c(-100,3200))+
  labs(subtitle = 'a',title = 'Wet Conditions',
       x='Lag 6 hour Rainfall (mm)', y= expression(Peak~Streamflow~(mm~s^-1~x~10^5)))+
  scale_x_continuous(breaks = raindf$y,labels = raindf$x, limits = c(0,5))+
  scale_y_continuous(breaks = maxdf$y,labels = maxdf$x, limits = c(0,10))+
  scale_color_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))+
  scale_fill_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))

ip2<- plot(pmaxd)+theme_bw()+
  #lims(y=c(-100,3200))+
  labs(subtitle='b',title='Dry Conditions',x='Lag 6 hour Rainfall (mm)', y = '')+
  scale_x_continuous(breaks = raindf$y,labels = raindf$x, limits = c(0,5))+
  scale_y_continuous(breaks = maxdf2$y,labels = maxdf2$x, limits = c(0,10))+
  scale_color_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))+
  scale_fill_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))

# grid.arrange(ip1,ip2,nrow=1)



ip3<- plot(pminw)+theme_bw()+
  # lims(y=c(0,70))+
  labs(subtitle = 'c',title = '',
      x='Duration (hours)', y= expression(Low~Streamflow~(mm~s^-1~x~10^5)))+
  scale_x_continuous(breaks = drydf$y,labels = drydf$x, limits = c(2,7))+
  scale_y_continuous(breaks = mindf$y,labels = mindf$x, limits = c(0,5))+
  scale_color_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))+
  scale_fill_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))

ip4<- plot(pmind)+theme_bw()+
  # lims(y=c(0,70))+
  labs(subtitle = 'd',title = '',
       x='Duration (hours)', y= '')+
  scale_x_continuous(breaks = drydf$y,labels = drydf$x, limits = c(2,7))+
  scale_y_continuous(breaks = mindf$y,labels = mindf$x, limits = c(0,5))+
  scale_color_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))+
  scale_fill_manual(name = "Forest Cover", labels = c("50%",'100%'), values = c('sienna','darkgreen'))

# grid.arrange(ip3,ip4)



legend_a <- get_legend(ip2 + theme(legend.position="bottom"))

# arrange the two plots in two rows
pint <- cowplot::plot_grid( ip1 + theme(legend.position="none"),
                              ip2 + theme(legend.position="none"),
                              ip3 + theme(legend.position="none"),
                             ip4 + theme(legend.position="none"),
                             align = 'vh',hjust = -1, nrow = 2)
# combine the plots with the shared legend
p1 <- cowplot::plot_grid( pint, legend_a, ncol = 1, rel_heights =  c(1, .1))
p1

# export figure 
# jpeg(paste0(fileloc,'Figures/Interactions_wetdry__20watersheds.jpg'),
#     width = 5, height = 6, units = 'in', res = 600)
# p1
# dev.off()






# export a watershed characteristics table ----

# export for the manuscript watershed characteristics table: 

# create a truncated version of the watershed characteristics with relevant chars
twc<- watershedchars %>% mutate(Area_ha = Area_m2/10000) %>% dplyr::select(site_no, Area_ha, ForestP, A700P)#, AnnRain,AnnRainMSWEP)
# determine the mean volume and duration value for rainfall and dry events during our study period: 
wvols<- rain %>% group_by(site_no) %>% dplyr::summarise(meanVol = mean(Volume))
wdurs<- dry %>% group_by(site_no) %>% dplyr::summarise(meanDur = mean(Duration))

# merge with twc
twc<- merge(twc, wvols, by='site_no')
twc<- merge(twc, wdurs, by='site_no')

twc<- twc %>% mutate(Area_ha = round(Area_ha),meanVol = round(meanVol), meanDur = round(meanDur))%>% 
  dplyr::rename(MeanVolume=meanVol, MeanDuration = meanDur)

# export for later use: 
# also make sure to note in the final table the ones we excluded from the mixed models 
# write.csv(twc, paste0(fileloc,"/Tables/WatershedCharacteristics_210719.csv"), row.names=F)


# export a watershed map, color by whether it is high or low rain ----

# read in PR shapefile 
pr<- st_read('D:/Dropbox/Backup/PuertoRico/outline_PR/pr_outline_utm.shp')
pr<- st_transform(pr, crs=crs(forest2))
# and thge wgs pr 
pr2<- st_read('D:/Dropbox/Backup/PuertoRico/outline_PR/pr_outline_wgs.shp')

# make a map with tmap ----
library(tmap)
library(stars)
library(tinter)
darkergreen<- darken('darkgreen',0.5)
lightergreen<- lighten('darkseagreen4',0.8)
data(World, metro, rivers, land)
class(land)

# process forest cover
forest4<- read_stars('D:/Dropbox/Backup/Chapter1/DataforMs/GIS/LandCover/CCAP2010/Forest_Nonforest.tif')
forest5<- st_crop(forest4, st_bbox(pr2))
forestpoly = st_as_sf(forest5, as_points = FALSE, merge = TRUE)
forestpoly2<- forestpoly %>% dplyr::filter(Forest_Nonforest.tif %in% 1)
forestpoly2<- st_geometry(forestpoly2)
forestpoly2<- st_combine(forestpoly2)

# process watershed chars 
ws_chars3<- ws_chars2
ws_chars3$meanVol = wvols$meanVol

# create a 
# Now create a point feature with the watershed coordinates
scoord<- DataAvailable %>% dplyr::filter(site_no %in% unique(ws_chars2$site_no)) %>% dplyr::select(site_no, dec_lat_va, dec_long_va)
scoord<- merge(scoord, wvols, by='site_no')
scoord <- st_as_sf(scoord,coords = c("dec_long_va", "dec_lat_va"),crs = 4268)
scoord2<- st_transform(scoord, st_crs(pr2))
#scoord2$meanVolround<- round(scoord2$meanVol)
scoord2<- scoord2 %>% dplyr::mutate('Mean Rain Volume (mm)' = round(meanVol))

# create a custom plotting space 
maplims<- st_bbox(forest4)
maplims[2]<- 17.8 # make ylim lower to make space for the scale bar and north arrow
maplims[3]<- -65.3
tmap_mode("plot")


# first add the PR shapfile with pr border in grey
wmap<- tm_shape(pr2, bbox = maplims) +
  tm_grid(ticks=T, lines=F)+
  tm_compass(position = c("left", "bottom"))+
  tm_scale_bar(breaks=c(0,10,20,30),position=c("left", "bottom"))+
  tm_borders("darkgrey", lwd = .5) +
  tm_fill(lighten('lightgrey',0.3), lwd = .5) +
# then add the forest cover layer
  tm_shape(forestpoly2) +
  tm_fill(col=lightergreen)+
  tm_add_legend('fill', col=lightergreen, title='Forest Cover')+
# watersheds
tm_shape(ws_chars3) +
  tm_borders("black", lwd = 2)+
  tm_add_legend('line', col='black', title='Watersheds')+
# stations
tm_shape(scoord2) +
  tm_dots('meanVol', size=0.3, 
          palette = RColorBrewer::brewer.pal(6, "RdYlBu"), 
          title = "Stream Gages\nmean event rain\n(mm)", border.col='black')+
  tm_layout(legend.outside=FALSE, legend.position= c("right", "center"))


# tiff('D:/Dropbox/Backup/Chapter1/DataforMs/Figures/WatershedMap.tif', 
#      width = 6, heght = 3.5, res=600)


tmap_save(wmap, 'D:/Dropbox/Backup/Chapter1/DataforMs/Figures/WatershedMap3.tiff', dpi=600, 
          width =11.2011/1.3, height = 4.374569/1.3)
# 11.2011 by 4.374569 - default size
#  divided by 2 - text too big
#  divided by 1.5 text still too big
# divided by 1.3
