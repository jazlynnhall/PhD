# process the streamflow and rainfall for rain and dry events to create the final mixed model dfs


# read in the csvs for rainfall and dry events

# rainfall
revents<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210707.csv')) %>% dplyr::select(-X) %>% mutate(site_no = factor(site_no))
rts<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210707.csv') )%>% dplyr::select(-X) %>% mutate(site_no = factor(site_no), DateTime3 = force_tz(ymd_hms(DateTime3), 'America/Puerto_Rico'))
# rchar<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210707.csv')) %>% dplyr::select(-X) %>% mutate(site_no = factor(site_no))
# dry events
devents<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEvents_6hrminrainless_210707.csv'))%>% dplyr::select(-X) %>% mutate(site_no = factor(site_no))
dts<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventsTimeSeries_6hrminrainless_210707.csv'))%>% dplyr::select(-X) %>% mutate(site_no = factor(site_no), DateTime3 = force_tz(ymd_hms(DateTime3), 'America/Puerto_Rico'))
# dchar<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventChars_6hrminrainless_210707.csv')) %>% dplyr::select(-X) %>% mutate(site_no = factor(site_no))%>%
  # we don't really care about the antecedent volume or intensity as it was a made up number, remove these for simplicity 
  # dplyr::select(-c(remonth_meanVol,remonth_sumVol,remonth_meanInt, reweek_meanVol,reweek_sumVol, reweek_meanInt, re14d_meanVol,re14d_sumVol,re14d_meanInt))




# Need to filter out the rainfall events that we have no/little original streamflow data for. How to do this?
# merge sr3 with the rts, keep all of the rows, and remove the rain events with NA values in total flow

rts2<- merge(rts, sr3%>% dplyr::select(-Rain3hourly), by=c('site_no','DateTime3')) 
dts2<- merge(dts, sr3%>% dplyr::select(-Rain3hourly), by=c('site_no','DateTime3'))

# find a threshold for the duration of the event - we want to make sure we can include all sites 
# ggplot(revents, aes(x=Duration, y=site_no))+geom_point()+lims(x=c(0,100))
# ggplot(devents, aes(x=Duration, y=site_no))+geom_point()+lims(x=c(0,100))

# also only consider rain events that lasted more than 9 hours
rts2<- rts2 %>% dplyr::filter(Duration>=12)
dts2<- dts2 %>% dplyr::filter(Duration>=12)

# find the minimum duration of an event for all sites 
# test<- devents %>% group_by(site_no) %>% summarize(mindur = min(Duration), meanDur = mean(Duration))


# test whether this makes sense against the streamflow and rainfall data
# test<- merge(rts2, s3, by=c('site_no','DateTime3'))
test2<- rts2 %>%  dplyr::filter(site_no %in% unique(s$site_no)[1], 
                                DateTime3 >=  force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"),
                                DateTime3 <=  force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico"))
test3<- sr3 %>%  dplyr::filter(site_no %in% unique(s$site_no)[1], 
                               DateTime3 >=  force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"),
                               DateTime3 <=  force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico"))
test4<- sr3 %>%  dplyr::filter(site_no %in% unique(s$site_no)[1], 
                               DateTime3 >=  force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"),
                               DateTime3 <=  force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico"))

p1<- ggplot(test2, aes(x=DateTime3))+geom_point(aes(y=Ftot3_cm3h, color=factor(Number.Event)))+theme(legend.position = 'none')+
  lims(x=c(force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"), force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico")))
p2<- ggplot(test2, aes(x=DateTime3))+geom_point(aes(y= Rain3hourly, color=factor(Number.Event)))+geom_line(aes(y= Rain3hourly, color=factor(Number.Event)))+theme(legend.position = 'none')+
  lims(x=c(force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"), force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico")))
p3<- ggplot(test3, aes(x=DateTime3))+geom_point(aes(y= Ftot3_cm3h))+
  lims(x=c(force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"), force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico")))
p4<- ggplot(test4, aes(x=DateTime3))+geom_point(aes(y= Rain3hourly))+geom_line(aes(y= Rain3hourly))+
  lims(x=c(force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"), force_tz(ymd_hms("2005-03-31 01:00:00"), tzone = "America/Puerto_Rico")))

# grid.arrange(p1,p3,p2,p4,ncol=1)





# Now summarize flow for each event 
srevents<- rts2 %>% group_by(site_no, Number.Event)%>%
  summarize(Fmaxe_cms = max(Fmax3_cms, na.rm=F), Fmine_cms = min(Fmin3_cms, na.rm=F),Ftote_cme = sum(Ftot3_cm3h))%>%
  dplyr::filter(!is.na(Ftote_cme))
sdevents<- dts2 %>% group_by(site_no, Number.Event)%>%
  summarize(Fmaxe_cms = max(Fmax3_cms, na.rm=F), Fmine_cms = min(Fmin3_cms, na.rm=F),Ftote_cme = sum(Ftot3_cm3h))%>%
  dplyr::filter(!is.na(Ftote_cme))

# determine why we have 0 flow rainfall events 
# test<- srevents%>% dplyr::filter(Ftot3_cme %in% 0)
# test2<- revents %>% dplyr::filter(site_no %in% unique(test$site_no), Number.Event %in% unique(test$Number.Event))
# test3<- s3 %>% dplyr::filter(site_no %in% unique(test$site_no), DateTime3 >= test2$Starting[1], DateTime3 <= test2$End[1])
# test4<- s %>% dplyr::filter(site_no %in% unique(test$site_no), DateTime >= test2$Starting[1], DateTime <= test2$End[1])
# test5<- r3 %>% dplyr::filter(site_no %in% unique(test$site_no), DateTime3 >= test2$Starting[1], DateTime3 <= test2$End[1])
# test5
# rainfall for this event was pretty small and all original values say 0, so I will accept 0

# merge srevents with rts to only keep the events with streamflow data 
rts2<- merge(rts2, srevents, by=c('site_no','Number.Event'))
dts2<- merge(dts2, sdevents, by=c('site_no','Number.Event'))


# update the other rainfall/dry event dfs to only include rainfall events with flow data
revents<- rts2 %>% dplyr::select(names(revents)) %>% distinct()
devents<- dts2 %>% dplyr::select(names(devents)) %>% distinct()

length(unique(revents$site_no))
length(unique(devents$site_no))

# Find the predictors for peak flow
# peak flow is instantaneous - so we need to identify rainfall for the last 3-6 hours before the peak flow happened
# Martha said there is evidence showing that most streams in PR reack peak flow within 1-3 hours. SO a lag would be good. 
# However, we currently have rain and flow aggregated to 3 hours. So take the rainfall from the previous 3 hour period (Rainlag) and the sum of Rainlag and the rainfall for the 3hour period with peak flow (Rain3hourly)
# calculate the rainfall for the last 6 hours before the max values for rainfall events 

r3_2<- r3 %>%dplyr::mutate(Rainlag = lag(Rain3hourly), Rain6hourly = Rain3hourly + Rainlag) 
rts<- merge(rts2, r3_2 %>% dplyr::select(site_no, DateTime3, Rain6hourly, Rainlag))
# identify the row for each site and event that has the peak flow in it
maxrain <- rts %>% group_by(site_no, Number.Event) %>% #dplyr::filter(abs(Fmax3_cms - max(Fmax3_cms)))
  slice(which.min(abs(Fmax3_cms - Fmaxe_cms)))%>% dplyr::select(site_no, DateTime3, Number.Event, Fmaxe_cms,Rain6hourly, Rainlag )

# Now identify flow at the beginning of the 6hour rainfall period before peak flow (including the first 3hour rainfall period in which peak flow occurred)
maxrain<- maxrain %>% mutate(DateTime_6hr = ymd_hms(DateTime3) - hours(3))

# Identify the flow value nearest to the DateTime_6hr value and extract it 
datalist<- list()
for(i in 1:nrow(maxrain)){
  df<- maxrain[i,]
  bf<- s[s$site_no %in% unique(df$site_no),]
  bf<- bf %>%slice(which.min(abs(DateTime - df$DateTime_6hr))) %>%
    dplyr::select(site_no, Flow_Inst) %>% dplyr::rename(F6hrlag = Flow_Inst)
  bf<- merge(df, bf, by='site_no')
  datalist[[i]]<- bf
}

maxrain2<- bind_rows(datalist)
head(maxrain2)

maxrain2 <- maxrain2 %>% group_by(site_no, Number.Event) %>% dplyr::select(site_no, Number.Event, Fmaxe_cms,Rain6hourly, Rainlag,  F6hrlag)


# add it to the srevents 
srevents<- merge(srevents, maxrain2, by=c('site_no','Number.Event', 'Fmaxe_cms'))
head(srevents)


# find the predictors for low flow 
# minimum flow SHOULD be at the end of the dry period, but that may not necessarily be the case, especially if there is undetected rainfall
# find the duration of the 

# identify the row for each site and event that has the peak flow in it
mindur <- dts2 %>% group_by(site_no, Number.Event) %>% #dplyr::filter(abs(Fmax3_cms - max(Fmax3_cms)))
  slice(which.min(abs(Fmin3_cms - Fmine_cms)))%>% dplyr::select(site_no, Number.Event, Fmine_cms,Starting, DateTime3 )
# now calculate the duration of the dry event when the minimum flow value was reached 
mindur<- mindur %>% group_by(site_no, Number.Event) %>%
  mutate(Starting = force_tz(ymd_hms(Starting), 'America/Puerto_Rico'), 
         minDuration = DateTime3 -Starting) # this is in seconds for some reason
mindur<- mindur %>% mutate(minDuration = as.numeric(minDuration)/60/60)
head(mindur)
# add it to the srevents 
sdevents<- merge(sdevents, mindur %>% dplyr::select(-c( Starting,DateTime3)), by=c('site_no','Number.Event', 'Fmine_cms'))
head(sdevents)


# Now I need to merge the rainfall, flow, and antecedent characteristics for each event and flow type 

# Rainfall events 
rain<- merge(revents, srevents, by=c('site_no', 'Number.Event'))
# rain<- merge(rain, rchar, by=c('site_no', 'Number.Event'))

# Dry events 
dry<- merge(devents, sdevents, by=c('site_no','Number.Event'))
# dry<- merge(dry, dchar, by=c('site_no','Number.Event'))

# arrange 
rain<- rain %>% arrange(site_no, Number.Event)
dry<- dry %>% arrange(site_no, Number.Event)


# Add antecedent rain conditions to dry event df ---- 
# I shouldn't use dry period duration to find the antecedent rainfall conditions for dry events.
# Instead I should calculate the antecedent rainfall for the events in the previous n days 
# format the df before I find antecedent rain for the dry periods
rain<- rain  %>% 
  mutate(site_no = factor(site_no), Starting = force_tz(ymd_hms(Starting), tzone = 'America/Puerto_Rico'),End = force_tz(ymd_hms(End), tzone = 'America/Puerto_Rico'))
dry<- dry  %>% 
  mutate(site_no = factor(site_no), Starting = force_tz(ymd_hms(Starting), tzone = 'America/Puerto_Rico'),End = force_tz(ymd_hms(End), tzone = 'America/Puerto_Rico'), endofrain = Starting - hours(3))

# modify rainfall event df ao I can join the volume columns to the dry event directly after each rainfall event 
# testrain<- rain %>%  dplyr::rename(R.Event = Number.Event, endofrain= End) %>% dplyr::select(site_no, R.Event, endofrain, remonth_sumVol, reweek_sumVol, re14d_sumVol)
# dry<- merge(dry, testrain, by=c('site_no','endofrain'))


## Merge with the watershed characteristics ----
# first rescale in case we have changed out site selection

# recale the values with the new set of sites 



rain<- merge(rain, watershedchars, by='site_no')
dry<- merge(dry, watershedchars, by='site_no')





# export for later:
# write.csv(rain, paste0(fileloc, 'Data/ModelData/RainfallEvents_total_peak_210706.csv'), row.names = F)
# write.csv(dry, paste0(fileloc, 'Data/ModelData/DryEvents_total_low_210706.csv'), row.names = F)
# 

# write.csv(rain, paste0(fileloc, 'Data/ModelData/RainfallEvents_total_peak_210708.csv'), row.names = F)
# write.csv(dry, paste0(fileloc, 'Data/ModelData/DryEvents_total_low_210708.csv'), row.names = F)

# including flow 6 hours before peak flow is met 
# write.csv(rain, paste0(fileloc, 'Data/ModelData/RainfallEvents_total_peak_210723.csv'), row.names = F)
# write.csv(dry, paste0(fileloc, 'Data/ModelData/DryEvents_total_low_210723.csv'), row.names = F)



# Try something else - don't look at the absolute value, instead look at the change in the values for rain and dry periods
# 
# # determine the flow at the beginning of the rainfall event
# datalist<- list()
# for(i in 1:nrow(rain)){
#   df<- rain[i,]
#   bf<- s[s$site_no %in% unique(df$site_no),] 
#   bf<- bf %>%slice(which.min(abs(DateTime - df$Starting))) %>% 
#     dplyr::select(site_no, Flow_Inst) %>% dplyr::rename(BF_cme = Flow_Inst)
#   bf<- merge(df, bf, by='site_no')
#   datalist[[i]]<- bf
# }
# 
# rain2<- bind_rows(datalist)
# head(rain2)
# 
# # export and use for later: 
# 
# # STILL NEED TO RUN THIS EVENTUALLY
# 
# # determine the flow at the beginning of the dry event
# datalist<- list()
# for(i in 1:nrow(dry)){
#   df<- dry[i,]
#   bf<- s[s$site_no %in% unique(df$site_no),] 
#   bf<- bf %>%slice(which.min(abs(DateTime - df$Starting))) %>% 
#     dplyr::select(site_no, Flow_Inst) %>% dplyr::rename(BF_cme = Flow_Inst)
#   bf<- merge(df, bf, by='site_no')
#   datalist[[i]]<- bf
# }
# 
# dry2<- bind_rows(datalist)
# head(dry2)
# 
# # export and use for later: 
# 


# write.csv(rain2, paste0(fileloc, 'Data/ModelData/RainfallEvents_total_peak_210711.csv'), row.names = F)
# write.csv(dry2, paste0(fileloc, 'Data/ModelData/DryEvents_total_low_210711.csv'), row.names = F)



# This script was originally used before the mixed model to test if I get different results using the difference in FMax - F at beginning of event 
# I do not. 
# # TEST ----
# Instead of using the peak and low flows, examine the change in stream response from beginning of the events to the extremes
# peak flow = peak flow during rainfall event - flow at the beginning of the event
# low flow = peak flow during dry event - flow at the beginning of the event

# UPDATED - INSTEAD OF FLOW AT THE BEGINNING OF THE EVENT, TRY FLOW AT THE BEGINNING OF THE LAG 6HOUR RAIN PERIOD

rain<- rain %>% mutate(dMax_cms = Fmaxe_cms-BF_cme, dMax_mm = dMax_cms/DRNAREA_m2*1000*1000000)

# hist(rain$dMax_mm)
# summary(rain$dMax_mm)

# some values are negative, what went wrong?
# table(rain$site_no[rain$dMax<0])
# Only about 6 events are negative, we can probably remove those. It seems to do a decent job otherwise

# check out the first negative value on the 3hourly data
# ggplot(sr3 %>% dplyr::filter(site_no %in% '50025155', Date >= as.Date('2006-09-15'), Date <= as.Date('2006-10-10'))
#  )+geom_line(aes(DateTime3, Ftot3_cm3h/10000))+geom_line(aes(DateTime3, Rain3hourly), color='red')

# It looks like more of a fluke than anything. The rain event-streamflow relationship looks pretty good except for the one bad value
rain<- rain %>% dplyr::filter(dMax_mm>0)
# Really quickly, just change log max to the log of the difference
rain<- rain %>% mutate(logmax = log(dMax_mm+0.1))


# END TEST----
