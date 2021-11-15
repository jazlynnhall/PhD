# This script identifies rainfall events from the MSWEP rain data 

# Determine rainfall events and characteristics using the IETD package ----

#  using the IETD package: 

# subset rainfall by site_no 
rain<- r3 %>% dplyr::filter(site_no %in% unique(r3$site_no)[1]) %>% dplyr::select(DateTime3, Rain3hourly)


# need to determine the time threshold beyond which the event is independent 
# https://www.researchgate.net/post/How-to-select-events-from-long-time-series-rainfall-and-discharge-data
# Voorhees, M.L., 1981. An evaluation of the urban design storm concept.
# Restrepo-Posada, P.J., Eagleson, P.S., 1982. Identification of independent rainstorms. Journal of Hydrology, 55(1-4): 303-319.
# Brown, B.G., Katz, R.W., Murphy, A.H., 1985. Exploratory Analysis of Precipitation Events with Implications for Stochastic Modeling. Journal of Climate and Applied Meteorology, 24(1): 57-67.


# FIND THE APPROPRIATE RAINLESS PERIOD BETWEEN RAINFALL EVENTS

# try this, pick an interevent time that gives a CV of 1
# https://www.openswmm.org/topic/668/rainfall-analysis-inter-event-period
# Restrepo-Posada and Eagleson. "Identification of independent rainstorms," _Jnl of Hydrology_, v55p303, 1982. Brown, et al. "Exploratory analysis of precipitation events..." _Jnl of Climate & Appl Meterology_ v24(12) p1285, 1985
# Sariahmed and Kisiel, "Synthesis of sequences of summer thunderstorms..." _Proc, Intl Assoc of the Hydrologic Sciences Symposium on Use of Analog and Digital Computers in Hydrology_, v2p439, 1968

# run a function to find the minimum rainless period to define rainfall events for each site and compare them across sites
# datalist<- list()
# for( i in unique(r3$site_no)){
#   rain<- r3 %>% dplyr::filter(site_no %in% i) %>% dplyr::select(DateTime3, Rain3hourly)
#   sn<- r3 %>% dplyr::filter(site_no %in% i) %>% dplyr::select(site_no) %>%distinct()
#   IETD_CVA<- data.frame(IETD::CVA(rain)[[2]])[which.min(abs(data.frame(IETD::CVA(rain)[[2]])$CV-1)),'IETD']
#   # we can also do this using an autcorrelation approach
#   IETD_AC<- data.frame(IETD::AutoA(rain)[[2]])[which.min(data.frame(IETD::AutoA(rain)[[2]])$ACF),'Lag_Time']
#   df<- data.frame(site_no = sn, IETD_CVA = IETD_CVA, IETD_AC = IETD_AC)
#   datalist[[i]]<- df
# }

rainless<- bind_rows(datalist)
rainless
# 
# # 6 hours for CVA is by far the majority closest to 1, so I will go with that
# Martha also suggested that most streams in PR reach peak flow between 1-6 hours after rain begins, so this is a logical step


# actually, the AC suggests that we need a higher threshold. 24 is the highest value present. Let's do that instead


# look at the general values of rainfall to find a threshold: 
hist(r3$Rain3hourly, breaks=1000, xlim=c(0,1))
# default volume threshold for a rainfall event is 0.5, seems reasonable 

# plot rainfall over time for a sample site to determine a rainless threshold
# test<- r3 %>%  dplyr::filter(site_no %in% unique(s$site_no[1]),
#                                 DateTime3 >=  force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"),
#                                 DateTime3 <=  force_tz(ymd_hms("2005-01-31 01:00:00"), tzone = "America/Puerto_Rico"))
# test2<- s3 %>%  dplyr::filter(site_no %in% unique(s$site_no[1]),
#                              DateTime3 >=  force_tz(ymd_hms("2005-01-01 01:00:00"), tzone = "America/Puerto_Rico"),
#                              DateTime3 <=  force_tz(ymd_hms("2005-01-31 01:00:00"), tzone = "America/Puerto_Rico"))
# test3<- merge(test, test2, by=c('site_no','DateTime3'))
# p1<- ggplot(test3, aes(x=DateTime3))+geom_line(aes(y= Rain3hourly))#+lims(y=c(0,1))
# p2<- ggplot(test3, aes(x=DateTime3))+geom_line(aes(y= Ftot3_cm3h))
# grid.arrange(p1,p2,ncol=1)


# CALCULATE RAINFALL EVENTS
# could also try this link for a more hands on approach: 
# https://stackoverflow.com/questions/51371155/r-select-rainfall-events-and-calculate-rainfall-event-total-from-time-series-da


# Ok it doesn't like lapply for some reason, try a for loop instead 
datalist<- list()
for (i in unique(r3$site_no)){
  df<- r3 %>% dplyr::filter(site_no %in% i)
  df<- df %>% dplyr::select(DateTime3, Rain3hourly)
  df2<- drawre(df,6, Thres = 0.5)
  df3<- df2[[1]]
  df3$site_no = i
  datalist[[i]]<- df3
}

revents<- bind_rows(datalist)
head(revents)

# write.csv(revents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210622.csv'))

# I changed the thresholds to 0 and the number of rainless hours to be 9 - this should help define rainfall events a little better
# export for later 
# write.csv(revents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210705.csv'))

# I changed the thresholds back to 0.5 and instead required a higher amount of time for rain events to be independent: 24 hours 
# write.csv(revents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_24hrminrainless_210706.csv'))
# write.csv(revents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210707.csv'))


# great, now I have a df with each rainfall event, but I need a time series


# modify the above for loop to create a ts df with each 3hourly rainfall value and each event characteristic
datalist<- list()
for (i in unique(r3$site_no)){
  df<- r3 %>% dplyr::filter(site_no %in% i) %>% dplyr::select(DateTime3, Rain3hourly, site_no)
  elist<- drawre(df, 6)
  for( j in seq_along(elist[[2]])){
    elist[[2]][[j]]$Number.Event <- j
  }
  echars<- elist[[1]]
  ets<- bind_rows(elist[[2]])
  erdf<- merge(echars, ets, by='Number.Event')
  datalist[[i]]<- erdf
}

re_ts<- bind_rows(datalist)
head(re_ts)

# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210622.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210705.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_24hrminrainless_210706.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210707.csv'))


# find the number of events by month, year, and site_no

# nevents<- revents %>% mutate(yrmo = format(Starting, '%Y%m')) %>% group_by(site_no, yrmo) %>%
#   summarize(n_re = n_distinct(Number.Event))
# head(nevents)
# That works! But I still need to find the number of events in the last month before each rainfall event begins
# May need to run a for loop for that 



# # first, separate the df by site_no 
# re_ts_list<- revents %>% group_by(site_no) %>% group_split()
# testlist<- list(re_ts_list[[1]],re_ts_list[[2]])
# testdf<- re_ts_list[[1]]
# head(testdf)
# 
# # try to make the following code a function to lapply to all site_nos
# 
# find_raineventchars<- function(testdf){
#   datalist<- list()
#   for(i in seq(1,nrow(testdf))){
#     df<- testdf[i,]
#     monthbefore <- force_tz(ymd_hms(df$Starting), tzone = 'America/Puerto_Rico')-months(1)
#     weekbefore <- force_tz(ymd_hms(df$Starting), tzone = 'America/Puerto_Rico')-weeks(1)
#     days14before <- force_tz(ymd_hms(df$Starting), tzone = 'America/Puerto_Rico')-days(14)
#     
#     dfmonth<- testdf %>% dplyr::filter(Starting>= monthbefore, Starting < df$Starting) %>%
#       summarize(remonth_n = n_distinct(Number.Event), 
#                 remonth_meanDur = mean(Duration), remonth_sumDur = sum(Duration),
#                 remonth_meanVol = mean(Volume), remonth_sumVol = sum(Volume),
#                 remonth_meanInt = mean(Intensity)
#       )
#     dfweek<- testdf %>% dplyr::filter(Starting>= weekbefore, Starting < df$Starting) %>%
#       summarize(reweek_n = n_distinct(Number.Event), 
#                 reweek_meanDur = mean(Duration), reweek_sumDur = sum(Duration),
#                 reweek_meanVol = mean(Volume), reweek_sumVol = sum(Volume),
#                 reweek_meanInt = mean(Intensity)
#       )
#     df14day<- testdf %>% dplyr::filter(Starting>= days14before, Starting < df$Starting) %>%
#       summarize(re14d_n = n_distinct(Number.Event), 
#                 re14d_meanDur = mean(Duration), re14d_sumDur = sum(Duration),
#                 re14d_meanVol = mean(Volume), re14d_sumVol = sum(Volume),
#                 re14d_meanInt = mean(Intensity)
#       )
#     
#     # Now bind all of the values together and create a df 
#     df2<- cbind(dfmonth,dfweek,df14day)
#     df2$site_no <- df$site_no
#     df2$Number.Event <- df$Number.Event
#     datalist[[i]] <- df2
#   }
#   
#   t<- bind_rows(datalist)
#   return(t)
# }
# 
# 
# # now try running it on a few sites 
# eventchars<- lapply(re_ts_list, function(x) find_raineventchars(x))
# eventchars<- bind_rows(eventchars)
# head(eventchars)
# 
# 
# eventchars2<- eventchars %>% distinct()
# head(eventchars2)
# export this for later
# write.csv(eventchars2, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210622.csv'))
# write.csv(eventchars2, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210705.csv'))
# write.csv(eventchars2, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_24hrminrainless_210706.csv'))
# write.csv(eventchars2, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210707.csv'))




# Determine dry events and characteristics using the IETD package ----
# read in previous data 

# revents<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210622.csv')) %>% dplyr::select(-X) %>% dplyr::mutate(Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
# rts<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210622.csv'))%>% dplyr::select(-X) %>% dplyr::mutate(DateTime3 = force_tz(ymd_hms(DateTime3),tzone = "America/Puerto_Rico"), Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
# reventchars<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210622.csv'))%>% dplyr::select(-X)%>% dplyr::mutate(site_no = as.factor(as.character(site_no)))

# revents<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_24hrminrainless_210706.csv')) %>% dplyr::select(-X) %>% dplyr::mutate(Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
# rts<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_24hrminrainless_210706.csv'))%>% dplyr::select(-X) %>% dplyr::mutate(DateTime3 = force_tz(ymd_hms(DateTime3),tzone = "America/Puerto_Rico"), Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
# reventchars<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_24hrminrainless_210706.csv'))%>% dplyr::select(-X)%>% dplyr::mutate(site_no = as.factor(as.character(site_no)))

revents<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_Events_6hrminrainless_210707.csv')) %>% dplyr::select(-X) %>% dplyr::mutate(Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
rts<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventsTimeSeries_6hrminrainless_210707.csv'))%>% dplyr::select(-X) %>% dplyr::mutate(DateTime3 = force_tz(ymd_hms(DateTime3),tzone = "America/Puerto_Rico"), Starting = force_tz(ymd_hms(Starting),tzone = "America/Puerto_Rico"),  End = force_tz(ymd_hms(End),tzone = "America/Puerto_Rico"), site_no = as.factor(as.character(site_no)))
reventchars<- read.csv(paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_EventChars_6hrminrainless_210707.csv'))%>% dplyr::select(-X)%>% dplyr::mutate(site_no = as.factor(as.character(site_no)))


# Now we want to find the time since each rainfall event so we can identify dry periods 

# Now merge with rts with r3 
rall<- merge(r3,rts, by=c('site_no','DateTime3','Rain3hourly'), all.x = T)
head(rall)

# create a column that is the opposite of rain events 
rall <- rall %>% mutate(Dry.Event01 = ifelse(is.na(Number.Event), 1, 0))
head(rall)
rall<- rall %>% arrange(site_no, DateTime3) 
# ggplot(rall, aes(x=DateTime3, y=Rain3hourly, color=factor(Number.Event)))+geom_line()+facet_wrap(~site_no, scales='free_y')+theme(legend.position='none')
# ggplot(rall, aes(x=DateTime3, y=Dry.Event01, color=factor(Dry.Event01)))+geom_point()+facet_wrap(~site_no, scales='free_y')

# modify the above for loop to create a ts df with each 3hourly rainfall value and each event characteristic
datalist<- list()
for (i in unique(rall$site_no)){
  df<- rall %>% dplyr::filter(site_no %in% i) %>% dplyr::select(DateTime3, Dry.Event01, site_no)
  elist<- drawre(df, IETD = 6, Thres=0)
  for( j in seq_along(elist[[2]])){
    elist[[2]][[j]]$Number.Event <- j
  }
  echars<- elist[[1]]
  ets<- bind_rows(elist[[2]])
  erdf<- merge(echars, ets, by='Number.Event')
  datalist[[i]]<- erdf
}



re_ts<- bind_rows(datalist)
head(re_ts)




# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventsTimeSeries_6hrminrainless_210622.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventsTimeSeries_6hrminrainless_210705.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventsTimeSeries_24hrminrainless_210706.csv'))
# write.csv(re_ts, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventsTimeSeries_6hrminrainless_210707.csv'))


devents<- re_ts %>% dplyr::select(-c(DateTime3, Dry.Event01)) %>% distinct()
head(devents)
# write.csv(devents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEvents_6hrminrainless_210622.csv'))
# write.csv(devents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEvents_6hrminrainless_210705.csv'))
# write.csv(devents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEvents_24hrminrainless_210706.csv'))
# write.csv(devents, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEvents_6hrminrainless_210707.csv'))


# Determine dry event characteristics
# re_ts_list<- devents %>% group_by(site_no) %>% group_split()
# 
# 
# # Determine the dry event characteristics
# eventchars<- lapply(re_ts_list, function(x) find_raineventchars(x))
# eventchars<- bind_rows(eventchars)
# head(eventchars)
# 
# # export this for later
# # write.csv(eventchars, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventChars_6hrminrainless_210622.csv'))
# # write.csv(eventchars, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventChars_6hrminrainless_210705.csv'))
# # write.csv(eventchars, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventChars_24hrminrainless_210706.csv'))
# write.csv(eventchars, paste0(fileloc, 'Data/Rainfall/MSWEP_3hourly_20052016_DryEventChars_6hrminrainless_210707.csv'))
# 
# 
# # change the names of the df and bind to rall df 
# 
# # format the two events-based dfs to be able to include them into one 
