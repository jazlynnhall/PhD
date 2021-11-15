# Thsis cript tests how well the MSWEP predicts rainfall in the Icocos basin by comparing it to Martha's15-minute rainfall data 

#Read in Martha's data
md1<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/Icacos/2011-2015_raw_rainfall_DCP_QCd_2011.csv')
md2<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/Icacos/2011-2015_raw_rainfall_DCP_QCd_2012.csv')
md3<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/Icacos/2011-2015_raw_rainfall_DCP_QCd_2013.csv')
md4<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/Icacos/2011-2015_raw_rainfall_DCP_QCd_2014.csv')
md5<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/Rainfall/Icacos/2011-2015_raw_rainfall_DCP_QCd_2015.csv')

md<- rbind(md1,md2,md3,md4,md5)
md<- md %>% dplyr::select(-X) %>% mutate(DateTime = force_tz(ymd_hms(Timestamp..UTC.04.00.), tz = 'America/Puerto_Rico')) %>%
  dplyr::select(DateTime, rainfall..mm)
head(md)


md<- md %>% drop_na()


# read in functions to process the data to 3hours 
# same processing methods as for streamflow 

# read in the function to add 3 hourly values to OG data
process_sf_3hourlytimes<- function(df){
  df2<- df  %>%
    mutate(
      # format date
      HourOG= format(DateTime, '%H'), Date = format(DateTime, '%Y-%m-%d'), prevDate = ymd(Date)-days(),
      Hour3 = ifelse(HourOG %in% c("23",'00','01'), "23",
                     ifelse(HourOG %in% c("02",'03','04'), "02",
                            ifelse(HourOG %in% c("05",'06','07'), "05",
                                   ifelse(HourOG %in% c("08",'09','10'), "08",
                                          ifelse(HourOG %in% c("11",'12','13'), "11",
                                                 ifelse(HourOG %in% c("14",'15','16'), "14",
                                                        ifelse(HourOG %in% c("17",'18','19'), "17",
                                                               "20"))))))),
      DateTime3 = force_tz(ymd_hms(ifelse(HourOG %in% c('00','01'), paste0(prevDate,' ',Hour3,':00:00'),paste0(Date,' ',Hour3,':00:00'))),'America/Puerto_Rico')
    )
  return(df2)
}


md<- process_sf_3hourlytimes(md)

# now rename rainfall metric 
md<- md %>% dplyr::rename(Raintip = rainfall..mm)



# Now calculate the total rainfall in each 3hourly period
# mmi = millimeters per interval
md_tot<- md  %>% arrange(DateTime)%>%
  mutate(interval = -1*as.numeric(DateTime - dplyr::lead(DateTime))/60, 
         #timeto3hour = -1*as.numeric(DateTime - dplyr::lead(DateTime)), 
         # this gives me the difference between the beginning of the 2hour period and the sample in minutes, units are originally in seconds for some reason
         minsince3hour = as.numeric(DateTime - DateTime3)/60,
         # find how many minutes until the next 3 hour period
         minto3hour = 360- minsince3hour,
         R_tonext = ifelse(interval > minto3hour, Raintip*minto3hour/15, Raintip),
         R_tonext  = ifelse(is.na(interval), Raintip*minto3hour/15,R_tonext), 
         # What if there is missing data at the beginning of the 3 hourly interval
         interval_lag =  as.numeric(DateTime - dplyr::lag(DateTime))/60,
         R_tolast  = ifelse(interval_lag > minsince3hour, Raintip*minsince3hour/15,0), # if the interval between samples goes until after the 3hour period, only scale flow to the end of the 3 hour window)
         R_tolast  = ifelse(is.na(interval_lag), Raintip*minsince3hour/15, R_tolast))%>% # if its the first measurement and there is no previous interval, scale flow to the minutes to the last 3hourly sample
  rowwise() %>%       
  mutate(R_mmi = sum(R_tolast, R_tonext, na.rm=T)) 
head(data.frame(md_tot[391:30000,]), 10)


# Now group by 3hourly period
md_tot2<- md_tot %>% group_by(DateTime3) %>% add_tally() 
md_tot2<- md_tot2 %>% group_by(DateTime3, n) %>%
  summarize(Raintip3hourly = sum(Raintip))
head(md_tot2)

# Now only keep the 3hourly intervals where I have at least 24 
md_tot2<- md_tot2 %>% dplyr::filter(n>=24)


# Now merge with the 3hourly rainfall for the icacos watershed
ir<- r3 %>% dplyr::filter(site_no %in% 50075000)
ir<- merge(md_tot2,ir, by='DateTime3')



# Not plot one rainfall against the other 
p1<- ggplot(ir, aes(x=Rain3hourly+1, y=Raintip3hourly+1))+geom_point()+
  #geom_bin2d()+ scale_fill_continuous(type = "viridis") +
  ggpubr::stat_cor()+
  geom_abline(slope = 1)+geom_smooth()+labs(x='MSWEP 3hourly + 1', y='USGS aggregated 3hourly + 1')+
  scale_y_continuous(trans = 'log', breaks=c(1,10,50,100), limits = c(1,150))+scale_x_continuous(trans = 'log', breaks=c(1,10,50,100), limits = c(1,150))
p2<- ggplot(ir, aes(x=Rain3hourly, y=Raintip3hourly))+geom_point()+
  #geom_bin2d()+ scale_fill_continuous(type = "viridis") +
  ggpubr::stat_cor()+
  geom_abline(slope = 1)+geom_smooth()+lims(x=c(0,150), y=c(0,150))+labs(x='MSWEP 3hourly', y='USGS aggregated 3hourly')

grid.arrange(p1,p2)

# It is really noisy, but honestly not terrible. Accounts for about 50% of the variance and hovers around the 1:1 line.

