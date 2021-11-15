# This script aggregates the 15-minute data into 3hourly summary values

# original 15-minute flow data:
head(s)

# need to format s df to 3 hourly estimates

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

s_split<- s %>% group_by(site_no) %>% group_split()

# apply the function on the dataset
s_split<- lapply(s_split, process_sf_3hourlytimes)
sh3<- bind_rows(s_split)


# Find min and max flow values for each 3hourly window 
sh3_minmax<- sh3 %>% group_by(site_no, DateTime3) %>%
  summarize(Fmax3_cms = max(Flow_Inst), Fmin3_cms = min(Flow_Inst))

# Now calculate the total in a more simple way
# cmi = cubic meters per interval
sh3_tot<- sh3 %>% group_by(site_no) %>% arrange(site_no, DateTime) %>%
  mutate(interval = -1*as.numeric(DateTime - dplyr::lead(DateTime)), 
         #timeto3hour = -1*as.numeric(DateTime - dplyr::lead(DateTime)), 
         # this gives me the difference between the beginning of the 2hour period and the sample in minutes, units are originally in seconds for some reason
         minsince3hour = as.numeric(DateTime - DateTime3)/60,
         # find how many minutes until the next 3 hour period
         minto3hour = 360- minsince3hour,
         Ftot_tonext = ifelse(interval > minto3hour, Flow_Inst*minto3hour*60, Flow_Inst*interval*60),
         Ftot_tonext  = ifelse(is.na(interval), Flow_Inst*minto3hour*60,Ftot_tonext), # if its the last measurement and there is no post sample interval, scale flow to the minutes to the last 3hourly sample
         # What if there is missing data at the beginning of the 3 hourly interval
         interval_lag =  as.numeric(DateTime - dplyr::lag(DateTime)),
         Ftot_tolast  = ifelse(interval_lag > minsince3hour, Flow_Inst*minsince3hour*60,0), # if the interval between samples goes until after the 3hour period, only scale flow to the end of the 3 hour window)
         Ftot_tolast  = ifelse(is.na(interval_lag), Flow_Inst*minsince3hour*60, Ftot_tolast))%>% # if its the first measurement and there is no previous interval, scale flow to the minutes to the last 3hourly sample
  rowwise() %>%       
  mutate(Ftot_cmi = sum(Ftot_tolast, Ftot_tonext, na.rm=T)) 
head(data.frame(sh3_tot))

# Now group by 3hourly period
sh3_tot2<- sh3_tot %>% group_by(site_no, DateTime3) %>%
  summarize(Ftot3_cm3h = sum(Ftot_cmi))


head(sh3_minmax)
head(sh3_tot2)

sh3_2<- merge(sh3_minmax, sh3_tot2, by=c('site_no','DateTime3'))
head(sh3_2)


# # Adjust the streamflow for the sites that need adjusting: ----
# adjust_sites<- c(50053025, # about 0.45 ft^3/s (0.01274258 cms) is withdrawn
#                  50071000)# about 10 ft^3/s (0.283168 cms) is withdrawn
# flowadj<- data.frame(site_no = as.factor(adjust_sites), Adjust_cms = c(0.01274258, 0.283168), Adjust_cm3h = c(0.01274258*60*60*3, 0.283168*60*60*3))
# sh3_2<- merge(sh3_2, flowadj, by='site_no', all=T)
# sh3_2<- sh3_2 %>% mutate(Adjust_cms= ifelse(is.na(Adjust_cms), 0, Adjust_cms), Adjust_cm3h = ifelse(is.na(Adjust_cm3h), 0, Adjust_cm3h),
#                    Fmax3_cms= Fmax3_cms+Adjust_cms, Fmin3_cms = Fmin3_cms+Adjust_cms, Fmean3_cms= Fmean3_cms+Adjust_cms, Ftot3_cm3h=Ftot3_cm3h+Adjust_cm3h)
# sh3_2<- sh3_2 %>% dplyr::select(-c(Adjust_cms, Adjust_cm3h))


# export the csv
# adjusted
# write.csv(sh3_2, paste0(fileloc, 'Data/Streamflow/Streamflow_3hourlysummary_intervals_210625.csv'), row.names = F)
# unadjusted
# write.csv(sh3_2, paste0(fileloc, 'Data/Streamflow/Streamflow_3hourlysummary_intervals_210705.csv'), row.names = F)
# unadjusted and added more sites 
# write.csv(sh3_2, paste0(fileloc, 'Data/Streamflow/Streamflow_3hourlysummary_intervals_210707.csv'), row.names = F)


