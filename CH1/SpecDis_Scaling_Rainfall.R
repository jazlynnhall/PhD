

# Need to evaluate: 
# 1. whether total runoff is related to forest cover to motivate the analysis 
# 2. whether discharge scales linearly with watershed area

# calculate total runoff for each watershed and normalize by the number of 3hourly periods with data
# find the number of samples for each site
ndf<- data.frame(site_no = as.factor(names(table(s3$site_no[!is.na(s3$Ftot3_cm3h)]))), n_samples = as.numeric(unname(table(s3$site_no[!is.na(s3$Ftot3_cm3h)]))))
s3<- merge(s3, ndf, by='site_no')
# summarize the total and normalize by number of samples
s3total<- s3 %>% group_by(site_no, n_samples) %>% summarize(Ftot_cmStudyPeriod = sum(Ftot3_cm3h, na.rm=T)) %>%
  mutate(Ftot_cmStudyPeriod_norm = Ftot_cmStudyPeriod * (35063/n_samples))
# merge with drainage area and forest cover 
s3total<- merge(s3total, st_drop_geometry(forest_ws), by='site_no')
s3total<- s3total %>% mutate(ELEV_m = ELEV/3.28084, MINBELEV_m = MINBELEV/3.28084, PRECIP_mm = PRECIP*25.4)%>% 
  dplyr::select(site_no, n_samples, Ftot_cmStudyPeriod_norm, ELEV_m, MINBELEV_m, PRECIP_mm, Forest_percent, DRNAREA_m2)

# does this have something to do with elevation or rainfall
# streamstats does not have average rainfall for all stations, so instead use MSWEP: 
ravg<- r3 %>% mutate(Year = as.numeric(format(Date, '%Y')))%>% dplyr::filter(Year>=2005) %>% group_by(site_no, Year)%>% summarize(R_year = sum(Rain3hourly))
ravg<- ravg %>% group_by(site_no) %>% summarize(R_annmean = mean(R_year))

rsum<- r3 %>% mutate(Year = as.numeric(format(Date, '%Y')))%>% dplyr::filter(Year>=2005) %>% group_by(site_no)%>% summarize(R_total = sum(Rain3hourly))

s3total<- merge(s3total, rsum, by='site_no')
s3total<- s3total %>% 
  mutate(Elev_highlow = ifelse(ELEV_m< 500, 'Low Elev','High Elev'),
         Precip_quants = ifelse(PRECIP_mm< 1686, 'Rain 0-25%Q',ifelse(PRECIP_mm<2075, 'Rain 25-50%Q', ifelse(PRECIP_mm < 2911, 'Rain 50-75%Q', 'Rain 75-100%Q'))),
         Precip_highlow = ifelse(PRECIP_mm<2075, 'Rain 0-50%Q','Rain 50-100%Q'), 
         R_quants = ifelse(R_total< 22018, 'Rain 0-25%Q',ifelse(R_total<22874, 'Rain 25-50%Q', ifelse(R_total < 23678, 'Rain 50-75%Q', 'Rain 75-100%Q'))),
         R_highlow = ifelse(R_total<22874, 'Rain 0-50%Q','Rain 50-100%Q'))

# check the difference between MSWEP rainfall and the USGS average annual rainfall: 
ggplot(s3total, aes(x=R_annmean, y=PRECIP_mm))+geom_point()+lims(x=c(1500,4000), y=c(1500,4000))
# now normalize by elevation 
ggplot(s3total, aes(x=R_annmean*(max(s3total$ELEV_m)/ELEV_m), y=PRECIP_mm))+geom_point()#+lims(x=c(1500,4000), y=c(1500,4000))


# does discharge scale with area? 
p1<- ggplot(s3total, 
            aes(x=DRNAREA_m2, y=Ftot_cmStudyPeriod_norm)) + geom_point()+geom_smooth()+ggpubr::stat_cor()+
  labs(x='Drainage Area (m^2)', y='Total runoff 2005-2016', title = 'Does Discharge scale with area? NO')
# does discharge scale with area when we divide by average annual rainfall according to Streamstats
p2<- ggplot(s3total, 
            aes(x=DRNAREA_m2, y=(Ftot_cmStudyPeriod_norm)/PRECIP_mm)) + geom_point()+geom_smooth(method='lm')+ggpubr::stat_cor()+
  labs(x='Drainage Area (m^2)', y='Total runoff (m3) / Avg.Ann, Rainfall (mm)', title = 'Does Discharge scale with area when we divde by STREAMSTATS rainfall? YES')
# does discharge scale with area when we divide by average annual rainfall according to MSWEP
p3<- ggplot(s3total, 
            aes(x=DRNAREA_m2, y=(Ftot_cmStudyPeriod_norm*1000)/R_total)) + geom_point()+geom_smooth(method='lm')+ggpubr::stat_cor()+
  labs(x='Drainage Area (m^2)', y='Total runoff / Total Rainfall ', title = 'Does Discharge scale with area when we divde by MSWEP rainfall? NO')

grid.arrange(p1,p2,p3, ncol=1)
# what about when we separate by annual rainfall
ggplot(s3total, 
       aes(x=DRNAREA_m2, y=(Ftot_cmStudyPeriod_norm*100)/R_total)) + geom_point()+geom_smooth(method='lm')+ggpubr::stat_cor()+
  facet_wrap(~R_highlow, nrow=1)
ggplot(s3total, 
       aes(x=Forest_percent, y=Ftot_cmStudyPeriod_norm)) + geom_point()+geom_smooth(method='lm')+ggpubr::stat_cor()+
  facet_wrap(~R_highlow, nrow=1)


ggplot(s3total, 
       aes(x=Forest_percent, y=Ftot_cmStudyPeriod_norm, group=Precip_highlow, color= Precip_highlow)) + geom_point()+geom_smooth(method='lm')+ggpubr::stat_cor()

# is total runoff related to forest cover? 
ggplot(s3total, aes(x=Forest_percent, y=Ftot_cmStudyPeriod_norm/DRNAREA_m2)) + geom_point()+geom_smooth()+ggpubr::stat_cor()

# one site is acting very strangely, which one? 
s3total$site_no[s3total$Forest_percent<70&s3total$Ftot_cmStudyPeriod_norm/s3total$DRNAREA_m2>100]
length(unique(r3$DateTime3[r3$DateTime3 >= min(s3$DateTime3)]))
# 50111500 looks to be directly downstream from alarge farm - this farm may be irrigated or regularly watered, I suspect this is changing the relationships, remove.

s3total<- s3total %>% dplyr::filter(!site_no %in% '50111500')

# test out normalizing by sample number 
# maximum of 35063 samples in the study period 
