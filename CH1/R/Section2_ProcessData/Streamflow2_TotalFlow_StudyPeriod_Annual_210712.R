# This script tests the relationship between forest cover and: 
# 1) total flow for the study period 
# 2) average annual flow for the study period


# Total flow for the study period ----
srnona<- sr3 %>% drop_na() %>% group_by(site_no) %>% add_tally()
sptot<- srnona %>% group_by(site_no, n) %>% dplyr::summarise(Total_StudyPeriod = sum(Ftot3_cm3h))
# merge with watershed chars 
sptot<- merge(sptot, watershedchars, by='site_no')
# calculate the total amount per study period (flow in m per study area), have to convert DRNArea from miles2 to m2 first 
sptot$Total_StudyPeriod_m<- sptot$Total_StudyPeriod/(sptot$DRNAREA*2.59e+6)



#normalize flow in m and flow in cm by the number of 3hourly periods with data 
# total number of 3hourly periods in the study period: 
# days in the study period * 8 3hourly periods in each day
# as.numeric(as.Date('2016-12-31')-as.Date('2005-01-01'))*8

sptot$Total_StudyPeriod_norm<- sptot$Total_StudyPeriod*(35056/sptot$n)
sptot$Total_StudyPeriod_m_norm<- sptot$Total_StudyPeriod_m*(35056/sptot$n)
# now normaloze by annual rainfall as well 
sptot$Total_StudyPeriod_norm2<- sptot$Total_StudyPeriod_norm*(mean(sptot$AnnRain)/sptot$AnnRain)
sptot$Total_StudyPeriod_m_norm2<- sptot$Total_StudyPeriod_m_norm*(mean(sptot$AnnRain)/sptot$AnnRain)
# try just taking the total streamflow/total average rainfall 
# sptot$Total_StudyPeriod_norm2<- sptot$Total_StudyPeriod/sptot$AnnRain
# sptot$Total_StudyPeriod_m_norm2<- sptot$Total_StudyPeriod_m/sptot$AnnRain

# plot totals for the study period against drainage area
# ggplot(sptot, aes(x=DRNAREA*2.59e+6, y=Total_StudyPeriod_norm))+geom_point()+ggpubr::stat_cor()+
#   ggrepel::geom_text_repel(aes(label = site_no))
# plot totals for the study period against forest cover 
ptotfor<- ggplot(sptot, aes(x=ForestP, y=Total_StudyPeriod_m_norm*1000))+geom_point()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='% Forest Cover', y='Total Specific Discharge (mm)')
# plot totals against the average annual rainfall accoding to streamstats: 
# STREAMSTATS IS MISSING THE TOTALS FOR A FEW, WILL NEED TO FIND THEM SEPARATELY
# sites 50055380 , 50055750 , 50061800 , 50100450 
parainfor<- ggplot(sptot, aes(x=ForestP, y=AnnRain))+geom_point()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='% Forest Cover', y='Mean Annual Rainfall (mm)')
ptotnormfor<- ggplot(sptot, aes(x=ForestP, y=Total_StudyPeriod_m_norm*1000/(AnnRain)))+geom_point()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='% Forest Cover', y='Study Period Total Specific Discharge')

# grid.arrange(ptotfor,parainfor,ptotnormfor)






# annual flow ----


dfaf<- sr3 %>% drop_na() %>%mutate(Year = format(DateTime3, '%Y')) %>% group_by(site_no,Year) %>% add_tally() %>% ungroup() %>%
  group_by(site_no, Year, n) %>% dplyr::summarise(Flow_cmyr = sum(Ftot3_cm3h))
dfaf<- merge(dfaf, arain, by=c('site_no','Year'))
dfaf<- dfaf %>% dplyr::filter(Year >2004) 
# merge with watershed chars 
dfaf<- merge(dfaf, watershedchars, by='site_no')
dfaf<- dfaf %>% mutate(nyear = (as.numeric(as.Date(paste0(Year,'-12-31'))-as.Date(paste0(Year,'-01-01')))+1)*8,
                       DRNAREA_m2=DRNAREA*2.59e+6,
                       Flow_mmyr = Flow_cmyr/DRNAREA_m2*1000,
                       Flow_mmyr_norm= Flow_mmyr*(nyear/n)
                       )

# now normaloze by PRISM annual rainfall as well 
# dfaf$Flow_mmyr_norm2<- dfaf$Flow_mmyr_norm*(mean(dfaf$AnnRain)/dfaf$AnnRain)


# plot totals for the study period against drainage area
# ggplot(dfaf, aes(x=DRNAREA_m2, y=Flow_cmyr, group=site_no))+geom_point()+ggpubr::stat_cor()#+
# ggrepel::geom_text_repel(aes(label = site_no))


# plot totals for the study period against forest cover 
p1<- ggplot(dfaf, aes(x=ForestP, y=Flow_mmyr_norm, group=site_no))+geom_boxplot()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  # ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='', y='Annual Flow (mm)', subtitle='a')
p2<- ggplot(dfaf, aes(x=ForestP, y=Flow_mmyr_norm/(AnnRain), group=site_no))+geom_boxplot()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='', y='Annual Runoff Ratio PRISM', subtitle='b')
p3<- ggplot(dfaf, aes(x=ForestP, y=Flow_mmyr_norm/(AnnRainMSWEP), group=site_no))+geom_boxplot()+theme_bw()+
  ggpubr::stat_cor()+geom_smooth()+
  #ggrepel::geom_text_repel(aes(label = site_no))+
  labs(x='% Forest Cover', y='Annual Runoff Ratio MSWEP', subtitle='c')

# grid.arrange(p1,p2,p3)


# plot annual flow over time for each of the sites, test for autocorrelation
# ggplot(dfaf, aes(x=Year, y=Flow_mmyr_norm_norm, group=1))+theme_bw()+theme(axis.text.x = element_text(angle = 90))+
#   geom_line()+geom_point(aes(color=Year))+
#   facet_wrap(~site_no, scales = 'free_y')

# test autocorrelation ----
datalist<- list() # create an empty list to store the data 

for(i in unique(dfaf$site_no)){
  x<-dfaf %>% dplyr::filter(site_no %in% i) 
  b <- acf(x$Flow_mmyr_norm, lag.max=13, plot=FALSE)
  df <- with(b, data.frame(lag, acf))%>% 
    mutate(n.used = b$n.used)
  df$site_no <- i
  datalist[[i]]<- df}

# rbind list to create a df including all values for all sites 
ac1<- bind_rows(datalist)

#Creating confidence interval:
alpha1 <- 0.95
limshigh<- ac1 %>% group_by(site_no) %>% summarize(
  conf.limhigh = max(qnorm((1 + alpha1)/2)/sqrt(n.used)))
limslow<- ac1 %>% group_by(site_no) %>% summarize(
  conf.limlow = min(-1*qnorm((1 + alpha1)/2)/sqrt(n.used)))

# plot autocorrelation over time
# ggplot(data = ac1, mapping = aes(x = lag, y = acf)) +
#   labs(y="Autocorrelation", x="Lag (Years)", title= "ACF Total Annual flow") +
#   geom_line(size=1) + theme_bw() +
#   geom_segment(mapping = aes(xend = lag, yend = 0)) +
#   geom_hline(aes(yintercept = 0)) +
#   geom_hline(data = limshigh, aes(yintercept=conf.limhigh), lty=2, col='blue', lwd=1) +
#   geom_hline(data = limslow, aes(yintercept=conf.limlow), lty=2, col='blue', lwd=1) +
#   xlim(0,11)+
#   facet_wrap(~site_no)

# ggplot(ac1, aes(x=lag, y=acf))+geom_point()+geom_line()+facet_wrap(~site_no)

# test distribution of the response
# ggplot(dfaf)+
#   geom_histogram(bins = 20, aes(x = log(Flow_mmyr_norm))) +
#   labs(x='Totan Annual Runoff')
# 
# ggplot(dfaf, aes(log(Rain_year), log(Flow_mmyr_norm)))+geom_point()+geom_smooth()+ggpubr::stat_cor()
#   # ggrepel::geom_text_repel(aes(label = site_no))
# awesome, looks great

dfaf<- dfaf %>% mutate(logtot = log(Flow_mmyr_norm), logyrain = log(Rain_year))










# Plots to export: ----

## MSWEP ~ PRISM Mean Annual Rainfall ----

p1<- ggplot(watershedchars, aes(x=AnnRain, y=AnnRainNormals8095))+geom_point()+theme_bw()+#+lims(y=c(1500,4500), x=c(1500,4500))
  geom_smooth()+
  # theme(axis.title.x = element_text(size=8),
  #       axis.title.y = element_text(size=8),
  #       axis.text.x = element_text(size=6),
  #       axis.text.y = element_text(size=6))+
  labs(x='PRISM (mm year-1)',y='MSWEP (mm year-1)',title='a')#+lims(y=c(1400,4000))+
  # geom_abline()
p1
# jpeg(paste0(fileloc,'Figures/Potential/PRISM_MSWEP_MAR.jpg'),
#     width = 9, height = 3, units = 'in', res = 600)
# grid.arrange(p1,p2,p3,nrow=1)
# dev.off()




## Total Annual Rainfall acf ----
# 
# jpeg(paste0(fileloc,'Figures/Potential/AnnualFlow_acf.jpg'),
#     width = 7, height = 5, units = 'in', res = 600)
# ggplot(data = ac1, mapping = aes(x = lag, y = acf)) +
#   labs(y="Autocorrelation", x="Lag (Years)", title= "ACF Total Annual flow") +
#   geom_line(size=1) + theme_bw() +
#   geom_segment(mapping = aes(xend = lag, yend = 0)) +
#   geom_hline(aes(yintercept = 0)) +
#   # geom_hline(data = limshigh, aes(yintercept=conf.limhigh), lty=2, col='blue', lwd=1) +
#   # geom_hline(data = limslow, aes(yintercept=conf.limlow), lty=2, col='blue', lwd=1) +
#   xlim(0,11)+
#   facet_wrap(~site_no)
# dev.off()




