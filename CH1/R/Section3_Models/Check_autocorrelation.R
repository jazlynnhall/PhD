# find the time difference between the event starting dates

# first determine the general days of the year for each site and response

# wetr<- wetr %>% mutate(month = format(Date, '%m'))
# dryr<- dryr %>% mutate(month = format(Date, '%m'))
# ggplot(wetr, aes(x=Date, y=site_no, color=factor(month)))+geom_point()
# ggplot(dryr, aes(x=Date, y=site_no, color=factor(month)))+geom_point()



# check autocorrelation of flow dfs 

# change these variables to quickly make plots without too much effort and coding

# which streamflow response are we looking at? 
mmresponse<- 'Low' # Annual, Peak, or Low
mmcondition <- 'Dry' # Wet or Dry

if(mmresponse %in% 'Annual'){acfdf<- dfaf3;acfdf$response = log(acfdf$Flow_mmyr_norm); mmtitle = 'Annual Flow';mmsubtitle=''; xlab = 'Lag (Years)'; nlag = 13}
if(mmresponse %in% 'Peak' & mmcondition %in% 'Wet'){acfdf<- wetr;acfdf$response = acfdf$logmax; mmtitle = 'Peak Flow';mmsubtitle='Wet Conditions'; xlab = 'Lag (Events)'; nlag = 25}
if(mmresponse %in% 'Peak' & mmcondition %in% 'Dry'){acfdf<- dryr;acfdf$response = acfdf$logmax; mmtitle = 'Peak Flow';mmsubtitle='Dry Conditions'; xlab = 'Lag (Events)'; nlag = 25}
if(mmresponse %in% 'Low' & mmcondition %in% 'Wet'){acfdf<- wetd;acfdf$response = acfdf$logmin; mmtitle = 'Low Flow';mmsubtitle='Wet Conditions'; xlab = 'Lag (Events)'; nlag = 25}
if(mmresponse %in% 'Low' & mmcondition %in% 'Dry'){acfdf<- dryd;acfdf$response = acfdf$logmin; mmtitle = 'Low Flow';mmsubtitle='Dry Conditions'; xlab = 'Lag (Events)'; nlag = 25}



datalist<- list() # create an empty list to store the data 

for(i in unique(acfdf$site_no)){
  x<-acfdf %>% dplyr::filter(site_no %in% i) 
  b <- pacf(x$response, lag.max=nlag, plot=FALSE)
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
p_pacf<- ggplot(data = ac1, mapping = aes(x = lag, y = acf)) +
  labs(y="Partial Autocorrelation", x=xlab, title= mmtitle, subtitle = mmsubtitle) +
  geom_line(size=1) + theme_bw() +
  geom_segment(mapping = aes(xend = lag, yend = 0)) +
  geom_hline(aes(yintercept = 0)) +
  geom_hline(data = limshigh, aes(yintercept=conf.limhigh), lty=2, col='blue', lwd=1) +
  geom_hline(data = limslow, aes(yintercept=conf.limlow), lty=2, col='blue', lwd=1) +
  xlim(0,nlag)+
  facet_wrap(~site_no)



if(mmresponse %in% 'Annual')
  jpeg(paste0(fileloc,'Figures/SI/pACF_Annual.jpg'),
      width = 5, height = 5, units = 'in', res = 600)
  p_pacf
  dev.off()
if(mmresponse %in% 'Peak' & mmcondition %in% 'Wet')
  jpeg(paste0(fileloc,'Figures/SI/pACF_PeakWet.jpg'),
       width = 5, height = 5, units = 'in', res = 600)
  p_pacf
  dev.off()
if(mmresponse %in% 'Peak' & mmcondition %in% 'Dry')
  jpeg(paste0(fileloc,'Figures/SI/pACF_PeakDry.jpg'),
       width = 5, height = 5, units = 'in', res = 600)
  p_pacf
  dev.off()

if(mmresponse %in% 'Low' & mmcondition %in% 'Wet')
  jpeg(paste0(fileloc,'Figures/SI/pACF_LowWet.jpg'),
       width = 5, height = 5, units = 'in', res = 600)
  p_pacf
  dev.off()

if(mmresponse %in% 'Low' & mmcondition %in% 'Dry')
  jpeg(paste0(fileloc,'Figures/SI/pACF_LowDry.jpg'),
       width = 5, height = 5, units = 'in', res = 600)
  p_pacf
  dev.off()



