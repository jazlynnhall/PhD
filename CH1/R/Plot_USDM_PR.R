# This script plots the USDM time series data for Puerto Rico from 2005-2017

library(ggplot2)

# set directory for the final figure locations 
finalloc<- 'D:/Dropbox/Backup/Chapter1/Manuscript/Hydrological Processes/Submission1/'


dm<- read.csv('D:/Dropbox/Backup/Chapter1/DataforMs/Data/USDM/USDM_PRpercentdrought_20052017.csv')
head(dm)

dm$Date<- as.Date(as.character(dm$MapDate), format = '%Y%m%d')

# create a month category to aggregate values in 
dm$mo<- data.table::month(as.POSIXlt(dm$Date))
dm$yr<-  data.table::year(as.POSIXlt(dm$Date))

dmd0<- aggregate(D0 ~ mo+yr, data=dm, FUN='max') 
dmd1<- aggregate(D1 ~ mo+yr, data=dm, FUN='max') 
dmd2<- aggregate(D2 ~ mo+yr, data=dm, FUN='max') 
dmd3<- aggregate(D3 ~ mo+yr, data=dm, FUN='max') 
dmd4<- aggregate(D4 ~ mo+yr, data=dm, FUN='max') 

dm2<- merge(dmd0, dmd1, by=c('yr','mo'))
dm2<- merge(dm2, dmd2, by=c('yr','mo'))
dm2<- merge(dm2, dmd3, by=c('yr','mo'))
dm2<- merge(dm2, dmd4, by=c('yr','mo'))
head(dm2)


dm2$Date <- as.Date(paste(as.numeric(dm2$mo), "01", dm2$yr, sep="-"), 
                   format = "%m-%d-%Y")
dm2$d0lab<- NA
dm2$d1lab<- NA
dm2$d2lab<- NA
dm2$d3lab<- NA

dm2$d0lab[dm2$Date==as.Date('2009-06-01')]<- 85
dm2$d1lab[dm2$Date==as.Date('2009-06-01')]<- 75
dm2$d2lab[dm2$Date==as.Date('2009-06-01')]<- 65
dm2$d3lab[dm2$Date==as.Date('2009-06-01')]<- 55


dry1<- ggplot(dm2)+
  geom_col(aes(x=Date,y=D0+D1+D2+D3+D4), col='yellow', fill='yellow')+
  geom_col(aes(x=Date,y=D1+D2+D3+D4), col='orange', fill='orange')+
  geom_col(aes(x=Date,y=D2+D3+D4), col='red', fill='red')+
  geom_col(aes(x=Date,y=D3+D4), col='sienna', fill='sienna')+
  geom_col(aes(x=Date,y=D4), col='black', fill='black')+
  geom_point(aes(x=Date,y=d0lab), col='yellow', size=2, shape=15)+
  geom_point(aes(x=Date,y=d1lab), col='orange', size=2, shape=15)+
  geom_point(aes(x=Date,y=d2lab), col='red', size=2, shape=15)+
  geom_point(aes(x=Date,y=d3lab), col='sienna', size=2, shape=15)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  ylim(0,100)+ylab('% Area in USDM Drought')+
  annotate(geom="text", x=as.Date('2011-06-01'), y=85, label="Abnormally Dry", size=2)+
  annotate(geom="text", x=as.Date('2011-09-01'), y=75, label="Moderate Drought", size=2)+
  annotate(geom="text", x=as.Date('2011-07-01'), y=65, label="Severe Drought", size=2)+
  annotate(geom="text", x=as.Date('2011-08-01'), y=55, label="Extreme Drought", size=2)+
  theme(axis.text=element_text(size=5),axis.title=element_text(size=5))
dry1


# # export 
# png('C:/Users/Jazlynn/Dropbox/Backup/Chapter1/Figs/USDM/USDM_PR_percentinDrought.png',
#     width = 3, height = 2, units = 'in', res = 300)
#   dry1
# dev.off()


# # export 
# tiff(paste0(finalloc,'Figures/Figure2_USDM.tif'),
#     width = 3, height = 2, units = 'in', res = 600)
#   dry1
# dev.off()



