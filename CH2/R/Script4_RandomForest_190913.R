#Determining the predictors of dNPV Script - 
#This script: 
# 1.Sample 20k points from dNPV raster after processing in ArcGIS for road and stream masks
# 2. Extracts values for risk factors to 20k sample points
# 3. Creates a df with the dNPV values and risk factors
# 4. Runs RF variable importance analysis 
# 5. Exports figures for manuscript 

# load packages 
library(sp) #provides object oriented classes for spatial data in R 
library(raster) #provides classes and methods for raster datasets
library(ggplot2)# Graphics engine
#library("dplyr") # Better data manipulations
library(parallel) # mclapply for multicore processing

# Analysis packages.
library(randomForestSRC) # random forests for survival, regression and
# classification and visualization
library(ggRandomForests) # ggplot2 random forest figures (This!)


library(gridExtra) # allows plotting of multiple ggplots in one 

# 1.Sample 20k points from dNPV raster after processing in ArcGIS for road and stream masks

#read in raster dNPV 
# this file is the Sentinel-2 NPV values - pre(9/15-11/01, 2016) and post(9/21-11/01, 2017) Maria
#rastersource<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/GIS/NPV_S2_190731/AGB_processing/npvmask_wgs'
rastersource<- 'C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/NPV/npvmskwgs0902'

r<- raster(rastersource)
# determine CRS
r@crs # WGS84

#plot(r)




#plot(r.sampled)

# Read in risk factor rasters 
canopyheight<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/ch040_10m')
expos<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/expos20d10m')
windspeed<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/suswin_kmh')
#curvature<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/curve10m2')
slope<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/slope10m')
# if using different resolution landscape values 
#curvature<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/curve1000m')
#curvature<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/curve100m')
#slope<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/slope1000')
# if using general curvature rather than profile: 
curvature<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/curveg100m')
antecrain<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/rainsum619cm')
mariarain<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/rainsum2021cm')
waterstorage<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/aws0150_10m')
geology<- raster('C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/GIS/RiskFactors/geoprp83')

#r.poly<- rasterToPolygons(r)


#convert to spatial points dataframe 
r.spgrd<- as(r,'SpatialPointsDataFrame')
str(r.spgrd)


set.seed(1)
#create sample points randomly distributed within the raster 
selectedPoints = sample(1:length(r.spgrd[[1]]), 20000)
#Create a spatial points dataframe out of the sample points 
r.sampled = r.spgrd[selectedPoints,]

# extract risk factor values for sample point locations 
#(have to do separately instead of stacking due to different raster extents)
rasValue1=extract(canopyheight, r.sampled)
rasValue2=extract(expos, r.sampled)
rasValue3=extract(windspeed, r.sampled)
rasValue4=extract(curvature, r.sampled)
rasValue5=extract(slope, r.sampled)
rasValue6=extract(antecrain, r.sampled)
rasValue7=extract(mariarain, r.sampled)
rasValue8=extract(waterstorage, r.sampled)
rasValue9=extract(geology, r.sampled)

#Combine risk factor values with point NPV values 
combinePointValue=cbind(r.sampled,rasValue1,rasValue2,rasValue3,
                        rasValue4,rasValue5,rasValue6,rasValue7,rasValue8,rasValue9)

str(combinePointValue)

# create a df of the npv and ch points from the files ----
v<- as.data.frame(combinePointValue@data)
names(v)[1]<- 'dNPV'
names(v)[2]<- 'CanopyHeight'
names(v)[3]<- 'Exposure'
names(v)[4]<- 'WindSpeed'
names(v)[5]<- 'Curvature'
names(v)[6]<- 'Slope'
names(v)[7]<- 'AntecRainfall'
names(v)[8]<- 'MariaRainfall'
names(v)[9]<- 'AvailableWaterStorage'
names(v)[10]<- 'Geology'

head(v)

# now remove missing values in risk factors 
v<- na.omit(v)
length(v$dNPV)
# determine ranges of predictors
range(v$CanopyHeight)
range(v$Exposure)
range(v$WindSpeed)
range(v$Curvature)
range(v$Slope)
range(v$AntecRainfall)
range(v$MariaRainfall)
range(v$AvailableWaterStorage)
range(v$Geology)

# geology is the numeric value based on factor data, change this
# value of three corresponds to water bodies, get rid of these. 
v<- v[v$Geology!=3,]
# create a new variables
v$Geology2<- NA
v$Geology2[v$Geology==1]<- 'S' # sedimendary 
v$Geology2[v$Geology==2]<- 'Q' # quaternary alluvium deposits
v$Geology2[v$Geology==4 | v$Geology==5]<- 'V' # volcanic
v$Geology2[v$Geology==6]<- 'G' # granitic
v$Geology2[v$Geology==7]<- 'U' # ultramafic
v$Geology<- v$Geology2
# remove original geology value
v<- v[,1:10]

# how many data points?
length(v$dNPV) # 18914


# change units on rainfall variables - cm to mm 
v$MariaRainfall<- v$MariaRainfall*10
v$AntecRainfall<- v$AntecRainfall*10

vex<- cbind(as.data.frame(combinePointValue@data), as.data.frame(combinePointValue@coords))

names(vex)[1]<- 'dNPV'
names(vex)[2]<- 'CanopyHeight'
names(vex)[3]<- 'Exposure'
names(vex)[4]<- 'WindSpeed'
names(vex)[5]<- 'Curvature'
names(vex)[6]<- 'Slope'
names(vex)[7]<- 'AntecRainfall'
names(vex)[8]<- 'MariaRainfall'
names(vex)[9]<- 'AvailableWaterStorage'
names(vex)[10]<- 'Geology'

head(vex)

# now remove missing values in risk factors 
vex<- na.omit(vex)
length(vex$dNPV)

vex<- vex[vex$Geology!=3,]
# create a new variables
vex$Geology2<- NA
vex$Geology2[vex$Geology==1]<- 'S' # sedimendary 
vex$Geology2[vex$Geology==2]<- 'Q' # quaternary alluvexium deposits
vex$Geology2[vex$Geology==4 | vex$Geology==5]<- 'V' # volcanic
vex$Geology2[vex$Geology==6]<- 'G' # granitic
vex$Geology2[vex$Geology==7]<- 'U' # ultramafic
vex$Geology<- vex$Geology2
# remove original geology value
vex<- vex[,1:12]

# how many data points?
length(vex$dNPV) # 18914

vex$MariaRainfall<- vex$MariaRainfall*10
vex$AntecRainfall<- vex$AntecRainfall*10


# convert coordinates and data to sf objects 
vex2<- st_as_sf(vex, coords = c("x", "y"), 
                crs = crs(combinePointValue))
# export for Lora
# st_write(vex2,"C:/Users/Jazlynn/Documents/LoraDocs/NPV_randomsamples/NPV_PRrandsamples_190913.shp", driver = "ESRI Shapefile")


# find correlation between rainfall and wind speed  ----
library(corrplot)
cordf<- v
names(cordf)[1]<- 'NPV'
names(cordf)[2]<- 'Canopy Height'
names(cordf)[3]<- 'Exposure'
names(cordf)[4]<- 'Maximum Wind Speed'
names(cordf)[5]<- 'Curvature'
names(cordf)[6]<- 'Slope'
names(cordf)[7]<- 'Antecedent Rainfall'
names(cordf)[8]<- 'Maria Rainfall'
names(cordf)[9]<- 'Available Water Storage'

call <- cor(cordf[,c(1:9)])
# basic corplot 
corrplot(call, method="color", type='upper', tl.col='black', diag=FALSE, 
         addgrid.col = 'black', addCoef.col = 'black')
text(1,6,expression(paste0(Delta)))



# now create the figure for the correlation fig supplementary figure ####
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/RandomForest/CorrelationMatrix_190913.tiff",
#      width = 7, height = 7, units = 'in', res = 300)
# corrplot(call, method="color", type='upper', tl.col='black', diag=FALSE,
#          addgrid.col = 'black', addCoef.col = 'black')
# text(0.6,9,expression(Delta))
# dev.off()

# optional, set all negative NPV values to zero
#v$dNPV[v$dNPV<0]<- 0
# test -- only take values with damage
#v<- v[v$dNPV>=0,]

# add in loras rugosity metric 200214
# downloaded the r workspace and am now checking it 
library(dplyr)
v<- dat %>% dplyr::select(dNPV, canopy_height, Exposur,WindSpd,Curvatr,Slope,AntcRnf,MrRnfll,AvlblWS,Geology, rugosity)%>%
  rename(CanopyHeight = canopy_height, Exposure= Exposur,
         WindSpeed = WindSpd,Curvature=Curvatr,AntecRainfall=AntcRnf,
         MariaRainfall = MrRnfll, = AvlblWS, Rugosity = rugosity)


# save df as it is now for later interaction analysis
d3<- v

# for the labels ---
st.labs2 <- c('CanopyHeight' = "Canopy Height (m)", "Exposure" = "Exposure", "WindSpeed" = expression('Max Wind Speed (km hr'^-1*')'), 
              "Curvature" = "Curvature (cm)", "Slope" = "Slope (degrees)", "AntecRainfall" = "Antecedent Rainfall (mm)", 
              "MariaRainfall" = "Rainfall (mm)", "AvailableWaterStorage" = "Available Water Storage (mm)", "Geology"="Geology", "Rugosity" = "Rugosity")
st.labs3 <- c('CanopyHeight' = "", "Exposure" = "", "WindSpeed" = '', 
              "Curvature" = "", "Slope" = "", "AntecRainfall" = "", 
              "MariaRainfall" = "", "AvailableWaterStorage" = "", "Geology"="")


# INCLUDE ONLY IF I KEEP BINARY VARIABLES
# Set modes correctly. For binary variables: transform to logical
v$Exposure <- as.logical(v$Exposure)
# v$Geology <- as.factor(v$Geology) # geology is no longer binary - included all geology types
v$Geology<- as.factor(as.character(v$Geology))

# v<- v %>% dplyr::filter(Rugosity >= 5)

# Run the random forest model: ----
rfm <- rfsrc(dNPV~., data=v)
# print the forest summary
rfm


# Test variable importance

# Plot the VIMP rankings of independent variables.
# first calculate the variable importance values 
vi_rf<- gg_vimp(rfm) # THIS LINE TAKES A WHILE TO RUN
#vi_rf2<- randomForestSRC::vimp(gg_vimp)

# Select important variables using minimal depth 
varsel_NPV <- var.select(rfm)

# Save the gg_minimal_depth object for later use.
gg_md <- gg_minimal_depth(varsel_NPV)

library(grid)
# visualize variable importance Fig 5 
viplot1<- plot(vi_rf, lbls=st.labs2)+ geom_blank()+ labs(title=' ')+
  theme(plot.margin=grid::unit(c(0.2,0.4,0.2,0.2),"cm"),
        #axis.title.x=element_blank(),
        axis.text.y=element_text(colour="black"), title =element_text(colour="black"))+ 
  theme(panel.grid.major = element_blank(),panel.grid.minor = element_blank(),
        panel.border = element_rect(colour = "black", fill=NA), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        panel.grid = element_blank(), 
        axis.text.x = element_text(colour = 'white'),axis.title.x = element_text(colour = 'white'),
        axis.ticks.x = element_blank())
viplot<- plot(vi_rf, lbls=st.labs2)+geom_col(aes(y=vimp*1000, x=vars), fill='darkblue', color='black', width=0.6)+ 
  labs(y='VIMP', title='A')+#ylim(0, 16)  + 
  annotate("text", x=1, y=5, label="R^2 == 0.51",parse=T,color="black")+
  theme(plot.margin=grid::unit(c(0.2,0.2,0.2,0.1),"cm"), # t,r,b,l order
    #axis.title.x=element_blank(),
    axis.text.x=element_text(colour="black"), 
    axis.text.y=element_text(colour="black"), title =element_text(colour="black"))+ 
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),panel.background = element_blank(), axis.line = element_line(colour = "black"))
mdplot<- plot(gg_md, lbls=st.labs2) +
  geom_point(aes(x=names , y=depth), col='darkblue', stat = "identity")+
  theme(#plot.margin=grid::unit(c(0.2,0.2,0.2,0.),"cm"),
    #axis.title.x=element_blank(),
    axis.text.x=element_text(colour="black"),
    axis.text.y=element_text(colour="black"),
    title =element_text(colour="black"))+
  theme(panel.border = element_rect(colour = "black", fill=NA), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +labs(y='Mean Minimal Depth', title='B')
grid.arrange(viplot1,viplot, mdplot, nrow = 1)
grid.arrange(viplot, mdplot, nrow = 1)


# export variable importance figure figure for manuscript (should be figure 5 of main text ) ----
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/RandomForest/vimp_md_190913.tiff",
#      #width = 5, height = 3, units = 'in', res = 300)
#      width = 6.5, height = 3, units = 'in', res = 300)
# grid.arrange(viplot, mdplot, nrow = 1)
# #grid.arrange(viplot1,viplot, mdplot, nrow = 1)
# dev.off()



# now find partial dependence of the risk factors 

# We want the top ranked minimal depth variables only,
# plotted in minimal depth rank order.
xvar <- gg_md$topvars
xvar

# Calculate partial dependence: THIS TAKES A WHILE 
partial_NPV <- plot.variable(rfm,
                             xvar=gg_md$topvars,
                             partial=TRUE, sorted=FALSE,
                             show.plots = FALSE )

# generate a list of gg_partial objects, one per xvar.
gg_p <- gg_partial(partial_NPV)

# plot(gg_p)
# combine all PD plots into one 
pd1<-gg_p[["CanopyHeight"]]
pd2<-gg_p[["MariaRainfall"]]
pd3<-gg_p[["WindSpeed"]]
pd4<-gg_p[["AntecRainfall"]]
pd5<-gg_p[["AvailableWaterStorage"]]
pd6<-gg_p[["Geology"]]
pd7<-gg_p[["Rugosity"]]
pd8<-gg_p[["Slope"]]
pd9<-gg_p[["Exposure"]]
pd10<-gg_p[["Curvature"]]


pdplot1<- plot(pd1)+ #ylim(0.15,0.35)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('A')+
  labs(y=expression(paste(Delta ,"NPV")), x="Canopy Height (m)") +geom_smooth()+ theme_bw()
pdplot2<- plot(pd2)+ #ylim(0.15,0.35)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('B')+
  labs(y='', x="Rainfall (mm)") +geom_smooth()+ theme_bw()
pdplot3<- plot(pd3)+ #ylim(0.15,0.35)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('C')+
  labs(y='', x=expression('Wind Speed (km hr'^-1*')')  ) +
  geom_smooth()+ theme_bw()
pdplot4<- plot(pd4)+ #ylim(0.18,0.28)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('D')+
  labs(y=expression(paste(Delta ,"NPV")), x="Antec. Rainfall (mm)") +geom_smooth()+ theme_bw()
pdplot5<- plot(pd5)+ #ylim(0.18,0.28)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('E')+
  labs(y='', x="Water Storage (mm)") +geom_smooth()+ theme_bw()
pdplot6<- plot(pd6)+ #ylim(0.18,0.28)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('F')+
  labs(y='', x="Geology") +geom_smooth()+ theme_bw()+ 
  theme(axis.text.x=element_text(size=8))+
  scale_x_discrete(labels = c('Gran','Allu','Sedi','Ultr','Volc'))
pdplot7<- plot(pd7)+ #ylim(0.225,0.25)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('G')+
  labs(y='', x="Rugosity") +geom_smooth()+ theme_bw()
pdplot8<- plot(pd8)+ #ylim(0.225,0.25)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('G')+
  labs(y='', x="Slope (degrees)") +geom_smooth()+ theme_bw()
pdplot9<- plot(pd9)+ #ylim(0.225,0.25)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('H')+
  labs(y='', x="Exposure") +geom_smooth()+ theme_bw() + 
  scale_x_discrete(labels = c('Unexposed','Exposed'))
pdplot10<- plot(pd10)+ #ylim(0.225,0.25)+
  theme(panel.spacing = unit(1, "lines"))+ ggtitle('I')+
  labs(y=expression(paste(Delta ,"NPV")), x="Curvature (cm)") +geom_smooth()+ theme_bw()

grid.arrange(pdplot1, pdplot2, pdplot3, pdplot4, pdplot5, 
             pdplot6, pdplot7, pdplot8, pdplot9,pdplot10, ncol=3)

# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/RandomForest/FigS5_PDPlot_190915.tiff",
#      width = 6, height = 6, units = 'in', res = 300)
# grid.arrange(pdplot1, pdplot2, pdplot3, pdplot4, pdplot5,
#              pdplot6, pdplot7, pdplot8, pdplot9, nrow = 3)
# dev.off()




# now test for interactions 
# calculate the H statistic 

library(pre)
library(iml)
#library(gbm)

#Use the pre package to test interactions ---- 
# set binary values to factor 
head(d3)
d3$Exposure<- as.factor(as.character(d3$Exposure))
d3$Geology<- as.factor(as.character(d3$Geology))

# it's too computationally intense for the whole df, try a subsample
set.seed(10)
d4<- d3[sample(nrow(d3), 3000), ]
d4_pre <- pre(dNPV ~ ., data=d4[complete.cases(d4),])
#interact(d4_pre)

# use the iml package to test interactions ----
# Create a model object the package can recognize
mod = Predictor$new(d4_pre, data = d4[-which(names(d4) == "dNPV")]) 

# Measure the interaction strength
ia = Interaction$new(mod)
# ia_ch = Interaction$new(mod, feature = 'CanopyHeight')
# ia_mr = Interaction$new(mod, feature = 'MariaRainfall')
# ia_aws = Interaction$new(mod, feature = 'AvailableWaterStorage')
# ia_ar = Interaction$new(mod, feature = 'AntecRainfall')
# ia_sl = Interaction$new(mod, feature = 'Slope')


# Plot the resulting leaf nodes
plot(ia) 

# Extract the results 
# dat = ia$results
# head(dat)

# change the theme for the final plot
ia2<- plot(ia)
ia2<- ia2 + theme_bw() + xlab('Overall Interaction Strength')+ylab('')+
  scale_y_discrete(labels= st.labs2)
ia2
# tiff("C:/Users/Jazlynn/Dropbox/Backup/Chapter2/DataForMs/Figures/RandomForest/FigS4_Interactions_191017.tiff",
#      width = 4, height = 4, units = 'in', res = 300)
# ia2
# dev.off()

