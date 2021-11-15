# This script aggregates various watershed-level characteristics for the selected watersheds 

# FOREST COVER 

# read in the land cover raster, must be projected into utm or stateplane, but r doesn't like working wth state plane in PR
# lc<- raster('D:/Dropbox/Backup/Chapter1/DataforMs/GIS/LandCover/CCAP2010/Job636223_2010_PuertoRico_CCAP.tif')
lc<- raster('D:/Dropbox/Backup/Chapter1/DataforMs/GIS/LandCover/CCAP2010_utm20/Job636249_2010_PuertoRico_CCAP.tif')

# project the watersheds to PR utm for accurate analysis of areas 
ws_utm<- st_transform(ws, crs=crs(lc))

# this link is useful for manipulating categorical raster data in r
# https://heima.hafro.is/~einarhj/spatialr/pre_rasters.html#rasters_with_categorical_(factor)_data
rat <- levels(lc)[[1]] # Extract the RAT
rat<- rat %>% mutate(Forest = ifelse(str_detect(CLASS_NAME, 'Forest'),'Forest',''),
                     Forest01 = ifelse(str_detect(CLASS_NAME, 'Forest'),1,0))%>% # add levels
  dplyr::select(ID, Forest01, CLASS_NAME,Forest)
#levels(lc) <- rat # NO NEED TO RUN THIS LINE 
# rat

# using the levels in rat, reclassify raster to forest and nonforest 
m2<- c(0, 8, 0, 
       9, 11, 1, 
       12, 12, 0, 
       13, 13, 1, 
       14, 15, 0, 
       16,16, 1,
       17, 25, 0)
recl <- matrix(m2, ncol=3, byrow=TRUE)

forest<-reclassify(lc, rcl=recl, right=NA)
forest<- ratify(forest)
names(forest)<- 'Forest_01'
# writeRaster(forest, 'D:/Dropbox/Backup/Chapter1/DataforMs/GIS/LandCover/CCAP2010_utm20/Forest_Nonforest.tif')
mapview(forest, zcol='Forest_01', col.regions=c('grey','darkgreen'))

# change the raster values to all = 1 then calculate the total number of pixels in each watershed
allpixels<- forest
allpixels[allpixels <10] <- 1
# plot(allpixels) # good, now all pixels =1

# for my own sanity, separate each watershed and calculate the sum of all forested pixels and all pixels to get % forest cover 
# want to make sure each site_no is maintained in the right order
datalist<- list()
for (i in unique(ws_utm$site_no)){
  sample_ws<- ws_utm %>% dplyr::filter(site_no %in% i)
  pixels_forest <- raster::extract(forest, as_Spatial(sample_ws), fun=sum, na.rm=T) 
  pixels_total <- raster::extract(allpixels, as_Spatial(sample_ws), fun=sum, na.rm=T) 
  sample_ws<- sample_ws %>% mutate(n_pixels_forested=as.vector(pixels_forest), 
                                   n_pixels_total= as.vector(pixels_total),
                                   Forest_prop = n_pixels_forested/n_pixels_total,
                                   Forest_percent = round(Forest_prop*100))
  datalist[[i]]<- sample_ws
}

forest_ws<- bind_rows(datalist)




# elevation 
getelev<- DataAvailable %>% dplyr::filter(site_no %in% unique(ws$site_no))%>% 
  mutate(Elevation = as.numeric(as.character(alt_va))/3.28084) %>% 
  dplyr::select(site_no, Elevation)# need to convert from ft to m 

# Instead of elevation, I will include the poroprtion of the watershed above 700m 
# This will help correct for the 

# NEED TO REDO THESE ATTRIBUTES COMPLETELY IN R - FOR NOW GRAB FROM ARCGIS----

# elevation above 700m 
e700<- raster("D:/Dropbox/Backup/Chapter1/GIS/Elevation/above700m")
e700<- projectRaster(e700, crs=crs(ws_utm))
# change the raster values to all = 1 then calculate the total number of pixels in each watershed
allpixels<- e700
allpixels[allpixels <10] <- 1
# plot(allpixels) # good, now all pixels =1

# for my own sanity, separate each watershed and calculate the sum of all forested pixels and all pixels to get % forest cover 
# want to make sure each site_no is maintained in the right order
datalist<- list()
for (i in unique(forest_ws$site_no)){
  sample_ws<- forest_ws %>% dplyr::filter(site_no %in% i)
  pixels_above <- raster::extract(e700, as_Spatial(sample_ws), fun=sum, na.rm=T) 
  pixels_total <- raster::extract(allpixels, as_Spatial(sample_ws), fun=sum, na.rm=T) 
  sample_ws<- sample_ws %>% mutate(n_pixels_above=as.vector(pixels_above), 
                                   n_pixels_total= as.vector(pixels_total),
                                   Above_prop = n_pixels_above/n_pixels_total,
                                   Above700_percent = round(Above_prop*100))
  datalist[[i]]<- sample_ws
}

above700_ws<- bind_rows(datalist)


# Now calculate collect slope 
elev<- raster("D:/Dropbox/Backup/Chapter1/GIS/Elevation/demnoaa_fill")
# calculate the slope 
slope<- raster::terrain(elev, opt="slope", unit="degrees", neighbors=8)
slope<- projectRaster(slope, crs = crs(ws_utm))

datalist<- list()
for (i in unique(above700_ws$site_no)){
  sample_ws<- above700_ws %>% dplyr::filter(site_no %in% i)
  mean_slope <- raster::extract(slope, as_Spatial(sample_ws), fun=mean, na.rm=T) 
  mean_elev <- raster::extract(elev, as_Spatial(sample_ws), fun=mean, na.rm=T) 
  sample_ws<- sample_ws %>% mutate(Slope = mean_slope, Elevation = mean_elev)
  datalist[[i]]<- sample_ws
}

ws_chars<- bind_rows(datalist)
head(ws_chars)

ws_chars2<- ws_chars %>% dplyr::select(site_no, SSURGOB,ROCKDEP, DRNAREA, Forest_percent, Above700_percent, Slope, PRECIP, Elevation) %>% 
  dplyr::rename(ForestP=Forest_percent, A700P=Above700_percent) # have to shorten names or they get shortened


# st_write(ws_chars2, paste0(wshed_shp_dir2, 'Watersheds_selected_chars_210624.shp'), delete_layer =T)


# ANNUAL RAINFALL 
# taken from the NOAA NWS https://water.weather.gov/precip/archive/
# in inches per year 

# shapefile already projected and clipped to PR using ArcGIS 
ar<- st_read(paste0(fileloc, 'GIS/AnnualRainfall/nws_normal_wgs_prmbr.shp'))

# this is a points file, interpolate between points using methods highlighted here: 
# http://jonkatz2.github.io/2017/11/15/interpolating-points-to-raster-in-r
# https://nceas.github.io/oss-lessons/spatial-data-gis-law/4-tues-spatial-analysis-in-r.html

# convert the annual values from inches to mm 
ar$AnnualRainfall <- ar$Globvalue*25.4
# project to the utm20 to match the previous rasters
ar<- st_transform(ar,crs=crs(slope))

# convert it to a spdf 
ar2<- as_Spatial(ar)
# create an empty grid of values ranging from the xmin-xmax, ymin-ymax
grd <- expand.grid(x = seq(from = min(ar2@coords[,1]),
                           to = max(ar2@coords[,1]), 
                           by = 3000),
                   y = seq(from = min(ar2@coords[,2]),to =max(ar2@coords[,2]), 
                           by = 3000))  # expand points to grid

# Convert grd object to a matrix and then turn into a spatial
# points object
coordinates(grd) <- ~x + y
# turn into a spatial pixels object
gridded(grd) <- TRUE
crs(grd)<- crs(ar2)



# interpolate the data
idw_pow6 <- idw(formula = AnnualRainfall ~ 1,
                locations = ar2,
                newdata = grd,
                idp = 6)
plot(idw_pow6,
     col = rev(topo.colors(55)))

annrain<- raster(idw_pow6)

# extract the watershed-level values 

datalist<- list()
for (i in unique(ws_chars2$site_no)){
  sample_ws<- ws_chars2 %>% dplyr::filter(site_no %in% i)
  mean_rain <- raster::extract(annrain, as_Spatial(sample_ws), fun=mean, na.rm=T) 
  sample_ws<- sample_ws %>% mutate(AnnualRainfall = mean_rain)
  datalist[[i]]<- sample_ws
}

ws_chars3<- bind_rows(datalist)
ws_chars3<- ws_chars3 %>% dplyr::rename(AnnRain = AnnualRainfall)


# st_write(ws_chars3, paste0(wshed_shp_dir2, 'Watersheds_selected_chars_210705.shp'), delete_layer =T)
# including a few sites with reservoirs upstream
# st_write(ws_chars3, paste0(wshed_shp_dir2, 'Watersheds_selected_chars_210706.shp'), delete_layer =T)
