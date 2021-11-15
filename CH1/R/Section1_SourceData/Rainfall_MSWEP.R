# This script procsses the nc files from the MSWEP 3 hourly rainfall dataset for Puerto Rico 



# Not necessary but useful for later
# read in the data 
# nc_data <- nc_open('C:/Users/Jazlynn/Documents/rclone/3hourly/2013001.00.nc')

# Save the print(nc) dump to a text file
# {sink('C:/Users/Jazlynn/Documents/rclone/metadata/MSWEP_3hourly_metadata.txt')
#   print(nc_data)
#   sink()}
# take a look at the metadata after running the commented script above to get a feel for the variables 



# read the .nc file in using the raster package
fileloc_mswep<- 'D:/rclone/MSWEP/'
rastlist <- list.files(path = fileloc_mswep, pattern='.nc$', all.files=TRUE, full.names=TRUE)

# make sure there is a test raster for a crs to project every other layer into
testr<- raster(rastlist[which(str_detect(rastlist, '/2005'))][[1]])

# crop this file to Puerto Rico 
# read in PR extent - minimum bounding rect 
pr<- readOGR(paste0(fileloc, 'GIS/Outlines/pr_mbrd_wgs.shp'))
pr<- spTransform(pr, crs(testr))

# read in PR extent for plotting
pr2<- readOGR(paste0(fileloc, 'GIS/Outlines/pr_outline_wgs.shp'))
pr2<- spTransform(pr2, crs(testr))

# # Read in the watersheds for the chapter 
# # the shapefile for all watersheds
# watersheds<-  readOGR(paste0(wshed_shp_dir2, "/", "Watersheds_selected_20052016_210616.shp"))
# watersheds<- spTransform(watersheds, crs(testr))
# plot(pr2)
# plot(watersheds, add=T)


# create a list of potential years for the study period
years<- seq(2004,2016,1)
years<- as.list(years)

# Stack all rasters from the entire study period (originally in list format separated by year) then unlist
rastlist2<- unlist(lapply(years, function(x) rastlist[which(str_detect(rastlist, paste0('/',x)))]))
r<- stack(rastlist2)
r.crop <- crop(r,pr)

for(i in seq(1,length(names(r.crop)))){
  names(r.crop)[i] <- paste0('X',substr(rastlist2[[i]], 17, 26))
}


# Now only extract the site number from the watershed characteristics 
ws_trunc<- ws %>% dplyr::select(site_no)

# now extract the mean 3hourly rainfall from the raster for each watershed
rasValue1=raster::extract(r.crop, ws_trunc, fun='mean')

#Combine raster values for selected points in EYNF
combinePointValue=cbind(ws_trunc,rasValue1)


# create a df of the watersheds and watershed-level 3hourly rainfall from the files ----
v<- data.frame(st_drop_geometry(combinePointValue))
# clean up the data
v<- v %>% 
  pivot_longer(
    cols = starts_with("X"),
    names_to = "DateTime",
    names_prefix = "X",
    values_to = "Rain3hourly",
    values_drop_na = TRUE)

head(v)

# how many years do we have? 
unique(substr(v$DateTime, 1, 4))




# export the cropped rasters and the csv file 
fileloc_mswep2<- paste0(fileloc,'Data/Rainfall/')

# the datetime column doesn't export well, try to add a couple of other columns that may help us get around it 
v2<- v %>% dplyr::select(site_no, DateTime, Rain3hourly)%>%
  mutate(Year = substr(DateTime, 1, 4), Day = substr(DateTime, 5, 7), Hour = substr(DateTime, 9, 10))

# didn't actually export, no reason to, just keep the original files 
# writeRaster(r.crop, filename=paste0(fileloc_mswep2,'rasters/', names(r.crop)), bylayer=TRUE,format="GTiff")
# write.csv(v2, paste0(fileloc_mswep2, 'MSWEP_3hourly_20052016_210621.csv'))
# with relaxed requirements about reservoirs
# write.csv(v2, paste0(fileloc_mswep2, 'MSWEP_3hourly_20052016_210707.csv'))


