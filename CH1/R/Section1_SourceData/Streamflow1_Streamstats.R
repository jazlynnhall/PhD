# Find watersheds with available data ----

# Find available data for sites in PR
DataAvailable <- whatNWISdata(stateCd = "PR", service="uv", parameterCd = "00060")
# filter out only stream sites - no canals, ditches, etc 
DataAvailable<- DataAvailable %>% 
  dplyr::filter(site_tp_cd %in% "ST", begin_date <= as.Date("2011-01-01"), end_date >= as.Date("2016-01-01"))
# site type codes are found at https://maps.waterdata.usgs.gov/mapper/help/sitetype.html
sites<- unique(DataAvailable$site_no) 


# StreamStats, delineate watersheds and extract watershed characteristics ----


# create a function to loop through all unique site values 
# this function won;t run all at once, I had to repeat multiple times, instead of starting at 1, i started from the point where it failed
datalist<- list()
for (i in seq(1,nrow(DataAvailable))){
ws1 <- delineateWatershed(xlocation = DataAvailable$dec_long_va[i], ylocation = DataAvailable$dec_lat_va[i], crs = 4269, # NAD83 
                          rcode='PR', includeparameters = "true", includeflowtypes = "true")
# convert ws1 to a spatial polygon dataframe 
ws1_sp<- toSp(ws1)
ws1_sp$site_no = DataAvailable$site_no[i]
writeOGR(ws1_sp, wshed_shp_dir, layer = paste0('w',i), driver = "ESRI Shapefile", overwrite_layer = T)
#datalist[[i]]<- ws1_sp
}


# read in all watersheds in the file folder 
wslist <- list.files(path = paste0(wshed_shp_dir,'/'), 
                       pattern=".shp", 
                       #pattern=".hdr", 
                       all.files=TRUE, full.names=TRUE)

wslist<- lapply(wslist, readOGR)

ws<- do.call(rbind, wslist)
# plot(ws)



# export for later 
# writeOGR(ws, wshed_shp_dir2, layer = 'Watersheds_allsites_20052016.shp',  driver = "ESRI Shapefile", overwrite_layer = T)

# came back and redid this retrospectively - I wanted sites with at least 5 years of data and that had the 2015 drought, so period of record must begin by at least 2011 and cannot end before 2016
# ws<- st_read(wshed_shp_dir2, layer = 'Watersheds_allsites_20052016.shp')
# ws<- ws %>% dplyr::filter(site_no %in% sites)
# sf::st_write(ws, paste0(wshed_shp_dir2, "/", "Watersheds_allsites_20052016_210616.shp"), delete_layer = T)
