# This script downloads the 15-minute streamflow data for the selected stations 


# read in the selected watershed locations
ws<-  st_read(paste0(wshed_shp_dir2, "/", "Watersheds_selected_20052016_210616.shp"))
sites<- unique(ws$site_no)



# Download data ----

# Download all selected sites with 15-minute data, have to process individually then bind together
siteslist<- as.list(sites)
uv_list<- lapply(siteslist, function(x) 
  readNWISuv(siteNumbers = as.character(x), parameterCd = "00060", 
             startDate = "2005-01-01", endDate = "2017-01-01",
             tz = "UTC"))
uv_list<- lapply(uv_list, function(x) renameNWISColumns(x))
uv<- bind_rows(uv_list)


# Process uv values 
uv<- uv %>% 
  mutate(site_no = as.factor(site_no),dateTime = as.POSIXct(dateTime, format="%Y-%m-%d %H:%M:%S"), Flow_Inst = Flow_Inst/35.3147, Flow_units = as.character('cms')) %>% # CONVERT FLOW FROM FT^3/S TO M^3/S - USGS automatically puts units in imperial system
  mutate(dateTime = with_tz(dateTime, tzone = "America/Puerto_Rico"), Date = as.Date(format(dateTime, '%Y-%m-%d'))) %>% # change the time zone from UTC to Puerto Rico time 
  mutate(tz_cd = 'America/Puerto_Rico')

# what are the codes for the measurement quality? 
table(uv$Flow_Inst_cd) 
# all values are accepted, 4 points with likely greater than measured values, but that it fine, 10 million total points


# check out the time series for each of the selected sites - do they all have reasonable measurement periods? 
ts <- ggplot(uv,aes(dateTime, Flow_Inst)) + geom_line()+ facet_wrap(~site_no, scales='free_y')
# ts # looks good! more or less continuous data for all sites maintained


# export data for later use 
# write.csv(uv, paste0(fileloc,'Data/Streamflow/Streamflow_SelectedSites_20052016.csv'), row.names=FALSE)
# write.csv(uv, paste0(fileloc,'Data/Streamflow/Streamflow_SelectedSites_20052016_210707.csv'), row.names=FALSE)

