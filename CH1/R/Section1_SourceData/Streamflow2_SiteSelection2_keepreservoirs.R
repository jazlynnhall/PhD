# Compare watershed spatial characteristics with the NHD and WBD data
# this script assumes I have already ran the file Streamflow1_Streamstats.R and the directores in the MasterScript.R file

# Read in the watershed shapefile ----
ws<- sf::st_read(paste0(wshed_shp_dir2, "/", "Watersheds_allsites_20052016_210616.shp"))
# and again in the PR UTM projection - sf prefers non decimal projections for spatial joins, #PR UTM 20N https://spatialreference.org/ref/epsg/3920/
ws2<- st_transform(st_read(paste0(wshed_shp_dir2, "/", "Watersheds_allsites_20052016_210616.shp")), crs=3920)

# Find available data for sites in PR
DataAvailable <- whatNWISdata(stateCd = "PR", service="uv", parameterCd = "00060")
# filter out only stream sites - no canals, ditches, etc 
DataAvailable<- DataAvailable %>% 
  dplyr::filter(site_tp_cd %in% "ST", begin_date <= as.Date("2011-01-01"), end_date >= as.Date("2016-01-01"))
# site type codes are found at https://maps.waterdata.usgs.gov/mapper/help/sitetype.html
sites<- unique(DataAvailable$site_no) 



# USGS near surface geology ---- 
# we only want to keep majority volcanic watersheds - limestone and granite watersheds have very different watershed hydrology
geo<- sf::st_read(paste0(fileloc, 'GIS/Geology/geo_pr_p83.shp'))
geo<- st_transform(geo, st_crs(ws2))
# mapview(geo, zcol='GEOLOGY')+mapview(s)
m_geo<- mapview(geo, zcol='GEOLOGY')
# create a layer with only volcanic geology 
volcanic<- geo[which(str_detect(geo$GEOLOGY, 'volcanic')),] %>% dplyr::select(GEOLOGY)

# for each watershed, identify the % volcanic 
datalist<- list()
for(i in unique(ws2$site_no)){
s<- ws2 %>% dplyr::filter(site_no %in% i)
svol<- st_intersection(s, st_crop(volcanic,s))%>% distinct()
s_area<- st_area(s)
v_area<- sum(st_area(svol))
s$prop_VOlcanic <- as.vector(v_area/s_area)
datalist[[i]]<- s
}

ws_volcanic<- bind_rows(datalist)


# which watersheds have at least 50% volcanic geologies?
maj_volcanic<- ws_volcanic %>% dplyr::filter(prop_VOlcanic >= 0.5)

# remove the non-majority volcanic watersheds from the original watersheds 
ws<- ws %>% dplyr::filter(site_no %in% unique(maj_volcanic$site_no))
ws2<- ws2 %>% dplyr::filter(site_no %in% unique(maj_volcanic$site_no))
length(unique(ws$site_no))
# 46 sites as of now


# NHD ----

# all Fcode point values from the NHD , https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/Complete_FCode_List.htm
# read in the geodatabase downloaded from the NHD 
fgdb <- paste0(fileloc, 'GIS/NHD/NHD_H_Puerto_Rico_State_GDB.gdb')

# List all feature classes in the geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature classes from the gdb
fcode<- sf::st_read(dsn = fgdb, layer = "NHDFCode") %>% dplyr::select(FCODE, DESCRIPTION) %>% dplyr::rename(FCode = FCODE)# all codes present in the NHD

# Read in the data and select relevant columns 
NHDFlowline<- st_transform(sf::st_read(dsn=fgdb,layer="NHDFlowline"), crs=3920)# %>% dplyr::select(Permanent_Identifier)
NHDWaterbody<- st_transform(sf::st_read(dsn = fgdb, layer = "NHDWaterbody"), crs=3920) #%>% dplyr::select(Permanent_Identifier)
NHDLine<- st_transform(sf::st_read(dsn=fgdb,layer="NHDLine"), crs=3920) #%>% dplyr::select(Permanent_Identifier)
NHDPoint<- st_transform(sf::st_read(dsn=fgdb,layer="NHDPoint"), crs=3920) #%>% dplyr::select(Permanent_Identifier)
# NHDArea<- st_transform(sf::st_read(dsn=fgdb,layer="NHDArea"), crs=3920) # not useful, no information
# events must be done by event type, not fcode
NHDLineEventFC <- st_transform(sf::st_read(dsn=fgdb,layer="NHDLineEventFC"), crs=3920) #%>% dplyr::select(Permanent_Identifier)
NHDPointEventFC <- st_transform(sf::st_read(dsn=fgdb,layer="NHDPointEventFC"), crs=3920) #%>% dplyr::select(Permanent_Identifier)


# spatial join all watersheds with the NHD data and convert to a dataframe
sjFL<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), st_zm(NHDFlowline), left=T))
sjWB<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), st_cast(NHDWaterbody, 'MULTIPOLYGON')))
sjL<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), st_zm(NHDLine), left=T))
sjP<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), NHDPoint, left=T))
sjLE<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), NHDLineEventFC, left=T))
sjPE<- st_drop_geometry(st_join(ws2%>% dplyr::select(site_no), NHDPointEventFC, left=T))



# merge with the existing sf and reproject to wgs 84
NHDFlowline<- merge(NHDFlowline, sjFL)
NHDWaterbody<- merge(NHDWaterbody, sjWB)
NHDLine<- merge(NHDLine, sjL)
NHDPoint<- merge(NHDPoint, sjP)
NHDLineEventFC<- merge(NHDLineEventFC, sjLE)
NHDPointEventFC<- merge(NHDPointEventFC, sjPE)


# for each fc, identify the relevant fcodes and remove those we are not interested in 
# Flow line 
FL_fcodes<- merge(fcode, sjFL %>% dplyr::select(FCode)%>%distinct(), by='FCode') # see for details: https://nhd.usgs.gov/userGuide/Robohelpfiles/NHD_User_Guide/Feature_Catalog/Hydrography_Dataset/NHDFlowline/NHDFlowline.htm
ignore_fcode<- c(55800, 46006, 46003)
FL_fcodes<- FL_fcodes %>% dplyr::filter(!FCode %in% ignore_fcode)#%>% dplyr::select(-FCODE)
NHDFlowline<- merge(NHDFlowline, FL_fcodes, by='FCode')

# Water body - I think the reservoirs are considered lakes in PR 
# change the format
NHDWaterbody<- st_cast(NHDWaterbody, 'MULTIPOLYGON')
NHDWaterbody<- st_zm(NHDWaterbody)

# line 
L_fcodes<- merge(fcode, sjL %>% dplyr::select(FCode)%>%distinct(), by='FCode') 
# only keep reservoir-related fcodes
ignore_fcode<- c(41100)
L_fcodes<- L_fcodes %>% dplyr::filter(!FCode %in% ignore_fcode)#%>% dplyr::select(-FCODE)
NHDLine<- merge(NHDLine, L_fcodes, by='FCode')

# point
# P_fcodes<- merge(fcode, sjP %>% dplyr::select(FCode)%>%distinct(), by='FCode') 
# No reason to include point from this stage. The only unique fcodes inside of watersheds are those for gaging stations

# line event
# sjLE %>% dplyr::select(EventType)%>%distinct()
# no relevant line event types inside the watersheds

# point event 
# needs to be done a little differently, no Fcodes for point events, 
newfcodes<- c(57100, # dam
               57201, # flow alteration - addition
               57202, # flow alteration - removal
               57203# flow alteration - unknown
               )

PE_fcodes<-  sjPE %>% dplyr::select(EventType)%>%distinct()%>% dplyr::filter(EventType %in% newfcodes)
# dams are the only code present in the watersheds
NHDPointEventFC<- merge(NHDPointEventFC, PE_fcodes, by='EventType')%>% dplyr::rename(FCode = EventType)

# Now add the new fcode for dams to the original fcode script 
fcode2<- rbind(fcode,data.frame(DESCRIPTION = 'Dam', FCode = 57100))




# Plot the features with the watersheds to determine which gaging stations to exclude 

# need to transform all files back to decimal degree for plotting 
# read in the first file and save the coordinate system 
refcrs<- sf::st_read(dsn=fgdb,layer="NHDFlowline")
NHDFlowline<- st_transform(NHDFlowline, st_crs(refcrs))
NHDWaterbody<- st_transform(NHDWaterbody, st_crs(refcrs))
NHDLine<- st_transform(NHDLine, st_crs(refcrs))
NHDLine<- st_zm(NHDLine)
NHDPointEventFC<- st_transform(NHDPointEventFC, st_crs(refcrs))

# do some extra processing before plotting 
# Add fcode to NHDWaterBody
NHDWaterbody<- merge(NHDWaterbody, fcode, by='FCode')
# add description to NHDPointEventFC - there is only one FCode
NHDPointEventFC$DESCRIPTION <- 'Dam'





# identify areas that likely alter flow ----
# pipelines 
pipelines<- NHDFlowline[which(str_detect(NHDFlowline$DESCRIPTION, 'Pipeline')),]

# Not sure how much canals and connectors affect flow, check at the end: 
# canals
canals<- NHDFlowline[which(str_detect(NHDFlowline$DESCRIPTION, 'Canal')),]
# connectors
connectors<- NHDFlowline[which(str_detect(NHDFlowline$DESCRIPTION, 'Connect')),]

# large lakes
lakes<- NHDWaterbody[which(str_detect(NHDWaterbody$DESCRIPTION, 'Lake')& NHDWaterbody$AreaSqKm > 0.1),]


# reservoirs
reservoirs<- NHDWaterbody[which(str_detect(NHDWaterbody$DESCRIPTION, 'Reservoir')),]
# dam/weir
dams_weirs<- NHDLine[which(str_detect(NHDLine$DESCRIPTION, 'Dam')),]
dams2<- NHDPointEventFC[which(str_detect(NHDPointEventFC$DESCRIPTION, 'Dam')),]
# identify the larger lakes (which are probably actually reservoirs)

# try with mapview
ws3<- st_transform(ws2, st_crs(refcrs))
m1<- mapview(ws3, zcol='site_no', legend=F, alpha.regions=0.2) 
m2<- mapview(NHDFlowline, zcol = "DESCRIPTION", legend = TRUE, color = rainbow(length(unique(NHDFlowline$DESCRIPTION))))
m3<- mapview(NHDWaterbody,zcol='DESCRIPTION', col.regions=rev(brewer.pal(length(unique(NHDWaterbody$DESCRIPTION)), "Blues")), alpha.regions=1, legend=T)
m4<- mapview(NHDLine,zcol='DESCRIPTION')
m5<- mapview(NHDPointEventFC, zcol='DESCRIPTION', col.regions= 'brown')

# m1
# m1+m2+m3
# m2+m3
# m1+m5
# m5+m4

# map out areas with likely flow modifications: 
m_pipelines<- mapview(pipelines, zcol='DESCRIPTION', color = mapviewPalette("mapviewTopoColors")(length(unique(pipelines$DESCRIPTION))))
m_lakes<- mapview(lakes,zcol='DESCRIPTION', col.regions=rev(brewer.pal(length(unique(lakes$DESCRIPTION)), "Blues")), alpha.regions=1, legend=T)
m_lakes2<- mapview(lakes, color='blue')
m_reservoirs<- mapview(reservoirs,zcol='DESCRIPTION', col.regions=brewer.pal(length(unique(reservoirs$DESCRIPTION)), "YlGnBu"), alpha.regions=1, legend=T)
m_dams_weirs<- mapview(dams_weirs, zcol='DESCRIPTION', color = 'red')
m_dams2<- mapview(dams2, zcol='DESCRIPTION', col.regions='brown')

m_canals<- mapview(canals, zcol='DESCRIPTION', color=c('green','gold'))
m_connectors<- mapview(connectors, zcol='DESCRIPTION', color='purple')

m_dams2+m_dams_weirs # more or less the same, but I will make sure to exclude each site that has either


# m3+m_lakes2


# Identify some key site selection variables ----

# sites containing pipelines ----
ws_pipe<- ws %>% dplyr::filter(site_no %in% unique(pipelines$site_no))
mapview(ws_pipe, zcol='site_no', legend=F, alpha.regions=0.2) + m_pipelines
# m1+m_pipelines




# sites with large lakes ----
# which sites have large lakes in their watersheds? 
ws_lakes<- ws %>% dplyr::filter(site_no %in% unique(lakes$site_no))
mapview(ws_lakes, zcol='site_no', legend=F, alpha.regions=0.2) + m_lakes2




# sites with reservoirs ----
ws_res<- ws %>% dplyr::filter(site_no %in% unique(reservoirs$site_no))
mapview(ws_res, zcol='site_no', legend=F, alpha.regions=0.2) + m_reservoirs
# there are so many that I can't simply exclude all watersheds containing reservoirs. 
# instead, read in the site_no locations and determine the watersheds with reservoirs within the first 1km of the watershed 

# DataAvailable and the NHD files have the same crs
# subset the dataavailable df to find the sites with reservoirs in them 
coords_res<- DataAvailable %>% dplyr::filter(site_no %in% unique(reservoirs$site_no))
xy <- coords_res[,c('dec_long_va','dec_lat_va')]
spdf <- SpatialPointsDataFrame(coords = xy, data = coords_res, proj4string = raster::crs(refcrs))
# I don't know why, but I had to convert the coords file to an spdf before I ould convert to an sf object
coords_res<- st_as_sf(spdf, xy)

# project site locations before running the buffer so the units will be in meters
# create a 5km buffer for each stream gage location, then run a spatial join to see which sites have reservoirs in that buffer
coords_2kbuff<- st_buffer(st_transform(coords_res, st_crs(ws2)), 2000)

# intersect the reservoir locations with the buffered area and with the watersheds
datalist<- list()
datalist2<- list()
for(i in unique(coords_2kbuff$site_no)){
  csite<- st_transform(coords_res, st_crs(ws2))%>% dplyr::filter(site_no %in% i)
  bsite<- coords_2kbuff%>% dplyr::filter(site_no %in% i)
  wsite<- ws2 %>% dplyr::filter(site_no %in% i)%>% dplyr::select(-site_no)
  rsite<- st_intersection(bsite, wsite)
  res_site<- st_intersection(rsite, st_transform(reservoirs%>% dplyr::select(-site_no), st_crs(ws2)))
  datalist[[i]]<- rsite
  datalist2[[i]]<- res_site
}

# for some reason 50047850 intersected with the area not inside of the watershed. Redo. 
csite<- st_transform(coords_res, st_crs(ws2))%>% dplyr::filter(site_no %in% 50047850)
bsite<- coords_2kbuff%>% dplyr::filter(site_no %in% 50047850)
wsite<- ws2 %>% dplyr::filter(site_no %in% 50047850)%>% dplyr::select(-site_no)
rsite<- st_difference(bsite, wsite)
res_site<- st_intersection(rsite, st_transform(reservoirs%>% dplyr::select(-site_no), st_crs(ws2)))
# mapview(list(wsite, bsite, rsite), col.regions = c('grey','blue','red'))

datalist[['50047850']]<- rsite
datalist2[['50047850']]<- res_site

# bind  all of the buffered watershed areas and relevant reservoirs together
ws_2kbuff<- st_cast(st_transform(bind_rows(datalist), st_crs(refcrs)), 'MULTIPOLYGON')
res_2kbuff<- st_transform(bind_rows(datalist2), st_crs(refcrs))

# which watersheds have reservoirs in the first 2km? 
ws_2kbuff<- ws_2kbuff %>% dplyr::filter(site_no %in% unique(res_2kbuff$site_no))
ws_res<- ws3 %>% dplyr::filter(site_no %in% unique(res_2kbuff$site_no))

# plot them
mapview(ws_res, zcol='site_no',legend=F, alpha.regions=0.2)+mapview(ws_2kbuff, zcol='site_no',col.regions='grey', legend=F)+mapview(st_cast(res_2kbuff, 'MULTIPOINT'), zcol='DESCRIPTION', col.regions=(rainbow(length(unique(res_2kbuff$DESCRIPTION)))), alpha.regions=1)



# which sites have reservoirs in them?
unique(res_2kbuff$site_no)
 # let's say for now that we will remove all sites with reservoirs in the first 2km of watershed area


# dams/weirs
ws_damweir<- ws3 %>% dplyr::filter(site_no %in% c(unique(dams_weirs$site_no), unique(dams2$site_no)))
mapview(ws_damweir, zcol='site_no', legend=F, alpha.regions=0.2) +m_dams2 + m_dams_weirs

# just dams
ws_dam<- ws3 %>% dplyr::filter(site_no %in% unique(dams2$site_no))
mapview(ws_dam, zcol='site_no', legend=F, alpha.regions=0.2) +m_dams2 



# Assess the sites we have selected so far ----

# remove all sites containing pipelines, lakes, reservoirs, and dams
removesites<- rbind(data.frame(site_no = pipelines$site_no), data.frame(site_no =lakes$site_no), #data.frame(site_no =res_2kbuff$site_no), 
                    data.frame(site_no = unique(dams2$site_no)))%>% distinct()
# remove sites containing pipelines, lakes, and dams
#removesites<- rbind(data.frame(site_no = pipelines$site_no), data.frame(site_no =lakes$site_no), data.frame(site_no = unique(dams2$site_no)))%>% distinct()
ws_selected<- ws3 %>% dplyr::filter(!site_no %in% unique(removesites$site_no))
ws_selected_utm<- st_transform(ws_selected, st_crs(ws2))
m6<- mapview(ws_selected, zcol='site_no', legend=F)
length(unique(ws_selected$site_no))
# 27 sites selected so far


# Now plot selected sites against the other potential flow modification 
m6+m_canals+m_connectors # canals and connectors aren't likely to affect flow based on their location and size in the selected watersheds


# Use HUMod codes from WBD HU12 to filter out karst modified watersheds ---- 

# The HUMOD codes are from the WBD HU12 file in the NHD download folder 
hu12<- sf::st_read(dsn=fgdb,layer="WBDHU12")
hu12_utm<- st_transform(hu12, st_crs(ws2))
# mapview(hu12, zcol='HUMod', col.regions = rainbow(length(unique(hu12$HUMod))))

# intersect the hu12 with the selected watersheds 
# first create an inner buffer for the watershed so we don't get overlapping hu12  
ws_innerbuff_utm<- st_buffer(ws_selected_utm, -500)
ws_innerbuff<- st_transform(ws_innerbuff_utm, st_crs(refcrs))
ws_hu12_utm<- st_join(hu12_utm, ws_innerbuff_utm) %>% dplyr::filter(!is.na(site_no))
ws_hu12<- st_cast(st_transform(ws_hu12_utm, st_crs(refcrs)), 'MULTIPOLYGON')

# Now remove all watersheds with karst 
ws_hu12_nok<- ws_hu12 %>% dplyr::filter(!HUMod %in% c('KA','KA,NC'))
karst<-  ws_hu12 %>% dplyr::filter(HUMod %in% c('KA','KA,NC'))

# update the selected watersheds excluding karst-modified watersheds 
ws_selected<- ws_selected %>% dplyr::filter(!site_no %in% unique(karst$site_no))
# update the modification layer to reflect this 
ws_hu12<- ws_hu12_nok  %>% dplyr::filter(site_no %in% unique(ws_selected$site_no))
m_humod<- mapview(ws_hu12_nok, zcol='HUMod', col.regions = rainbow(length(unique(ws_hu12$HUMod))), alpha.regions=0.4)


# take a look at the selected sites 
m7<- mapview(ws_selected, zcol='site_no', legend=F)
m_humod+mapview(geo, zcol='GEOLOGY')+m7

length(unique(ws_selected$site_no))
# 23 sites so far

# Visually inspect map and remove sites that have issues  ----

# One site doesn't look right 50043800 remove it as I suspect there may be an issue with delineation
# 50115900 and 50115240 are downstream from a dam, shown in HuMOD code but not on NHD, can see the dam location from the basemaps, remove

# isolate the problem sites and plot them
ws_problem<- ws_selected %>% dplyr::filter(site_no %in% c(50043800, 50115900, 50115240))
# After selecting only the sites with data form 2011-2016, sites 50115900, 50115240 were not included anyway
mapview(ws_problem, czol='site_no')
# remove these and replot the selected watersheds 
ws_selected<- ws_selected %>% dplyr::filter(!site_no %in% unique(ws_problem$site_no))

# ws_selected<- ws_selected %>% dplyr::filter(!site_no %in% c(50043800, 50115240))
m8<- mapview(ws_selected, zcol='site_no', legend=F)
m8
m8+m_humod+m_geo+m_dams2+m_pipelines+m_lakes+m_reservoirs
length(unique(ws_selected$site_no))
# 22 sites in total

# project the ws to wgs84 - this is the most common projection used by most of the files
ws_selected_wgs<- st_transform(ws_selected, st_crs(ws))

# export final selected sites 
# st_write(ws_selected_wgs, paste0(wshed_shp_dir2, "/", "Watersheds_selected_20052016_210616.shp"), delete_layer = T)
# st_write(ws_selected_wgs, paste0(wshed_shp_dir2, "/", "Watersheds_selected_20052016_210706.shp"), delete_layer = T)



