# This script runs the FDCs for the streamflow data 


# Run FDC values
datalist<- list() # create an empty list to store the data 
for(i in unique(s$site_no)){
  df<- s %>% dplyr::filter(site_no %in% i) %>%  dplyr::arrange(desc(Flow_Inst))%>%
    mutate(probExceeded = 100*(1-cume_dist(Flow_Inst))) %>% distinct()
  Qs<- fdc_func(df$Flow_Inst, df$probExceeded)
  df2<- data.frame(probExceeded= seq(from = 1, to = 100, by=1), Flow_Inst = Qs)
  df2$site_no <- i
  df2<- df2 %>% dplyr::select(site_no, probExceeded, Flow_Inst)
  datalist[[i]]<- df2
}

# rbind list to create a df including all values for all sites 
qp<- bind_rows(datalist)

ggplot(qp, aes(probExceeded,Flow_Inst))+geom_line()+facet_wrap(~site_no, scales='free_y')
ggplot(qp, aes(probExceeded,Flow_Inst, group=site_no))+geom_line()

# change the column names so they are easier to manipulate 
qp<- qp %>% dplyr::rename(PE = probExceeded, Q = Flow_Inst)


# now extract the percent values for 90, and 95 % 
q5<- extract_probflowexc(qp,5) %>% dplyr::rename(Q5 =  Q) %>% dplyr::select(-PE) 
q95<- extract_probflowexc(qp,95) %>% dplyr::rename(Q95 =  Q) %>% dplyr::select(-PE) 
# join both metrics
qvals<- left_join(q5, q95, by='site_no') 

# write.csv(qp, paste0(fileloc, 'Data/FDC/FDC_0100_15min_210706.csv'), row.names = F)
# write.csv(qvals, paste0(fileloc, 'Data/FDC/FDC_595_15min_210706.csv'), row.names = F)





# Fun FDC for antecedent rainfall conditions



# Run FDC values for wet periods

datalist<- list() # create an empty list to store the data 
for(i in unique(s_wet$site_no)){
  df<- s_wet %>% dplyr::filter(site_no %in% i) %>%  dplyr::arrange(desc(Flow_Inst))%>%
    mutate(probExceeded = 100*(1-cume_dist(Flow_Inst))) %>% distinct()
  Qs<- fdc_func(df$Flow_Inst, df$probExceeded)
  df2<- data.frame(probExceeded= seq(from = 1, to = 100, by=1), Flow_Inst = Qs)
  df2$site_no <- i
  df2<- df2 %>% dplyr::select(site_no, probExceeded, Flow_Inst)
  datalist[[i]]<- df2
}

# rbind list to create a df including all values for all sites 
qp<- bind_rows(datalist)

ggplot(qp, aes(probExceeded,Flow_Inst))+geom_line()+facet_wrap(~site_no, scales='free_y')
ggplot(qp, aes(probExceeded,Flow_Inst, group=site_no))+geom_line()

# change the column names so they are easier to manipulate 
qp<- qp %>% dplyr::rename(PE = probExceeded, Q = Flow_Inst)


# now extract the percent values for 90, and 95 % 
q5<- extract_probflowexc(qp,5) %>% dplyr::rename(Q5 =  Q) %>% dplyr::select(-PE) 
q95<- extract_probflowexc(qp,95) %>% dplyr::rename(Q95 =  Q) %>% dplyr::select(-PE) 
# join both metrics
qvals_wet<- left_join(q5, q95, by='site_no') 



# Run FDC values for dry periods

datalist<- list() # create an empty list to store the data 
for(i in unique(s_dry$site_no)){
  df<- s_dry %>% dplyr::filter(site_no %in% i) %>%  dplyr::arrange(desc(Flow_Inst))%>%
    mutate(probExceeded = 100*(1-cume_dist(Flow_Inst))) %>% distinct()
  Qs<- fdc_func(df$Flow_Inst, df$probExceeded)
  df2<- data.frame(probExceeded= seq(from = 1, to = 100, by=1), Flow_Inst = Qs)
  df2$site_no <- i
  df2<- df2 %>% dplyr::select(site_no, probExceeded, Flow_Inst)
  datalist[[i]]<- df2
}

# rbind list to create a df including all values for all sites 
qp<- bind_rows(datalist)

# ggplot(qp, aes(probExceeded,Flow_Inst))+geom_line()+facet_wrap(~site_no, scales='free_y')
# ggplot(qp, aes(probExceeded,Flow_Inst, group=site_no))+geom_line()

# change the column names so they are easier to manipulate 
qp<- qp %>% dplyr::rename(PE = probExceeded, Q = Flow_Inst)


# now extract the percent values for 90, and 95 % 
q5<- extract_probflowexc(qp,5) %>% dplyr::rename(Q5 =  Q) %>% dplyr::select(-PE) 
q95<- extract_probflowexc(qp,95) %>% dplyr::rename(Q95 =  Q) %>% dplyr::select(-PE) 
# join both metrics
qvals_dry<- left_join(q5, q95, by='site_no') 

