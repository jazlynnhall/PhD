# functions for the scripts 


# Find antecedent rainfall quantiles ----
find_ARain_quantiles<- function(x){
  df<- x
  sn<- unique(df$site_no)
  qmonth<- data.frame(ARain30_33 = unname(quantile(df$ARain30, 0.33)), ARain30_50 = unname(quantile(df$ARain30, 0.50)), ARain30_66 = unname(quantile(df$ARain30, 0.66)))
  qweek<- data.frame(ARain7_33 = unname(quantile(df$ARain7, 0.33)), ARain7_50 = unname(quantile(df$ARain7, 0.50)), ARain7_66 = unname(quantile(df$ARain7, 0.66)))
  df2<- cbind(sn, qmonth, qweek)
  return(df2)
}


# FDCS ----

# Extract flow values for values nearest to 1-100% P 
extract_probflowexc<- function(df,percent){
  df<- df %>% group_by(site_no) %>% slice(which.min(abs(PE - percent))) 
  return(df)}

# Estimate probability exceeded from using a spline function from the cume_dist function  above 
fdc_func<- function(Flow, probExceeded){
  x<- Flow
  y<- probExceeded
  f <- splinefun(y,x, method="hyman") # https://stackoverflow.com/questions/47839053/how-to-force-splinefun-values-to-be-positive
  p<- seq(from = 0.01, to = 1, by=0.01) 
  qp<- f(p)
  return(qp)}


# check the autocorrelation of responses in the mixed models ----

find_acflim<- function(df){
  df2<- df %>% group_by(site_no) %>% summarize(
    conf.limhigh = max(qnorm((1 + alpha1)/2)/sqrt(n.used)),
    conf.limlow = min(-1*qnorm((1 + alpha1)/2)/sqrt(n.used)))
  return(df2)
}


# limshigh<- ac1 %>% group_by(site_no) %>% summarize(
#   conf.limhigh = max(qnorm((1 + alpha1)/2)/sqrt(n.used)))
# limslow<- ac1 %>% group_by(site_no) %>% summarize(
#   conf.limlow = min(-1*qnorm((1 + alpha1)/2)/sqrt(n.used)))


# extract nlme model coefficients and CIs ----
findse = function(df){
  df2<- df %>% mutate(significant = ifelse(lower>0&upper>0|lower<0&upper<0, 1,0),
                      CI = ifelse(significant == 1 & est > 0, upper - est, 
                                  ifelse(significant ==1 & est < 0, abs(lower) - abs(est), 
                                         ifelse(significant == 0 & est > 0, upper - est, abs(lower)-abs(est)))), 
                      lower2 = est - (CI/2), 
                      upper2 = est + (CI/2),
                      significant_alpha = ifelse(significant==1,1, ifelse(lower2>0&upper2>0|lower2<0&upper2<0, 0.8,0.3)),
                      significant2 = ifelse(significant_alpha==1, '95% CI', ifelse(significant_alpha==0.8, '66% CI', 'Non-significant')),
                      significant3 = ifelse(significant==1,'Significant','Non-Significant'))
  return(df2)
}

extractresults<- function(mod, pmetric,pmetric2){
  df<- nlme::intervals(mod,which = "fixed")
  df2<- data.frame(prednames = c('Intercept', pmetric, '% Forest Cover', 
                                 '% Above 700m',
                                 #'Mean Slope',
                                 paste0(pmetric2,' : Forest Cover')),
                   preds = names(mod$coefficients$fixed),
                   lower = df$fixed[1:5], est = df$fixed[6:10], upper = df$fixed[11:15])
  df2<- findse(df2)
  return(df2)
}


# for total annual rainfall 
extractresults2<- function(mod, pmetric,pmetric2,pname, pname2){
  df<- nlme::intervals(mod,which = "fixed")
  df2<- data.frame(prednames = c('Intercept', pmetric, 
                                 pmetric2,
                                 '% Forest Cover', 
                                 '% Above 700m',
                                 #'Mean Slope',
                                 paste0(pname,' : Forest Cover'),
                                 paste0(pname2,' : Forest Cover')),
                   preds = names(mod$coefficients$fixed),
                   lower = df$fixed[1:7], est = df$fixed[8:14], upper = df$fixed[15:21])
  df2<- findse(df2)
  return(df2)
}

# for when we include LONGITUDE
extractresults3<- function(mod, pmetric,pmetric2){
  df<- nlme::intervals(mod,which = "fixed")
  df2<- data.frame(prednames = c('Intercept', pmetric, '% Forest Cover', 
                                 '% Above 700m',
                                 'Longitude',
                                 paste0(pmetric2,' : Forest Cover')),
                   preds = names(mod$coefficients$fixed),
                   lower = df$fixed[1:6], est = df$fixed[7:12], upper = df$fixed[13:18])
  df2<- findse(df2)
  return(df2)
}



# Shared legend function ----
grid_arrange_shared_legend <- function(...) {
  plots <- list(...)
  g <- ggplotGrob(plots[[1]] + theme(legend.position="bottom"))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  grid.arrange(
    do.call(arrangeGrob, lapply(plots, function(x)
      x + theme(legend.position="none"))),
    legend,
    ncol = 1,
    heights = unit.c(unit(1, "npc") - lheight, lheight))
}
