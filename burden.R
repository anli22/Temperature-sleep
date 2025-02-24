library(dplyr)
library(reshape2)
library(stringr)
library(dlnm)
library(splines)
library(nlme)
library(mgcv)
library(mvmeta)
library(tsModel)
library(lme4)
library(lmerTest)
library(tidyverse)
library(lubridate)
library(table1)
library(boot)
library(mixmeta)
library(survival)
library(survey)
library(foreign)
library(car)
library(broom)
library(Rcpp)
library(htmlTable)
library(gnm)
library(ggplot2)
library(grid)
library(forestploter)
library(RColorBrewer)
library(lme4)
##########################load data#########################
city_list <- read.csv("/d2/.../cityidmydata.csv")
city_names <- as.character(city_list$Number)
region<-city_list[,c(1,8)]


model_scenery<-c("ACCESS-CM2","ACCESS-ESM1-5","BCC-CSM2-MR","GFDL-ESM4",
                 "IITM-ESM","INM-CM4-8","IPSL-CM6A-LR","MIROC6","MIROC-ES2L",
                 "MPI-ESM1-2-HR","MPI-ESM1-2-LR","MRI-ESM2-0","NESM3",
                 "NorESM2-LM","NorESM2-MM")


#########################################126-national######################################

list_temp_burden_ssp126 <- list()


for(m in 1:15){
  
  i <- model_scenery[m]
  
  cat(i)
  
  list_temp_hist_ssp126_OR<-readRDS(paste0("/d2/.../model",m,".rds"))
  
  for(n in 1:336){
    
    j <- city_names[n]
    
    j <- as.numeric(j)
    
    cat(j)
    
    frame_temp_hist_ssp126_OR <- list_temp_hist_ssp126_OR[[j]]
    
    #########################Sleep Loss due to temperature increase-totalchange
    
    frame_temp_hist_ssp126_OR$totalchangeSD <- (frame_temp_hist_ssp126_OR$totalhigh-frame_temp_hist_ssp126_OR$totallow)/(1.96*2)
    
    frame_ssp126_k_merge <- c()
    
    num = 1000
    p<-1
    mean_126 <- frame_temp_hist_ssp126_OR[p,6]
    
    sd_126 <- frame_temp_hist_ssp126_OR[p,42]
    
    frame_ssp126_k <- matrix(rnorm(num, mean=mean_126, sd=sd_126))
    
    frame_ssp126_k_merge <-  frame_ssp126_k
    
    
    frame_temp_hist_ssp126_OR$totalAttr_burden_v <- frame_temp_hist_ssp126_OR$totalchange
    
    frame_temp_hist_ssp126_OR$totalAttr_burden_l <- NA
    
    frame_temp_hist_ssp126_OR$totalAttr_burden_h <- NA
    
    
    frame_temp_hist_ssp126_OR[,44]<-quantile(frame_ssp126_k_merge[,1],0.025, na.rm = TRUE)
    frame_temp_hist_ssp126_OR[,45]<-quantile(frame_ssp126_k_merge[,1],0.975, na.rm = TRUE)
    
    #########################Sleep Loss due to temperature increase-lightchange
    
    frame_temp_hist_ssp126_OR$lightchangeSD <- (frame_temp_hist_ssp126_OR$lighthigh-frame_temp_hist_ssp126_OR$lightlow)/(1.96*2)
    
    frame_ssp126_k_merge <- c()
    
    num = 1000
    p<-1
    mean_126 <- frame_temp_hist_ssp126_OR[p,12]
    
    sd_126 <- frame_temp_hist_ssp126_OR[p,46]
    
    frame_ssp126_k <- matrix(rnorm(num, mean=mean_126, sd=sd_126))
    
    frame_ssp126_k_merge <-  frame_ssp126_k
    
    frame_temp_hist_ssp126_OR$lightAttr_burden_v <- frame_temp_hist_ssp126_OR$lightchange
    
    frame_temp_hist_ssp126_OR$lightAttr_burden_l <- NA
    
    frame_temp_hist_ssp126_OR$lightAttr_burden_h <- NA
    
    
    frame_temp_hist_ssp126_OR[,48]<-quantile(frame_ssp126_k_merge[,1],0.025, na.rm = TRUE)
    frame_temp_hist_ssp126_OR[,49]<-quantile(frame_ssp126_k_merge[,1],0.975, na.rm = TRUE)
    
    
    #########################Sleep Loss due to temperature increase-deepchange
    
    frame_temp_hist_ssp126_OR$deepchangeSD <- (frame_temp_hist_ssp126_OR$deephigh-frame_temp_hist_ssp126_OR$deeplow)/(1.96*2)
    
    frame_ssp126_k_merge <- c()
    
    num = 1000
    p<-1
    mean_126 <- frame_temp_hist_ssp126_OR[p,15]
    
    sd_126 <- frame_temp_hist_ssp126_OR[p,50]
    
    frame_ssp126_k <- matrix(rnorm(num, mean=mean_126, sd=sd_126))
    
    frame_ssp126_k_merge <-  frame_ssp126_k
    
    frame_temp_hist_ssp126_OR$deepAttr_burden_v <- frame_temp_hist_ssp126_OR$deepchange
    
    frame_temp_hist_ssp126_OR$deepAttr_burden_l <- NA
    
    frame_temp_hist_ssp126_OR$deepAttr_burden_h <- NA
    
    
    frame_temp_hist_ssp126_OR[,52]<-quantile(frame_ssp126_k_merge[,1],0.025, na.rm = TRUE)
    frame_temp_hist_ssp126_OR[,53]<-quantile(frame_ssp126_k_merge[,1],0.975, na.rm = TRUE)
    
    
    
    #########################Sleep Loss due to temperature increase-dreamchange
    
    frame_temp_hist_ssp126_OR$dreamchangeSD <- (frame_temp_hist_ssp126_OR$dreamhigh-frame_temp_hist_ssp126_OR$dreamlow)/(1.96*2)
    
    frame_ssp126_k_merge <- c()
    
    num = 1000
    p<-1
    mean_126 <- frame_temp_hist_ssp126_OR[p,18]
    
    sd_126 <- frame_temp_hist_ssp126_OR[p,54]
    
    frame_ssp126_k <- matrix(rnorm(num, mean=mean_126, sd=sd_126))
    
    frame_ssp126_k_merge <-  frame_ssp126_k
    
    frame_temp_hist_ssp126_OR$dreamAttr_burden_v <- frame_temp_hist_ssp126_OR$dreamchange
    
    frame_temp_hist_ssp126_OR$dreamAttr_burden_l <- NA
    
    frame_temp_hist_ssp126_OR$dreamAttr_burden_h <- NA
    
    
    frame_temp_hist_ssp126_OR[,56]<-quantile(frame_ssp126_k_merge[,1],0.025, na.rm = TRUE)
    frame_temp_hist_ssp126_OR[,57]<-quantile(frame_ssp126_k_merge[,1],0.975, na.rm = TRUE)
    
    
    
    #########################Excess case due to temperature increase-OR
    
    frame_temp_hist_ssp126_OR$ORSD <- (frame_temp_hist_ssp126_OR$ORhigh-frame_temp_hist_ssp126_OR$ORlow)/(1.96*2)
    
    frame_ssp126_k_merge <- c()
    
    num = 1000
    p<-1
    mean_126 <- frame_temp_hist_ssp126_OR[p,9]
    
    sd_126 <- frame_temp_hist_ssp126_OR[p,58]
    
    frame_ssp126_k <- matrix(rnorm(num, mean=mean_126, sd=sd_126))
    
    frame_ssp126_k_merge <-  frame_ssp126_k
    
    
    for(k in 1:1000){
      frame_ssp126_k_merge[k,1] <- (frame_ssp126_k_merge[k,1]-1)
    }
    
    
    frame_temp_hist_ssp126_OR$Attr_burden_v <- (frame_temp_hist_ssp126_OR$OR-1)
    
    frame_temp_hist_ssp126_OR$Attr_burden_l <- NA
    
    frame_temp_hist_ssp126_OR$Attr_burden_h <- NA
    
    
    frame_temp_hist_ssp126_OR[,60]<-quantile(frame_ssp126_k_merge[,1],0.025, na.rm = TRUE)
    frame_temp_hist_ssp126_OR[,61]<-quantile(frame_ssp126_k_merge[,1],0.975, na.rm = TRUE)
    
    
    
    list_temp_burden_ssp126[[i]][[j]] <- frame_temp_hist_ssp126_OR
    
  }
}
save(list_temp_burden_ssp126, file = paste0("/d2/.../list_temp_burden_ssp126.rda"))


list_temp_burden_ssp126total<-list_temp_burden_ssp126

years <- data.frame(Year = 1950:2100)
data <- data.frame(matrix(NA, nrow = 151, ncol = 45))
col_names <- c()
for (i in 1:15) {
  col_names <- c(col_names, paste0("mode", i, "_Rb"), paste0("mode", i, "_Rlb"), paste0("mode", i, "_Rhb"))
}
colnames(data) <- col_names
outcome1<-cbind(years,data)
outcome2<-cbind(years,data)
outcome3<-cbind(years,data)
outcome4<-cbind(years,data)
outcome5<-cbind(years,data)

for (i in 1:15) {
  outtest<-c()
  cat(i)
  for (j in 1:336) {
    cat(j)
    test<-list_temp_burden_ssp126[[i]][[j]]
    test$year<-year(test$date)
    test$ORburden_v <- test$avg_pop*(test$case/test$total)*test$Attr_burden_v*(test$model_temp-test$basic_temp)
    test$ORburden_l <- test$avg_pop*(test$case/test$total)*test$Attr_burden_l*(test$model_temp-test$basic_temp)
    test$ORburden_h <- test$avg_pop*(test$case/test$total)*test$Attr_burden_h*(test$model_temp-test$basic_temp)
    test$ORpopcity <- test$avg_pop*(test$case/test$total)
    
    test$popcity <- test$avg_pop
    test$totalburden_v <- test$avg_pop*test$totalAttr_burden_v*(test$model_temp-test$basic_temp)
    test$totalburden_l <- test$avg_pop*test$totalAttr_burden_l*(test$model_temp-test$basic_temp)
    test$totalburden_h <- test$avg_pop*test$totalAttr_burden_h*(test$model_temp-test$basic_temp)
    
    
    test$lightburden_v <- test$avg_pop*test$lightAttr_burden_v*(test$model_temp-test$basic_temp)
    test$lightburden_l <- test$avg_pop*test$lightAttr_burden_l*(test$model_temp-test$basic_temp)
    test$lightburden_h <- test$avg_pop*test$lightAttr_burden_h*(test$model_temp-test$basic_temp)
    
    
    test$deepburden_v <- test$avg_pop*test$deepAttr_burden_v*(test$model_temp-test$basic_temp)
    test$deepburden_l <- test$avg_pop*test$deepAttr_burden_l*(test$model_temp-test$basic_temp)
    test$deepburden_h <- test$avg_pop*test$deepAttr_burden_h*(test$model_temp-test$basic_temp)
    
    
    
    test$dreamburden_v <- test$avg_pop*test$dreamAttr_burden_v*(test$model_temp-test$basic_temp)
    test$dreamburden_l <- test$avg_pop*test$dreamAttr_burden_l*(test$model_temp-test$basic_temp)
    test$dreamburden_h <- test$avg_pop*test$dreamAttr_burden_h*(test$model_temp-test$basic_temp)
    
    sum_by_pop <- test %>% 
      group_by(year) %>% 
      summarize(OR_pop = sum(ORpopcity, na.rm = TRUE))
    
    
    sum_by_year <- test %>% 
      group_by(year) %>% 
      summarize(ORburden_v = sum(ORburden_v, na.rm = TRUE))
    sum_by_year_l <- test %>% 
      group_by(year) %>% 
      summarize(ORburden_l = sum(ORburden_l, na.rm = TRUE))
    sum_by_year_h <- test %>% 
      group_by(year) %>% 
      summarize(ORburden_h = sum(ORburden_h, na.rm = TRUE))
    
    
    outprint<-left_join(sum_by_year,sum_by_year_l,by="year")
    outprint<-left_join(outprint,sum_by_year_h,by="year")
    outprint<-left_join(outprint,sum_by_pop,by="year") 
    
    sum_by_pop <- test %>% 
      group_by(year) %>% 
      summarize(total_pop = sum(popcity, na.rm = TRUE))
    
    
    sum_by_year <- test %>% 
      group_by(year) %>% 
      summarize(totalburden_v = sum(totalburden_v, na.rm = TRUE))
    sum_by_year_l <- test %>% 
      group_by(year) %>% 
      summarize(totalburden_l = sum(totalburden_l, na.rm = TRUE))
    sum_by_year_h <- test %>% 
      group_by(year) %>% 
      summarize(totalburden_h = sum(totalburden_h, na.rm = TRUE))
    
    
    outprint<-left_join(outprint,sum_by_pop,by="year")
    outprint<-left_join(outprint,sum_by_year,by="year")
    outprint<-left_join(outprint,sum_by_year_l,by="year")
    outprint<-left_join(outprint,sum_by_year_h,by="year") 
    
    sum_by_year <- test %>% 
      group_by(year) %>% 
      summarize(lightburden_v = sum(lightburden_v, na.rm = TRUE))
    sum_by_year_l <- test %>% 
      group_by(year) %>% 
      summarize(lightburden_l = sum(lightburden_l, na.rm = TRUE))
    sum_by_year_h <- test %>% 
      group_by(year) %>% 
      summarize(lightburden_h = sum(lightburden_h, na.rm = TRUE))
    
    outprint<-left_join(outprint,sum_by_year,by="year")
    outprint<-left_join(outprint,sum_by_year_l,by="year")
    outprint<-left_join(outprint,sum_by_year_h,by="year") 
    
    sum_by_year <- test %>% 
      group_by(year) %>% 
      summarize(deepburden_v = sum(deepburden_v, na.rm = TRUE))
    sum_by_year_l <- test %>% 
      group_by(year) %>% 
      summarize(deepburden_l = sum(deepburden_l, na.rm = TRUE))
    sum_by_year_h <- test %>% 
      group_by(year) %>% 
      summarize(deepburden_h = sum(deepburden_h, na.rm = TRUE))
    
    outprint<-left_join(outprint,sum_by_year,by="year")
    outprint<-left_join(outprint,sum_by_year_l,by="year")
    outprint<-left_join(outprint,sum_by_year_h,by="year") 
    
    
    sum_by_year <- test %>% 
      group_by(year) %>% 
      summarize(dreamburden_v = sum(dreamburden_v, na.rm = TRUE))
    sum_by_year_l <- test %>% 
      group_by(year) %>% 
      summarize(dreamburden_l = sum(dreamburden_l, na.rm = TRUE))
    sum_by_year_h <- test %>% 
      group_by(year) %>% 
      summarize(dreamburden_h = sum(dreamburden_h, na.rm = TRUE))
    
    outprint<-left_join(outprint,sum_by_year,by="year")
    outprint<-left_join(outprint,sum_by_year_l,by="year")
    outprint<-left_join(outprint,sum_by_year_h,by="year") 
    
    outtest<-rbind(outtest,outprint)
  }
  
  sum_by_pop <- outtest %>% 
    group_by(year) %>% 
    summarize(OR_pop = sum(OR_pop, na.rm = TRUE))
  
  
  sum_by_year <- outtest %>% 
    group_by(year) %>% 
    summarize(ORburden_v = sum(ORburden_v, na.rm = TRUE))
  sum_by_year_l <- outtest %>% 
    group_by(year) %>% 
    summarize(ORburden_l = sum(ORburden_l, na.rm = TRUE))
  sum_by_year_h <- outtest %>% 
    group_by(year) %>% 
    summarize(ORburden_h = sum(ORburden_h, na.rm = TRUE))
  
  
  outprint<-left_join(sum_by_year,sum_by_year_l,by="year")
  outprint<-left_join(outprint,sum_by_year_h,by="year")
  outprint<-left_join(outprint,sum_by_pop,by="year") 
  
  sum_by_pop <- outtest %>% 
    group_by(year) %>% 
    summarize(total_pop = sum(total_pop, na.rm = TRUE))
  
  
  sum_by_year <- outtest %>% 
    group_by(year) %>% 
    summarize(totalburden_v = sum(totalburden_v, na.rm = TRUE))
  sum_by_year_l <- outtest %>% 
    group_by(year) %>% 
    summarize(totalburden_l = sum(totalburden_l, na.rm = TRUE))
  sum_by_year_h <- outtest %>% 
    group_by(year) %>% 
    summarize(totalburden_h = sum(totalburden_h, na.rm = TRUE))
  
  
  outprint<-left_join(outprint,sum_by_pop,by="year")
  outprint<-left_join(outprint,sum_by_year,by="year")
  outprint<-left_join(outprint,sum_by_year_l,by="year")
  outprint<-left_join(outprint,sum_by_year_h,by="year") 
  
  sum_by_year <- outtest %>% 
    group_by(year) %>% 
    summarize(lightburden_v = sum(lightburden_v, na.rm = TRUE))
  sum_by_year_l <- outtest %>% 
    group_by(year) %>% 
    summarize(lightburden_l = sum(lightburden_l, na.rm = TRUE))
  sum_by_year_h <- outtest %>% 
    group_by(year) %>% 
    summarize(lightburden_h = sum(lightburden_h, na.rm = TRUE))
  
  outprint<-left_join(outprint,sum_by_year,by="year")
  outprint<-left_join(outprint,sum_by_year_l,by="year")
  outprint<-left_join(outprint,sum_by_year_h,by="year") 
  
  sum_by_year <- outtest %>% 
    group_by(year) %>% 
    summarize(deepburden_v = sum(deepburden_v, na.rm = TRUE))
  sum_by_year_l <- outtest %>% 
    group_by(year) %>% 
    summarize(deepburden_l = sum(deepburden_l, na.rm = TRUE))
  sum_by_year_h <- outtest %>% 
    group_by(year) %>% 
    summarize(deepburden_h = sum(deepburden_h, na.rm = TRUE))
  
  outprint<-left_join(outprint,sum_by_year,by="year")
  outprint<-left_join(outprint,sum_by_year_l,by="year")
  outprint<-left_join(outprint,sum_by_year_h,by="year") 
  
  
  sum_by_year <- outtest %>% 
    group_by(year) %>% 
    summarize(dreamburden_v = sum(dreamburden_v, na.rm = TRUE))
  sum_by_year_l <- outtest %>% 
    group_by(year) %>% 
    summarize(dreamburden_l = sum(dreamburden_l, na.rm = TRUE))
  sum_by_year_h <- outtest %>% 
    group_by(year) %>% 
    summarize(dreamburden_h = sum(dreamburden_h, na.rm = TRUE))
  
  outprint<-left_join(outprint,sum_by_year,by="year")
  outprint<-left_join(outprint,sum_by_year_l,by="year")
  outprint<-left_join(outprint,sum_by_year_h,by="year") 
  
  #total_pop #OR_pop
  
  outprint$total_burden_v<-outprint$totalburden_v/outprint$total_pop
  outprint$total_burden_h<-outprint$totalburden_h/outprint$total_pop
  outprint$total_burden_l<-outprint$totalburden_l/outprint$total_pop
  
  outprint$light_burden_v<-outprint$lightburden_v/outprint$total_pop
  outprint$light_burden_h<-outprint$lightburden_h/outprint$total_pop
  outprint$light_burden_l<-outprint$lightburden_l/outprint$total_pop
  
  outprint$deep_burden_v<-outprint$deepburden_v/outprint$total_pop
  outprint$deep_burden_h<-outprint$deepburden_h/outprint$total_pop
  outprint$deep_burden_l<-outprint$deepburden_l/outprint$total_pop
  
  outprint$dream_burden_v<-outprint$dreamburden_v/outprint$total_pop
  outprint$dream_burden_h<-outprint$dreamburden_h/outprint$total_pop
  outprint$dream_burden_l<-outprint$dreamburden_l/outprint$total_pop
  
  outprint$OR_burden_v<-outprint$ORburden_v/outprint$OR_pop
  outprint$OR_burden_h<-outprint$ORburden_h/outprint$OR_pop
  outprint$OR_burden_l<-outprint$ORburden_l/outprint$OR_pop
  
  outprint<-outprint[,c(1,19:33)]
  
  outcome1[,3*i-1]<-outprint[,2]
  outcome1[,3*i]<-outprint[,3]
  outcome1[,3*i+1]<-outprint[,4]
  
  outcome2[,3*i-1]<-outprint[,5]
  outcome2[,3*i]<-outprint[,6]
  outcome2[,3*i+1]<-outprint[,7]
  
  outcome3[,3*i-1]<-outprint[,8]
  outcome3[,3*i]<-outprint[,9]
  outcome3[,3*i+1]<-outprint[,10]
  
  outcome4[,3*i-1]<-outprint[,11]
  outcome4[,3*i]<-outprint[,12]
  outcome4[,3*i+1]<-outprint[,13]
  
  outcome5[,3*i-1]<-outprint[,14]
  outcome5[,3*i]<-outprint[,15]
  outcome5[,3*i+1]<-outprint[,16]
  
}

write.csv(outcome1,file="/d2/.../ssp126/TOTAL_burden.csv")
write.csv(outcome2,file="/d2/.../ssp126/LIGHT_burden.csv")
write.csv(outcome3,file="/d2/.../ssp126/DEEP_burden.csv")
write.csv(outcome4,file="/d2/.../ssp126/DREAM_burden.csv")
write.csv(outcome5,file="/d2/.../ssp126/OR_burden.csv")


outcome1[,47]<-rowMeans(outcome1[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44)])
outcome1[,48]<-rowMeans(outcome1[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)])
outcome1[,49]<-rowMeans(outcome1[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)])


outcome2[,47]<-rowMeans(outcome2[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44)])
outcome2[,48]<-rowMeans(outcome2[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)])
outcome2[,49]<-rowMeans(outcome2[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)])


outcome3[,47]<-rowMeans(outcome3[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44)])
outcome3[,48]<-rowMeans(outcome3[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)])
outcome3[,49]<-rowMeans(outcome3[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)])


outcome4[,47]<-rowMeans(outcome4[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44)])
outcome4[,48]<-rowMeans(outcome4[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)])
outcome4[,49]<-rowMeans(outcome4[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)])


outcome5[,47]<-rowMeans(outcome5[,c(2,5,8,11,14,17,20,23,26,29,32,35,38,41,44)])
outcome5[,48]<-rowMeans(outcome5[,c(3,6,9,12,15,18,21,24,27,30,33,36,39,42,45)])
outcome5[,49]<-rowMeans(outcome5[,c(4,7,10,13,16,19,22,25,28,31,34,37,40,43,46)])




Totalburdenyear <- data.frame(matrix(NA, nrow = 9, ncol = 4))
Totalburdenyear[,1]<-1:9
names(Totalburdenyear)<-c("years","126Total","126Totallow","126Totalhigh")

data<-outcome1[,c(1,47:49)]
data<-as.data.frame(data)
names(data)<-c("years","Total","Totallow","Totalhigh")
for (i in 1:9){
  k<-62+(i-1)*10
  dfff<-data[c((k):(k+9)),2]
  average1 <- mean(dfff)
  Totalburdenyear[i,2]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),3]
  average1 <- mean(dfff)
  Totalburdenyear[i,3]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),4]
  average1 <- mean(dfff)
  Totalburdenyear[i,4]<-average1*365/60
}


Lightburdenyear <- data.frame(matrix(NA, nrow = 9, ncol = 4))
Lightburdenyear[,1]<-1:9
names(Lightburdenyear)<-c("years","126Light","126Lightlow","126Lighthigh")

data<-outcome2[,c(1,47:49)]
data<-as.data.frame(data)
names(data)<-c("years","Light","Lightlow","Lighthigh")
for (i in 1:9){
  k<-62+(i-1)*10
  dfff<-data[c((k):(k+9)),2]
  average1 <- mean(dfff)
  Lightburdenyear[i,2]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),3]
  average1 <- mean(dfff)
  Lightburdenyear[i,3]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),4]
  average1 <- mean(dfff)
  Lightburdenyear[i,4]<-average1*365/60
}


Deepburdenyear <- data.frame(matrix(NA, nrow = 9, ncol = 4))
Deepburdenyear[,1]<-1:9
names(Deepburdenyear)<-c("years","126Deep","126Deeplow","126Deephigh")

data<-outcome3[,c(1,47:49)]
data<-as.data.frame(data)
names(data)<-c("years","Deep","Deeplow","Deephigh")
for (i in 1:9){
  k<-62+(i-1)*10
  dfff<-data[c((k):(k+9)),2]
  average1 <- mean(dfff)
  Deepburdenyear[i,2]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),3]
  average1 <- mean(dfff)
  Deepburdenyear[i,3]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),4]
  average1 <- mean(dfff)
  Deepburdenyear[i,4]<-average1*365/60
}


Dreamburdenyear <- data.frame(matrix(NA, nrow = 9, ncol = 4))
Dreamburdenyear[,1]<-1:9
names(Dreamburdenyear)<-c("years","126Dream","126Dreamlow","126Dreamhigh")

data<-outcome4[,c(1,47:49)]
data<-as.data.frame(data)
names(data)<-c("years","Dream","Dreamlow","Dreamhigh")
for (i in 1:9){
  k<-62+(i-1)*10
  dfff<-data[c((k):(k+9)),2]
  average1 <- mean(dfff)
  Dreamburdenyear[i,2]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),3]
  average1 <- mean(dfff)
  Dreamburdenyear[i,3]<-average1*365/60
  
  dfff<-data[c((k):(k+9)),4]
  average1 <- mean(dfff)
  Dreamburdenyear[i,4]<-average1*365/60
}

ORburdenyear <- data.frame(matrix(NA, nrow = 9, ncol = 4))
ORburdenyear[,1]<-1:9
names(ORburdenyear)<-c("years","126OR","126ORlow","126ORhigh")

data<-outcome5[,c(1,47:49)]
data<-as.data.frame(data)
names(data)<-c("years","OR","ORlow","ORhigh")
for (i in 1:9){
  k<-62+(i-1)*10
  dfff<-data[c((k):(k+9)),2]
  average1 <- mean(dfff)
  ORburdenyear[i,2]<-average1*100
  
  dfff<-data[c((k):(k+9)),3]
  average1 <- mean(dfff)
  ORburdenyear[i,3]<-average1*100
  
  dfff<-data[c((k):(k+9)),4]
  average1 <- mean(dfff)
  ORburdenyear[i,4]<-average1*100
}


write.csv(Totalburdenyear,file=paste0("/d2/.../ssp126/Totalburdenyear.csv"))
write.csv(Lightburdenyear,file=paste0("/d2/.../ssp126/Lightburdenyear.csv"))
write.csv(Deepburdenyear,file=paste0("/d2/.../ssp126/Deepburdenyear.csv"))
write.csv(Dreamburdenyear,file=paste0("/d2/.../ssp126/Dreamburdenyear.csv"))
write.csv(ORburdenyear,file=paste0("/d2/.../ssp126/ORburdenyear.csv"))

#===========================================================================================================================================

#SSP245 and SSP585 scenario data are similar
