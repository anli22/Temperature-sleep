library(lme4)
data<-readRDS("/d2/.../alldata.rds")

###################################
####sleep duration - temprtature#########
#################################
fmla<-as.formula(sleep_duration~lag_day_temp+age+as.factor(sex)+as.factor(BMIlevel)+as.factor(DOW)+as.factor(holiday)+(1|ID)+ns(lag_day_rhu,3)+as.factor(season)+as.factor(region)+as.factor(year))
model<-lmer(fmla,data=data,na.action=na.exclude)
beta = summary(model)$coefficients[2,1]
se=summary(model)$coefficients[2,2]

###################################
####sleep insufficiency - temprtature#########
#################################
fmla<-as.formula(sleep_insufficiency~lag_day_temp+age+as.factor(sex)+as.factor(BMIlevel)+as.factor(DOW)+as.factor(holiday)+ns(lag_day_rhu,3)+as.factor(season)+as.factor(region)+as.factor(year))
model<-glm(fmla,data=data,na.action=na.exclude,family = binomial)
beta = summary(model)$coefficients[2,1]
se=summary(model)$coefficients[2,2]
