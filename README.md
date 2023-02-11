Mapping the incidence rate of typhoid fever in sub-Saharan Africa
================
2023-02-10

### Log-linear regression

``` r
library(car)
library(MASS)
library(ciTools)
#setwd("D:/SynologyDrive/Research/Collaboration/IVI_Kim/codes/")

dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000     #change the unit

# 16 covariates
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  
age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

summary.dat<-NULL
for(j in 1:length(age.group)) { 
  age<-age.group[j]   
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  print(summary(sub.dat$Standardized_IR_100000_Year))
  summary.dat<-cbind(summary.dat,as.matrix(summary(sub.dat$Standardized_IR_100000_Year)))
}
colnames(summary.dat)<-age.group

####################################################
# 1. Check the multicollinearity => consider only cov.name2 variables for each age group
####################################################
# 1.1: age=0-1y
age<-age.group[1]    # 1-7
sub.dat<-subset(dat,dat$Age_Group_Edit==age)
sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name])
sub.dat2<-na.omit(sub.dat2) 
dim(sub.dat2)  # 22*19
GGally::ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
round(sort(abs(cor((cbind(LogIR=sub.dat2$LogIR, sub.dat2[,cov.name])))[,1])),3)
round(sort(abs(cor((cbind(RIR=sub.dat2$RIR, sub.dat2[,cov.name])))[,1])),3)

# Drop covariates with VIF>10:"underweight","stunting_prev","improved_sanitation","annual_mean_temp",
cov.name2<-c("improved_water","annual_rainfall","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
model2<-lm(formula2,data=sub.dat2)
print(vif(model2)) 
#improved_water  annual_rainfall         HIV_prev travel_time_city        elevation   distance_water      piped_water piped_sanitation    surface_water 
#3.238106         4.314691         2.292060         4.629299         2.193829         2.090204         5.526978         3.760084         2.374163 
#open_defecation          wasting         pop_size 
#2.850488         2.534876         3.110401 

# 1.2: age=2-4y
age<-age.group[2]    # 1-7
sub.dat<-subset(dat,dat$Age_Group_Edit==age)
sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name])
sub.dat2<-na.omit(sub.dat2) 
dim(sub.dat2)  # 22*19
GGally::ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
round(sort(abs(cor((cbind(LogIR=sub.dat2$LogIR, sub.dat2[,cov.name])))[,1])),3)
round(sort(abs(cor((cbind(RIR=sub.dat2$RIR, sub.dat2[,cov.name])))[,1])),3)
 
# drop covariates with VIF>10: "underweight","stunting_prev","improved_sanitation","annual_mean_temp",
cov.name2<-c("improved_water","annual_rainfall","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
model2<-lm(formula2,data=sub.dat2)
print(vif(model2)) 
#improved_water  annual_rainfall         HIV_prev travel_time_city        elevation   distance_water      piped_water 
#3.238106         4.314691         2.292060         4.629299         2.193829         2.090204         5.526978 
#piped_sanitation    surface_water  open_defecation          wasting         pop_size 
#3.760084         2.374163         2.850488         2.534876         3.110401 

# 1.3: age=5-14y
age<-age.group[3]    # 1-7
sub.dat<-subset(dat,dat$Age_Group_Edit==age)
sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name])
sub.dat2<-na.omit(sub.dat2) 
dim(sub.dat2)  # 26*19
GGally::ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
round(sort(abs(cor((cbind(LogIR=sub.dat2$LogIR, sub.dat2[,cov.name])))[,1])),3)
round(sort(abs(cor((cbind(RIR=sub.dat2$RIR, sub.dat2[,cov.name])))[,1])),3)

# drop VIF>10: "underweight","improved_sanitation","annual_mean_temp",
cov.name2<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
model2<-lm(formula2,data=sub.dat2)
print(vif(model2)) 
#improved_water  annual_rainfall    stunting_prev         HIV_prev travel_time_city        elevation   distance_water 
#3.435051         1.708362         5.229089         2.498686         3.694508         6.782835         2.498722 
#piped_water piped_sanitation    surface_water  open_defecation          wasting         pop_size 
#3.749475         3.244718         2.597554         3.085822         3.036277         3.554258 

# 1.4: age=>14y
age<-age.group[4]    # 1-7
sub.dat<-subset(dat,dat$Age_Group_Edit==age)
sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name])
sub.dat2<-na.omit(sub.dat2) 
dim(sub.dat2)  # 24*19
GGally::ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
round(sort(abs(cor((cbind(LogIR=sub.dat2$LogIR, sub.dat2[,cov.name])))[,1])),3)
round(sort(abs(cor((cbind(RIR=sub.dat2$RIR, sub.dat2[,cov.name])))[,1])),3)

# drop VIF>10: "underweight","improved_sanitation","annual_mean_temp",
cov.name2<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
model2<-lm(formula2,data=sub.dat2)
print(vif(model2)) 
#improved_water  annual_rainfall    stunting_prev         HIV_prev travel_time_city        elevation   distance_water 
#4.080860         1.848824         6.579878         2.486858         3.834253         8.608325         3.129198 
#piped_water piped_sanitation    surface_water  open_defecation          wasting         pop_size 
#3.570412         3.193919         3.385835         3.097890         2.627794         3.925886 


#############################################################
# 2. Linear regression model with stepwise 
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  
cov.name.12<-c("improved_water","annual_rainfall","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
cov.name.3<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
cov.name.4<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  

RMSE<-matrix(NA,length(age.group),2)
colnames(RMSE)<-c("RMSE","RMSE.CV")

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(j<=2) {age2<-age; cov.name2=cov.name.12}
  if(j==3) {age2<-age; cov.name2=cov.name.3}
  if(j==4) { age2<-"over 14y"; cov.name2=cov.name.4 }
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)
  
  formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-lm(formula2,data=sub.dat2)
  step.model<-stepAIC(model2, direction="both",trace = FALSE)
  sink(sprintf("outputs/LReg_Stepwise_summary_%s_v6.txt",age2))
  print(summary(step.model))
  sink()
  # write.csv(summary(step.model)$coefficients,sprintf("outputs/LReg_Stepwise_summary_%s_v6.csv",age2))
  step.model$anova
  
  pred.val<-predict(step.model)
  pred.val<-exp(pred.val)
  y<-sub.dat2[,"LogIR"]
  
  # Calculate RMSE1 using all dataset
  RMSE1<-sqrt(mean((exp(y)-pred.val)^2))
  selected.cov<-names(step.model$coefficients)[-1]
  
  # One-leave out cross-validation
  x<-as.matrix(sub.dat2[,selected.cov])
  colnames(x)<-selected.cov
  temp.dat<-data.frame(cbind(y,x))

  pred.val.CV1<-matrix(NA,nrow(temp.dat),ncol=3)
  colnames(pred.val.CV1)<-c("pred0","lower0","upper0")
 
  for(i in 1:nrow(temp.dat)){
    train.dat<-data.frame(temp.dat[-i,])
    test.dat<-data.frame(temp.dat[i,])
     fit.best<-glm(as.formula(paste("y~",paste(selected.cov,collapse="+"),sep="")),data=train.dat,family = gaussian)
     pred.CV<-add_pi(test.dat, fit.best, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
     pred.val.CV1[i,"pred0"]<-exp(pred.CV$pred)
     pred.val.CV1[i,"lower0"]<-exp(pred.CV$lpb)
     pred.val.CV1[i,"upper0"]<-exp(pred.CV$upb)
  }
  pred.val.CV2<-pred.val.CV1
  pred.val.CV2[pred.val.CV2>=1e4]<-1e4   # adjust the prediction value
  pred.val.CV<-cbind(pred.val.CV1,pred.val.CV2)
  colnames(pred.val.CV)<-c("pred0","lower0","upper0","pred","lower","upper")
  
  RMSE2<-sqrt(mean((exp(y)-pred.val.CV[,"pred"])^2))
  RMSE[j,]<-c(RMSE1,RMSE2)

  pred.dat<-cbind(y=exp(y),pred.val,pred.val.CV)
  # write.csv(pred.dat,sprintf("outputs/Prediction_Age_%s_LReg_Stepwise_v6.csv",age2))
  
}

RMSE.output<-cbind(age.group,RMSE)
# write.csv(RMSE.output,"outputs/LReg_Stepwise_RMSE_v6.csv")
```

### Poisson regression

``` r
library(car)
library(MASS)
library(ciTools)
library(AER)


dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000 # change the unit


age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

IR<-round(dat$Standardized_IR_100000_Year/10)
summary(IR)

#############################################################
# 1. Poisson regression model with stepwise
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  
cov.name.1<-c("annual_rainfall","HIV_prev","elevation","distance_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  # "piped_water","travel_time_city","improved_water",(high correlated and low correlated with RIR)
cov.name.2<-c("improved_water","annual_rainfall","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
cov.name.3<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
cov.name.4<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  

RMSE<-matrix(NA,length(age.group),3)
colnames(RMSE)<-c("RMSE","RMSE.CV","MAE.CV")

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(age == "0-1 y") {age2<-age; cov.name2=cov.name.1}
  if(age == "2-4 y") {age2<-age; cov.name2=cov.name.2}
  if(age =="5-14 y") { age2<-age; cov.name2=cov.name.3 }
  if(age == ">14 y") {age2<-"over 14y"; cov.name2=cov.name.4}
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)

  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm(formula2,data=sub.dat2,family=poisson())
 
  summary(model2)
  step.model<-stepAIC(model2, direction="both",trace = FALSE)
  summary(step.model)
  dispersiontest(step.model,alternative="greater")
  sink(sprintf("outputs/PoiReg_Stepwise_summary_%s_v6.txt",age2))
  print(summary(step.model))
  sink()
  # write.csv(summary(step.model)$coefficients,sprintf("outputs/PoiReg_Stepwise_summary_%s_v6.csv",age2))
  
  step.model$anova
  pred.val<-predict(step.model)*10
  pred.val[pred.val<0]<-0
  y<-sub.dat2[,"RIR"]
  y.IR<-sub.dat2[,"IR"]
  
  # Calculate RMSE1 using all dataset
  RMSE1<-sqrt(mean((y.IR-pred.val)^2))
  selected.cov<-names(step.model$coefficients)[-1]

  # One-leave out cross-validation
  x<-sub.dat2[,selected.cov]    
  temp.dat<-cbind(y,x)

  pred.val.CV1<-matrix(NA,nrow(temp.dat),ncol=3)
  colnames(pred.val.CV1)<-c("pred0","lower0","upper0")
  
  for(i in 1:nrow(temp.dat)){
    train.dat<-temp.dat[-i,]
    test.dat<-temp.dat[i,]
    fit.best<-glm(as.formula(paste("y~",paste(selected.cov,collapse="+"),sep="")),data=train.dat,family=poisson())
    pred.CV<-add_ci(test.dat, fit.best, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)
    pred.val.CV1[i,"pred0"]<-pred.CV$pred*10
    pred.val.CV1[i,"lower0"]<-pred.CV$lpb*10
    pred.val.CV1[i,"upper0"]<-pred.CV$upb*10
  }
  
  pred.val.CV2<-pred.val.CV1
  pred.val.CV2[pred.val.CV2<0]<-0   # adjust the prediction value
  pred.val.CV2[pred.val.CV2>=1e4]<-1e4   # adjust the prediction value
  pred.val.CV<-cbind(pred.val.CV1,pred.val.CV2)
  colnames(pred.val.CV)<-c("pred0","lower0","upper0","pred","lower","upper")
  
  RMSE2<-sqrt(mean((y.IR-pred.val.CV[,"pred"])^2))
  MAE<-mean(abs(y.IR-pred.val.CV[,"pred"]))
  
  RMSE[j,]<-c(RMSE1,RMSE2,MAE)
  round(RMSE,2)
  
  pred.dat<-cbind(y=y.IR,pred.val,pred.val.CV)
  # write.csv(pred.dat,sprintf("outputs/Prediction_Age_%s_PoiReg_Stepwise_v6.csv",age2))
  
}

RMSE.output<-cbind(age.group,RMSE)
RMSE.output
# write.csv(RMSE.output,"outputs/PoiReg_Stepwise_RMSE_v6.csv")

#############################################################
# 2-1. NB regression model with stepwise [Ver1]
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  
cov.name.1<-c("annual_rainfall","HIV_prev","elevation","distance_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  # "piped_water","travel_time_city","improved_water",(high correlated and low correlated with RIR),
#cov.name.2<-c("improved_water","annual_rainfall","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","pop_size")  #"wasting",
# For avoiding the variable selection errors
cov.name.2<-c("improved_water","annual_rainfall","distance_water","surface_water","pop_size")  #"wasting","HIV_prev","travel_time_city","piped_water","piped_sanitation","open_defecation","elevation",

cov.name.3<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  
cov.name.4<-c("improved_water","annual_rainfall","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","pop_size")  

RMSE<-matrix(NA,length(age.group),3)
colnames(RMSE)<-c("RMSE","RMSE.CV","MAE.CV")

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(age == "0-1 y") {age2<-age; cov.name2=cov.name.1}
  if(age == "2-4 y") {age2<-age; cov.name2=cov.name.2}
  if(age =="5-14 y") { age2<-age; cov.name2=cov.name.3 }
  if(age == ">14 y") {age2<-"over 14y"; cov.name2=cov.name.4}
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year, RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)

  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm.nb(formula2,data=sub.dat2,maxit=1000)
  if(j!=2) {  step.model<-stepAIC(model2, direction="both",trace = FALSE)}
  if(j==2) {  step.model<-model2}
  
  sink(sprintf("outputs/NBReg_Stepwise_summary_%s_v6.txt",age2))
  print(summary(step.model))
  sink()
  # write.csv(summary(step.model)$coefficients,sprintf("outputs/NBReg_Stepwise_summary_%s_v6.csv",age2))
  
  step.model$anova
  pred.val<-predict(step.model)*10
  pred.val[pred.val<0]<-0
  y<-sub.dat2[,"RIR"]
  y.IR<-sub.dat2[,"IR"]
  
  # Calculate RMSE1 using all dataset
  RMSE1<-sqrt(mean((y.IR-pred.val)^2))
  selected.cov<-names(step.model$coefficients)[-1]
  
  # One-leave out cross-validation
  x<-sub.dat2[,selected.cov]    
  temp.dat<-cbind(y,x)
  
  pred.val.CV1<-matrix(NA,nrow(temp.dat),ncol=3)
  colnames(pred.val.CV1)<-c("pred0","lower0","upper0")
  
  for(i in 1:nrow(temp.dat)){
    train.dat<-temp.dat[-i,]
    test.dat<-temp.dat[i,]
    fit.best<-glm.nb(as.formula(paste("y~",paste(selected.cov,collapse="+"),sep="")),data=train.dat)
    pred.CV<-add_ci(test.dat, fit.best, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000,response=TRUE)
    pred.val.CV1[i,"pred0"]<-pred.CV$pred*10
    pred.val.CV1[i,"lower0"]<-pred.CV$lpb*10
    pred.val.CV1[i,"upper0"]<-pred.CV$upb*10
  }
  
  pred.val.CV2<-pred.val.CV1
  pred.val.CV2[pred.val.CV2<0]<-0   # adjust the prediction value
  pred.val.CV2[pred.val.CV2>=1e4]<-1e4   # adjust the prediction value
  pred.val.CV<-cbind(pred.val.CV1,pred.val.CV2)
  colnames(pred.val.CV)<-c("pred0","lower0","upper0","pred","lower","upper")
  
  RMSE2<-sqrt(mean((y.IR-pred.val.CV[,"pred"])^2))
  MAE<-mean(abs(y.IR-pred.val.CV[,"pred"]))
  
  RMSE[j,]<-c(RMSE1,RMSE2,MAE)
  round(RMSE,2)
  
  pred.dat<-cbind(y=y.IR,pred.val,pred.val.CV)
  round(pred.dat)
  # write.csv(pred.dat,sprintf("outputs/Prediction_Age_%s_NBReg_Stepwise_v6.csv",age2))
  
}

RMSE.output<-cbind(age.group,RMSE)
RMSE.output

# write.csv(RMSE.output,"output/NBReg_stepwise_RMSE_v6.csv")

#############################################################
# 2-2. NB regression model with Variable selection for dropping non-significant varuable and smaller RMSE
#############################################################
# For variable selection 
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  
cov.name.1<-c("elevation","distance_water","piped_sanitation","surface_water","wasting","pop_size")  # "piped_water","travel_time_city","improved_water",(high correlated and low correlated with RIR),
# For avoiding the variable selection errors
#cov.name.2<-c("improved_water","annual_rainfall","distance_water","surface_water","pop_size")  #"open_defecation","wasting","piped_water","piped_sanitation","HIV_prev","travel_time_city","elevation",
cov.name.2<-c("improved_water","distance_water","surface_water","pop_size")  #"open_defecation","wasting","piped_water","piped_sanitation","HIV_prev","travel_time_city","elevation",
cov.name.3<-c("distance_water","piped_water","surface_water")    #,"annual_rainfall",,"wasting"
cov.name.4<-c("annual_rainfall","stunting_prev","distance_water","piped_water","piped_sanitation","surface_water")   #,"pop_size"

RMSE<-matrix(NA,length(age.group),3)
colnames(RMSE)<-c("RMSE","RMSE.CV","MAE.CV")

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(age == "0-1 y") {age2<-age; cov.name2=cov.name.1}
  if(age == "2-4 y") {age2<-age; cov.name2=cov.name.2}
  if(age =="5-14 y") { age2<-age; cov.name2=cov.name.3 }
  if(age == ">14 y") {age2<-"over 14y"; cov.name2=cov.name.4}
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year, RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)

  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm.nb(formula2,data=sub.dat2,maxit=1000)
  step.model<-model2
  summary(step.model)
  
  sink(sprintf("outputs/NBReg_Stepwise_summary_%s_v6_VS.txt",age2))
  print(summary(step.model))
  sink()
  # write.csv(summary(step.model)$coefficients,sprintf("outputs/NBReg_Stepwise_summary_%s_v6_VS.csv",age2))
  
  step.model$anova
  pred.val<-predict(step.model)*10
  pred.val[pred.val<0]<-0
  y<-sub.dat2[,"RIR"]
  y.IR<-sub.dat2[,"IR"]
  
  # Calculate RMSE1 using all dataset
  RMSE1<-sqrt(mean((y.IR-pred.val)^2))
  selected.cov<-names(step.model$coefficients)[-1]
  
  # One-leave out cross-validation
  x<-sub.dat2[,selected.cov]    
  temp.dat<-cbind(y,x)
  
  pred.val.CV1<-matrix(NA,nrow(temp.dat),ncol=3)
  colnames(pred.val.CV1)<-c("pred0","lower0","upper0")
  
  for(i in 1:nrow(temp.dat)){
    train.dat<-temp.dat[-i,]
    test.dat<-temp.dat[i,]
    fit.best<-glm.nb(as.formula(paste("y~",paste(selected.cov,collapse="+"),sep="")),data=train.dat)
    pred.CV<-add_ci(test.dat, fit.best, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000,response=TRUE)
    pred.val.CV1[i,"pred0"]<-pred.CV$pred*10
    pred.val.CV1[i,"lower0"]<-pred.CV$lpb*10
    pred.val.CV1[i,"upper0"]<-pred.CV$upb*10
  }
  
  pred.val.CV2<-pred.val.CV1
  pred.val.CV2[pred.val.CV2<0]<-0   # adjust the prediction value
  pred.val.CV2[pred.val.CV2>=1e4]<-1e4   # adjust the prediction value
  pred.val.CV<-cbind(pred.val.CV1,pred.val.CV2)
  colnames(pred.val.CV)<-c("pred0","lower0","upper0","pred","lower","upper")
  
  RMSE2<-sqrt(mean((y.IR-pred.val.CV[,"pred"])^2,na.rm=T))
  MAE<-mean(abs(y.IR-pred.val.CV[,"pred"]))
  
  RMSE[j,]<-c(RMSE1,RMSE2,MAE)
  round(RMSE,2)
  
  pred.dat<-cbind(y=y.IR,pred.val,pred.val.CV)
  round(pred.dat)
  # write.csv(pred.dat,sprintf("outputs/Prediction_Age_%s_NBReg_Stepwise_v6_VS.csv",age2))
  
}

RMSE.output<-cbind(age.group,RMSE)
RMSE.output

# write.csv(RMSE.output,"outputs/NBReg_stepwise_RMSE_v6_VS.csv")
```

### Prepare covariates for prediction

I may change this code simply to raster::resample

``` r
library(viridis)
library(raster)
library(ggplot2)
library(dplyr)
library(sp)

###############################################
# 1. Check the locations for prediction 
# weather covariates' locations are not equal to other covariates.
###############################################
cov.name<-c("elevation","distance_water","improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  

temp<-NULL
N.case<-rep(NA,length(cov.name))

for(i in 1:length(cov.name)){     # length(cov.name)
covname2<-cov.name[i]
if(i<=2) {title.name<-sprintf("%s_20km_africa",covname2)  }
if(i>2 && i<16) {title.name<-sprintf("%s_20km_af_2017_20220802",covname2)  }
if(i==16) {title.name<-"ppp_20km_mean_af_2017_20221123"  }
cov.dat <- readRDS(sprintf("data/prediction/%s.rds",title.name))
raster<- cov.dat
rst_points <- raster::rasterToPoints(raster)
rst_points_df.1 <- as.data.frame(rst_points)   # changed function
colnames(rst_points_df.1) <- c("lon", "lat", "val")  
dim(rst_points_df.1)
summary(rst_points_df.1)

coord.temp<-cbind(rst_points_df.1$lon, rst_points_df.1$lat)
pred.val<-rst_points_df.1$val
pred.pts<-SpatialPointsDataFrame(coords=coord.temp,data=data.frame(pred.val))
pred.df.raster<-rasterFromXYZ(pred.pts,crs="+proj=longlat +datum=WGS84 +no_defs")   # convert to raster type

N.case[i]<-nrow(rst_points_df.1)
temp<-rbind(temp,as.matrix(rst_points_df.1[,1:2]))
}

N.case  #  92091 92732 62799 62782 92023 92066 91802 72842 92068 62799 62782 62799 62782 91802 91802 93881
rank(N.case)

# Pick the location for the smallest case & draw the map
i=4  # 62782
covname2<-cov.name[i]
title.name<-sprintf("%s_20km_af_2017_20220802",covname2)  
cov.dat <- readRDS(sprintf("data/prediction/%s.rds",title.name))
raster<- cov.dat
rst_points <- raster::rasterToPoints(raster)
rst_points_df.1 <- as.data.frame(rst_points)   # changed function
colnames(rst_points_df.1) <- c("lon", "lat", "val")  
temp2<-as.data.frame(rst_points_df.1[,1:2])

# Save the location information
# write.csv(temp2,"outputs/locations_for_prediction_v6.csv",row.names=FALSE)


##############################################
# 2. Make the covariates for prediction
##############################################
cov.name<-c("elevation","distance_water","improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  

loc.pred<-read.csv("outputs/locations_for_prediction_v6.csv")
new.dat<-matrix(NA,nrow=nrow(loc.pred),ncol=(2+length(cov.name)))    # 62782
new.dat[,1:2]<-as.matrix(loc.pred)
colnames(new.dat)<-c("lon", "lat",cov.name)

for (i in 1:length(cov.name)) {
  covname2<-cov.name[i]
  if(i<=2) {title.name<-sprintf("%s_20km_africa",covname2)  }
  if(i>2 && i<16) {title.name<-sprintf("%s_20km_af_2017_20220802",covname2)  }
  if(i==16) {title.name<-"ppp_20km_mean_af_2017_20221123"  }
  cov.dat <- readRDS(sprintf("data/prediction/%s.rds",title.name))
  raster<- cov.dat
  rst_points <- raster::rasterToPoints(raster)
  rst_points_df <- as.data.frame(rst_points)   # changed function
  colnames(rst_points_df) <- c("lon", "lat", "val")  
  
  for(j in 1:nrow(new.dat)){
    temp1<-subset(rst_points_df,lon>=(new.dat[j,"lon"]-0.5) & lon<=(new.dat[j,"lon"]+0.5) & lat>=(new.dat[j,"lat"]-0.5) & lat<=(new.dat[j,"lat"]+0.5))
    dist.temp1<-spDistsN1(cbind(temp1$lon,temp1$lat),new.dat[j,1:2],longlat=TRUE)
    temp2<-cbind(temp1,dist=dist.temp1)
    if(nrow(temp2)!=0){
    temp3<-subset(temp2,temp2$dist==min(temp2$dist))
    new.dat[j,cov.name[i]]<-mean(temp3$val)
    }
  }
}

colnames(new.dat)<-c("lon", "lat",cov.name)
summary(new.dat)
dim(new.dat) # 62782    17
new.dat2<-na.omit(new.dat) # 59420    17
# write.csv(new.dat,"outputs/covariates_for_prediction_v6.csv",row.names=FALSE)  
```

### Forecasting for the log-linear model

``` r
library(car)
library(MASS)
library(ciTools)
library(raster)
library(plyr)

# Load modeling dataset
dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000

# Load forecasting covariate dataset
pred.cov<-read.csv("outputs/covariates_for_prediction_v6.csv")
pred.cov$annual_rainfall[pred.cov$annual_rainfall==0]<-0.1
pred.cov$annual_rainfall<-log(pred.cov$annual_rainfall)
pred.cov$pop_size[pred.cov$pop_size==0]<-0.1
pred.cov$pop_size<-log(pred.cov$pop_size)
pred.cov$elevation<-pred.cov$elevation/1000
pred.cov<-na.omit(pred.cov)
dim(pred.cov) #  59420    17

age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

#############################################################
# 1. Forecasting & Convert forecasting values into raster file
#############################################################
# Linear Reg with final variable selection
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(j<=3) {age2<-age;cov.name2<-read.csv(sprintf("outputs/LReg_Stepwise_summary_%s_v6.csv",age2))[-1,1]}
  if(j==4) { age2<-"over 14y";cov.name2<-read.csv(sprintf("outputs/LReg_Stepwise_summary_%s_v6.csv",age2))[-1,1] }

  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,LogIR=log(sub.dat$Standardized_IR_100000_Year+0.1),sub.dat[,cov.name2])
  if(j==4) {colnames(sub.dat2)[3]<-"wasting"}
  sub.dat2<-data.frame(na.omit(sub.dat2))
  
  formula2<-as.formula(paste("LogIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm(formula2,data=sub.dat2,family = gaussian)
  
  # for prediction using add_pi
  LogIR=rep(0,nrow(pred.cov))
  test.dat<-data.frame(cbind(pred.cov[,cov.name2],LogIR))
  if(j==4) {colnames(test.dat)[1]<-"wasting"}
  pred.val1<-add_pi(test.dat, model2, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)[,c("pred","lpb","upb")]
  pred.val1<- exp(pred.val1)
  pred.val2<-pred.val1
  pred.val2[pred.val2>=1e4]<-1e4   # adjust the prediction value
  
  pred2<-cbind(pred.cov[,1:2],pred.val1,pred.val2)  
  colnames(pred2)<-c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  # write.csv(pred2,sprintf("outputs/Forecasting/Forecasting_Age_%s_LReg_Stepwise_v6.csv",age2),row.names=FALSE) 
  
  pred.dat2<-na.omit(pred2)
  pred.df <- as.data.frame(pred.dat2)   # changed function
  colnames(pred.df) <- c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  
  for(i in c("pred.o","lower.o","upper.o","pred","lower","upper")) {
    coord.temp<-cbind(pred.df$lon, pred.df$lat)
    pred.val<-pred.df[,i]
    pred.pts<-SpatialPointsDataFrame(coords=coord.temp,data=data.frame(pred.val))
    pred.df.raster<-rasterFromXYZ(pred.pts,crs="+proj=longlat +datum=WGS84 +no_defs")   # convert to raster type
    # writeRaster(pred.df.raster,file=sprintf("outputs/Forecasting/Forecasting_%s_Age_%s_LReg_Stepwise_v6.tif",i,age2),overwrite=TRUE)
  }
}  
```

### Forecasting for the NegBin (ver 1)

``` r
library(car)
library(MASS)
library(ciTools)
library(raster)
library(plyr)


#setwd("D:/SynologyDrive/Research/Collaboration/IVI_Kim/codes/")

# Load modeling dataset
dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000

# Load forecasting covariate dataset
pred.cov<-read.csv("output/covariates_for_prediction_v6.csv")
pred.cov$annual_rainfall[pred.cov$annual_rainfall==0]<-0.1
pred.cov$annual_rainfall<-log(pred.cov$annual_rainfall)
pred.cov$pop_size[pred.cov$pop_size==0]<-0.1
pred.cov$pop_size<-log(pred.cov$pop_size)
pred.cov$elevation<-pred.cov$elevation/1000
pred.cov<-na.omit(pred.cov)
dim(pred.cov) #  59420    17

age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

#############################################################
# 1-1. Forecasting & Convert forecasting values into raster file 
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  


for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(j<=3) {age2<-age;cov.name2<-read.csv(sprintf("output/NBReg_Stepwise_summary_%s_v6.csv",age2))[-1,1]}
  if(j==4) { age2<-"over 14y";cov.name2<-read.csv(sprintf("output/NBReg_Stepwise_summary_%s_v6.csv",age2))[-1,1] }
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)
  
  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm.nb(formula2,data=sub.dat2,maxit=1000)
  
  # for prediction using add_pi
  RIR=rep(0,nrow(pred.cov))
  test.dat<-cbind(pred.cov[,cov.name2],RIR)    
  pred.val1<-add_pi(test.dat, model2, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)[,c("pred","lpb","upb")]
  pred.val1<- pred.val1*10
  pred.val2<-pred.val1
  pred.val2[pred.val2>=1e4]<-1e4   # adjust the prediction value
  
  pred2<-cbind(pred.cov[,1:2],pred.val1,pred.val2) 
  colnames(pred2)<-c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  write.csv(pred2,sprintf("output/Forecasting/Forecasting_Age_%s_NBReg_Stepwise_v6_Ver1.csv",age2),row.names=FALSE) 
  
  pred.dat2<-na.omit(pred2)
  pred.df <- as.data.frame(pred.dat2)   # changed function
  colnames(pred.df) <- c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  
  for(i in c("pred.o","lower.o","upper.o","pred","lower","upper")) {
    coord.temp<-cbind(pred.df$lon, pred.df$lat)
    pred.val<-pred.df[,i]
    pred.pts<-SpatialPointsDataFrame(coords=coord.temp,data=data.frame(pred.val))
    pred.df.raster<-rasterFromXYZ(pred.pts,crs="+proj=longlat +datum=WGS84 +no_defs")   # convert to raster type
    writeRaster(pred.df.raster,file=sprintf("output/Forecasting/Forecasting_%s_Age_%s_NBReg_Stepwise_v6_Ver1.tif",i,age2),overwrite=TRUE)
  }
}  
```

### Forecasting for the NegBin (ver 2)

``` r
library(car)
library(MASS)
library(ciTools)
library(raster)
library(plyr)


#setwd("D:/SynologyDrive/Research/Collaboration/IVI_Kim/codes/")

# Load modeling dataset
dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000

# Load forecasting covariate dataset
pred.cov<-read.csv("output/covariates_for_prediction_v6.csv")
pred.cov$annual_rainfall[pred.cov$annual_rainfall==0]<-0.1
pred.cov$annual_rainfall<-log(pred.cov$annual_rainfall)
pred.cov$pop_size[pred.cov$pop_size==0]<-0.1
pred.cov$pop_size<-log(pred.cov$pop_size)
pred.cov$elevation<-pred.cov$elevation/1000
pred.cov<-na.omit(pred.cov)
dim(pred.cov) #  59420    17

age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

#############################################################
# 1-1. Forecasting & Convert forecasting values into raster file 
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(j<=3) {age2<-age;cov.name2<-read.csv(sprintf("output/NBReg_Stepwise_summary_%s_v6_VS.csv",age2))[-1,1]}
  if(j==4) { age2<-"over 14y";cov.name2<-read.csv(sprintf("output/NBReg_Stepwise_summary_%s_v6_VS.csv",age2))[-1,1] }
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)
  
  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm.nb(formula2,data=sub.dat2,maxit=1000)
  
  # for prediction using add_pi
  RIR=rep(0,nrow(pred.cov))
  test.dat<-cbind(pred.cov[,cov.name2],RIR)    
  pred.val1<-add_pi(test.dat, model2, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)[,c("pred","lpb","upb")]
  pred.val1<- pred.val1*10
  pred.val2<-pred.val1
  pred.val2[pred.val2>=1e4]<-1e4   # adjust the prediction value
  
  pred2<-cbind(pred.cov[,1:2],pred.val1,pred.val2) 
  colnames(pred2)<-c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  write.csv(pred2,sprintf("output/Forecasting/Forecasting_Age_%s_NBReg_Stepwise_v6_Ver2.csv",age2),row.names=FALSE) 
  
  pred.dat2<-na.omit(pred2)
  pred.df <- as.data.frame(pred.dat2)   # changed function
  colnames(pred.df) <- c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  
  for(i in c("pred.o","lower.o","upper.o","pred","lower","upper")) {
    coord.temp<-cbind(pred.df$lon, pred.df$lat)
    pred.val<-pred.df[,i]
    pred.pts<-SpatialPointsDataFrame(coords=coord.temp,data=data.frame(pred.val))
    pred.df.raster<-rasterFromXYZ(pred.pts,crs="+proj=longlat +datum=WGS84 +no_defs")   # convert to raster type
    writeRaster(pred.df.raster,file=sprintf("output/Forecasting/Forecasting_%s_Age_%s_NBReg_Stepwise_v6_Ver2.tif",i,age2),overwrite=TRUE)
  }
}  
```

### Forecasting for the Poisson

``` r
library(car)
library(MASS)
library(ciTools)
library(raster)
library(plyr)


#setwd("D:/SynologyDrive/Research/Collaboration/IVI_Kim/codes/")

# Load modeling dataset
dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
dat$annual_rainfall<-log(dat$annual_rainfall)
dat$pop_size<-log(dat$pop_size)
dat$elevation<-dat$elevation/1000

# Load forecasting covariate dataset
pred.cov<-read.csv("output/covariates_for_prediction_v6.csv")
pred.cov$annual_rainfall[pred.cov$annual_rainfall==0]<-0.1
pred.cov$annual_rainfall<-log(pred.cov$annual_rainfall)
pred.cov$pop_size[pred.cov$pop_size==0]<-0.1
pred.cov$pop_size<-log(pred.cov$pop_size)
pred.cov$elevation<-pred.cov$elevation/1000
pred.cov<-na.omit(pred.cov)
dim(pred.cov) #  59420    17

age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")


#############################################################
# 1. Forecasting & Convert forecasting values into raster file
#############################################################
cov.name<-c("improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","elevation","distance_water","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")  

for(j in 1:length(age.group)) { 
  age<-age.group[j]
  if(j<=3) {age2<-age;cov.name2<-read.csv(sprintf("output/PoiReg_Stepwise_summary_%s_v6.csv",age2))[-1,1]}
  if(j==4) { age2<-"over 14y";cov.name2<-read.csv(sprintf("output/PoiReg_Stepwise_summary_%s_v6.csv",age2))[-1,1] }
  
  sub.dat<-subset(dat,dat$Age_Group_Edit==age)
  sub.dat2<-cbind(IR=sub.dat$Standardized_IR_100000_Year,RIR=round(sub.dat$Standardized_IR_100000_Year/10),sub.dat[,cov.name2])
  sub.dat2<-na.omit(sub.dat2)
  
  formula2<-as.formula(paste("RIR~",paste(cov.name2,collapse="+"),sep=""))
  model2<-glm(formula2,data=sub.dat2,family=poisson())
  
  # for prediction using add_pi
  RIR=rep(0,nrow(pred.cov))
  test.dat<-cbind(pred.cov[,cov.name2],RIR)    
  pred.val1<-add_pi(test.dat, model2, names = c("lpb", "upb"), alpha = 0.05, nsims = 20000)[,c("pred","lpb","upb")]
  pred.val1<- pred.val1*10
  pred.val2<-pred.val1
  pred.val2[pred.val2>=1e4]<-1e4   # adjust the prediction value
  
  pred2<-cbind(pred.cov[,1:2],pred.val1,pred.val2) 
  colnames(pred2)<-c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  write.csv(pred2,sprintf("output/Forecasting/Forecasting_Age_%s_PoiReg_Stepwise_v6.csv",age2),row.names=FALSE) 
  
  pred.dat2<-na.omit(pred2)
  pred.df <- as.data.frame(pred.dat2)   # changed function
  colnames(pred.df) <- c("lon","lat","pred.o","lower.o","upper.o","pred","lower","upper") # original estimates and estimates with upper limit
  
  for(i in c("pred.o","lower.o","upper.o","pred","lower","upper")) {
    coord.temp<-cbind(pred.df$lon, pred.df$lat)
    pred.val<-pred.df[,i]
    pred.pts<-SpatialPointsDataFrame(coords=coord.temp,data=data.frame(pred.val))
    pred.df.raster<-rasterFromXYZ(pred.pts,crs="+proj=longlat +datum=WGS84 +no_defs")   # convert to raster type
    writeRaster(pred.df.raster,file=sprintf("output/Forecasting/Forecasting_%s_Age_%s_PoiReg_Stepwise_v6.tif",i,age2),overwrite=TRUE)
  }
}  
```

### Plots

#### Figure 1

#### Figure 2

#### Figure 3

#### Figure 4
