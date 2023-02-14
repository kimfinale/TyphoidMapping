loglinear_model <- function(){

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
  ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
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
  ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
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
  ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
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
  ggpairs(cbind(LogIR=sub.dat2$LogIR, RIR=sub.dat2$RIR, sub.dat2[,cov.name]),title="Pearson's corr.")
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
    sink(sprintf("output/LReg_Stepwise_summary_%s_v6.txt",age2))
    print(summary(step.model))
    sink()
    write.csv(summary(step.model)$coefficients,sprintf("output/LReg_Stepwise_summary_%s_v6.csv",age2))
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
    write.csv(pred.dat,sprintf("output/Prediction_Age_%s_LReg_Stepwise_v6.csv",age2))

  }
  RMSE.output<-cbind(age.group,RMSE)
  write.csv(RMSE.output,"output/LReg_Stepwise_RMSE_v6.csv")

  return(RMSE.output)

}

