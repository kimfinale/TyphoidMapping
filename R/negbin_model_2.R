negbin_model_2 <- function(){

  library(car)
  library(MASS)
  library(ciTools)
  library(AER)

  #setwd("D:/SynologyDrive/Research/Collaboration/IVI_Kim/codes/")

  dat<-read.csv("data/Outcome_Covariates_sites_catchment areas_v6.csv")
  dat$annual_rainfall<-log(dat$annual_rainfall)
  dat$pop_size<-log(dat$pop_size)
  dat$elevation<-dat$elevation/1000 # change the unit

  age.group<-c("0-1 y","2-4 y","5-14 y",">14 y")

  IR<-round(dat$Standardized_IR_100000_Year/10)
  summary(IR)
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

    sink(sprintf("output/NBReg_Stepwise_summary_%s_v6_VS.txt",age2))
    print(summary(step.model))
    sink()
    write.csv(summary(step.model)$coefficients,sprintf("output/NBReg_Stepwise_summary_%s_v6_VS.csv",age2))

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
    write.csv(pred.dat,sprintf("output/Prediction_Age_%s_NBReg_Stepwise_v6_VS.csv",age2))

  }

  RMSE.output<-cbind(age.group,RMSE)
  RMSE.output

  write.csv(RMSE.output,"output/NBReg_stepwise_RMSE_v6_VS.csv")

  return(RMSE.output)
}


