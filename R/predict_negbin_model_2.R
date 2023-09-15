predict_negbin_model_2 <- function(){
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
  # cov.name<-c("improved_water","improved_sanitation","annual_rainfall",
  #             "annual_mean_temp","stunting_prev","HIV_prev","travel_time_city",
  #             "elevation","distance_water","piped_water","piped_sanitation",
  #             "surface_water","open_defecation","wasting",
  #             "underweight","pop_size")

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
}
