get_prediction_covariate <- function(){
  ##############################################
  # 2. Make the covariates for prediction
  ##############################################
  cov.name<-c("elevation","distance_water","improved_water","improved_sanitation","annual_rainfall","annual_mean_temp","stunting_prev","HIV_prev","travel_time_city","piped_water","piped_sanitation","surface_water","open_defecation","wasting","underweight","pop_size")

  loc.pred<-read.csv("output/locations_for_prediction_v6.csv")
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
  write.csv(new.dat,"output/covariates_for_prediction_v6.csv",row.names=FALSE)

  return(new.dat)

}
