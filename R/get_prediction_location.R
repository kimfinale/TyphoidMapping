get_prediction_location <- function(){


  ###############################################
  # 1. Check the locations for prediction
  # weather covariates' locations are not equal to other covariates.
  ###############################################

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
  write.csv(temp2,"output/locations_for_prediction_v6.csv",row.names=FALSE)

  return(temp2)
}
