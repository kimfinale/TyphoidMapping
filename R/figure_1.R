figure_1 <- function(){
  dat <- fread("data/af_tf_incidence_v10_edit_cov_20221114.csv")
  dat <- dat[REGION == "Africa"]
  dat <- dat[COUNTRY != "Egypt"]
  dat[COUNTRY == "Democratic Republic of the Congo", COUNTRY := "DR Congo"]
  dat <- dat[AGE_GRP %in% c("0-1 y","2-4 y","5-14 y",">14 y")]
  dat <- dat[, YEAR := paste0(YEAR_OBSERVATION_BEGAN,"-",YEAR_OBSERVATION_ENDED) ]
  dat <- dat[, LOCATION := paste0(SITE, ", ", COUNTRY) ]

  # plot the incidence rates standardized
  dat[, IR_plotting := IR * BLOOD_CULTURE_SENSITIVITY] #
  dat[, IR_LOWER_plotting := IR_LOWER * BLOOD_CULTURE_SENSITIVITY] #
  dat[, IR_UPPER_plotting := IR_UPPER * BLOOD_CULTURE_SENSITIVITY]
  dat[, AGE_GRP := age_grp_new] # age_grp_new has some simplifications for AGE_GRP (eg, 5-15 yo -> 5-14 yo)

  dat <- dat[, c("SITE","COUNTRY","LOCATION","AGE_GRP",
                 "YEAR","IR_plotting","IR_LOWER_plotting","IR_UPPER_plotting")]

  dat$AGE_GRP <-
    factor(dat$AGE_GRP,
           levels = c("0-1 y","2-4 y","5-14 y",">14 y"))
  dat$YEAR <- factor(dat$YEAR, levels = unique(dat$YEAR)[order(unique(dat$YEAR))])

  dat <- dat[!(is.na(IR_plotting)), ]
  dat <- dat[order(dat$COUNTRY),]
  dat$LOCATION <- factor(dat$LOCATION, levels=unique(dat$LOCATION))
  # blues <- brewer.pal(9,"Blues")
  # blue_range <- colorRampPalette(blues)
  # goldred <- c("gold","darkred")
  # color_range <- colorRampPalette(goldred)

  mycolors <- c("dodgerblue2", "#E31A1C", "green4", "#6A3D9A", "#FF7F00",
                "black", "gold1", "skyblue2", "palegreen2", "#FDBF6F",
                "gray70", "maroon", "orchid1", "darkturquoise",
                "darkorange4", "brown")

  plt <- ggplot(dat, aes(x=LOCATION, y=IR_plotting+1, group=AGE_GRP)) +
    geom_point(aes(shape=AGE_GRP, colour=YEAR),
               position=position_dodge(width=0.75), size = 3) +
    scale_shape_manual(values=c(15:18), name="Age group") +
    scale_color_manual("Surveillance year", values=mycolors[1:11])+
    # scale_colour_discrete(name="Surveillance year") +
    # scale_colour_manual(values = color_range(length(unique(dat$YEAR))),
    #                     name = "Surveillance year") +
    labs(y="Incidence rate per 100,000 person years", x="") +
    geom_errorbar(aes(ymin=IR_LOWER_plotting,
                      ymax=IR_UPPER_plotting, color=YEAR),
                  position = position_dodge(width = 0.75),
                  width=0) +
    theme_bw() +
    scale_y_continuous(trans='log', breaks=c(1,10,100,1000,10000)) +
    theme(axis.text.x=element_text(angle=60,hjust=1,size=13),
          axis.text.y=element_text(size=13),
          axis.title.y=element_text(size=13),
          legend.title=element_text(size=13),
          legend.text=element_text(size=13))

  return(plt)
}
