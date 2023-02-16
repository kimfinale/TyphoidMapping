figure_2 <- function(){
  library(data.table)
  dlist <- list()
  dlist[[1]] <- fread("data/Prediction_Age_0-1 y_PoiReg.csv")
  dlist[[2]] <- fread("data/Prediction_Age_2-4 y_NBReg.csv")
  dlist[[3]] <- fread("data/Prediction_Age_5-14 y_NBReg.csv")
  dlist[[4]] <- fread("data/Prediction_Age_over 14y_NBReg.csv")
  ages <- c("0-1 yo", "2-4 yo", "5-14 yo", "15+ yo")
  for (i in 1:4) {
    dlist[[i]]$age <- ages[i]
  }
  d <- do.call("rbind", dlist)
  d$age <- factor(d$age, levels = c("0-1 yo", "2-4 yo", "5-14 yo", "15+ yo"))

  breaks_y=c(1,10,100,1000,10000)
  breaks_x=c(1,10,100,1000,10000)

  plt <- ggplot(d) +
    geom_errorbar(aes(y+1, ymin=lower+1, ymax=upper+1), width=0)+
    geom_point(aes(y+1, pred+1)) +
    scale_x_continuous(trans = "log",
                       breaks = breaks_x, limits=c(1,max(breaks_x+1))) +
    scale_y_continuous(trans = "log",
                       breaks=breaks_y,limits=c(1,max(breaks_y+1))) +
    geom_abline(slope=1, intercept=0, linetype="dashed", color="red") +
    labs(x="Observed", y="Predicted") +
    facet_wrap(~age, nrow=2) +
    theme_bw()

  return(plt)
}
