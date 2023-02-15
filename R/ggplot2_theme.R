theme_pub <- function( base_size=10, base_family="sans" ) {
    library(grid)
    library(ggthemes)
    ( theme_foundation( base_size=base_size, base_family=base_family )
        + theme( 
            plot.title = element_text( face="bold", size=rel(1.2), hjust=0.5 ),
            text = element_text(),
            panel.background = element_rect( colour=NA ),
            plot.background = element_rect( colour=NA ),
            panel.border = element_rect( colour=NA ),
            axis.title = element_text( face="plain", size=rel(1) ),
            axis.title.y = element_text( angle=90, vjust=2 ),
            axis.title.x = element_text( vjust=-0.1 ),
            axis.text = element_text(), 
            axis.line = element_line( colour="black" ),
            axis.ticks = element_line(),
            panel.grid.major = element_line( colour="#f0f0f0" ),
            panel.grid.minor = element_blank(),
            legend.key = element_rect( fill="transparent", colour=NA ),
            legend.position = "bottom",
            legend.direction = "horizontal",
            legend.key.size = unit( 2, "mm" ),
            legend.spacing = unit( 0.2, "mm" ),
            legend.margin = margin( 0, 0, 0, 0, unit="mm" ),
            legend.title = element_text( face="plain" ),
            legend.box.background = element_rect( fill="transparent", colour=NA ),
            legend.background = element_rect( fill="transparent", colour=NA ),
            plot.margin = unit( c(1,1,1,1), "mm" ),
            strip.background = element_rect( colour="#f0f0f0", fill="#f0f0f0" ),
            strip.text = element_text( face="bold" )
        ) )
}

## custom theme for plotting a map
theme_map <- function() {
  theme( 
    plot.background = element_blank(),
    panel.background = element_blank(), # bg of the panel
    legend.background = element_blank(), # get rid of legend bg
    legend.box.background = element_blank(),
    panel.spacing = unit( c(0,0,0,0), "null" ),
    plot.margin = unit( c(0,0,0,0), "null" ),
    axis.line = element_blank(), 
    axis.text.x = element_blank(),
    axis.text.y = element_blank(), 
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = c(0.25,0.38) )  
}

scale_fill_pub <- function(...){
    library(scales)
    discrete_scale("fill","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}

scale_colour_pub <- function(...){
    library(scales)
    discrete_scale("colour","Publication",manual_pal(values = c("#386cb0","#fdb462","#7fc97f","#ef3b2c","#662506","#a6cee3","#fb9a99","#984ea3","#ffff33")), ...)
    
}