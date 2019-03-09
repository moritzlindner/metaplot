tbltheme<-ggplot2::theme_minimal() + 
  ggplot2::theme(legend.position = "none",
        panel.border = ggplot2::element_blank(), 
        panel.grid.major = ggplot2::element_blank(),
        panel.grid.minor = ggplot2::element_blank(),
        axis.text.x = ggplot2::element_text(colour="black",size=8,face="bold", vjust=1),
        axis.text.y = ggplot2::element_blank(), 
        axis.ticks = ggplot2::element_line(colour="white"),
        strip.background = ggplot2::element_rect(fill = "white", colour="grey"),
        strip.placement = "inside",
        plot.margin = grid::unit(c(0,0,0,0), "lines"))
