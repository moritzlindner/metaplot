#' Internal Function
#'
#' Creates the actual forest plot. Called by Forest_Plot
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param type Type of Data shown (e.g. Sensitivity). Is X axis Lable
#' @param median Median
#' @param lci Lower boundary of confidence interval
#' @param uci Upper boundary of confidence interval
#' @param weightmark column with values for weightings
#' @export
#' @return none

Bargraph <- function(source_tbl,
                     study,
                     group_var=NULL,
                     type="Sensitivity",
                     median="sens",
                     lci="sens.ci.lower",
                     uci="sens.ci.upper",
                     weightmark=NULL){
  if (!is.null(weightmark)){
    source_tbl$markersize<-scale(as.numeric(source_tbl[[weightmark]]), center=FALSE)
  }else{
    source_tbl$markersize<-1
  }
  
  if (!is.null(group_var)){
    source_tbl$group<-source_tbl[[group_var]]
  }
  
  bg<-ggplot2::ggplot(data=source_tbl,ggplot2::aes_string(y = study,x = median, xmin = lci, xmax = uci))+
    ggplot2::geom_point(ggplot2::aes(size=markersize),shape=18)+
    ggplot2::geom_errorbarh(height=0.5)+
    ggplot2::labs(x=type,y="")+
    ggplot2::scale_x_continuous(position = "top") +
    ggplot2::theme_void() +
    ggplot2::scale_size(range=c(2,4))+
    ggplot2::theme(text = ggplot2::element_text(vjust=0,size=8,face="plain",
                                       color="black",angle = 0, hjust=0, 
                                       lineheight=20,margin=ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
          axis.text.x = ggplot2::element_text(size=8),
          axis.title.x = ggplot2::element_text(size=8,face="bold", hjust=0.5),
          axis.text.y=ggplot2::element_blank(),
          panel.grid.major.x = ggplot2::element_line(size = 0.5, colour = "gray",linetype = "solid",
                                            lineend = "butt", arrow = NULL,
                                            inherit.blank = FALSE),
          axis.line.x = ggplot2::element_line(size = 1, colour = "gray",linetype = "solid",
                                              lineend = "butt", arrow = NULL,
                                              inherit.blank = FALSE),
          axis.ticks.x = ggplot2::element_line(size = 2, colour = "gray",linetype = "solid",
                                               lineend = "square", arrow = NULL,
                                               inherit.blank = FALSE),
          axis.ticks.y=ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank(),
          legend.position = "none",
          plot.margin = ggplot2::unit(c(1, 1, 1, 1), "pt"))+
    ggplot2::guides(x=ggplot2::element_blank())
  
  if (!is.null(group_var)){
    bg<-bg+ggplot2::facet_grid(group~., scales = "free", space = "free",switch = "y")
  }
  return(bg)
}
