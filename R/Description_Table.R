#' Internal Function
#'
#' This function creates a six column table of the format "median ( LCI - UCI )". Called by Forest_Plot
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param group_var Column to group by. Must be contained in \emph{disp_vars}.
#' @param disp_vars List of columns to display
#' @export
NULL

Description_Table <- function(source_tbl, 
                              study,
                              group_var=NULL,
                              disp_vars=NULL){
  if (is.null(disp_vars)){
    disp_vars<-study
  }
  labtbl<-as.data.frame(source_tbl[,c(study,disp_vars)])
  colnames(labtbl)[1]<-"study"
  labtbl<-tidyr::gather(labtbl,key,value,-c("study",group_var))

  if (!is.null(group_var)){
    labtbl$group<-labtbl[[group_var]]
    labtbl$group<-as.factor(labtbl$group)
  }
  
  data_table <- ggplot2::ggplot(labtbl, ggplot2::aes(x = key, y = study, label = format(value, nsmall = 1))) +
    ggplot2::geom_raster(fill="white", color="black") +
    ggfittext::geom_fit_text(size = 8, min.size = 0, reflow = T, place="left",height=1) + 
    ggplot2::scale_x_discrete(position = "top") +
    tbltheme+
    ggplot2::labs(x="",y="")+
    ggplot2::theme(strip.text.y = ggplot2::element_text(size=8,face="bold", vjust=1))
  
  if (!is.null(group_var)){
    data_table<-data_table+ggplot2::facet_grid(group~., scales = "free", space = "free",switch = "y")
      #facet_wrap(vars(group), ncol = 1)#, scales = "free", space = "free",switch = "y")
  }
  return(data_table)
}
