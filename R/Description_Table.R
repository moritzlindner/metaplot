#' Internal Function
#'
#' This function creates a six column table of the format "median ( LCI - UCI )". Called by Forest_Plot
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param group_var Column to group by. Must be contained in \emph{disp_vars}.
#' @param disp_vars List of columns to display
#' @import data.table
#' @import ggplot2
#' @importFrom tidyr gather
#' @export
#' @return none

Description_Table <- function(source_tbl, 
                              study,
                              group_var=NULL,
                              disp_vars=NULL){
  if (is.null(disp_vars)){
    disp_vars<-study
  }
  labtbl<-as.data.frame(source_tbl[,c(study,disp_vars)])
  colnames(labtbl)[1]<-"study"
  
  labtbl<-gather(labtbl,key,value,-c("study",group_var))
  
  labtbl$key <- factor(labtbl$key, levels = disp_vars)
  labtbl<-data.table::as.data.table(labtbl)
  #labtbl<-labtbl[, width:=max(nchar(value)),by=key]
  labtbl<-labtbl[, width:=(if(max(nchar(value))>max(nchar(as.character(key)))){max(nchar(value))}else{max(nchar(as.character(key)))}),by=key]
  labtbl<-as.data.frame(labtbl)
  #labtbl<-lapply(split(labtbl, labtbl$key), function(y) max(nchar(y$value)))
  if (!is.null(group_var)){
    labtbl$group<-labtbl[[group_var]]
    labtbl$group<-as.factor(labtbl$group)
  }
  
  labtbl$key <- droplevels(labtbl$key)
  for (i in 1:length(levels(labtbl$key))){
    if (i>1){
      labtbl$width[labtbl$key==levels(labtbl$key)[i]]<-labtbl$width[labtbl$key==levels(labtbl$key)[i]]+unique(labtbl$width[labtbl$key==levels(labtbl$key)[i-1]])
    }else{
      #labtbl$width[labtbl$key==levels(labtbl$key)[i]]<-0
    }
  }
  
   for (i in length(levels(labtbl$key)):1){
     if (i>1){
       labtbl$width[labtbl$key==levels(labtbl$key)[i]]<-labtbl$width[labtbl$key==levels(labtbl$key)[i-1]]
     }else{
       labtbl$width[labtbl$key==levels(labtbl$key)[i]]<-0
     }
   }
  
  maxwidth<-(max(labtbl$width)-min(labtbl$width))*1.1
  
  data_table <- ggplot2::ggplot(labtbl, ggplot2::aes(x = width, y = study, label = format(value, nsmall = 1),width=width)) +
    ggplot2::geom_text(size = 8/ggplot2:::.pt, hjust=0)+
    ggplot2::scale_x_continuous(position = "top", limits=c(0,maxwidth),breaks = unique(labtbl$width),labels=levels(labtbl$key)) +
    tbltheme+
    ggplot2::theme_void()+
    ggplot2::labs(x="",y="")+
    ggplot2::theme(strip.text.y = ggplot2::element_text(size=8,face="bold", vjust=1,margin=ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "pt")),
                  strip.background = ggplot2::element_rect(color="black", fill="white"),
                  axis.text.x = ggplot2::element_text(size=8,face="bold", hjust=0,vjust = -1,margin=ggplot2::margin(t = 1, r = 1, b = 1, l = 1, unit = "pt")),
                  plot.margin = ggplot2::unit(c(1, 1, 1, 1), "pt"))
  
  if (!is.null(group_var)){
    data_table<-data_table+ggplot2::facet_grid(group~., scales = "free", space = "free",switch = "y")
      #facet_wrap(vars(group), ncol = 1)#, scales = "free", space = "free",switch = "y")
  }
  data_table$maxwidth<-maxwidth
  return(data_table)
}
