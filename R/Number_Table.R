#' Internal Function
#'
#' This function creates a six column table of the format "median ( LCI - UCI )". Called by Forest_Plot
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param group_var Column to group by
#' @param median Median
#' @param lci Lower boundary of confidence interval
#' @param uci Upper boundary of confidence interval
#' @param round_digits Number of decimal places to display
#' @importFrom tidyr unite
#' @import ggplot2
#' @export
#' @return none

Number_Table <- function(source_tbl,
                         study,
                         group_var=NULL,
                         median="sens",
                         lci="sens.ci.lower",
                         uci="sens.ci.upper",
                         round_digits=2){
  df<-as.data.frame(source_tbl[,c(study,group_var,median,lci,uci)])
  df[c(median,lci,uci)]<-round(df[c(median,lci,uci)], digits = round_digits)
  df[c(median,lci,uci)]<-format(df[c(median,lci,uci)], nsmall = 2)
  df$open<- rep(" [",length(df[[study]]))
  df$to<- rep("-",length(df[[study]]))
  df$close<- rep("]",length(df[[study]]))
  colnames(df)[1]<-"study"
  df<-unite(df,"merge",c(median,"open", lci,"to",uci,"close"),sep="")

  if (!is.null(group_var)){
    df$group<-df[[group_var]]
  }
  
  dftbl <- ggplot2::ggplot(df, ggplot2::aes(x = 1, y = as.factor(study), label = merge)) +
    ggplot2::geom_text(size = 8/ggplot2:::.pt)+
    tbltheme+
    ggplot2::labs(x="",y="")+
    ggplot2::scale_size_identity()+
    ggplot2::theme_void()+
    ggplot2::theme( axis.text.x = ggplot2::element_blank(),
          strip.text.y = ggplot2::element_blank(),
          axis.ticks=ggplot2::element_blank(),
          plot.margin = ggplot2::unit(c(1, 1, 1, 1), "pt"))
  
  if (!is.null(group_var)){
    dftbl<-dftbl+ggplot2::facet_grid(group~., scales = "free", space = "free",switch = "y")
  }
  
  return(dftbl)
}
