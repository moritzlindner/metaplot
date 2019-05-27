
#' A forest plot for diagnostic accuracy studies
#'
#' This function creates a six column table of the format "median ( LCI - UCI )". Called by Forest_Plot
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param group_var Column variable containig group identifiers for subplots. Leave empty if not required.
#' @param sort_var Column to sort rows by.
#' @param disp_vars Column to display in tabulat part of the plot
#' @param sens_vars Columns to use for Sensitivity Forest Plot (median,lci,uci)
#' @param spec_vars Columns to use for Specificity Forest Plot (median,lci,uci)
#' @param weightmark column with values for weightings
#' @export
Forest_Diag <- function(source_tbl,
                        study,
                        sort_var=NULL,
                        group_var=NULL,
                        disp_vars=NULL,
                        sens_vars=c("sens", "sens.ci.lower", "sens.ci.upper"),
                        spec_vars=c("spec", "spec.ci.lower", "spec.ci.upper"),
                        weightmark=NULL){
  if(!is.null(sort_var)){
    #source_tbl<-source_tbl[order(source_tbl[[sort_var]]),]
    source_tbl[[study]] <- factor(source_tbl[[study]], levels = source_tbl[[study]][order(source_tbl[[sort_var]], decreasing = TRUE)])
  }
  if (is.null(disp_vars)){
    disp_vars<-study
  }
  ## this is the descriptive table
  desc_tbl<-Description_Table(source_tbl, study,group_var=group_var,disp_vars)
  ## this is the sensitivity plot
  barsens<-Bargraph(source_tbl,study,group_var=group_var,type="Sensitivity", median=sens_vars[1],lci=sens_vars[2],uci=sens_vars[3],weightmark = weightmark)
  ## this is the sensitivity table
  numsens<-Number_Table(source_tbl, round_digits=2, study,group_var=group_var, median=sens_vars[1],lci=sens_vars[2],uci=sens_vars[3])
  
  ## this is the sensitivity plot
  barspec<-Bargraph(source_tbl, study,group_var=group_var,type="Specificity", median=spec_vars[1],lci=spec_vars[2],uci=spec_vars[3],weightmark = weightmark)
  
  ## this is the sensitivity table
  numspec<-Number_Table(source_tbl, round_digits=2, study,group_var=group_var, median=spec_vars[1],lci=spec_vars[2],uci=spec_vars[3])
  
  relwidths<-c(1,(c(1,0.5,1,0.5)/3))
  sp<-cowplot::plot_grid(desc_tbl, barsens,numsens, barspec, numspec, ncol=5, rel_widths=relwidths, align="h")+ggplot2::theme(plot.margin = ggplot2::unit(c(0, 0, 0, 0), "cm"))
  sp$rows<-unique(source_tbl[[study]])
  sp$minrecwidth<-desc_tbl$maxwidth/0.352778*(2/3)*0.1*2
  return(sp)
}
