
#' Draws ROC curves for diagnostic accuracy studies
#'
#' This function creates an ROC plot. Returns or prints a ggplot object.
#' @param source_tbl Dataframe containing the meta-analysis data
#' @param study Index column for unique study identifier e.g. "Obelix 50 B.c."
#' @param TP Column  name for column containing the True Positive values
#' @param FN Column  name for column containing the False Negative values.
#' @param FP Column  name for column containing the False Positive values.
#' @param TN Column  name for column containing the True Negative values.
#' @param group_var Name of the column containig group identifiers for setting marker shape. Leave empty if not required.
#' @param marker Name of the column containing values for setting marker size. Leave empty if not required.
#' @param colours List of colours for group_var. Standard is greyscale.
#' @param fontsize Font Size
#' @param plabs Should each point be labled?
#' @param axlabs X and Y axis lables, given as a list: c("X","Y")
#' @export
#' @return none

ROCplot <- function (source_tbl,
                     study,
                     TP="TP",
                     FN="FN",
                     FP="FP",
                     TN="TN",
                     group_var=NULL,
                     marker=NULL,
                     colours=NULL,
                     fontsize=8,
                     plabs=TRUE,
                     axlabs=NULL){
  dt<-dat1[,c(study,group_var,marker,TP,FN,FP,TN)]
  dt$sens<-dt[[TP]]/(dt[[TP]]+dt[[FN]])
  dt$fpr<-dt[[FP]]/(dt[[FP]]+dt[[TN]])
  
  if(is.null(group_var)){
    group_var="group"
    dt[,group_var]<-TRUE
  }
  
  # calculate marker size
  if(is.null(marker)){
    dt[,"marker"]<-1/15
  }else{
    dt[["marker"]]<-(dt[[marker]]/max(dt[[marker]])*5)+5
  }
  marker="marker"
  print(dt)
  # make summary stats graph
  se<-NULL
  ii<-1
  if (length(colours)!=length(unique(dt[[group_var]]))){
    if (length(unique(dt[[group_var]]))>1){
      colours<-gray((0:(length(unique(dt[[group_var]]))-1))/((length(unique(dt[[group_var]]))-1)*2))
    }else{
      colours<-"black"
    }
  }
  for (i in unique(dt[[group_var]])){
    fo <- mada::reitsma(dt[dt[[group_var]]==i,],TP="TP",FN="FN",FP="FP",TN="TN")
    if (ii==1){
      el<-as.data.frame(mada::ROCellipse(fo)$ROCellipse)
      el$orig<-i
      el$col<-colours[ii]
      sr<-as.data.frame(mada::sroc(fo))     
      sr$orig<-i
      sr$col<-colours[ii]
      sr<-sr[sr$fpr<max(dt$fpr[dt[[group_var]]==i]),]
      sr<-sr[sr$fpr>min(dt$fpr[dt[[group_var]]==i]),]
      sr<-sr[sr$V2<max(dt$sens[dt[[group_var]]==i]),]
      sr<-sr[sr$V2>min(dt$sens[dt[[group_var]]==i]),]
      se$sens<-summary(fo)$coefficients["sensitivity","Estimate"]
      se$fpr<-summary(fo)$coefficients["false pos. rate","Estimate"]
      se$orig<-i
      se<-data.frame(se)
      se$col<-colours[ii]
    }else{
      tmp<-as.data.frame(mada::ROCellipse(fo)$ROCellipse)
      tmp$orig<-i
      tmp$col<-colours[ii]
      el<-rbind(el,tmp)
      tmp<-as.data.frame(mada::sroc(fo))
      tmp$orig<-i
      tmp$col<-colours[ii]
      tmp<-tmp[tmp$fpr<max(dt$fpr[dt[[group_var]]==i]),]
      tmp<-tmp[tmp$fpr>min(dt$fpr[dt[[group_var]]==i]),]
      tmp<-tmp[tmp$V2<max(dt$sens[dt[[group_var]]==i]),]
      tmp<-tmp[tmp$V2>min(dt$sens[dt[[group_var]]==i]),]
      sr<-rbind(sr,tmp)
      tmp<-NULL
      tmp$sens<-summary(fo)$coefficients["sensitivity","Estimate"]
      tmp$fpr<-summary(fo)$coefficients["false pos. rate","Estimate"]
      tmp$orig<-i
      tmp$col<-colours[ii]
      se<-rbind(se,data.frame(tmp))
    }
    ii<-ii+1
  }
  
  rocgraph <- ggplot2::ggplot(data=dt, ggplot2::aes_string(x = "fpr", y= "sens",colour=group_var,label=study))+
    ggplot2::geom_point(show.legend = FALSE,ggplot2::aes_string(size=marker), stroke = 0, shape = 20)+
    ggplot2::geom_line(inherit.aes = FALSE, data = sr, ggplot2::aes(x=fpr, y=V2, colour=orig),show.legend = FALSE)+
    ggplot2::geom_path(inherit.aes = FALSE, data = el, ggplot2::aes(x=V1, y=V2, colour=orig),show.legend = FALSE)+
    ggplot2::geom_point(inherit.aes = FALSE, data = se, ggplot2::aes(x=fpr, y=sens, fill=orig),shape=24)+
    ggplot2::scale_colour_manual(values = colours)+
    ggplot2::scale_fill_manual(values = colours)+
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.title=ggplot2::element_text(size=fontsize,face="bold"),
          axis.text =ggplot2::element_text(size=fontsize),
          axis.title = ggplot2::element_text(size=fontsize,face="bold"),
          axis.line = ggplot2::element_line(colour = "black"),
          axis.ticks = ggplot2::element_line(colour = "black"),
          legend.title = ggplot2::element_blank(),
          legend.position = "bottom")+
    ggplot2::guides(color = ggplot2::element_blank())

  # legend?
  if(length(colours)==1){
    rocgraph <- rocgraph+ggplot2::theme(
      legend.position = "none")
  }
  
  if(plabs==TRUE){
    rocgraph<-rocgraph+ggrepel::geom_label_repel(size=fontsize/ggplot2:::.pt, show.legend = FALSE, force=10)
  }
  if(!is.null(axlabs)){
    rocgraph<-rocgraph+xlab(axlabs[1])+ylab(axlabs[2])
  }
  
  
  return(rocgraph)
}
