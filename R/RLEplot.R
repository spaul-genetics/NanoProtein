#' Create RLE plot
#'
#' @param nano_obj A Nano object
#' @param count_slot Slot to grab the count matrix
#' @param assay_name If count matrix is from count_norm there are matrices with different name. Provide name of the count matrix to use
#' @param color_by Color the boxplot by any column on the id data
#'
#' @export RLEplot
#' @author Subrata Paul
#' @import ggplot2




RLEplot<-function(nano_obj, count_slot = 'count', assay_name = NULL, color_by = NULL){
  x = slot(nano_obj, count_slot)
  if(is.list(x)){
    if(is.null(assay_name)){
      stop('Provide assay name')
    }
    x = x[[assay_name]]
  }
  x = log(x)
  rowmedians = apply(x, 1, median, na.rm = T)
  x = sweep(x, MARGIN = 1, STATS = rowmedians)
  x = data.frame(x, check.names = F)
  x$Transcript = rownames(x)
  plot_data = reshape2::melt(x, id.vars = 'Transcript', value.name = 'Count', variable.name = 'ROI')
  if(!is.null(color_by)){
    plot_data = merge(plot_data,data.frame(ROI = rownames(nano_obj@id), color = nano_obj@id[[color_by]]))
    names(plot_data)[4]<-color_by
  }

  if(is.null(color_by)){
    fig <- ggplot(data = plot_data, aes(x = ROI, y = Count))+
      geom_boxplot()+
      geom_hline(yintercept = 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      labs(y = 'log(Count) - median(log(Count))', x = '')
  }else{
    fig <- ggplot(data = plot_data, aes(x = ROI, y = Count, color = !!sym(color_by)))+
      geom_boxplot()+
      geom_hline(yintercept = 0)+
      theme_bw()+
      theme(axis.text.x = element_text(angle = 90))+
      labs(y = 'log(Count) - median(log(Count))', x = '')
  }
  return(fig)
}
