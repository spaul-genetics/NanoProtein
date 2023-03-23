#' Signal to Background Ratio
#'
#' @param nano_obj A nano object
#' @param neg_controls A vector of negetive controls if not all negative controls contains string 'IgG'
#'
#' @description
#' The IgG genes are used as background in the nanostring experiment. Signal to background ratio determines how well the target expression were counted compared to background noise.
#' This function looks for 'IgG' sring on the gene/protein names to identify the negative controls. For other situation a vector of negetive controls can be supplied.
#' The signal to background ratio is calculated as the ratio of counts divided by the geometric mean of negative controls.
#' The function gives a boxplot with individual points of the signal to background ratio.
#'
#'
#' @export signal_to_bg_ratio


signal_to_bg_ratio<-function(nano_obj, neg_controls = NULL){
  if(is.null(neg_controls)){
    IgG_geomean = exp(colMeans(log(nano_obj@count[grepl('IgG', rownames(nano_obj@count)),])))
  }else{
    IgG_geomean = exp(colMeans(log(nano_obj@count[neg_controls,])))
  }


  plot_data = nano_obj@count%*%diag(1/IgG_geomean)
  plot_data = plot_data[!grepl('HYB',rownames(plot_data)),]
  plot_data = data.frame(t(plot_data))
  rownames(plot_data) = colnames(nano_obj@count)
  ordered_cols = names(plot_data)[order(colMeans(plot_data))]
  if(is.null(neg_controls)){
    ordered_cols = c(grep('IgG', ordered_cols, value = T), ordered_cols[!grepl('IgG', ordered_cols)])
  }else{
    ordered_cols = c(neg_controls, setdiff(ordered_cols, neg_controls))
  }

  plot_data = suppressMessages(reshape2::melt(plot_data))
  plot_data$variable = factor(plot_data$variable, levels = ordered_cols)
  fig = ggplot(data = plot_data,
               aes(x = variable, y = log2(value)))+
    geom_boxplot()+
    geom_jitter(alpha = 0.2, size = 0.5)+
    theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    geom_hline(yintercept = 0)+
    labs(x = '', y = 'Log2 Signal-to-Background Ratio')
  return(fig)
}
