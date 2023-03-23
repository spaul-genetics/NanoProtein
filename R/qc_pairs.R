#' Pairs plot of set of features
#'
#' Depending on the selected set of RNA or Proteins it will create a pairs plot of the Proteins within the set.
#'
#' @param nano_obj A nano object to plot
#' @param target Character string identifying the set of proteins/RNA.
#' @param category_col Column to used to identify the target set. By default 'Target_Group'. The count matrix will be subsetted by the rows that contains "target" in the "category_col".
#' @param color_by Column in the id data to be used for coloring the points
#' @param shape_by Column in the id data to determine the shape of the points. By default no shape is used.
#'
#' @import GGally
#' @export qc_pairs



qc_pairs<-function(nano_obj, target, category_col = 'Target_Group', color_by, shape_by = NA){
  if(!category_col%in%names(nano_obj@rows)){
    stop('The category column was not found in the row data.')
  }

  if(sum(grepl(target, nano_obj@rows[[category_col]]))<2){
    stop('Target string was found less than twice in the category column')
  }

  plot_data = data.frame(t(nano_obj@count[grepl(target, nano_obj@rows[[category_col]]),]))
  if(!is.na(shape_by)){
    plot_data = cbind(plot_data, data.frame(Color = nano_obj@id[[color_by]], Shape = nano_obj@id[[shape_by]]))
    names(plot_data)[names(plot_data)=='Shape']<-shape_by
  }else{
    plot_data = cbind(plot_data, data.frame(Color = nano_obj@id[[color_by]]))
  }

  names(plot_data)[names(plot_data)=='Color']<-color_by


  if(is.na(shape_by)){
    fig = GGally::ggpairs(plot_data, columns = 1:(ncol(plot_data)-1), aes(color = !!sym(color_by)))
  }else{
    legend_plot = ggplot(data = plot_data, aes(x = !!sym(names(plot_data)[1]), y = !!sym(names(plot_data)[2]), shape = !!sym(shape_by) ))+geom_point()+theme(legend.position = 'bottom')
    legend = grab_legend(legend_plot)
    fig = GGally::ggpairs(plot_data, columns = 1:(ncol(plot_data)-2), aes(color = !!sym(color_by), shape = !!sym(shape_by)), legend = legend) + theme(legend.position = 'bottom')
  }
  return(fig)

}
