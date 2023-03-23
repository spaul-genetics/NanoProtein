#' Scatter Plot of Housekeeping and Background
#'
#' @param nano_obj A nano object to plot
#' @param color_by Column name os id data to be used to color the points
#' @param shape_by Column name of id data to be used to identify the points
#'
#' @export qc_scatter_between

qc_scatter_between<-function(nano_obj, color_by, shape_by = NA){
  plot_data = data.frame(
    exp(colMeans(log(nano_obj@count[grepl('Background',nano_obj@rows$Target_Group),]))),
    exp(colMeans(log(nano_obj@count[grepl('Housekeeping', nano_obj@rows$Target_Group),]))),
    unlist(nano_obj@id[[color_by]])
  )
  names(plot_data) = c('Background_geomean', 'Housekeeping_geomean', color_by)

  if(!is.na(shape_by)){
    plot_data[[shape_by]]<-unlist(nano_obj@id[[shape_by]])
    fig = ggplot(data = plot_data, aes(x = Background_geomean, y = Housekeeping_geomean, shape = !!sym(shape_by), color = !!sym(color_by)))+geom_point()+
      theme_classic()
  }else{
    fig = ggplot(data = plot_data, aes(x = Background_geomean, y = Housekeeping_geomean, color = !!sym(color_by)))+geom_point()+
      theme_classic()
  }
  fig = fig + ggpubr::stat_cor(method = 'pearson')
  return(fig)
}
