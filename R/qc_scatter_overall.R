#' Scatter plot with area and neucli count
#'
#' @param nano_obj A nano object
#' @param Area Rowname of the sample data that contains Area of the ROI
#' @param Nuclei Row of the sample data for AOI nuclei count
#' @param color_by Column of id data for coloring the points
#' @param shape_by Optional. Column of id data for point shape
#'
#' @export qc_scatter_overall

qc_scatter_overall<-function(nano_obj, Area = 'AOI surface area', Nuclei_column = 'AOI nuclei count', color_by = 'Organism', shape_by = NA){
  plot_data = data.frame(
    IgG_geomean = exp(colMeans(log(nano_obj@count[grepl('IgG',rownames(nano_obj@count)),]))),
    HK_geomean = exp(colMeans(log(nano_obj@count[grepl('Housekeeping', nano_obj@rows$Target_Group),]))),
    Area = as.numeric(unlist(nano_obj@sample[Area,])),
    Nuclei = as.numeric(unlist(nano_obj@sample[Nuclei,])),
    Color = nano_obj@id[[color_by]]
  )

  if(!is.na(shape_by)){
    plot_data$Shape = nano_obj@id[[shape_by]]
  }

  if(var(plot_data$Area)<0.001){
    plot_columns = c(1,2,4)
  }else{
    plot_columns = 1:4
  }
  if(is.na(shape_by)){
    fig = GGally::ggpairs(plot_data, columns = plot_columns, aes(color = Color))
  }else{
    legend_plot = ggplot(data = plot_data, aes(x = IgG_geomean, y = HK_geomean, shape = Shape))+geom_point()+theme(legend.position = 'bottom')
    fig = GGally::ggpairs(plot_data, columns = plot_columns, aes(color = Color, shape = Shape), legend = grab_legend(legend_plot))+theme(legend.position = 'bottom')
  }
  return(fig)
}
