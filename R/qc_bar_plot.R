#' Creates a barplot of log2 geometric mean
#'
#' @author Subrata Paul
#'
#' @param nano_obj The nano object to plot
#' @param filter_by Column name of the rows.data to be used for filtering
#' @param filter_for String to look for in the filter_by column. Any feature (RNA/Protein) with filter_for string on the filter_by column of the rows.data will be used.
#' @param annot_by Column on the id.data to use for annotation
#'
#'
#' @export qc_bar_plot



qc_bar_plot<-function(nano_obj, filter_by, filter_for, annot_by){
  plot_data = data.frame(Log2GeoMean = colMeans(log2(nano_obj@count[grepl(filter_for, nano_obj@rows[[filter_by]]),])))
  plot_data$annot = unlist(nano_obj@id[[annot_by]])
  plot_data = plot_data[order(plot_data$Log2GeoMean),]
  ggplot(data = plot_data, aes(x = 1:nrow(plot_data),y = Log2GeoMean, fill = annot))+geom_bar(stat = 'identity')+
    theme_classic()
}
