#' Plot UMAP
#'
#' @param nano_obj A nano object
#' @param slot_id Either count or count_norm to use raw count or normalize count, respectively.
#' @param array_name The array name of the normalize count
#' @param color_by Column of id data used for coloring the points
#'
#' @export umap_plot
#'
#'
umap_plot<-function(nano_obj, slot_id = 'count', array_name = NA, color_by = 'Status'){
  if(is.na(array_name)){
    x_mat = t(slot(nano_obj, slot_id))
  }else{
    x_mat = t(slot(nano_obj, slot_id)[[array_name]])
  }
  nano.umap = umap::umap(x_mat)
  plot_data = data.frame(nano.umap$layout)
  if(!is.na(color_by)){
    plot_data = cbind(plot_data, nano_obj@id)
  }

  if(is.na(color_by)){
    fig = ggplot(data = plot_data, aes(x = X1, y = X2))+geom_point()+theme_bw()+labs( x = 'UMAP1', y = 'UMAP2')
  }else{
    fig = ggplot(data = plot_data, aes(x = X1, y = X2, color = !!sym(color_by)))+geom_point()+theme_bw()+labs( x = 'UMAP1', y = 'UMAP2')
  }

  return(fig)
}
