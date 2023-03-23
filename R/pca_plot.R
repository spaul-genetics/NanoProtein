#' Plots principal components
#'
#' @param nano_obj A nano object
#' @param slot_id Name of the slot to use for PCA. One of count and count_norm for raw and normalized count matrix, respectively.
#' @param array_name If count_norm slot is used, name of the array within the slot. The name is same as given in the normalization step
#' @param features One of "Endogenous", "Negative" or "Control" to select the features within these CodeClass. Also a vector of selected features can be given containing more than one features.
#' @param x_index Index of PC on the x axis (default 1)
#' @param y_index Index of PC on the y axis (default 2)
#' @param color_by Column of id data to use to color the points
#'
#' @export pca_plot

pca_plot<-function(nano_obj, slot_id = 'count', array_name = NA, features = 'Endogenous',x_index = 1, y_index = 2, color_by = NA){
  xaxis = paste0('PC', x_index)
  yaxis = paste0('PC', y_index)

  if(length(features)==1){
    features = rownames(nano_obj@rows)[nano_obj@rows$CodeClass==features]
  }

  if(is.na(array_name)){
    x_mat = t(slot(nano_obj, slot_id)[features,])
  }else{
    x_mat = t(slot(nano_obj, slot_id)[[array_name]][features,])
  }


  prin_comp = prcomp(x_mat, scale = T, center = T)
  components <- prin_comp[['x']]
  components <- data.frame(components)
  if(!is.na(color_by)){
    components <- cbind(components, nano_obj@id)
  }

  pov = 100*prin_comp$sdev^2/sum(prin_comp$sdev^2)
  xlab = paste0(xaxis, ' (', round(pov[x_index]),'%)')
  ylab = paste0(yaxis, ' (', round(pov[y_index]),'%)')
  if(is.na(color_by)){
    fig = ggplot(data = components, aes(x = !!sym(xaxis), y = !!sym(yaxis)))+geom_point()+theme_bw()+
      labs(x = xlab, y = ylab)
  }else{
    fig = ggplot(data = components, aes(x = !!sym(xaxis), y = !!sym(yaxis), color = !!sym(color_by)))+geom_point()+theme_bw()+
      labs(x = xlab, y = ylab)
  }
  return(fig)

}
