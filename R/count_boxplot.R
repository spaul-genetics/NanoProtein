#' Boxplot of the counts
#'
#' @param nano_obj A nano object
#' @param by feature or sample.
#' @param slot_id Slot to used for the plot. For raw counts use 'count' (default) and for normalized count use 'count_norm'
#' @param array_name If normalize count is used then enter the name of the array which was given in the normalization command
#' @param filter_for Character string. CodeClass name for the features you want to include in the count. For example Endogenous, Negative, Control etc.
#' @param features Instead of filter_for you can also give a vector of target names (RNA or protein). Both filter_for and features can't be supplied.
#' @param color_by Column name of the id data to use for coloring the points
#'
#' @export count_boxplot
#'
#' @description
#' If by = 'feature' then the independent variables are the targets (RNA/Protein) otherwise boxplots represents count on each ROI.
#'


count_boxplot<-function(nano_obj, by = 'feature', slot_id = 'count', array_name = NA, features = NA, filter_for = NA, color_by = NA){

  if(is.na(array_name)){
    if('list'%in%class(slot(nano_obj, 'count'))){
      stop('Your selected slot is a list of arrays. Provide array_name.')
    }
    plot_data = data.frame(slot(nano_obj, slot_id))
  }else{
    plot_data = data.frame(slot(nano_obj, slot_id)[[array_name]])
    if(sum(dim(plot_data))==0){
      stop('No data found matching slot_id and array_name. Check your input.')
    }
  }


  if(!is.na(filter_for) & !is.na(features)){
    stop("Both filter_for and features can't be supplied")
  }

  if(!is.na(filter_for)){
    features = rownames(nano_obj@rows)[nano_obj@rows$CodeClass==filter_for]
    if(length(features)==0){
      stop(paste0('No ', filter_for, ' features found on CodeClass') )
    }
  }

  if(length(features)==1){
    if(!is.na(features)){
      plot_data = plot_data[features,]
    }
  }



  plot_data = plot_data%>%tibble::rownames_to_column('Feature')%>%tidyr::pivot_longer(cols = !Feature, names_repair = 'check_unique')

  if(!is.na(color_by)){
    color_data = nano_obj@id
    color_data$name = make.names(rownames(color_data))
    color_data = color_data[,c(color_by, 'name')]
    plot_data = merge(plot_data, color_data, by = 'name')
  }

  if(by == 'feature'){


    if(is.na(color_by)){
      fig = ggplot(data = plot_data, aes(x = Feature, y = value))+
        geom_boxplot(outlier.shape = NA)+
        geom_jitter(size = 0.5) +
        labs(y = 'Count')+
        theme_bw()+theme(axis.text.x = element_text(angle = 90))
    }else{
      fig = ggplot(data = plot_data, aes(x = Feature, y = value))+
        geom_boxplot(outlier.shape = NA)+
        geom_jitter(size = 0.5, aes(color = !!sym(color_by))) +
        labs(y = 'Count')+
        theme_bw()+theme(axis.text.x = element_text(angle = 90))
    }
  }else{
    if(is.na(color_by)){
      fig = ggplot(data = plot_data, aes(x = name, y = value))+
        geom_boxplot(outlier.shape = NA)+
        geom_jitter(size = 0.5) +
        labs(y = 'Count')+
        theme_bw()+theme(axis.text.x = element_text(angle = 90))
    }else{
      fig = ggplot(data = plot_data, aes(x = name, y = value))+
        geom_boxplot(outlier.shape = NA)+
        geom_jitter(size = 0.5, aes(color = !!sym(color_by))) +
        labs(y = 'Count')+
        theme_bw()+theme(axis.text.x = element_text(angle = 90))
    }
  }

  return(fig)
}
