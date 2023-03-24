#' Line Plot of the counts
#' 
#' @param nano_obj A nano object
#' @param target Lines to highlight such as Background and Housekeeping
#' @param shadow The lines to keep in grey color. By default Endogenous. It can be any category from CodeClass
#' @param target_group_col Where to find the targets. The function will `grep` the `target` at this column of row data. Default is Target_Group
#' @param split_by Split by any column of id data. 
#' @param interactive If TRUE returns a plotly graph.
#' 
#' @author Subrata Paul
#' @export qc_line_plot
#' @import ggplot2
#' @import plotly

qc_line_plot<-function(nano_obj, 
                       target, 
                       shadow = 'Endogenous', 
                       target_group_col = 'Target_Group', 
                       split_by = NULL,
                       interactive = F){
  target_features = rownames(nano_obj@rows)[grepl(target, nano_obj@rows[[target_group_col]])]
  shadow_features = rownames(nano_obj@rows)[grepl('Endogenous', nano_obj@rows$CodeClass)]
  
  plot_data = data.frame(nano_obj@count[c(shadow_features, target_features),], check.names = F)
  geomean = data.frame(exp(colMeans(log(plot_data[target_features,]))))
  geomean = data.frame(t(geomean))
  rownames(geomean) = 'GeoMean'
  names(geomean)<-names(plot_data)
  
  plot_data$Protein = rownames(plot_data)
  plot_data$Group = c(rep(shadow, length(shadow_features)), target_features)
  geomean$Protein = 'GeoMean'
  geomean$Group = 'GeoMean'
  
  plot_data = rbind(plot_data, geomean)
  
  plot_data = reshape2::melt(plot_data, value.name = 'Count', id.vars = c('Group','Protein'), variable.name = 'ROI')
  plot_data$ROI = factor(plot_data$ROI, levels = unique(plot_data$ROI))
  
  
  if(!is.null(split_by)){
    ROI_status = data.frame(ROI = rownames(nano_obj@id), Status= nano_obj@id[[split_by]])
    
    plot_data = merge(plot_data, ROI_status, by = 'ROI')    
  }

  
  col_breaks <- setdiff(unique(plot_data$Group), shadow)
  col_values <- RColorBrewer::brewer.pal(length(col_breaks),'Set1')
  col_breaks = c(col_breaks, shadow)
  col_values = c(col_values, "#BDBDBD")
  
  
  p = ggplot(data = plot_data, 
             aes(x = ROI, y = log2(Count), color = Group, group = Protein))+
    geom_line(aes(linewidth = Group))+
    scale_color_manual(values = col_values, breaks = col_breaks)+
    scale_discrete_manual("linewidth", breaks = col_breaks, values = c(rep(0.6,length(col_breaks)-1), 0.3))
  if(!is.null(split_by)){
    p = p + facet_wrap(vars(Status), scales = 'free_x')
  }
    
  p = p + theme_bw()+
    theme(axis.text.x = element_text(angle = 90))+
    labs(x = '')
  
  if(interactive){
    return(plotly::ggplotly(p, tooltip = c('group','color', 'y')))  
  }else{
    return(p)
  }
  
}

