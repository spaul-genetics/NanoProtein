#' Scatter Plot of Housekeeping and Background
#'
#' @param nano_obj A nano object to plot
#' @param color_by Column name os id data to be used to color the points
#' @param shape_by Column name of id data to be used to identify the points
#' @param individual Boolean. If true a grid of scatter plot for each pair of background and housekeeping proteins will be produced
#'
#' @export qc_scatter_between

qc_scatter_between<-function(nano_obj, color_by, shape_by = NULL, individual = F){
  if(individual){
    plot_data<-data.frame()
    backgrounds <- unique(rownames(nano_obj@rows)[grepl('Background', nano_obj@rows$Target_Group)])
    housekeepings <- unique(rownames(nano_obj@rows)[grepl('Housekeeping', nano_obj@rows$Target_Group)])
    for(background in backgrounds){
      for(housekeeping in housekeepings){
        temp = data.frame(
          Background_Protein = rep(background, ncol(nano_obj@count)),
          Housekeeping_Protein = rep(housekeeping, ncol(nano_obj@count)),
          Background_Count = nano_obj@count[background,],
          Housekeeping_Count = nano_obj@count[housekeeping,],
          color_by = nano_obj@id[[color_by]],
          shape_byy = if(!is.null(shape_by)){
            nano_obj@id[[shape_by]]
          }else{
            NA
          }
        )
        names(temp)[5]<-color_by
        if(!is.null(shape_by)){
          names(temp)[6]<-shape_by
        }
        plot_data = rbind(plot_data, temp)
      }
    }

    if(is.null(shape_by)){
      fig = ggplot(data = plot_data, aes(x = Background_Count, y = Housekeeping_Count, color = !!sym(color_by)))+
        geom_point()+
        geom_smooth(method = 'lm', formula = 'y~x')+
        facet_wrap(Background_Protein ~ Housekeeping_Protein, scales = 'free')+
        theme_bw()
    }else{
      fig = ggplot(data = plot_data, aes(x = Background_Count, y = Housekeeping_Count, color = !!sym(color_by), shape = !!sym(shape_by)))+
        geom_point()+
        geom_smooth(method = 'lm', formula = 'y~x')+
        facet_wrap(Background_Protein ~ Housekeeping_Protein, scales = 'free')+theme_bw()
    }
    return(fig)
  }else{
    plot_data = data.frame(
      exp(colMeans(log(nano_obj@count[grepl('Background',nano_obj@rows$Target_Group),]))),
      exp(colMeans(log(nano_obj@count[grepl('Housekeeping', nano_obj@rows$Target_Group),]))),
      unlist(nano_obj@id[[color_by]])
    )
    names(plot_data) = c('Background_geomean', 'Housekeeping_geomean', color_by)

    if(!is.null(shape_by)){
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

}
