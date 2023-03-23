#' Normalize Nanostring data
#'
#' @param nano_obj A nano object
#' @param method Normalization method. One of hghk, posonly, 'Endogenous'
#' @param out_name The array name where the normalized count will be saved within the count_norm slot of nano object
#' @param used_igg A list of background genes/proteins to use for background subtraction
#' @param used_hk A list of housekeeping genes/proteins to use for housekeeping normalization factor
#'
#' @description
#' The count data is first normalized by the Positive CodeClass. The normalization factor is calculated as mean of positive (spike-in protein) over all ORI divided by the the count on each ROI
#' If method='posonly' no further scalling is performed. For 'bghk' method geometric mean of backgrounds are subtracted and scaled by the normalization factors calculated using the 'used_hk' genes.
#' In method = 'Endogenous', after scalling by positive spike-in, normalization factor based on all endogenous genes/proteins is used to scale the counts.
#'
#' @export normalize_nano
#'


normalize_nano<-function(nano_obj, method = 'bghk', out_name = NA, used_igg, used_hk){
  if(is.na(out_name)) out_name = method

  if(sum(nano_obj@rows$CodeClass=="Positive")>1){
    roiSpikein <-  apply(nano_obj@count[which(nano_obj@rows$CodeClass=="Positive"),], 2, geometric.mean)
    scale.factor <- roiSpikein/mean(roiSpikein)
    if (any(scale.factor < 0.3 | scale.factor > 3)) {
      warning(cat("Identified positive scale factor outside recommended range \n(0.3-3).\nCheck samples prior to conducting analysis.\n",
                  file = logfile, append = TRUE))
    }

  }else{
    roiSpikein <-  nano_obj@count[which(nano_obj@rows$CodeClass=="Positive"), ]
    scale.factor <- roiSpikein/mean(roiSpikein)
    if (any(scale.factor < 0.3 | scale.factor > 3)) {
      warning(cat("Identified positive scale factor outside recommended range \n(0.3-3).\nCheck samples prior to conducting analysis.\n",
                  file = logfile, append = TRUE))
    }
  }
  nano_obj@count_norm[['Positive']]<- sweep(nano_obj@count, 2, scale.factor, "/")

  if(method == 'posonly'){
    return(nano_obj)
  }

  if(method == 'bghk'){
    if(length(used_igg)==1){
      if(is.na(used_igg)){
        used_igg = grep('IgG', rownames(nano_obj@count), value = T)
      }
    }

    if(length(used_hk)==1){
      if(is.na(used_hk)){
        used_hk = rownames(nano_obj@rows)[grepl('Housekeep',nano_obj@rows$Target_Group)]
      }
    }

    background_to_subtract = exp(colMeans(log(nano_obj@count_norm[['Positive']][used_igg,])))
    housekeeper_to_scale = exp(colMeans(log(nano_obj@count_norm[['Positive']][used_hk,])))
    housekeeper_to_scale = housekeeper_to_scale/mean(housekeeper_to_scale)
    endogenous <- rownames(nano_obj@rows)[nano_obj@rows$CodeClass=='Endogenous']
    #endogenous = setdiff(endogenous, unused_endo)
    count_after_bg_subt<- sweep(x = nano_obj@count_norm[['Positive']][endogenous,], MARGIN = 2, STATS = background_to_subtract)
    feature_with_low_count = unique(rownames(which(count_after_bg_subt<0, arr.ind = T)))
    if(length(feature_with_low_count)>0){
      message(paste0(paste(feature_with_low_count, collapse = ', '), ' have at least one ROI with less count than the background'))
    }

    nano_obj@count_norm[[out_name]] = sweep(x = count_after_bg_subt, MARGIN = 2, STATS = housekeeper_to_scale, FUN = '/')
    return(nano_obj)
  }


  if(method == 'Endogenous'){
    endogenous <- rownames(nano_obj@rows)[nano_obj@rows$CodeClass=='Endogenous']
    scale_factor = exp(colMeans(log(nano_obj@count_norm[['Positive']][endogenous,])))
    scale_factor = scale_factor/mean(scale_factor)
    nano_obj@count_norm[[out_name]] = sweep(x = nano_obj@count_norm[['Positive']][endogenous,], MARGIN = 2, STATS = scale_factor, FUN = '/')
    return(nano_obj)
  }
}
