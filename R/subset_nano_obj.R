#' Subset a nano object
#'
#' Subsets a nano object based on conditions
#' @description
#' If keep_id or keep_feature is given then the object is subsetted based on sample or feature. No other operation will be performed in that case.
#' If they are not given then the obsject will be subsetted based on filter_by slot and using filter_str expression.
#'
#'
#' @param nano_obj A nano_object to subset
#' @param keep_id A vector of sample ids to keep if not provided all the sample are kept
#' @param keep_feature A vector of feature names to keep. If not provided all the features are kept
#' @param filter_by One of the three metadata slots: id, rows, or sample corresponding to id.data, row.data, and sample.data, respectively. A column of given data.frame will be used for filtering.
#' @param filter_str An evalutable condition that will be used for filtering such as "Status=='Normal'" will keep the normal tissue samples given that the Status variable on the id.data contains this information.
#'
#' @export subset_nano_obj
#'


subset_nano_obj<-function(nano_obj, filter_by = NA, filter_str = NA, keep_id = NULL, keep_feature = NA){

  nano_obj_subset = nano_obj
  if(!is.null(keep_id)){
    nano_obj_subset@id[keep_id,]
    nano_obj_subset@count<-nano_obj_subset@count[, rownames(nano_obj_subset@id)]
    nano_obj_subset@sample<-nano_obj_subset@sample[, rownames(nano_obj_subset@id)]
    return(nano_obj_subset)
  }


  if(!is.na(keep_feature)){
    nano_obj_subset@rows[keep_feature,]
    nano_obj_subset@count<-nano_obj_subset@count[rownames(nano_obj_subset@rows),]
    return(nano_obj_subset)
  }

  if(is.na(filter_by) | is.na(filter_str)){
    stop('filter_by or filter_for is missing')
  }

  eval(parse(text = paste0("keep = slot(nano_obj_subset, '", filter_by, "')$", filter_str)))

  slot(nano_obj_subset, filter_by) = slot(nano_obj_subset, filter_by)[keep,]
  if(filter_by == 'id'){
    nano_obj_subset@count<-nano_obj_subset@count[, rownames(nano_obj_subset@id)]
    nano_obj_subset@sample<-nano_obj_subset@sample[, rownames(nano_obj_subset@id)]
  }else{
    nano_obj_subset@count<-nano_obj_subset@count[rownames(nano_obj_subset@rows),]
  }
  return(nano_obj_subset)
}
