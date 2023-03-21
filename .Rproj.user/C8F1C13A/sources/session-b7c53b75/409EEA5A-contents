#' Creates a nano object
#'
#' From the matrices of count data, sample information, and target information this function creates a nano object
#' @param sample.data A data frame of sample information such as ROI names, Area, Muclei count etc. The rownames are the attributes and the column names are the ROI identifier
#' @param id.data A data.frame with information about the ROI such as Condition (Infected or Uninfected)
#' @param row.data A data.frame containing information about the features (RNA or Protein). CodeClass column should be one of the columns.
#' @param count.data The count matrix. The rownames should match the rownames of row.data and column names should match the rownames of id.data
#' @author Subrata Paul
#' @description
#' Creates a S3 object of all the information about target features and samples. This object is used for the other functions in this package. An item on this object can be access as slot(nano_obj, item_name)
#'
#' @export creaet_nano_obj
#'




create_nano_obj<-function(sample.data, id.data, count.data, row.data){
  setClass("nano", slots =list(sample = 'data.frame', id = 'data.frame', count = 'matrix', rows = 'data.frame', count_norm = 'list'))
  if(!all.equal(colnames(count.data),rownames(id.data))){
    stop('The column names of count.data and rownames of id.data do not match.')
  }

  if(!all.equal(rownames(count.data), rownames(row.data))){
    stop('The rownames of count.data and row.data do not match.')
  }
  nano_obj = new('nano', sample = sample.data, id = id.data, count = as.matrix(count.data), rows = row.data)
  return(nano_obj)
}
