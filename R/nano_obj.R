#' An S4 object used by the package
#'
#' @author Subrata Paul
#'
#'
#' @export nano


nano <- setClass("nano",
                 slots =list(sample = 'data.frame',
                             id = 'data.frame',
                             count = 'matrix',
                             rows = 'data.frame',
                             count_norm = 'list'))
