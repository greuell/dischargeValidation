#' validationDataClass
#' 
#' validationData class
#'
#' @return
#' @export
#'
#' @examples
validationDataClass = function() {
  return("validationData")
}

#' isValidationData
#' 
#' check if valid validationData class
#'
#' @param x 
#'
#' @return  true or false
#' @export
#'
#' @examples
isValidationData = function(x) {
  if (class(x) == class.validation.data()) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}
