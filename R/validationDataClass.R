validationDataClass = function() {
  return("validationData")
}

#' Checks wether an object is of type "validationData"
#'
#' @param x The object
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
