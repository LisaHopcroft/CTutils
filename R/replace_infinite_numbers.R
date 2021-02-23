#' Replace infinite numbers with NA
#'
#' @param x the values to be checked
#'
#' @return the modified list of values
#' @export
replace_infinite_numbers = function(x){
  ifelse(is.finite(x),x,NA)
}
