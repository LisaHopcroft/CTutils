#' Replace NAs with another value
#'
#' @param val The values in which to look for NAs
#' @param replace The value to replace the NAs in val
#'
#' @return The values with NAs replaced
#' @export
replace_func = function(val, replace) {
  ifelse(is.na(val), replace, val)
}
