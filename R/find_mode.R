#' Find the mode in the list of values (i.e., the most frequent value)
#'
#' @param v The list of values for which to find the mode
#'
#' @return The mode
#' @export
find_mode = function(v) {
  uniqv = unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}
