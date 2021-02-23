#' Find the mode in a list of values (i.e., the most
#' common value).
#'
#' @param v A list of values
#'
#' @return The mode (most common value)
#' @export
#'
#' @examples
#' calculate_mode( c( 1, 1, 1, 10, 10000) )
#' calculate_mode( c( "A", "B", "B", "C") )
#' calculate_mode( c(LETTERS[1:3], LETTERS[3:5] ) )
calculate_mode <- function(v) {
  uniqv <- unique(v)
  return(uniqv[which.max(tabulate(match(v, uniqv)))])
}

