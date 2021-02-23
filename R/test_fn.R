### https://community.rstudio.com/t/how-to-solve-no-visible-binding-for-global-variable-note/28887

#' Print a name
#'
#' This function takes a name and prints it.
#'
#' @param data_in Name to be printed
#'
#' @return The string printed
#'
#' @importFrom rlang .data
#'
#' @export
test_fn <- function( data_in ){
  return ( max( data_in %>% pull( .data$a ) ) )
}
