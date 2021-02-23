#' Return possible test results (repeated symbols)
#'
#' Given a symbol and a number (x), repeat that symbol x
#' number of times.  Used to generate test results (e.g.,
#' for protein dipstick results). The number (x) can be
#' a vector, to all the generation of a list of possible
#' testresult strings.
#'
#' @param these.times Number of times to repeat a symbol
#' @param symbol Symbol to repeat (default="+")
#'
#' @return the resulting test result string
#' @export
#'
#' @examples
#' create_testresult_string( 5 )
#' create_testresult_string( 1:3, "-" )
create_testresult_string = function( these.times = 0,
                                    symbol = "+") {
  return( unlist( lapply( these.times,
                          function(x) { paste( rep.int( symbol, x ), collapse="" ) }
  ) ) )
}
