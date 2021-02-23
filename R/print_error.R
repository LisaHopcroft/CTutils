#' Print an error message
#'
#' Print an error to standard out to update the user.
#'
#' @param message The error message that is to be printed.
#'
#' @export
#'
#' @examples
#' print_error( "example" )
print_error = function( message ) {

  if ( length( message ) > 1 ) {
    print_message( message[1],
                   section="CTutils::ERROR" )

    for ( i in 2:length(message) ) {
      print_message( message[i],
                     print_title = FALSE,
                     section="CTutils::ERROR" )
    }

  } else {
    print_message( message,
                   section="CTutils::ERROR" )
  }
}
