#' Print a warning message
#'
#' Print a warning to standard out to update the user.
#'
#' To do: add the warning to a list to report at the end of the process.
#'
#' @param message The warning message that is to be printed.
#'
#' @export
#'
#' @examples
#' print_warning( "example" )
print_warning = function( message ) {
  if ( length( message ) > 1 ) {
    print_message( message[1],
                   section="CTutils::Warning" )

    for ( i in 2:length(message) ) {
      print_message( message[i],
                     print_title = FALSE,
                     section="CTutils::Warning" )
    }

  } else {
    print_message( message,
                   section="CTutils::Warning" )
  }
}
