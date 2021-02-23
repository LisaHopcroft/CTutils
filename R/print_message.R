#' Print a message
#'
#' Print pretty messages to standard out to update the user.
#'
#' @param message The message that is to be printed.
#' @param print_title Print a title to the message (defined in section)
#' @param mask_character Use this character in place of the title (if print_title is FALSE).  Default is a space.
#' @param section The section name to be printed in message preamble.
#' @param level The level of indentation required for this message.
#'
#' @importFrom stringr str_length
#'
#' @export
#'
#' @examples
#' print_message( "example" )
print_message = function( message,
                          print_title=TRUE,
                          mask_character = " ",
                          section="CTutils",
                          level=0 ) {

  pre.strings = ""

  if ( print_title ) {
    pre.strings = sprintf( "[%s]", section )
  } else {
    pre.strings = paste( rep.int( mask_character, str_length(section) + 2 ), collapse="" )
  }

  cat( sprintf( "%s %s\n", pre.strings, message ) )

  # indent_size = 2
  #
  # if ( level==0 ) {
  #   cat( sprintf( "[%s] %s",
  #                 section,
  #                 message ) )
  # } else {
  #   this_indent = rep.int( " ", level * indent_size )
  #   cat( sprintf( "[%s] %s%s",
  #                 section,
  #                 this_indent,
  #                 message ) )
  # }
}
