#' Create a new SSR directory
#'
#' Initialise the current directory with skeleton directories and files
#' to start a new SSR.
#'
#' *** NB: use a double backslash to escape spaces in directories; e.g.,
#'         create_SSR_structure(".../01\\ Projects/...")
#'
#' @param template The location of the template to copy
#'
#' @export
create_SSR_structure = function( template ) {
  print_message( c( sprintf( "Initialising SSR directory, copying structure from:" ),
                    template ),
                 section="SETUP" )
  copy_command = sprintf( "cp -r %s/* .", template )
  system( copy_command )
}
