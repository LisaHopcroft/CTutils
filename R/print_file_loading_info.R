#' Print information about which files were loaded.
#'
#' @param file_list A list of files that were loaded.
#'
#' @export
print_file_loading_info = function( file_list ) {
  cat( sprintf( "Loading file: %s\n",
                unlist( lapply( file_list, get ) ) ) )
}
