#' Check whether all files in a list exist
#'
#' @param file_list A list of files (will assume local directory if no directory provided)
#'
#' @return TRUE/FALSE to indicate whether all files exist
#' @export
check_all_files_exist = function( file_list ) {
  return( all( unlist( lapply( unlist( lapply( file_list, get ) ), file.exists ) ) ) )
}
