#' Do all the files listed actually exist?
#'
#' @param file_list A list of files
#'
#' @return TRUE/FALSE do all files exist?
#' @export
all_files_exist = function( file_list ) {
  return( all( unlist( lapply( unlist( lapply( file_list, get ) ), file.exists ) ) ) )
}
