VERBOSE_REPORTING = TRUE
DATA_EXTRACT_RDAT_LOCATION = "test.RDat"

#' Save CT data to file.
#'
#' This function saves the current version of the data to file.
#' It should be run between every chapter of the report to ensure
#' that all data are available.  If the infile parameter is NULL, the
#' globally defined location for the data extract file will be used.
#'
#' @param infile Path to the input file
#' @param verbose Be verbose?  Default is true.
#'
#' @export
save_image <- function( infile,
                        verbose = TRUE ) {

  if ( is.null( infile  ) ) { infile = DATA_EXTRACT_RDAT_LOCATION }
  if ( is.null( verbose ) ) { verbose = VERBOSE_REPORTING }

  if ( verbose ) {
    print_message(
      sprintf( "Saving data image to file [%s]\n",
               infile
      )
    )
  }
  save.image( infile )
}
