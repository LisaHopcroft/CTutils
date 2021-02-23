#' Check whether the date recorded for the extract matches the file name.
#'
#' @param filename The name of the extract file
#' @param trial The name of the trial
#' @param date_provided The recorded date of the extract
#'
#' @importFrom stringr str_remove
#' @importFrom lubridate ymd
#' @importFrom dplyr %>%
#'
#' @return Whether the name of the file and the extract date match
#' @export
#'
#' @examples
#' check_extract_date( "NAXIVA20200304.csv", "NAXIVA", lubridate::ymd("2020-03-04") )
#' check_extract_date( "NAXIVA20200304.csv", "NAXIVA", lubridate::ymd("2022-03-04")  )
check_extract_date = function( filename,
                               trial,
                               date_provided ) {
  date_from_filename = filename %>%
    str_remove("_.*") %>%
    str_remove( trial ) %>%
    str_remove( "\\..*" ) %>%
    ymd

  return( date_from_filename == date_provided )
}
