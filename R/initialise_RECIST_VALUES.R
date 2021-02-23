#' Initialise RECIST values as global variables
#'
#' @param version The version of RECIST to be used (currently only 1.1)
#'
#' @return NA
#' @export
initialise_RECIST_VALUES = function(version = "1.1") {

  if ( !is.character( version ) ) version = as.character( version )

  vars = list()

  if ( version == "1.1") {
    vars = list(
      RECIST.COMPLETE.VALUE = 10,
      RECIST.PARTIAL.VALUE  = 0.30,
      RECIST.PROGRESSIVE_PERC.VALUE = 1.20,
      RECIST.PROGRESSIVE_ABS.VALUE  = 5
    )
  }

  list2env( vars, envir=globalenv() )
}

