#' RECIST_COMPLETE
#'
#' @param diam1 The individual lesion diameters values at the timepoint under consideration
#'
#' @return TRUE/FALSE indicating whether the data meet the complete criteria
#' @export
RECIST_COMPLETE = function(diam1) {
  if ( any( !is.na(diam1) ) ) {
    diam1.clean = diam1[!is.na(diam1)]
    return ( all( diam1.clean <= RECIST.COMPLETE.VALUE ) )
  } else {
    return( NA )
  }
}

#RECIST_COMPLETE = function(sum1) {
#  sum1.clean = sum1[!is.na(sum1)]
#  return ( all( sum1.clean <= RECIST.COMPLETE.VALUE ) )
#}
