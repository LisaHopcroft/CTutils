#' RECIST_PARTIAL
#'
#' @param sum0 The sum values at baseline
#' @param sum1 The sum values at the timepoint under consideration
#'
#' @return TRUE/FALSE indicating whether the data meet the partial criteria
#' @export
RECIST_PARTIAL = function(sum0,
                          sum1) {
  return_value = NA

  clean.mask = !is.na(sum0) & !is.na(sum1)

  if ( sum( clean.mask )>0 ) {
    sum0.clean = sum0[clean.mask]
    sum1.clean = sum1[clean.mask]

    this_reduction = RECIST_PARTIAL_calculation(sum0.clean, sum1.clean)
    return_value = this_reduction >= RECIST.PARTIAL.VALUE
  }

  return( return_value )
}

#' RECIST_PARTIAL calculation
#'
#' @param v0 The sum values at baseline
#' @param v1 The sum values at the timepoint under consideration
#'
#' @return the calculated value that is relevant to the PARTIAL RECIST assessment
#' @export
RECIST_PARTIAL_calculation = function(v0,
                                      v1) {
  return( 1-(v1/v0) )
}
