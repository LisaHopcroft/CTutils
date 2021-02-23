#' RECIST_PROGRESSIVE
#'
#' @param sum0 The sum values at baseline
#' @param sum1 The sum values at the timepoint under consideration
#' @param diam0 The diameter values at baseline
#' @param diam1 The diameter values at the timepoint under consideration
#'
#' @return TRUE/FALSE indicating whether the data meet the progressive criteria
#' @export
RECIST_PROGRESSIVE = function(sum0 , sum1,
                              diam0, diam1 ) {
  return_value = NA

  clean.sum.mask = !is.na(sum0) & !is.na(sum1)

  sum0.clean = sum0[clean.sum.mask]
  sum1.clean = sum1[clean.sum.mask]

  clean.diam.mask = !is.na(diam0) & !is.na(diam1)

  diam0.clean = diam0[clean.diam.mask]
  diam1.clean = diam1[clean.diam.mask]

  clean.mask = clean.sum.mask & clean.diam.mask

  if ( any( clean.mask ) ) {

  count0 = diam0[clean.diam.mask] %>% length
  count1 = diam1[clean.diam.mask] %>% length

  this_increase_perc  = RECIST_PROGRESSIVEperc_calculation( sum0.clean, sum1.clean )
  this_increase_abs   = RECIST_PROGRESSIVEabs_calculation ( sum0.clean, sum1.clean )

  return_value =  ( ( this_increase_perc >= RECIST.PROGRESSIVE_PERC.VALUE ) &
    ( this_increase_abs >= RECIST.PROGRESSIVE_ABS.VALUE ) ) |
    ( (count1 > count0) )

  }

  return( return_value )


}

#' RECIST PROGRESSIVE percentage increase calculation
#'
#' @param v0 The sum values at baseline
#' @param v1 The sum values at the timepoint under consideration
#'
#' @return the calculated value that is relevant to the PROGRESSIVE RECIST assessment (percentage increase)
#' @export
RECIST_PROGRESSIVEperc_calculation = function(v0,
                                              v1) {
  return( v1/v0 )
}

#' RECIST PROGRESSIVE absolute increase calculation
#'
#' @param v0 The sum values at baseline
#' @param v1 The sum values at the timepoint under consideration
#'
#' @return the calculated value that is relevant to the PROGRESSIVE RECIST assessment (absolute increase)
#' @export
RECIST_PROGRESSIVEabs_calculation = function(v0,
                                             v1) {
  return( v1-v0 )
}



