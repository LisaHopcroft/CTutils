TRIAL.dose_scale = c( 2, 3, 5, 7, 10 )
TRIAL.starting_dose = 5

#' Modify dose according to protocol schedule
#'
#' This function takes an existing scale for dose modifications
#' (as TRIAL.dose_scale) and, given an input value and a direction
#' (+ for escalation and - for descalation), returns the modified dose.
#'
#' @param value The current value.
#' @param direction The direction (+/-) to move in on the dose scale (default="+")
#' @param dose_scale The existing dose escalation/descalation scale for this trial.
#'
#' @return The new dose.
#' @export
#'
#' @examples
#' this.dose_scale = c( 1,2,4,8,16 )
#' calculate_dose_modification( 2 , dose_scale = this.dose_scale )
#' calculate_dose_modification( 16, "-", dose_scale = this.dose_scale )
calculate_dose_modification = function(value,
                                       direction="+",
                                       dose_scale=TRIAL.dose_scale ) {
  current.pos = which( dose_scale == value )
  new.pos = NA

  if ( direction == "+" ) {
    new.pos = current.pos + 1
  } else if ( direction == "-" ) {
    new.pos = current.pos - 1
  }

  return( dose_scale[new.pos] )

}
