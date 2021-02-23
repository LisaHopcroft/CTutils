#' Change the levels of a factor to use an explicit list
#'
#' @param this.data The tibble in which the column exists
#' @param this.var The column name to refactor
#' @param these.levels The new levels for this factor (default = Yes, No, Missing)
#'
#' @importFrom dplyr mutate
#'
#' @return The updated tibble
#' @export
refactor_variable = function( this.data,
                              this.var,
                              these.levels = c("Yes", "No", "Missing") ){
  this.data.updated = this.data  %>%
    mutate( flag = factor(!!this.var, levels = these.levels))
  return( this.data.updated )
}
