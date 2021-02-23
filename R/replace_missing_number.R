#' Replace missing numeric value with another number.
#'
#' Given a tibble and a column, this function will find target
#' values (by default NA) and replace them with another number (by
#' default 0).
#'
#'
#' @param this_data The tibble in which the column exists
#' @param this_var The column name (if not a quosure, it will be made into one)
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param replacement_targets The value to be replaced (default=NA)
#' @param replacement_value The value to replace it with (default=0)
#'
#' @importFrom dplyr na_if
#' @importFrom rlang .data
#' @importFrom rlang quo
#' @importFrom tidyr replace_na
#'
#' @return The updated tibble
#' @export
replace_missing_number = function( this_data,
                                   this_var,
                                   this_function = NA,
                                   replacement_targets = NA,
                                   replacement_value   = 0  ) {

  ### For error/warning messages:
  if ( is.na( this_function ) ) {
    #this_function = get_current_function()
    this_function = as.character( match.call()[[1]] )
  }

  ###################################################################
  ###################################################################
  ### Error handling                                              ###
  ###################################################################
  ###################################################################

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.
  check_for_quosures( this_function, this_var )

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function,
                               this_data, this_var )

  ### Check that the variable is numeric
  check_variable_is_numeric( this_function,
                             this_data, this_var )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  table.data = this_data %>%
    # Replace empty fields with NA
    mutate( flag = na_if( !!this_var, replacement_targets ) ) %>%
    # Replace NAs with the missing string
    replace_na( list( flag={{replacement_value}} ) ) %>%
    mutate( {{this_var}}:=.data$flag ) %>%
    select( -.data$flag )

  return( table.data )

}
