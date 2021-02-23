#' Replace missing values with another string.
#'
#' Given a tibble and a column, this function will find target
#' values (by default "") and replace them with a string (by
#' default "Missing").
#'
#' Function can be repurposed to replace any target value
#' with another string.
#'
#' @param this_data The tibble in which the column exists
#' @param this_var The column name (if not a quosure, it will be made into one)
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param replacement_targets The value to be replace (default="")
#' @param replacement_value The value to replace it with (default="Missing)
#'
#' @importFrom dplyr na_if
#' @importFrom rlang .data
#' @importFrom rlang quo
#' @importFrom tidyr replace_na
#'
#' @return The updated tibble
#' @export
replace_missing_string = function( this_data,
                                   this_var,
                                   this_function = NA,
                                   replacement_targets = "",
                                   replacement_value   = "Missing" ) {

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
  check_for_quosures( this_function, this_var )

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function,
                               this_data, this_var )

  ### Check that the variable is numeric
  check_variable_is_character( this_function,
                               this_data,
                               this_var )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  table.data = this_data  %>%
    # Replace empty fields with NA
    mutate( flag = as.character(na_if( !!this_var, replacement_targets ) ) ) %>%
    # Replace NAs with the missing string
    replace_na( list( flag={{replacement_value}} ) )

  return( table.data )

}
