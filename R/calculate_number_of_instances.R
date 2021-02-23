#' Calculate number of instances
#'
#' This function returns the number of non-empty entries,
#' optionally for each patient and each timepoint.
#' Ensure that the tibble provided includes ALL rows (i.e.,
#' this is the extended form of the data from MACRO, with lots
#' of empty values).  If grouping by timepoint is specified,
#' the data will be returned with the timepoints spread as columns.
#'
#' @param this_data The tibble in which the column exists
#' @param this_var The column name (if not a quosure, it will be made into one)
#' @param patient_column The column name containing the patient ID to group by (default is to not group)
#' @param timepoint_column The column name containing the timepoint to group by (default is to not group)
#' @param spread_timepoints A column to spread the results by timepoint (default is TRUE)
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#'
#' @importFrom dplyr ungroup
#' @importFrom rlang .data
#' @importFrom tidyr complete
#' @importFrom tidyr pivot_wider
#' @importFrom tidyr spread
#'
#' @return tibble containing the patient and their tumour counts for each timepoint.
#' @export
calculate_number_of_instances = function( this_data,
                                          this_var,
                                          patient_column = NULL,
                                          timepoint_column = NULL,
                                          spread_timepoints = TRUE,
                                          this_function = NA ) {

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

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function,
                               this_data, this_var )
  if ( !is.null(patient_column  ) ) {
    check_variables_are_in_data( this_function, this_data, patient_column )
  }

  if ( !is.null(timepoint_column) ) {
    check_variables_are_in_data( this_function, this_data, timepoint_column )
  }


  ### This can only handle one variable at a time
  check_for_individual_variable( this_function, this_var )

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.  Check also the optional variables for the timepoint
  ### and patient columns that may be used for grouping (if they are
  ### present).
  check_for_quosures( this_function, this_var )
  if ( !is.null(patient_column  ) ) check_for_quosures( this_function, patient_column   )
  if ( !is.null(timepoint_column) ) check_for_quosures( this_function, timepoint_column )

  ###################################################################
  ###################################################################
  ### Warnings: report and record                                 ###
  ###################################################################
  ###################################################################

  ### Print warning if there is no entry in the glossary for the variable
  # check_glossary( this_function, this_var, key_for_variables )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  ### Optionally group by the requested variables
  this_data.tmp = this_data %>% ungroup()

  if ( !is.null( patient_column ) ) {
    this_data.tmp = this_data.tmp %>%
      group_by( {{patient_column}}, add=TRUE )
  }

  if ( !is.null( timepoint_column ) ) {
    this_data.tmp = this_data.tmp %>%
      group_by( {{timepoint_column}}, add=TRUE )
  }

  ### Calculate the numbers
  num_instances = this_data.tmp %>%
    mutate( info = {{this_var}} ) %>%
    filter( .data$info!="" ) %>%
    summarise( n = n() ) %>%
    complete( {{patient_column}},
              {{timepoint_column}},
              fill=list(n=0) )

  check_for_extended_data_use( this_function,
                               num_instances %>% pull( .data$n ) )

  if ( spread_timepoints ) {
    num_instances = num_instances %>%
      ungroup() %>%
      pivot_wider( names_from  = quo_name( timepoint_column ),
                   values_from = "n" )
  }

  return( num_instances )
}
