#' Calculates whether data indicates a complete response by RECIST criteria
#'
#' This function takes tumour measurement data and calculates the RECIST outcome.
#' It calculates a response for one timepoint measurement against the baseline.
#' If multiple timepoint measurements are to be compared to a baseline, then this
#' function will need to be run multiple times.
#' It presumes that the data that are passed here are clean (i.e., all data are present,
#' none are missing; all diameters add up to the sum etc.).  The data in
#' should also have a column that represents the minimum sum of tumour diameters
#' (across all timepoints) for each patient.
#'
#' @param this_data The tibble in which the column exists.
#' @param id_variable The column that defines each patient (i.e., the patient ID)
#' @param baseline_diam The variable that represents the individual diameters at baseline
#' @param baseline_sum The variable that represents the sum at baseline
#' @param min_sum The variable that represents the smallest diameter in the study (if absent, assumes this value is the baseline_sum)
#' @param timepoint_diam The variable that represents the individual diameters at the later timepoint
#' @param timepoint_sum The variable that represents the sum at the later timepoint
#' @param label_string The string to use for the result column
#' @param this_function A label for error/warning messages (by default it is the name of this function)
#'
#' @importFrom dplyr case_when
#' @importFrom rlang .data
#'
#' @return A list, containing (1) the assessment (using RECIST v1.1 criteria) and (2) the supporting data
#' @export
calculate_RECIST_response = function( this_data,
                                      id_variable,
                                      baseline_diam,
                                      baseline_sum,
                                      min_sum = NULL,
                                      timepoint_diam,
                                      timepoint_sum,
                                      this_function = NA,
                                      label_string = "RECIST"   ) {

  ### For error/warning messages:
  if ( is.na( this_function ) ) {
    #this_function = get_current_function()
    this_function = as.character( match.call()[[1]] )
  }

  ###############################
  ### THIS IS THE NEW VERSION ###
  ###############################


  if ( is.null( min_sum ) ) { min_sum = baseline_sum }

  ###################################################################
  ###################################################################
  ### Error handling                                              ###
  ###################################################################
  ###################################################################

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.
  check_for_quosures( this_function, id_variable     )
  check_for_quosures( this_function, baseline_diam   )
  check_for_quosures( this_function, baseline_sum    )
  check_for_quosures( this_function, min_sum         )
  check_for_quosures( this_function, timepoint_diam  )
  check_for_quosures( this_function, timepoint_sum   )

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function, this_data, id_variable     )
  check_variables_are_in_data( this_function, this_data, baseline_diam   )
  check_variables_are_in_data( this_function, this_data, baseline_sum    )
  check_variables_are_in_data( this_function, this_data, min_sum         )
  check_variables_are_in_data( this_function, this_data, timepoint_diam  )
  check_variables_are_in_data( this_function, this_data, timepoint_sum   )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  RECIST.data = this_data %>%
    group_by( {{id_variable}} ) %>%
    select( {{id_variable}},
            {{baseline_diam}},
            {{baseline_sum}},
            {{min_sum}},
            {{timepoint_diam}},
            {{timepoint_sum}}
    )

  RECIST.out = RECIST.data %>%
    summarise( comp = RECIST_COMPLETE   ( diam1 = {{timepoint_diam}} ),
               part = RECIST_PARTIAL    ( sum0  = {{baseline_sum  }},
                                          sum1  = {{timepoint_sum }} ),
               prog = RECIST_PROGRESSIVE( sum0  = {{min_sum       }},
                                          sum1  = {{timepoint_sum }},
                                          diam0 = {{baseline_diam }},
                                          diam1 = {{timepoint_diam}} )
    ) %>%
    mutate( missing = is.na(comp) & is.na(part) & is.na(prog) ) %>%
    mutate( {{label_string}} := case_when(
      comp    ~ "Complete response",
      part    ~ "Partial response",
      prog    ~ "Progressive disease",
      missing ~ "Not available",
      TRUE    ~ "Stable disease"
    ) )


  RECIST.data = RECIST.data %>%
    rename( id_variable    = {{id_variable}},
            baseline_diam  = {{baseline_diam}},
            baseline_sum   = {{baseline_sum}},
            min_sum        = {{min_sum}},
            timepoint_diam = {{timepoint_diam}},
            timepoint_sum  = {{timepoint_sum}}
            )

  return( list( assessment      = RECIST.out,
                supporting_data = RECIST.data ) )

}
