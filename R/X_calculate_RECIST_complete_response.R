#' #glossary.dictionary = NULL
#' #RECIST.COMPLETE_value = 10
#'
#'
#' RECIST_COMPLETE = function(sum1) {
#'   RECIST.COMPLETE_value = 10
#'   sum1.clean = sum1[!is.na(sum1)]
#'   return ( all( sum1.clean <= RECIST.COMPLETE_value ) )
#' }
#'
#' RECIST_PARTIAL = function(sum0,sum1) {
#'   RECIST.PARTIAL_value = 0.30
#'   sum0.clean = sum0[!is.na(sum0)]
#'   sum1.clean = sum1[!is.na(sum1)]
#'   this_reduction = 1-(sum1.clean/sum0.clean)
#'   return ( this_reduction >= RECIST.PARTIAL_value )
#' }
#'
#'
#' RECIST_PROGRESSIVE = function(sum0 , sum1,
#'                               diam0, diam1) {
#'   RECIST.PROGRESSIVE_value1 = 1.20
#'   RECIST.PROGRESSIVE_value2 = 5
#'
#'   sum0.clean = sum0[!is.na(sum0)]
#'   sum1.clean = sum1[!is.na(sum1)]
#'
#'   count0 = diam0[!is.na(diam0)] %>% length
#'   count1 = diam1[!is.na(diam1)] %>% length
#'
#'   this_increase_perc  = sum1.clean/sum0.clean
#'   this_increase_abs   = sum1.clean-sum0.clean
#'
#'   return ( ( this_increase_perc >= RECIST.PROGRESSIVE_value1 ) &
#'              ( this_increase_abs >= RECIST.PROGRESSIVE_value2 ) &
#'              ( ! (count1 > count0) ) )
#' }
#'
#'
#' #' Calculates whether data indicates a complete response by RECIST criteria
#' #'
#' #' This function takes tumour measurement data and calculates the RECIST outcome.
#' #' It calculates a response for one timepoint measurement against the baseline.
#' #' If multiple timepoint measurements are to be compared to a baseline, then this
#' #' function will need to be run multiple times.
#' #' It presumes that the data that are passed here are clean (i.e., all data are present,
#' #' none are missing; all diameters add up to the sum etc.).  The data in
#' #' should also have a column that represents the minimum sum of tumour diameters
#' #' (across all timepoints) for each patient.
#' #'
#' #' @param this_data The tibble in which the column exists.
#' #' @param id_variable The column that defines each patient (i.e., the patient ID)
#' #' @param baseline_diam The variable that represents the individual diameters at baseline
#' #' @param baseline_sum The variable that represents the sum at baseline
#' #' @param min_sum The variable that represents the smallest diameter in the study (if absent, assumes this value is the baseline_sum)
#' #' @param timepoint_diam The variable that represents the individual diameters at the later timepoint
#' #' @param timepoint_sum The variable that represents the sum at the later timepoint
#' #' @param label_string The string to use for the result column
#' #' @param this_function A label for error/warning messages (by default it is the name of this function)
#' #'
#' #' @importFrom dplyr case_when
#' #' @importFrom rlang .data
#' #'
#' #' @return RECIST results (v1.1 criteria)
#' #' @export
#' calculate_RECIST_response = function( this_data,
#'                                       id_variable,
#'                                       baseline_diam,
#'                                       baseline_sum,
#'                                       min_sum = NULL,
#'                                       timepoint_diam,
#'                                       timepoint_sum,
#'                                       this_function = NA,
#'                                       label_string = "RECIST"   ) {
#'
#'   ### For error/warning messages:
#'   if ( is.na( this_function ) ) {
#'     this_function = get_current_function()
#'   }
#'
#'   if ( is.null( min_sum ) ) { min_sum = baseline_sum }
#'
#'   ###################################################################
#'   ###################################################################
#'   ### Error handling                                              ###
#'   ###################################################################
#'   ###################################################################
#'
#'   ### If the variable to be counted (this_var) is not a quosure,
#'   ### then stop.
#'   check_for_quosures( this_function, id_variable    )
#'   check_for_quosures( this_function, baseline_diam  )
#'   check_for_quosures( this_function, baseline_sum   )
#'   check_for_quosures( this_function, min_sum        )
#'   check_for_quosures( this_function, timepoint_diam )
#'   check_for_quosures( this_function, timepoint_sum  )
#'
#'   ### Check that the variables are in the data
#'   check_variables_are_in_data( this_function, this_data, id_variable    )
#'   check_variables_are_in_data( this_function, this_data, baseline_diam  )
#'   check_variables_are_in_data( this_function, this_data, baseline_sum   )
#'   check_variables_are_in_data( this_function, this_data, min_sum        )
#'   check_variables_are_in_data( this_function, this_data, timepoint_diam )
#'   check_variables_are_in_data( this_function, this_data, timepoint_sum  )
#'
#'   ###################################################################
#'   ###################################################################
#'   ### Functionality                                               ###
#'   ###################################################################
#'   ###################################################################
#'
#'   RECIST.out = this_data %>%
#'     group_by( {{id_variable}} ) %>%
#'     summarise( comp = RECIST_COMPLETE   ( sum1 = {{timepoint_sum}} ),
#'                part = RECIST_PARTIAL    ( sum0 = {{baseline_sum }},
#'                                           sum1 = {{timepoint_sum}} ),
#'                prog = RECIST_PROGRESSIVE( sum0 = {{min_sum      }},
#'                                           sum1 = {{timepoint_sum}},
#'                                           diam0 = {{baseline_diam }},
#'                                           diam1 = {{timepoint_diam}} )
#'     ) %>%
#'     mutate( {{label_string}} := case_when(
#'       comp ~ "Complete response",
#'       part ~ "Partial response",
#'       prog ~ "Progressive disease",
#'       TRUE ~ "Stable disease"
#'     ))
#'
#'   return( RECIST.out )
#'
#' }
