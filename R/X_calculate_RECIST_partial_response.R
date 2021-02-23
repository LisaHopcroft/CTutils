#' Calculates whether data indicates a partial response by RECIST criteria
#'
#' This function takes data and
#'
#' @param this_data The tibble in which the column exists.
#' @param id_variable The column that defines each patient (i.e., the patient ID)
#' @param baseline_diameter_variable Which variable represents the diameter measurement at baseline?
#' @param timepoint_diameter_variables Which variables represent the diameter measurements at subsequent timepoints?
#' @param glossary The glossary dictionary.
#'
#' @importFrom dplyr select
#' @importFrom dplyr left_join
#' @importFrom dplyr full_join
#' @importFrom rlang .data
#' @importFrom tidyr spread
#'
#' @return
#' @export
#'
#' @examples
# calculate_RECIST_partial_response = function( this_data,
#                                               id_variable = quo("Label"),
#                                               baseline_sum_variable,
#                                               timepoint_sum_variables,
#                                               glossary = glossary.dictionary ) {
#   ### Calculating the complete response for each patient (TRUE/FALSE)
#
#   partial_response.tmp = this_data %>% select(
#     !!id_variable,
#     !!baseline_sum_variable,
#     !!!timepoint_sum_variables
#   ) %>%
#     mutate( baseline_sum = replace_na( {{baseline_sum_variable}}, 999 ) ) %>%
#     select( -!!baseline_sum_variable ) %>%
#     gather( column_name, sum, -(!!id_variable), -baseline_sum ) %>%
#     left_join( glossary %>% select( column_name, param_visit ),
#                by="column_name" )
#
#
#   partial_response.tmp2 = partial_response.tmp %>%
#     mutate( partial_response_val = (sum-baseline_sum)/(baseline_sum)*100 ) %>%
#     select( -column_name, -sum, -baseline_sum ) %>%
#     mutate( partial_response_FLAG = partial_response_val <= (-30) )
#
#   partial_response.FLAG = partial_response.tmp2 %>%
#     select( -partial_response_val ) %>%
#     spread( param_visit, partial_response_FLAG )
#
#   partial_response.VALUE = partial_response.tmp2 %>%
#     select( -partial_response_FLAG ) %>%
#     spread( param_visit, partial_response_val )
#
#   partial_response = partial_response.VALUE %>%
#     full_join( partial_response.FLAG,
#                by=quo_name(id_variable),
#                suffix=c("_PR_VALUE",
#                         "_PR_FLAG"))
#
#   return( partial_response )
#
# }
