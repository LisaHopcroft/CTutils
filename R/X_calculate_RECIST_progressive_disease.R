#' Calculates whether data indicates a progressive response by RECIST criteria
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
# calculate_RECIST_progressive_disease = function( this_data,
#                                                  id_variable = quo("Label"),
#                                                  baseline_sum_variable,
#                                                  timepoint_sum_variables,
#                                                  glossary = glossary.dictionary ) {
#
#   progressive_disease.tmp = this_data %>% select(
#     !!id_variable,
#     !!baseline_sum_variable,
#     !!!timepoint_sum_variables
#   ) %>%
#     mutate( baseline_sum = replace_na( {{baseline_sum_variable}}, 999 ) ) %>%
#     select( -!!baseline_sum_variable ) %>%
#     gather( column_name, sum, -(!!id_variable), -baseline_sum ) %>%
#     left_join( glossary %>%
#                  select( column_name, param_visit ),
#                by="column_name" )
#
#
#   progressive_disease.tmp2 = progressive_disease.tmp %>%
#     mutate( progressive_disease_perc = (sum-baseline_sum)/(baseline_sum)*100,
#             progressive_disease_abs  = sum-baseline_sum) %>%
#     select( -column_name, -sum, -baseline_sum ) %>%
#     mutate( progressive_disease_FLAG = (progressive_disease_perc >= (20)) &
#               (progressive_disease_abs>=5 ) )
#
#   progressive_disease.FLAG = progressive_disease.tmp2 %>%
#     select( -progressive_disease_perc, -progressive_disease_abs ) %>%
#     spread( param_visit, progressive_disease_FLAG )
#
#   colnames_torename = list(progressive_disease.FLAG %>% select(-Label) %>% colnames)
#   names(colnames_torename) = sprintf( "%s_PD_FLAG", colnames_torename )
#   progressive_disease.FLAG = progressive_disease.FLAG %>% rename( !!!colnames_torename )
#
#   progressive_disease.PERC = progressive_disease.tmp2 %>%
#     select( -progressive_disease_FLAG, -progressive_disease_abs ) %>%
#     spread( param_visit, progressive_disease_perc )
#
#   progressive_disease.ABS = progressive_disease.tmp2 %>%
#     select( -progressive_disease_FLAG, -progressive_disease_perc ) %>%
#     spread( param_visit, progressive_disease_abs )
#
#
#   progressive_disease = progressive_disease.PERC %>%
#     full_join( progressive_disease.ABS,
#                by=quo_name(id_variable),
#                suffix=c("_PD_PERC",
#                         "_PD_ABS")) %>%
#     full_join( progressive_disease.FLAG,
#                by=quo_name(id_variable))
#
#   return( progressive_disease )
#
# }
