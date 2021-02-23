#' CAlculate how much data is missing for each patient, visit and test battery.
#'
#' @param this_data The tibble representing the patient population to be checked for missing data.
#' @param this_glossary The glossary dictionary with parameter visit/test battery etc information.
#' @param id_var A quosure representing the column that contains the patient label.
#' @param site_var A quosure representing the column that contains the site name for that patient.
#' @param specification Which fields for which patients should be considered when calculating how much data is missing.
#'
#' @importFrom dplyr pull
#' @importFrom dplyr n
#' @importFrom dplyr filter
#' @importFrom dplyr inner_join
#' @importFrom dplyr select
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_all
#' @importFrom dplyr ungroup
#' @importFrom dplyr group_by
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer
#'
#' @return Missing data percentages in a tibble
#' @export
calculate_missing_data = function( this_data,
                                   this_glossary,
                                   id_var,
                                   site_var,
                                   specification ) {

  ##################################################################
  ##################################################################
  ### FOR THE EXPECTED DATA (IE THOSE DATA WITHOUT DEPENDENCIES) ###
  ##################################################################
  ##################################################################

  patient_list     = this_data %>% pull( !!id_var ) %>% unique %>% as.character
  expected_columns = specification %>% pull( .data$column_name )

  expected_data.raw = this_data %>%
    mutate_all( as.character ) %>%
    select( !!id_var, !!site_var, !!expected_columns ) %>%
    pivot_longer( -c({{id_var}}, {{site_var}}),
                  names_to = "column_name",
                  values_to = "value" ) %>%
    mutate( value = na_if( .data$value, "" ) ) %>%
    ### Required to avoid the warning:
    ###  Warning message:
    ###  Column `column_name` has different attributes on LHS and RHS of join
    ### It seems that pivot_longer adds a names attribute?
    mutate( column_name = as.character(.data$column_name)) %>%
    inner_join( specification %>% select({{id_var}},
                                         .data$column_name),
                by=c( quo_name(id_var), "column_name" )) %>%
    mutate( present = !is.na( .data$value ) )

  expected_data.annotated = expected_data.raw %>%
    inner_join( this_glossary %>% select( .data$column_name,
                                          .data$param_visit,
                                          .data$param_testbattery ),
                by="column_name")

  expected_data.final = expected_data.annotated %>%
    group_by( {{id_var}} ) %>%
    mutate( perc_missing = 100*(sum(as.numeric(!.data$present))/n()) ) %>%
    group_by( {{id_var}}, .data$param_visit ) %>%
    mutate( Label_Visit_perc_missing = 100*(sum(as.numeric(!.data$present))/n()) ) %>%
    group_by( {{id_var}}, .data$param_visit, .data$param_testbattery ) %>%
    mutate( Label_Visit_testbattery_perc_missing = 100*(sum(as.numeric(!.data$present))/n()) )

  return( expected_data.final %>% ungroup() )

}
