#' Tabulate Yes/No entries in one or more columns.
#'
#' This function takes a data frame and defined list of columns
#' (optionally one) and summarises the numbers of Yes/No in a table.
#'
#' @param this_data The tibble in which the column exists.
#' @param this_var The column name (if not a quosure, error will be thrown).  Also defines the order of the rows.
#' @param missing_value The text to use for missing (i.e., "") entries (default is Missing)
#' @param these_levels Which levels to display (default = Yes/No/Missing).
#' @param missing_data_limit Print warning for any result showing at least this percentage of missing data.
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param key_for_variables Glossary to use for parameters.
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr mutate_if
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr ungroup
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_expand
#' @importFrom forcats fct_relevel
#' @importFrom purrr discard
#' @importFrom rlang as_label
#' @importFrom rlang quo_name
#' @importFrom rlang syms
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#' @importFrom tidyr replace_na
#' @importFrom tidyr spread
#'
#' @return Counts of levels in the factor.
#'
#' @export
do_count = function( this_data,
                     this_var,
                     missing_value = "Missing",
                     these_levels = c("Yes", "No", "Missing" ),
                     missing_data_limit = 10,
                     this_function = NA,
                     key_for_variables = glossary.key ) {

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

  ### If the value to be used for missing data is not in the levels
  ### specificed, then stop.
  check_missing_value_is_present( this_function, missing_value, these_levels )

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.
  check_for_quosures( this_function, this_var )

  ###################################################################
  ###################################################################
  ### Warnings: report and record                                 ###
  ###################################################################
  ###################################################################

  ### Print warning if there is no entry in the glossary for the variable
  check_glossary( this_function, this_var, key_for_variables )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  this_var.asStrings = convert_quo_to_strings( this_var )

  ### Calculate counts
  table.data = this_data %>%
    gather( key="variable",
            value="value",
            !!! this_var ) %>%
    mutate( value = as.character(na_if( .data$value, "" ) ) ) %>%
    replace_na( list( value=missing_value ) ) %>%
    mutate( value = factor(.data$value, levels = these_levels) ) %>%
    group_by( .data$variable, .data$value ) %>%
    summarise( count=n() ) %>%
    spread( fill=0, .data$value, .data$count, drop=FALSE  ) %>%
    ungroup() %>%
    mutate( variable = factor(.data$variable,levels=this_var.asStrings)) %>%
    #mutate_if( is.numeric, as.integer ) %>%
    arrange( .data$variable )

  table.toprint = table.data %>%
    ungroup() %>%
    mutate( variable = recode( .data$variable, !!!key_for_variables ) ) %>%
    mutate_if( is.double, as.integer ) #%>%
  ### If the above causes problems where the column title is `0`,
  ### you might have to use this.
  #mutate(across(where(is.double), as.integer))

  ### Calculate data missing
  missing_data.list = table.data %>%
    group_by( .data$variable ) %>%
    mutate(  denominator = sum(!!!syms(these_levels) ),
             numerator   = sum(!!!syms(missing_value) ) ) %>%
    mutate( perc_missing = ( .data$numerator / .data$denominator ) * 100 )  %>%
    select( .data$variable, .data$perc_missing )

  ### Check for missing data
  check_for_missing_data_breach( f = this_function,
                                 d = missing_data.list,
                                 v = "variable",
                                 p = "perc_missing",
                                 perc_limit = missing_data_limit )

  return( table.toprint )

}


