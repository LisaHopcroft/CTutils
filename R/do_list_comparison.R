#' Tabulate two separate list occurence counts and merge together.
#'
#' Perform counts of occurences in a list for two separate groups.
#' Then merge these groups based on a specific variable.
#'
#' @param this_data The tibble in which the column exists.
#' @param group1_variable The variable to use for the first group.
#' @param group1_column The column of interest for the first group (default="count").
#' @param group1_name The name of the first group (default="Group1").
#' @param group2_variable The variable to use for the second group.
#' @param group2_column The column of interest for the second group (default="count").
#' @param group2_name The name of the second group (default="Group2").
#' @param join_col The column to use to merge (default="variable").
#' @param replacement_value What to replace empty fields with (default="Missing").
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param vocab_for_variables The vocabulary for the parameters, as extracted via parse_MACRO_DSD_file()
#'
#' @importFrom dplyr select
#' @importFrom dplyr full_join
#' @importFrom dplyr mutate
#' @importFrom dplyr na_if
#' @importFrom tidyr replace_na
#' @importFrom rlang .data
#'
#' @return The merged summarised counts.
#'
#' @export
do_list_comparison = function( this_data,
                               group1_variable,
                               group1_column = "count",
                               group1_name = "Group1",
                               group2_variable,
                               group2_column = "count",
                               group2_name = "Group2",
                               join_col = "variable",
                               replacement_value = "Missing",
                               this_function = NA,
                               vocab_for_variables ) {

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

  ### Check that the values in the variable column are going to be
  ### the same for both groups, so that they will join properly.
  check_for_matching_vocabularies( this_function,
                                   group1_variable,
                                   group2_variable,
                                   vocab_for_variables )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  data.set.1 = do_list_extraction( this_data, group1_variable,
                                   this_function = this_function,
                                   vocab_for_variables = vocab_for_variables ) %>%
    # Replace empty fields with NA
    mutate( variable = as.character( na_if( .data$flag, "" ) ) ) %>%
    # Replace NAs with the missing string
    replace_na( list( variable={{replacement_value}} ) ) %>%
    select( .data$variable, .data$count )
  colnames(data.set.1)[1] = join_col

  data.set.2 = do_list_extraction( this_data, group2_variable,
                                   this_function = this_function,
                                   vocab_for_variables = vocab_for_variables  ) %>%
    # Replace empty fields with NA
    mutate( variable = as.character( na_if( .data$flag, "" ) ) ) %>%
    # Replace NAs with the missing string
    replace_na( list( variable={{replacement_value}} ) ) %>%
    select( .data$variable, .data$count )
  colnames(data.set.2)[1] = join_col

  data.out = data.set.1 %>% select( {{join_col}}, {{group1_column}}) %>%
    full_join( data.set.2 %>% select ( {{join_col}}, {{group2_column}}),
               by=join_col )

  colnames(data.out) = c( join_col, group1_name, group2_name )

  return( data.out )
}
