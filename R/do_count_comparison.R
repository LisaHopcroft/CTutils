#' Tabulate two separate Yes/No counts and merge together.
#'
#' Perform two separate tabulation of Yes/No entries in one or more columns,
#' for two separate groups.  Then merge these groups based on a specific
#' variable.  Use this where Yes/No counts for the same measurements are
#' done at different timepoints (e.g., planned invasive surgery and actual
#' invasive surgery).
#'
#' @param this_data The tibble in which the column exists.
#' @param group1_variables The variable to use for the first group.
#' @param group1_column The column of interest for the first group (default="Yes").
#' @param group1_name The name of the first group (default="Group1").
#' @param group2_variables The variable to use for the second group.
#' @param group2_column The column of interest for the second group (default="Yes").
#' @param group2_name The name of the second group (default="Group1").
#' @param join_col The column to use to merge (default="variable").
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param key_for_variables Glossary to use for parameters.
#'
#' @importFrom dplyr select
#' @importFrom dplyr full_join
#'
#' @return The merged summarised counts.
#'
#' @export
do_count_comparison = function( this_data,
                                group1_variables,
                                group1_column = "Yes",
                                group1_name = "Group1",
                                group2_variables,
                                group2_column = "Yes",
                                group2_name = "Group2",
                                join_col = "variable",
                                this_function = NA,
                                key_for_variables = glossary.key ) {

  ### For error/warning messages:
  if ( is.na( this_function ) ) {
    #this_function = get_current_function()
    this_function = as.character( match.call()[[1]] )
  }

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function, this_data, group1_variables )
  check_variables_are_in_data( this_function, this_data, group2_variables )

  ### Check that the values in the variable column are going to be
  ### the same for both groups, so that they will join properly.

  check_for_matching_variables( this_function,
                                group1_variables,
                                group2_variables,
                                key_for_variables )

  data.set.1 = do_count( this_data,
                         group1_variables,
                         this_function = this_function,
                         key_for_variables = key_for_variables ) %>%
    mutate( variable = as.character( .data$variable ))

  data.set.2 = do_count( this_data,
                         group2_variables,
                         this_function = this_function,
                         key_for_variables = key_for_variables  ) %>%
    mutate( variable = as.character( .data$variable ))

  data.out = data.set.1 %>% select( {{join_col}}, {{group1_column}}) %>%
    full_join( data.set.2 %>% select ( {{join_col}}, {{group2_column}}),
               by=join_col )

  colnames(data.out) = c( join_col, group1_name, group2_name )

  return( data.out )
}
