#parameter.vocabulary = NULL


#' Count the occurence of items in a list
#'
#' Summarises the items in a list and provides a count of each.
#' Missing values are replaced with "Missing".
#'
#' @param this_data The tibble in which the column exists.
#' @param this_var The column name (if not a quosure, it will be made into one).
#' @param add_total If true, add a line to the table with the totals for all numeric columns (default=TRUE).
#' @param expand_levels If true, include all levels of the factor, as defined in the glossary (i.e., as extracted from the DSD).  Default = TRUE.
#' @param order_by_vocab If true, present levels of the factor in the order defined parameter vocabulary (i.e., as extracted from the DSD).  Default = FALSE
#' @param missing_data_limit The limit for missing data, beyond which a warning is thrown.
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param vocab_for_variables The vocabulary for the parameters, as extracted via parse_MACRO_DSD_file()
#'
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr group_by
#' @importFrom dplyr arrange
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom forcats fct_drop
#' @importFrom forcats fct_expand
#' @importFrom forcats fct_relevel
#' @importFrom purrr discard
#' @importFrom rlang quo_name
#' @importFrom rlang .data
#' @importFrom stringr str_detect
#'
#' @return Counts of levels in the factor.
#'
#' @export
do_list_extraction = function( this_data,
                               this_var,
                               add_total = TRUE,
                               expand_levels = TRUE,
                               order_by_vocab = FALSE,
                               missing_data_limit = 10,
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

  ### Check that the variables are in the data
  check_variables_are_in_data( this_function, this_data, this_var )

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.
  check_for_quosures( this_function, this_var )

  ### This can only handle one variable at a time
  check_for_individual_variable( this_function, this_var )

  ### If the vocabulary is to be expanded, but the variable is not in
  ### the parameter vocabulary, then stop.
  if ( expand_levels ) check_variable_in_vocabulary( this_function, this_var, vocab_for_variables )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  this.replacement_value = "Missing"
  this.data.tmp = replace_missing_string(this_data,
                                         this_var,
                                         replacement_value = this.replacement_value )


  if ( expand_levels ) {

    new_levels = c( this.data.tmp$flag %>% unique %>% as.character,
                    "Missing" )

    if ( length(vocab_for_variables[[quo_name(this_var)]])>0 ) {
      new_levels = c( vocab_for_variables[[quo_name(this_var)]],
                      "Missing" )
    }

    this.data.tmp = this.data.tmp %>%
      mutate( flag=fct_drop(.data$flag) ) %>%
      mutate( flag=fct_expand(.data$flag,new_levels)) %>%
      mutate( flag=fct_relevel(.data$flag,new_levels))

  } else {

    new_levels = levels(this_data[[quo_name(this_var)]]) %>%
      discard(~str_detect(.x,"^$") )

    this.data.tmp = this.data.tmp %>%
      mutate( flag=fct_expand(.data$flag,new_levels)) %>%
      mutate( flag=fct_relevel(.data$flag,new_levels))
  }

  count.out =  this.data.tmp %>%
    group_by( .data$flag, .drop=FALSE ) %>%
    summarise( count=n() )

  ### If not ordering by vocabulary, use alphabetical ordering
  if ( !order_by_vocab ) {
    count.out = count.out %>% arrange( .data$flag )
  }

  ### Add Missing (if present) at the bottom
  if ( any( str_detect( count.out %>% pull( .data$flag ),
                   this.replacement_value ) ) ) {
    count.out = count.out %>%
      mutate( flag = fct_relevel(.data$flag,
                                 this.replacement_value,
                                 after = Inf ) ) %>%
      arrange( .data$flag )

  }


  ### Add a line for the total if requested
  count.out = add_total_line( count.out, "flag" )

  ### Calculate data missing
  missing_data.list = tibble(
    variable = quo_name( this_var ),
    numerator = count.out %>% filter( .data$flag == this.replacement_value ) %>% pull( .data$count ),
    denominator = count.out %>% filter( .data$flag == "Total" ) %>% pull( .data$count )
  ) %>%
    mutate( perc_missing = 100 * .data$numerator/.data$denominator)

  ### Check for missing data
  check_for_missing_data_breach( this_function,
                                 missing_data.list,
                                 perc_limit = missing_data_limit )

  if ( !add_total ) { count.out = count.out %>% filter( .data$flag != "Total" ) }

  return( count.out )
}
