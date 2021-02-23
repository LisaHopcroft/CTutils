glossary.key = NULL

#' Summarise numerical data into a table and boxplot
#'
#' This functions takes one or more numerical variables
#' (as quo or quos respectively) and summarises the data
#' as a table and as a plot.  Both are returned in a list.
#'
#' @param this_data The tibble in which the column exists
#' @param this_var The column name (if not a quosure, it will be made into one)
#' @param col_limit Limit the number of graphs per row in a facet_wrap
#' @param scale_string Whether scales should be free, free_y, free_x etc (default=free)
#' @param missing_data_limit Print warning for any result showing at least this percentage of missing data.
#' @param this_function A label for error/warning messages (by default it is the name of this function).
#' @param key_for_variables Glossary to use for parameters
#'
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr mutate
#' @importFrom dplyr n
#' @importFrom dplyr recode
#' @importFrom dplyr select
#' @importFrom dplyr summarise
#' @importFrom dplyr %>%
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 facet_wrap
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @importFrom ggplot2 geom_jitter
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 xlab
#' @importFrom ggplot2 ylab
#' @importFrom rlang .data
#' @importFrom rlang quo
#' @importFrom stats median
#' @importFrom stats IQR
#' @importFrom stats sd
#' @importFrom tidyr gather
#'
#' @return A list containing (1) table (2) plot
#' @export
do_summary = function( this_data,
                       this_var,
                       col_limit = NULL,
                       scale_string = "free",
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

  ### If the variable to be counted (this_var) is not a quosure,
  ### then stop.
  check_for_quosures( this_function, this_var )

  ###################################################################
  ###################################################################
  ### Warnings: report and record                                 ###
  ###################################################################
  ###################################################################

  ### Print warning if there is no entry in the glossary for the variable
  this_var.asStrings = convert_quo_to_strings( this_var )

  check_glossary( this_function, this_var.asStrings, key_for_variables )

  ###################################################################
  ###################################################################
  ### Functionality                                               ###
  ###################################################################
  ###################################################################

  these.values = this_data %>%
    gather( key="variable",
            value="value",
            !!! this_var ) %>%
    mutate( variable = recode( .data$variable, !!!key_for_variables ) )


  table.data = these.values %>%
    group_by( .data$variable ) %>%
    summarise( n         = n     (),
               n.missing = sum   (is.na(.data$value) ),
               min       = min   (.data$value, na.rm=TRUE),
               median    = median(.data$value, na.rm=TRUE),
               mean      = mean  (.data$value, na.rm=TRUE),
               max       = max   (.data$value, na.rm=TRUE),
               sd        = sd    (.data$value, na.rm=TRUE),
               IQR       = IQR   (.data$value, na.rm=TRUE) ) %>%
    mutate( variable = recode( .data$variable,
                               !!!key_for_variables ) )

  ### Calculate data missing
  missing_data.list = table.data %>%
    group_by( .data$variable ) %>%
    mutate( perc_missing = ( .data$n.missing / .data$n ) * 100 )  %>%
    select( .data$variable, .data$perc_missing )

  ### Check for missing data
  check_for_missing_data_breach( this_function,
                                 missing_data.list,
                                 perc_limit = missing_data_limit )


  these.values = these.values %>%
    filter( !is.na(.data$value) ) %>%
    select( .data$variable, .data$value ) %>%
    group_by( .data$variable ) %>%
    mutate( n_points = n() )

  these.values.plot = ggplot( these.values %>%
                                filter( !is.na(.data$value) ) %>%
                                select( .data$variable, .data$value ),
                              aes( x=.data$variable, y=.data$value)) +
    geom_boxplot(data=these.values %>%
                   filter(.data$n_points>1),
                 outlier.shape = NA) +
    geom_jitter( width=0.10, height=0 ) +
    theme_bw() + facet_wrap(~.data$variable,
                            ncol=col_limit,
                            scales=scale_string) +
    xlab( "" )


  return( list( table=table.data,
                plot=these.values.plot ) )

}

