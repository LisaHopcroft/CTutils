VERBOSE_REPORTING = TRUE
SILENT_REPORTING = FALSE

#' Expand glossary
#'
#' The DSD document will only define the fields for a
#' test battery for the first visit during which that test
#' battery is used.  This function will expand the glossary
#' to include all visit that use a test battery, as specificed
#' by the expansion list provided.
#'
#' @param glossary.in The existing glossary object.
#' @param expansion_list Which test batteries should be added to which visits.
#' @param verbose Set to true/false to override the default (VERBOSE_REPORTING).
#' @param silent Set to true/false to override the default (SILENT_REPORTING).
#'
#' @return An updated glossary object with new test batteries added.
#'
#' @importFrom rlang .data
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#'
#' @export
expand_glossary = function( glossary.in,
                            expansion_list,
                            verbose,
                            silent) {

  if ( is.null(verbose) ) { verbose = VERBOSE_REPORTING }
  if ( is.null(silent ) ) { silent  =  SILENT_REPORTING }

  glossary.tmp = glossary.in

  # For each of these glossary expansions...
  for ( expansion.i in 1:nrow( expansion_list) ) {
    # Identify which test battery we are repeating
    # at which visit.
    this.param_visit       = (expansion_list %>% pull(.data$param_visit)      )[expansion.i]
    this.param_testbattery = (expansion_list %>% pull(.data$param_testbattery))[expansion.i]
    this.param_visit_name  = (expansion_list %>% pull(.data$param_visit_name ))[expansion.i]

    if ( verbose ) {
      print_message( sprintf( "Expanding glossary for [%s] tests at [%s/]\n",
                              this.param_testbattery,
                              this.param_visit,
                              this.param_visit_name ) )
    }

    # Identify the existing information for this test battery,
    # update the visit to the visit needing to be added,
    # update the column name to reflect these changes.
    glossary.new = glossary.in %>%
      filter( .data$param_testbattery == !!this.param_testbattery) %>%
      mutate( param_visit = !!this.param_visit ) %>%
      mutate( param_visit_name = !!this.param_visit_name ) %>%
      mutate( column_name = sprintf( "%s_%s_%s",
                                           .data$param_visit,
                                           .data$param_testbattery,
                                           .data$param_name ) )

    # Add to the existing glossary.
    glossary.tmp = glossary.tmp %>% bind_rows( glossary.new )

    if ( !silent ) {
      print_message( sprintf( "%d new items added to glossary [%s/%s]\n",
                              nrow( glossary.new ),
                              this.param_testbattery,
                              this.param_visit)  )
    }

  }

  return( glossary.tmp )

}




