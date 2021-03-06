FLAG.Flextable = FALSE

#' Print the M0/M1 data
#'
#' This function receives data separated for M0 and M1
#' and prints the table as a kable if FLAG.flextable if FALSE
#' or as a flextable object if FLAG.flextable is TRUE.
#'
#' @param tb the data as a tibble
#' @param column_recode optional named list for renaming columns
#' @param std_border optional standard border definition for header
#' @param heavy_border optional heavy border definition for header
#' @param bg_colour optional background colour definition for header
#'
#' @importFrom dplyr recode
#' @importFrom flextable add_header_row
#' @importFrom flextable align
#' @importFrom flextable autofit
#' @importFrom flextable bg
#' @importFrom flextable bold
#' @importFrom flextable border_remove
#' @importFrom flextable border
#' @importFrom flextable flextable
#' @importFrom flextable hline_top
#' @importFrom flextable hline_bottom
#' @importFrom flextable set_formatter_type
#' @importFrom flextable set_header_labels
#' @importFrom flextable width
#' @importFrom knitr kable
#' @importFrom officer fp_border
#' @importFrom stringr str_replace
#'
#' @return Table object to print
#' @export
print_M0M1_flextable = function(tb,
                                column_recode = NA,
                                std_border=fp_border(color="black", width = 1),
                                heavy_border=fp_border(color="black", width = 2),
                                bg_colour="lightgrey") {

  tb.out = knitr::kable( tb )

  if ( ! PIPELINE.FLEXTABLE ) {
    if ( !is.na( column_recode ) ) {

      column.titles = colnames(tb) %>% recode( !!!column_recode ) %>%
        str_replace( "variable", "" ) %>%
        str_replace( "flag"    , "" )

      tb.out = knitr::kable( tb,
                             col.names=column.titles,
                             digits=2)
    } else {
      colnames.new = colnames(tb) %>%
        str_replace( "variable", "" ) %>%
        str_replace( "flag"    , "" )

      tb.out = knitr::kable( tb,
                             col.names=colnames.new,
                             digits=2 )
    }

  }  else {

    column_names = colnames(tb)

    tb.flex = flextable::flextable( tb ) %>%
      # Format dates consistently
      flextable::set_formatter_type( fmt_date = "%d-%m-%Y",
                                     fmt_double = "%.02f",
                                     fmt_integer = "%.0f",
                                     na_str = "-" ) %>%

      flextable::add_header_row( values=c("","M0","M1"),colwidths=c(1,2,2))  %>%
      flextable::autofit() %>%
      # theme_booktabs() %>%
      flextable::bg( part="header", bg=bg_colour ) %>%
      flextable::bold( bold = TRUE, part = "header" ) %>%
      flextable::align(align = "center", part="header", i=1) %>%
      flextable::border_remove() %>%
      flextable::hline_top(part="header",border=heavy_border) %>%
      flextable::hline_top( part="body", border=heavy_border) %>%
      flextable::hline_bottom( part="body", border=heavy_border) %>%
      flextable::border(i=1,j=2:5,part="header", border.bottom = std_border) %>%
      flextable::width(j=1  , width=1.5) %>%
      flextable::width(j=2:5, width=0.7)


    if ( !is.na( column_recode ) ) {

      tb.flex = tb.flex %>%
        # Change headers
        flextable::set_header_labels(values=column_recode )
    }

    if ( any(column_names=="variable") ) {
      # Remove any header for a 'variable"
      tb.flex = tb.flex %>%
        flextable::set_header_labels(values=c(variable=""))
    }
    if ( any(column_names=="flag") ) {
      # Remove any header for a 'variable"
      tb.flex = tb.flex %>%
        flextable::set_header_labels(values=c(flag=""))
    }

    tb.out = tb.flex
  }

  return( tb.out )

}
