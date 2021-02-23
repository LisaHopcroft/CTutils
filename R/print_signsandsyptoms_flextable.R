#' Print the signs and symptoms data
#'
#' This function receives data for signs and symptoms (toxicities)
#' and formats it appropriately.  It prints the table as a kable if
#' FLAG.flextable if FALSE or as a flextable object if FLAG.flextable
#' is TRUE.
#'
#' @param tb the data as a tibble
#' @param column_recode optional named list for renaming columns
#' @param std_border optional standard border definition for header (flextable only)
#' @param heavy_border optional heavy border definition for header (flextable only)
#' @param bg_colour optional background colour definition for header (flextable only)
#' @param highlight_colour.level1 optional background colour definition for highlight level 1 (flextable only)
#' @param highlight_colour.level2 optional background colour definition for highlight level 2 (flextable only)
#'
#' @importFrom dplyr recode
#' @importFrom dplyr mutate_if
#'
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
print_signsandsyptoms_flextable = function(tb,
                                           column_recode = NA,
                                           std_border=fp_border(color="black", width = 1),
                                           heavy_border=fp_border(color="black", width = 2),
                                           bg_colour="lightgrey",
                                           highlight_colour.level1 = "#F9E79F",
                                           highlight_colour.level2 = "#F5B7B1"
) {

  tb.out = knitr::kable( tb )

  if ( ! PIPELINE.FLEXTABLE ) {
    if ( !is.na( column_recode ) ) {
      column.titles = colnames(tb) %>% recode( !!!column_recode ) %>%
        str_replace( "variable", "" ) %>%
        str_replace( "flag"    , "" )

      tb.out = knitr::kable( tb,
                             col.names=column.titles,
                             digits=2)
    }  else {
      colnames.new = colnames(tb) %>%
        str_replace( "variable", "" ) %>%
        str_replace( "flag"    , "" )

      tb.out = knitr::kable( tb,
                             col.names=colnames.new,
                             digits=2 )

    }
  } else {

    column_names = colnames(tb)

    tb.flex = flextable::flextable( tb %>% mutate_if(is.double,as.integer) ) %>%
      # Format dates consistently
      flextable::set_formatter_type( fmt_date = "%d-%m-%Y",
                          fmt_double = "%.02f",
                          fmt_integer = "%.0f",
                          na_str = "-" ) %>%
      flextable::bg( i=~`1`>0, bg=highlight_colour.level1 ) %>%
      flextable::bg( i=~`2`>0, bg=highlight_colour.level1 ) %>%
      flextable::bg( i=~`3`>0, bg=highlight_colour.level2 ) %>%
      flextable::bg( i=~`4`>0, bg=highlight_colour.level2 ) %>%
      flextable::bg( i=~`5`>0, bg=highlight_colour.level2 ) %>%
      flextable::add_header_row( values=c("","Grade",""),colwidths=c(1,6,1))  %>%
      flextable::autofit() %>%
      # theme_booktabs() %>%
      flextable::bg( part="header", bg=bg_colour ) %>%
      flextable::bold( bold = TRUE, part = "header" ) %>%
      flextable::align(align = "center", part="header", i=1) %>%
      flextable::border_remove() %>%
      flextable::hline_top(part="header",border=heavy_border) %>%
      flextable::hline_top( part="body", border=heavy_border) %>%
      flextable::hline_bottom( part="body", border=heavy_border) %>%
      flextable::border(i=1,j=2:7,part="header",border.bottom = std_border)

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

    if ( !is.na( column_recode ) ) {

      tb.flex = tb.flex %>%
        # Change headers
        flextable::set_header_labels(values=column_recode )
    }

    tb.out = tb.flex

  }

  return( tb.out )

}
