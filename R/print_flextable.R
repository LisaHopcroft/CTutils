#' Print the labetests data
#'
#' This function receives data for labtests and formats it
#' appropriately.  It prints the table as a kable if FLAG.flextable
#' if FALSE or as a flextable object if FLAG.flextable is TRUE.
#'
#' @param tb the data as a tibble
#' @param column_recode optional named list for renaming columns
# @param std_border optional standard border definition for header
# @param heavy_border optional heavy border definition for header
#' @param bg_colour optional background colour definition for header
#' @param column_padding (optional) list of paddings to use for each row; add a new item to the list for each column.
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
#' @importFrom flextable valign
#' @importFrom flextable width
#' @importFrom knitr kable
# @importFrom officer fp_border
#' @importFrom stringr str_replace
#'
#' @return Table object to print
#' @export
print_flextable = function(tb,
                           column_recode = NA,
                           #std_border=officer::fp_border(color="black", width = 1),
                           #heavy_border=officer::fp_border(color="black", width = 2),
                           bg_colour="lightgrey",
                           column_padding = NA ) {

  tb.out = knitr::kable( tb )

  if ( ! PIPELINE.FLEXTABLE ) {

    if ( !all(is.na( column_recode )) ) {
      column.titles = colnames(tb) %>% recode( !!!column_recode,
                                               variable = "",
                                               flag = "" )
      tb.out = knitr::kable( tb,
                             col.names=column.titles,
                             digits=2 )
    } else {
      colnames.new = colnames(tb) %>%
        str_replace( "variable", "" ) %>%
        str_replace( "flag"    , "" )

      tb.out = knitr::kable( tb,
                             col.names=colnames.new,
                             digits=2 )

    }

  } else {

    column_names = colnames(tb)

    tb.flex = flextable::flextable( tb ) %>%
        # Format dates consistently
      flextable::set_formatter_type( fmt_date = "%d-%m-%Y",
                                     fmt_double = "%.02f",
                                     fmt_integer = "%.0f",
                                     na_str = "-" ) %>%
      # When header breaks over multiple lines,
      # the text should align at the bottom
      flextable::valign( valign = "bottom", part = "header" )  %>%
      # Header is bold and has a background colour
      flextable::bold( bold = TRUE, part = "header" ) %>%
      flextable::bg( part="header", bg=bg_colour ) %>%
      # Autofit contents
      flextable::autofit()

    if ( !all(is.na( column_recode )) ) {
      tb.flex = tb.flex %>%
        # Change headers
        flextable::set_header_labels( values=column_recode )
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

    if ( !is.na( column_padding ) ) {
      for( column.i in 1:length(column_padding) ) {
        tb.flex = tb.flex %>%
          flextable::padding( j=column.i, padding.left=column_padding[[column.i]] )
      }
    }


    tb.out = tb.flex
  }

  return( tb.out )
}
