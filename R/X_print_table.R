# print_table = function( table.data,
#                         caption_text="",
#                         column_strings=NA,
#                         digits = 2) {
#
#   if ( all(is.na(column_strings)) ) {
#     column_strings = colnames(table.data) %>%
#       str_replace( "^variable$", "" ) %>%
#       str_replace( "^flag$"    , "" ) %>%
#       str_replace( "^", "**" ) %>%
#       str_replace( "$", "**" ) %>%
#       str_replace( "\\*\\*\\*\\*", "" )
#   }
#
#   knitr::kable(table.data,
#                col.names = column_strings,
#                caption   = caption_text,
#                digits    = digits)
#
# }
#
