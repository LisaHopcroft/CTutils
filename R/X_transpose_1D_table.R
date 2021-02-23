# transpose_1D_table = function( table.in,
#                                count_name="New count",
#                                count_var="n",
#                                class_var="flag",
#                                column_order = list(
#                                  Yes = sym("Yes"),
#                                  No = sym("No"),
#                                  Missing = sym("Missing")
#                                ) ) {
#
#   tmp = t(table.in %>% select( {{count_var}} ) %>% pull() )
#   colnames(tmp) = table.in %>% select( {{class_var}} ) %>% pull()
#   rownames(tmp) = count_name
#
#   out = as_tibble(tmp) %>%
#     mutate( Measurement=count_name) %>%
#     select( Measurement, !!!column_order )
#
#   return( out )
# }
