
# generate_basic_presence_table = function( this.data,
#                                           this.var,
#                                           count_name = "Presence count" ) {
#
#   if ( class( this.var )[1] == "quosure" ) {
#
#     table.out = replace_missing_values( this.data, this.var)
#     table.out = refactor_variable( table.out, quo( flag ) )
#     table.toprint.tmp = table.out %>% count( flag, .drop=FALSE  )
#     n.positive        = extract_presence_n( table.toprint.tmp, quo(flag) )
#     table.toprint     = transpose_1D_table( table.toprint.tmp,
#                                             count_name=count_name )
#
#     return( list( table.toprint = table.toprint,
#                   table.data    = table.out,
#                   n.positive    = n.positive      ) )
#
#   } else {
#     return( NULL )
#   }
#
# }
