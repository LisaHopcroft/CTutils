# extract_presence_n = function( this.table,
#                                this.var,
#                                presence.value=c( "Yes" ),
#                                count.column="n") {
#
#   if ( class( this.var )[1] == "quosure" ) {
#
#     n.present = sum(  this.table %>%
#                         filter( !!this.var %in% {{presence.value}} ) %>%
#                         pull( {{count.column}} ) )
#   } else {
#
#   }
#   return( n.present )
# }
