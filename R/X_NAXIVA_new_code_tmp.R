# ### If the version of Pandoc is new enough, then set this flag
# ### to true.
#
# FLAG.Flextable = compareVersion( as.character(pandoc_version()),
#                                  "2.0" ) >= 0
#
# replace_these_values = function( x,
#                                  target="",
#                                  replacement="Missing") {
#   if ( x == target ) {
#     return( x )
#   } else {
#     return( replacement )
#   }
# }
#
#
#
#
# print_data_queries_message = function( dq.df ) {
#   verb.string   = "were"
#   amount.string = as.character( nrow( dq.df ) )
#   object.string = "data queries"
#
#   if ( nrow( dq.df ) == 0 ) {
#     amount.string = "No"
#   } else if ( nrow( dq.df ) == 1 ) {
#     verb.string = "was"
#     object.string = "data query"
#   }
#
#   cat( sprintf( "%s %s %s identified.\n",
#            amount.string,
#            object.string,
#            verb.string ) )
# }
#
#
#
# return_MRV_IVC_status <- function( MRV_info, IVC_info ) {
#   MRV_info = as.character( MRV_info )
#   IVC_info = as.character( IVC_info )
#
#   if ( all( c(MRV_info,IVC_info)=="" ) ) {
#     return( "Missing" )
#   } else if ( all( c(MRV_info,IVC_info)=="Yes" ) ) {
#     return("MRV+IVC")
#   } else if ( MRV_info=="Yes" ) {
#     return( "MRV only" )
#   } else if ( IVC_info=="Yes" ) {
#     return( "IVC only" )
#   } else {
#     return ("WEIRDNESS")
#   }
# }
#
#
