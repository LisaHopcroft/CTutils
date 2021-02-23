

#
# all_files_exist = function( file_list ) {
#
#   return( all( unlist( lapply( unlist( lapply( file_list, get ) ), file.exists ) ) ) )
#
# }
#
# print_file_loading_info = function( file_list ) {
#   cat( sprintf( "Loading file: %s\n",
#                 unlist( lapply( file_list, get ) ) ) )
# }
#
# record_data_query = function( patient_list,
#                               query_message,
#                               report_reference = NULL,
#                               CRF_reference = NULL ) {
#
#   data_queries.list <<- data_queries.list %>% add_row(
#     label   = patient_list,
#     message = query_message,
#     DMC_ref = report_reference,
#     CRF_ref = CRF_reference
#   )
#
# }
