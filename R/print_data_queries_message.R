#' Print data queries summary message.
#'
#' @param dq.df The data frame containing all data queries.
#'
#' @export
#'
print_data_queries_message = function( dq.df ) {
  verb.string   = "were"
  amount.string = as.character( nrow( dq.df ) )
  object.string = "data queries"

  if ( nrow( dq.df ) == 0 ) {
    amount.string = "No"
  } else if ( nrow( dq.df ) == 1 ) {
    verb.string = "was"
    object.string = "data query"
  }

  cat( sprintf( "%s %s %s identified.\n",
                amount.string,
                object.string,
                verb.string ) )
}

