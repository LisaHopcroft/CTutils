#' Generate a label for each visit for display purposes
#'
#' @param t The string representing the visit parameter name (original column name)
#' @param s The string to remove from the visit parameter name
#' @param v The string containing the overall visit name
#'
#' @return The label string for the visit
generate_visit_label = function(t,
                                s,
                                v) {
  return( t %>%
            as.character %>%
            str_replace( s, "" ) %>%
            str_replace( as.character(v), "" ) %>%
            str_replace( "^_", "" ) %>%
            str_replace( "_$", "" ) %>%
            str_replace( "_", " " ) )

}
