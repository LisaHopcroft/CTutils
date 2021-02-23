#' Tidy/reformat a dependency
#'
#' Tidy and reformat a dependency between fields so that it can be used
#' in a filter statement. Steps are:
#' 1. remove repeated ands/ors (these might occur if a dependency was removed earlier)
#' 2. remove leading/training/internal space
#' 3. remove ands/ors at the beginning/end of the string
#' 4. replace and/or with &/|
#' 5. replace = with ==
#' 6. (but change !== back to !=)
#'
#' @param t The dependency (character string).
#'
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_trim
#' @importFrom stringr str_squish
#'
#' @return The tidied dependency.
tidy_logical_expression = function( t ) {

  out = t %>%
    ### Remove redundant, repeated ands/ors
    str_replace_all( "(and )+", "and " ) %>%
    str_replace_all( "(or )+" , "or "  ) %>%
    str_replace_all( "( and)+", " and" ) %>%
    str_replace_all( "( or)+" , " or"  ) %>%
    ### Remove trailing/beginning/internal white space
    str_trim() %>%
    str_squish() %>%
    ### Removing trailing/beginning ands/ors
    str_replace_all( " or\\s*$", "" ) %>%
    str_replace_all( "^\\s*or ", "" ) %>%
    str_replace_all( " and\\s*$", "" ) %>%
    str_replace_all( "^\\s*and ", "" ) %>%
    ### Replace text with symbols
    str_replace_all( " or ", " | " ) %>%
    str_replace_all( " and ", " & " ) %>%
    str_replace_all( "=", "==" ) %>%
    str_replace_all( "<>", "!=" ) %>%
    str_replace_all( "!==", "!=" )

  return( out )

}
