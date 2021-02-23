#' Given Date of Birth and Date of Registration, annotate the
#' patient with their EudraCT factory.
#'
#' @param dob A list of Dates of birth (as lubridate objects)
#' @param dor A list of Dates of registration (as lubridate objects)
#'
#' @importFrom lubridate ymd
#' @importFrom lubridate days
#' @importFrom lubridate years
#' @importFrom lubridate interval
#' @importFrom dplyr case_when
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom tibble tribble
#'
#'
#' @return A list of EudraCT age catetgories
#' @export
#'
#' @examples
#' convert_dates_to_EudraCT_age_categories(
#'      lubridate::ymd( "1982-04-05" ),
#'      lubridate::ymd( "2020-10-06" ) )
convert_dates_to_EudraCT_age_categories = function( dob,
                                                    dor ) {

  EudraCT.AGEGROUPS = tribble(
    ~tag                   , ~text,
    "inUtero"              , "In utero"                                          ,
    "pretermNewbornInfants", "Preterm newborn infants (gestational age < 37 wks)",
    "newborns"             , "Newborns (0-27 days)"                              ,
    "infantsAndToddlers"   , "Infants and toddlers (28 days-23 months)"          ,
    "children"             , "Children (2-11 years)"                             ,
    "adolescents"          , "Adolescents (12-17 years)"                         ,
    "adults"               , "Adults (18-64 years)"                              ,
    "elderly65To84"        , "From 65-84 years"                                  ,
    "elderlyOver85"        , "85 years and over" )


  agegroup = rep.int( NA, length( dob ) )

  for ( i in 1:length( dob ) ) {
    this.dob = dob[i]
    this.dor = dor[i]

    if ( !is.na( this.dob ) & !is.na( this.dor ) ) {

      difference_days   = floor( interval(this.dob,this.dor) %/% days  (1) )
      difference_months = floor( interval(this.dob,this.dor) %/% months(1) )
      difference_years  = floor( interval(this.dob,this.dor) %/% years (1) )

      agegroup[ i ] = case_when(
        difference_days   <= 27 ~ EudraCT.AGEGROUPS %>% filter( tag=="newborns"           ) %>% pull(text),
        difference_months <= 23 ~ EudraCT.AGEGROUPS %>% filter( tag=="infantsAndToddlers" ) %>% pull(text),
        difference_years  <= 11 ~ EudraCT.AGEGROUPS %>% filter( tag=="children"           ) %>% pull(text),
        difference_years  <= 17 ~ EudraCT.AGEGROUPS %>% filter( tag=="adolescents"        ) %>% pull(text),
        difference_years  <= 64 ~ EudraCT.AGEGROUPS %>% filter( tag=="adults"             ) %>% pull(text),
        difference_years  <= 84 ~ EudraCT.AGEGROUPS %>% filter( tag=="elderly65To84"      ) %>% pull(text),
        TRUE                    ~ EudraCT.AGEGROUPS %>% filter( tag=="elderlyOver85"      ) %>% pull(text)
      )
    }
  }

  return( agegroup )
}


