###https://github.com/tidyverse/tidyr/issues/250
unfill_vec <- function(x) {
  same <- x == dplyr::lag(x)
  ifelse(!is.na(same) & same, "", x)
}

#' Read in values from YAML and load them into the global environment
#'
#' @param yml.file The file from which to read
#'
#' @importFrom yaml read_yaml
#' @importFrom lubridate ymd
#' @importFrom dplyr %>%
#'
#' @export
read_report_YAML = function ( yml.file ="settings.yml" ) {
  yml_vars = read_yaml( yml.file ) %>%
    unlist %>% as.list


  ### Making the flextable flag a boolean
  FLEXTABLE.mask = names( yml_vars ) %>% str_detect( "PIPELINE.FLEXTABLE" )
  yml_vars[ FLEXTABLE.mask ] = as.logical( yml_vars[ FLEXTABLE.mask ] )

  ### Making the missing_data_limit a number
  MISSING_DATA_LIMIT.mask = names( yml_vars ) %>% str_detect( "PIPELINE.MISSING_DATA_LIMIT" )
  yml_vars[ MISSING_DATA_LIMIT.mask ] = as.numeric( yml_vars[ MISSING_DATA_LIMIT.mask ] )

  ### Creating the DMC object
  DMC_committee.mask = names( yml_vars ) %>% str_detect( "^DMC.COMMITTEE")
  DMC.names  = yml_vars[ DMC_committee.mask & str_detect( names(yml_vars), "NAME"  ) ] %>% unlist
  DMC.roles  = yml_vars[ DMC_committee.mask & str_detect( names(yml_vars), "ROLE"  ) ] %>% unlist
  DMC.emails = yml_vars[ DMC_committee.mask & str_detect( names(yml_vars), "EMAIL" ) ] %>% unlist

  DMC.committee = tibble(
    Role = DMC.roles,
    Name = DMC.names,
    Email = DMC.emails
    )

  ### Creating the DMC date table
  # DMC.dates = tribble(
  #   ~DMC_number, ~DMC_date        , ~Data_freeze     ,
  #   1          , ymd("2018-11-8") , ymd("2018-10-25"),
  #   2          , ymd("2019-01-22"), ymd("2018-12-21"),
  #   3          , ymd("2019-04-02"), ymd("2019-03-17"),
  #   4          , ymd("2019-10-02"), ymd("2019-09-10")
  # )

  DMC_meeting.mask = names( yml_vars ) %>% str_detect( "^DMC.MEETINGS")
  DMC.number = yml_vars[ DMC_meeting.mask & str_detect( names(yml_vars), "TITLE"  ) ] %>% unlist
  DMC.date   = yml_vars[ DMC_meeting.mask & str_detect( names(yml_vars), "DATE"   ) ] %>% unlist
  DMC.freeze = yml_vars[ DMC_meeting.mask & str_detect( names(yml_vars), "FREEZE" ) ] %>% unlist

  DMC.meetings = tibble(
    DMC_number  = DMC.number,
    DMC_date    = ymd( DMC.date ),
    Data_freeze = ymd( DMC.freeze )
  )


  ### Visit list
  VISIT.mask = names( yml_vars ) %>% str_detect( "PROTOCOL.VISIT" )
  VISIT.strings = yml_vars[ VISIT.mask ]
  VISIT.labels  = names( yml_vars )[VISIT.mask]

  VISIT.i = VISIT.labels %>% str_replace( "PROTOCOL.VISIT", "" ) %>% as.numeric %>% order
  VISIT.ORDER = VISIT.strings[ VISIT.i ] %>% as.character

  ### Creating the glossary objects
  GLOSSARY.dictionary_file = paste(yml_vars$GLOSSARY.LOCATION,
                                   yml_vars$GLOSSARY.DICTIONARY.FILE,
                                   sep="//" )
  GLOSSARY.vocab_file = paste(yml_vars$GLOSSARY.LOCATION,
                              yml_vars$GLOSSARY.VOCAB.FILE,
                              sep="//" )
  GLOSSARY.file = paste(yml_vars$GLOSSARY.LOCATION,
                        yml_vars$GLOSSARY.FILE,
                        sep="//" )
  DEPENDENCY.file = paste(yml_vars$GLOSSARY.LOCATION,
                          yml_vars$GLOSSARY.DEPENDENCY.FILE,
                          sep="//" )

  ### Converting all the DATE objects to actual dates
  DATE.mask = names( yml_vars ) %>% str_detect( "(DATE|FREEZE)$" )
  for ( this.date in which( DATE.mask ) ) {
    yml_vars[[ this.date ]] = ymd( yml_vars[ this.date ] )
  }

  ### Creating the DMC object
  yml_vars.amended = c( yml_vars[!DMC_committee.mask & !DMC_meeting.mask & !VISIT.mask],
                        list(
                          ### DMC information
                          DMC.COMMITTEE=DMC.committee,
                          DMC.MEETINGS=DMC.meetings,
                          ### Glossary locations
                          GLOSSARY.DICTIONARY.FILE=GLOSSARY.dictionary_file,
                          GLOSSARY.VOCAB.FILE     =GLOSSARY.vocab_file,
                          GLOSSARY.FILE           =GLOSSARY.file,
                          GLOSSARY.DEPENDENCY.FILE=DEPENDENCY.file,
                          ### Order of visits
                          VISIT.ORDER = VISIT.ORDER
                        )
  )

  invisible( list2env( yml_vars.amended, envir=globalenv() ) )
}
