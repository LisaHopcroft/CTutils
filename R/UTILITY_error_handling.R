###################################################################
### Internal utility functions                                  ###
###################################################################

### Get the name of the function currently running?
get_current_function = function() as.character( match.call()[[1]] )

### Convert quo/quos to strings
convert_quo_to_strings = function( q ) {
  q.asStrings = lapply( q, as_label ) %>%
    unlist %>% grep( pattern="~", value=TRUE, invert=TRUE )
  return( q.asStrings )
}

###################################################################
### Internal check functions for ERROR handling                 ###
###################################################################

### Check that the variables are acutally in the data
check_variables_are_in_data = function( f, d, vars ) {
  vars.asString = convert_quo_to_strings( vars )

  if ( !all( vars.asString %in% colnames( d ) ) ) {

    missing_vars.list = setdiff( vars.asString, colnames( d ) )

    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "%s is not a column in the dataset",
                  missing_vars.list ) )
    )
    )
  }
}

### Check that a variable is numeric
### Used in replace_missing_number()
check_variable_is_numeric = function( f, d, v ) {

  if ( !is.numeric( d[[quo_name(v)]] ) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Value %s should be numeric",
                  quo_name(v) ) )
    )
    )
  }
}


### Check that a variable is numeric
### Used in replace_missing_string()
check_variable_is_character = function( f, d, v ) {
  if ( !is.character( d %>% pull(!!v) ) & !is.factor(d %>% pull(!!v)) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Value %s should be a string or a factor",
                  quo_name(v) ) )
    )
    )
  }
}


### Check that a value to be used for missing data is in the listed
### levels to be reported
check_missing_value_is_present = function( f, value, levels ) {


  if ( ! (value %in% levels) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Missing value %s is not an expected level",
                  value ) )
    )
    )
  }
}

### Check that the variable is a quosure or a list of quosures
check_for_quosures = function( f, var ) {
  if ( !str_detect( class( var )[[1]], "^quosure[s]?$" ) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Variable supplied is not a quosure or a list of quosures" ) )
    )
    )
  }
}

### Check that the variable is a quosure, list of quosures, sym or list of syms
check_for_variable_format = function( f, var ) {
  if ( !str_detect( class( var )[[1]], "^quosure[s]?$" ) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Variable supplied is not a quosure or a list of quosures" ) )
    )
    )
  }
}

### Check that the variable has an entry in the vocabulary list
### This is used in eg do_list_extraction() when expand_vocab is true
check_variable_in_vocabulary = function( f, var, vocab ) {
  if ( !quo_name(var) %in% names( vocab ) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "Request to expand vocabulary but variable (%s) is not in the vocabulary list",
                  quo_name(var)) )
    )
    )
  }
}

### Check that we have only one quosure, not a list of quosures
### Relevant for do_list_extraction()
check_for_individual_variable = function( f, var ) {
  if ( stringr::str_detect( class( var )[[1]], "quosures" ) ) {
    stop( print_error(
      c( sprintf( "%s()", f ),
         sprintf( "This function can only handle one quosure at a time (not quosures)" ) )
    )
    )
  }
}

###################################################################
### Internal check functions for WARNING handling               ###
###################################################################

### Add a warning if a glossary term is missing for a variable
check_glossary = function(f, var, g)  {
  vs = lapply( var, as_label ) %>%
    unlist %>% grep( pattern="~", value=TRUE, invert=TRUE )

  if ( any( lapply( g[vs], is.null ) %>% unlist ) ) {
    vs_missing = vs[ lapply( g[vs], is.null ) %>% unlist ]
    warning( print_warning(
      c( sprintf( "%s()", f ),
         sprintf( "Glossary term missing for variable '%s'", vs_missing ) )
    )
    )
  }
}

### Add a warning if any variables breach the limit on
### percentage missing data that is acceptable - note that
### the percentage is calculated previously as data structures
### are all different
check_for_missing_data_breach = function( f, d,
                                          v = "variable",
                                          p = "perc_missing",
                                          perc_limit ) {
  missing.index = which( d[[p]] >= perc_limit )

  if ( any( missing.index ) ) {

    warning( print_warning(
      c( sprintf( "%s()", f ),
         sprintf( "%.2f%% data missing for '%s'",
                  d[[p]][missing.index],
                  d[missing.index,] %>% pull( !!v ) %>% as.character ),
         sprintf( "This meets or exceeds the missing data limit of %d%%",
                  rep.int( perc_limit, length(missing.index) ) ))
    )
    )
  }

}

### Check that the glossary values for both groups are the same.
### For do_count_comparison()
check_for_matching_variables = function(f,
                                        var1,
                                        var2,
                                        g) {

  var1.s = convert_quo_to_strings( var1 )
  var2.s = convert_quo_to_strings( var2 )

  var1.recoded = dplyr::recode( var1.s, !!!g )
  var2.recoded = dplyr::recode( var2.s, !!!g )

  if ( !( all( var1.recoded %in% var2.recoded ) &
          all( var2.recoded %in% var1.recoded ) ) ) {

    different_variables.list = c( setdiff( var1.recoded, var2.recoded ),
                                  setdiff( var2.recoded, var1.recoded ) )

    warning( print_warning(
      c( sprintf( "%s()", f ),
         sprintf( "Variable only found in one group: '%s'", different_variables.list ) )
    )
    )
  }

}

### Check for matching vocabularies for two variables
### For do_list_comparison()
check_for_matching_vocabularies = function(f,
                                           var1,
                                           var2,
                                           vocab) {

  var1.s = convert_quo_to_strings( var1 )
  var2.s = convert_quo_to_strings( var2 )

  var1.vocab = vocab[[var1.s]]
  var2.vocab = vocab[[var2.s]]

  if ( !( all( var1.vocab %in% var2.vocab ) &
          all( var2.vocab %in% var1.vocab ) ) ) {

    different_variables.list = c( setdiff( var1.vocab, var2.vocab ),
                                  setdiff( var2.vocab, var1.vocab ) )

    warning( print_warning(
      c( sprintf( "%s()", f ),
         sprintf( "Vocabulary item only found in one list: '%s'", different_variables.list ) )
    )
    )
  }

}

### Where counts are all 0 or 1, are you sure you shouldn't
### be using the extended dataset?
### Used in calculate_number_of_instances()
check_for_extended_data_use = function( f, l ) {
  if ( all( l %in% c(0,1,NA)) ){
    warning( print_warning(
      c( sprintf( "%s()", f ),
         sprintf( "All instance counts are 0, 1 or NA" ),
         sprintf( "Check whether you should be using the extended data" )
      )
    )
    )
  }
}
