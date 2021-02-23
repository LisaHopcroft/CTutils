#' Resolves a dependency, so that it can be used as a filter statement.
#'
#' Receives a dependency (s) and splits it into a list of its constituent
#' components.  Each component is then checked:
#' 1. Does it check against a value (i.e. does it contain a =?)
#'    a) is it a does not equal (!=) rather than an equal (=)?
#'    b) does the field exist in the glossary?
#'    c) does the value need to be translated using the vocabulary?
#'
#'
#' @param s The dependency (character string).
#' @param c The column to which this dependency belongs.
#' @param g The glossary in which we can check that fields exist.
#' @param dm The dependency map (contains vocabulary in the form that we need).
#'
#' @importFrom dplyr filter
#' @importFrom dplyr pull
#' @importFrom stringr str_replace
#' @importFrom stringr str_split
#'
#' @return The resolved dependency.
resolve_individual_filter_statement = function( s,
                                                c,
                                                g,
                                                dm ) {

  # this.dependency_string,
  # this.column_name,
  # this_glossary,
  # these_dependencies

  ### the new filter statement will be saved in s.resolved
  s.resolved = s

  ### a list of individual components in the filter statement
  s.list = str_split( s, " and | or " ) %>% unlist

  for ( i in 1:length(s.list) ) {

    ### the component
    s.item = s.list[i]

    ### If there is no equals sign, the component will not resolved
    ### (i.e., it will be TRUE or FALSE).
    ### If it has an equals sign in it it will need resolved.
    if ( str_detect( s.item, "=" )  ) {

      tmp.split = str_split( s.item, "=") %>% unlist

      this.field = tmp.split[ 1 ]
      this.value = tmp.split[ 2 ]

      ### Check whether we are actually looking at
      ### != and not =
      this.operator = "="
      if ( str_detect( this.field, "!$" ) ) {
        this.operator = "!="
        this.field    = str_replace( this.field, "!$", "" )
      }

      ### Check whether the field we are interested in
      ### actually exists in the glossary

      this.field_exists = ( g %>%
                              filter( .data$column_name == this.field ) %>%
                              nrow ) != 0

      if ( this.field_exists ) {


        this.vocab = dm %>%
          filter( .data$column_name == this.field ) %>%
          pull( .data$param_closedvocab_list ) %>%
          unlist %>% rev

        if ( this.value == "''" ) { this.vocab = NA }

        ### Do we have vocabulary that we can refer to?
        if ( all( !is.null(this.vocab) & !is.na(this.vocab) ) )  {

          ### Is the value that we're looking for actually
          ### in the vocabulary list?
          if ( this.value %in% names(this.vocab) ) {
            ### If so, construct a new version of the item,
            ### with the updated value (having referred to
            ### the vocabulary).
            s.item.resolved = sprintf( "%s%s'%s'",
                                       this.field,
                                       this.operator,
                                       this.vocab[[this.value]] )
          } else {
            ### If not, the item will be replaced with an empty string,
            ### which will effectively remove this dependency.  A warning
            ### will be printed.
            print_warning( c( sprintf( "Illegal vocabulary for field [%s]",
                                       this.field ),
                              sprintf( "provided in dependency for [%s].",
                                       c ),
                              "Dependency will be removed." ) )
            s.item.resolved = ""
          }

          s.resolved = str_replace( s.resolved,
                                    s.item,
                                    s.item.resolved )
        }
      } else {
        print_warning( c( sprintf( "Dependency string refers to field [%s]",
                                   this.field ),
                          sprintf( "that does not exist in the glossary.",
                                   c ),
                          "Dependency will be removed." ) )
        s.resolved = NA
      }

    }
  }

  s.resolved = tidy_logical_expression( s.resolved )

  return( s.resolved )

}
