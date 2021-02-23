VERBOSE_REPORTING = TRUE
SILENT_REPORTING = FALSE

#' Parse information from DSD (MACRO) document
#'
#' This function extracts the relevant information from a
#' Definition Study Document (DSD), obtained via MACRO, that
#' describes the visits, sections and fields that comprise
#' the CRF for a study.  It builds a glossary key with the
#' information that it extracts to allow informative annotation
#' of fields in SSRs.
#'
#' @param XML_DSD_LOCATION The location of the DSD file as extracted from MACRO
#' @param verbose Set to true/false to override the default (VERBOSE_REPORTING)
#' @param silent Set to true/false to override the default (SILENT_REPORTING)
#'
#' @importFrom dplyr pull
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom dplyr bind_rows
#' @importFrom dplyr %>%
#' @importFrom rlang .data
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom tidyr fill
#' @importFrom xml2 read_xml
#' @importFrom xml2 xml_find_all
#' @importFrom xml2 xml_children
#' @importFrom xml2 xml_name
#' @importFrom xml2 xml_text
#'
#' @return A glossary key object.
#' @export
parse_MACRO_DSD_file = function( XML_DSD_LOCATION,
                                 verbose,
                                 silent ) {

  if ( is.null(verbose) ) { verbose = VERBOSE_REPORTING }
  if ( is.null(silent ) ) { silent  =  SILENT_REPORTING }

  section_name = "GLOSSARY"

  # Read in XML file.
  xml.all  = read_xml( XML_DSD_LOCATION )

  # Find the body tag - there should only be one of these
  # and it should include all the information we need.
  # xml.objects is the list of all the first step children
  # of the body tag.
  xml.body = xml_find_all(xml.all, ".//w:body" )
  xml.objects = xml_children( xml.body )

  # Setting up the data.frame that will be used to store
  # all the parsed information.
  glossary.tmp = data.frame(
    param_desc       =NA, # The description of the parameter
    param_name       =NA, # The actual name of the parameter
    param_closedvocab=NA,
    param_visit      =NA, # In which visit does this param appear first?
    param_testbattery=NA) # To which test battery does this param belong?

  # The visit and test battery information are set by the content
  # of the <w:p> tags that preceed the <w:tbl> information.  These
  # variables keep track of where we are in the document so that this
  # information can be added to the glossary.  It is important because
  # the column names are constructed from (i) the visit
  #                                      (ii) the test battery
  #                                     (iii) the parameter name
  # separated by underscores.
  current_visit       = NA
  current_testbattery = NA

  # This value will be incremented before each value is added to the
  # glossary data.frame.
  record.i = 0

  for ( object.i in 1:length(xml.objects) ) {

    # Getting the type of the object in the list.
    # We are interested in parsing out information in <w:p> tags
    # and <w:tbl> tags.
    object.type = xml_name(xml.objects[[object.i]])

    if ( verbose ) print_message( sprintf( "Object (%3d) : %s\n",
                                           object.i,
                                           object.type ),
                                  section_name,
                                  level=1 )

    if ( object.type == "p" ) {

      if ( verbose ) print_message( sprintf( "<p> content to be parsed\n" ),
                                    section_name,
                                    level=1 )

      # Here, the XML structure is walked down, through as follows:
      # <w:r>
      #    +--<w:t>
      # We want the content of <w:t>_________</w:t>
      xml_r = xml_find_all(xml.objects[[object.i]], ".//w:r")

      if ( length(xml_r)>0 ) {
        for ( r.i in 1:length(xml_r) ) {
          xml_t = xml_find_all( xml_r[[r.i]], ".//w:t")
          xml_t.content = xml_text( xml_t )

          if ( length( xml_t.content )>0 ){
            # If we have content in the <w:t></w:t> tag, then:
            # (1) Find the content tag (this will be 'CRF' or 'Visit'")
            content_tag  = xml_t.content %>% str_replace( ":.*", "" )
            # (2) Find the content code (this will be the test battery code,
            #     e.g., Lab_Inv_FU or Sign_Symp_Post)
            content_code = xml_t.content %>%
              str_replace( ".*? \\[(.*)\\]$", "\\1" )
            # (3) Record the content description (any other text that is
            #     not the things that we described above)
            content_description = NA
            if ( content_tag %in% c( "CRF", "Visit" ) ) {
              content_description = xml_t.content %>%
                str_replace( ".*: ", "" ) %>%
                str_replace( "\\[.*", "" )
            }

            if ( verbose ) {
              print_message( sprintf( "Line: [%s]\n", xml_t.content      ), section_name, level=2 )
              print_message( sprintf( "tag  = %s\n", content_tag         ), section_name, level=2 )
              print_message( sprintf( "code = %s\n", content_code        ), section_name, level=2 )
              print_message( sprintf( "desc = %s\n", content_description ), section_name, level=2 )
            }

            # Update the current_testbattery if we have met a new one
            # or reset the current_testbattery if we are at the start
            # of a new visit.
            if ( content_tag == "CRF" ){
              current_testbattery = content_code
            } else if ( content_tag == "Visit" ) {
              current_visit = content_code
              current_testbattery = NA
            }
          }
        }
      }


    } else if ( object.type == "tbl" ) {

      if ( verbose ) print_message( sprintf( "<tbl> content to be parsed\n"),
                                    section_name,
                                    level=1 )

      # Here, the XML structure is walked down, through as follows:
      # <w:tr>
      #    +--<w:tc>
      #          +--<w:p>
      #                +--<w:r>
      #                      +--<w:t>
      # We want the content of <w:t>_________</w:t>

      xml.tr = xml_find_all(xml.objects[[object.i]], ".//w:tr" )

      for ( tr.i in 1:length( xml.tr )) {



        if ( FALSE ) {
          record.i = 0
          object.i = 1
          object.type = xml_name(xml.objects[[object.i]])
          xml.tr = xml_find_all(xml.objects[[object.i]], ".//w:tr" )
          tr.i = 1
          tc.i = 1
          xml_p = xml_find_all(xml_tc[[tc.i]], ".//w:p")
          p.i = 1
          xml_r = xml_find_all(xml_p[[p.i]], ".//w:r")
          r.i = 1
        }



        # Every <w:tr> element is the start of a new row
        # and therefore the start of a new parameter to be recorded.
        # This list structure is used to record all the relevant
        # information about this new parameter.
        this.record_information = list( param_desc        = NA,
                                        param_name        = NA,
                                        param_closedvocab = NA,
                                        param_visit       = NA,
                                        param_testbattery = NA )

        xml_tc = xml_find_all(xml.tr[[tr.i]], ".//w:tc")

        # The variable description is held in cell #1
        # The variable name is held in cell #2
        # of the table row.  We are only interested in
        # the values of these two cells so we only need
        # to set tc.i to 1 and 2.
        for ( tc.i in 1:length(xml_tc) ) {

          if ( tc.i %in% c( 1,2,4 ) ) {
            xml_p = xml_find_all(xml_tc[[tc.i]], ".//w:p")
            for( p.i in 1:length(xml_p) ) {
              xml_r = xml_find_all(xml_p[[p.i]], ".//w:r")
              if ( length(xml_r)>0 ) {
                for ( r.i in 1:length(xml_r) ) {

                  xml_t = xml_find_all( xml_r[[r.i]], ".//w:t")
                  xml_t.content = xml_text( xml_t )

                  if ( length( xml_t.content )>0 ){
                    # If we have content in the <w:t></w:t> tag, then:
                    # - if tc.i = 1 we have the description.
                    # - if tc.i = 2 we have the name
                    # - if tc.i = 4 we have the optional values
                    # If the value isn't NA, then we have already added
                    # some information.  If this is the case, then this
                    # is a description that is spread over multiple cells.
                    # We just keep concatenating information to the cell
                    # to obtain the whole thing.

                    record.store_here = tc.i
                    # If we are at tc.i = 4 we are at the values
                    # that this variable can take.  We don't want to
                    # store the numeric prefix to these definitions, so
                    # we should remove it.
                    if ( tc.i == 4 ) {

                      record.store_here = 3

                      # ALL OPTIONS SHOULD START WITH A "X=", where
                      # X is a whole number, starting at 1.  If a record
                      # doesn't start with this, then it should be tagged
                      # on to the last list item.

                      if (xml_t.content %>% str_detect("^\\d+=")) {
                        xml_t.content = xml_t.content %>%
                          str_replace(".*=","")

                        if ( is.na(this.record_information[[ record.store_here ]][1]) ) {
                          this.record_information[[ record.store_here ]] = xml_t.content
                        } else {
                          this.record_information[[ record.store_here ]] = c(
                            this.record_information[[ record.store_here ]],
                            xml_t.content )
                        }

                      } else {
                        # There is no preceeding X= so this bit of text needs to be
                        # added to the previous one.  If I was being strict,
                        # I wouldn't let a BRAND new record be made here, as it
                        # violates the formatting that is expected.  But I've left it
                        # in, on the assumption that I can't make many assumptions.

                        if ( is.na(this.record_information[[ record.store_here ]][1]) ) {
                          this.record_information[[ record.store_here ]] = xml_t.content
                        } else {

                          last_item = length(this.record_information[[ record.store_here ]])
                          this.record_information[[ record.store_here ]][last_item] =
                            sprintf( "%s%s",
                                     this.record_information[[ record.store_here ]][last_item],
                                     xml_t.content )
                        }

                      }
                    } else {

                      if ( is.na(this.record_information[[ record.store_here ]][1]) ) {
                        this.record_information[[ record.store_here ]] = xml_t.content
                      } else {
                        this.record_information[[ record.store_here ]] = c(
                          this.record_information[[ record.store_here ]],
                          xml_t.content )
                      }
                    }
                  }
                }
              }
            }
          }
        }

        if ( !all(unlist(lapply(this.record_information,is.na))) ) {

          if ( verbose ) {

            #print(this.record_information$param_closedvocab)

            #cat( sprintf( "=================================\n") )
            print_message( sprintf( "record.i = %d\n", record.i ), "GLOSSARY_DEBUG", level=1 )
            print_message( sprintf( "Object.i = %d\n", object.i ), "GLOSSARY_DEBUG", level=1 )
            print_message( sprintf( "tr.i = %d\n", tr.i ), "GLOSSARY_DEBUG", level=1 )
            #cat( sprintf( "---------------------------------\n") )
            print_message( sprintf( "name = %s\n", this.record_information$param_name ), "GLOSSARY_DEBUG", level=1 )
            print_message( sprintf( "desc = %s\n", this.record_information$param_desc ), "GLOSSARY_DEBUG", level=1 )
            if ( !all(is.na( this.record_information$param_closedvocab )) ) {
              print_message( sprintf( "opts (%d)\n",length( this.record_information$param_closedvocab ) ), "GLOSSARY_DEBUG", level=1 )
              print_message( sprintf( "* %d: %s\n",
                            1:length(this.record_information$param_closedvocab),
                            this.record_information$param_closedvocab ), "GLOSSARY_DEBUG", level=1)
            }
            print_message( sprintf( "visit= %s\n", current_visit ), "GLOSSARY_DEBUG", level=1 )
            print_message( sprintf( "CRF  = %s\n", current_testbattery ), "GLOSSARY_DEBUG", level=1 )

          }

          # We have come to the point where we have processed a whole
          # <w:tr> and therefore a whole parameter.
          # Increment the record.i counter and add the information to
          # the glossary data.frame.
          record.i = record.i + 1

          current_name = NA
          if ( length(this.record_information$param_name)>1 ) {
            current_name = paste( this.record_information$param_name, collapse="")
          } else if (!is.na(this.record_information$param_name)) {
            current_name = this.record_information$param_name
          }

          current_desc = NA
          if ( length(this.record_information$param_desc)>1 ) {
            current_desc = paste( this.record_information$param_desc, collapse="")
          } else if (!is.na(this.record_information$param_desc)) {
            current_desc = this.record_information$param_desc
          }

          glossary.tmp[ record.i, c( "param_name",
                                     "param_desc",
                                     "param_visit",
                                     "param_testbattery")] = c(
                                       current_name,
                                       current_desc,
                                       current_visit,
                                       current_testbattery )

          glossary.tmp$param_closedvocab[record.i] = list(this.record_information$param_closedvocab)


          # Print some information out so we know things are happening.
          if ( !silent & record.i%%20==0 ) print_message( sprintf( "%04d records added to glossary\n", record.i),
                                                           section_name,
                                                           level=1)
        }
      }
    }
  }

  # NA.mask = glossary.key$param_desc == "NA"
  # glossary.key$param_desc[ NA.mask ] = NULL

  # We have processed all the elements within the body tag, so we must
  # be finished adding the information to the glossary.
  # Tidy up and generate the column names.
  glossary.key = glossary.tmp %>%
    # Remove any records that are missing a param_name.
    filter( !is.na(.data$param_name) ) %>%
    # Descriptions may need to be filled down if there are
    # multiple param_names linked to the same description.
    fill( .data$param_desc, .direction="down" ) %>%
    # Remove anything with missing visit or missing test battery info.
    filter( !is.na(.data$param_visit) & !is.na(.data$param_testbattery)) %>%
    # Remove DataItemCodes that mean nothing to us but are parsed
    # out of the XML.
    filter( .data$param_name != "DataItemCode" ) %>%
    # Finally, generate the real column names.
    mutate( column_name = sprintf( "%s_%s_%s",
                                   .data$param_visit,
                                   .data$param_testbattery,
                                   .data$param_name ) )

  return( glossary.key )
}


