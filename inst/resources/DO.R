rm( list=ls() )

publication_date_string = format( lubridate::today(),'%y%m%d' )

### === Where will the final document be saved? ================== ###
document_dir = "! GENERATED"

### === The main document ======================================== ###
main_document.output_name = sprintf( "%s_Final-report.docx",
                                     publication_date_string )
bookdown::render_book(input       = "index.Rmd",
                      config_file = "_main.yml",
                      clean       = TRUE,
                      output_dir  = document_dir,
                      output_file = main_document.output_name )


### === Supporting document (e.g., summary  for open session ===== ###
summary_document.output_name = sprintf( "%s_Final-report_SUMMARY.docx",
                                        publication_date_string )
bookdown::render_book(input       = "index_supporting.Rmd",
                      config_file = "_summary.yml",
                      clean       = TRUE,
                      output_dir  = document_dir,
                      output_file = summary_document.output_name )


### When ready, uncomment this section for the Eudract/ClinicalTrials.gov
### report generation
### === Supporting document: EUdraCT information === ###
# summary_document.output_name = sprintf( "%s_Final-report_Eudract.docx",
#                                     publication_date_string )
# bookdown::render_book(input       = "index_supporting.Rmd",
#                       config_file = "_eudract.yml",
#                       clean       = TRUE,
#                       output_dir  = document_dir,
#                       output_file = summary_document.output_name )



######################################################################
######################################################################
### Add other documents as required, examples are provided below.  ###
### Any additional document should be specified with a .yml file.  ###
######################################################################
######################################################################

### === Supporting document (e.g., appendices) =================== ###
# appendices.output_name = sprintf( "%s_Final-report_APPENDICES.docx",
#                                   publication_date_string )
# bookdown::render_book(input       = "index_supporting.Rmd",
#                       config_file = "_appendices.yml",
#                       clean       = TRUE,
#                       output_dir  = document_dir,
#                       output_file = appendices.output_name )

### === Supporting document (e.g., data queries)================== ###
# data_queries.output_name = sprintf( "%s_Final-report_DATA-INTEGRITY.docx",
#                                     publication_date_string )
# bookdown::render_book(input       = "index_supporting.Rmd",
#                       config_file = "_data-queries.yml",
#                       clean       = TRUE,
#                       output_dir  = document_dir,
#                       output_file = data_queries.output_name )


