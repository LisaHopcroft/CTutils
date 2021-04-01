
output_file = paste( EXTRACT.LOCATION,
                     EXTRACT.RDAT,
                     sep="//")

CTutils::print_message( sprintf( "Saving to %s.", output_file ) )

save_image(infile = output_file)

