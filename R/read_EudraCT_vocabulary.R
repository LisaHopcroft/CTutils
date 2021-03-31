read_EudraCT_vocabulary = function() {
  
  EudraCT_vocabulary = read_excel( system.file( "resources",
                                                "EudraCT_vocabulary.xlsx",
                                                package = "CTutils" ),
                                   sheet="EudraCT" )
  
  RMS_vocabulary = read_excel( system.file( "resources",
                                            "EudraCT_vocabulary.xlsx",
                                            package = "CTutils" ),
                               sheet="RMS" )
  
  return( EudraCT = EudraCT_vocabulary,
          RMS = RMS_vocabulary )
  
}