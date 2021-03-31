#' Provide necessary EudraCT and RMS vocabulary for use when generating
#' the EudraCT export
#'
#' @importFrom readxl read_excel
#'
#' @export
read_EudraCT_vocabulary = function() {
  
  EudraCT_vocabulary = readxl::read_excel( system.file( "resources",
                                                "EudraCT_vocabulary.xlsx",
                                                package = "CTutils" ),
                                   sheet="EudraCT" )
  RMS_vocabulary = readxl::read_excel( system.file( "resources",
                                            "EudraCT_vocabulary.xlsx",
                                            package = "CTutils" ),
                               sheet="RMS" )
  return( list( EudraCT = EudraCT_vocabulary,
          RMS = RMS_vocabulary ) )
}