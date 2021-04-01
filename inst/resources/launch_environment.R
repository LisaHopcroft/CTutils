#####################################################################
### Set up your preferred knitr preferences here ####################
#####################################################################
knitr::opts_chunk$set( fig.width =6,
                       fig.height=4,
                       ft.align="left",
                       echo      =FALSE,
                       message   =FALSE,
                       warning   =FALSE,
                       error     =FALSE )

#####################################################################
### Include any libraries necessary to generate the report here #####
#####################################################################
require( gtable )
require( cowplot )
require( readr )
require( ggrepel )
require( tidyverse )
require( stringr )
require( lubridate )
require( RColorBrewer )
require( knitr )
require( psych )
require( magrittr )
require( flextable )
require( rmarkdown )
require( officer )
require( grid )
require( pander )
require( tidyverse )
require( xml2 )
require( broom )
require( flextable )
require( eudract )
library( readxl )

#####################################################################
### Include any libraries necessary to generate the report here #####
#####################################################################
source( "LOCAL_functions.R" )
