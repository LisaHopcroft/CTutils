#' CTutilsproject
#' 
#' @description Create a directory structure to match that used by CTutils (PHS). This function is meant to be used within RStudio by going to the File menu, then New Project.
#'
#' @param path Filepath for the project
#' @param author_name Name of the main author for the project
#' @param author_email Email of the main author for the project
#' @param author_organisation Organisation of the main author for the project
#' @param trial_name Short name of the trial (e.g., NAXIVA, SPRING)
#' @param git Initialise the project with Git
#' @param renv Initialise the project with package management using renv
#' 
#' @importFrom tibble tribble
#' @importFrom stringr str_replace
#' @importFrom dplyr %>%
#' 
#' @return New project created according to the CTutils project structure.
#' 
#' @export
#' 
#' @examples
#' \dontrun{
#' phsproject(path = file.path(getwd(), "testproj"), author_name = "A Person")
#' }
#'
CTutilsproject <- function(path,
                           author_name = "Lisa Hopcroft",
                           author_email = "Lisa.Hopcroft@phs.scot",
                           author_organisation = "Public Health Scotland",
                           trial_name = "TRIAL_NAME",
                           git = TRUE,
                           renv = FALSE) {

  if (dir.exists(path)) {
    stop("This directory already exists")
  }

  # n_scripts <- as.numeric(n_scripts)
  # stopifnot(!is.na(n_scripts) && n_scripts >= 1 && n_scripts <= 10)

  ### STEP [0]: generate the necessary directories
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(path, "! DISTRIBUTED"), showWarnings = FALSE)
  dir.create(file.path(path, "! GENERATED"), showWarnings = FALSE)
  dir.create(file.path(path, "! GENERATED/EudraCT"), showWarnings = FALSE)
  dir.create(file.path(path, "dat" ), showWarnings = FALSE)
  dir.create(file.path(path, "img"), showWarnings = FALSE)

  ### STEP [1]: generate the necessary system files
  gitignore <- c(
    ".Rproj.user",
    ".Rhistory",
    ".RData",
    ".Ruserdata",
    "",
    "# 'data' folder #",
    "dat/",
    "",
    "# Common text files that may contain data #",
    "*.[cC][sS][vV]",
    "*.[tT][xX][tT]",
    "",
    "# Excel files #",
    "*.[xX][lL][sS]*",
    "",
    "# SPSS formats #",
    "*.[sS][aA][vV]",
    "*.[zZ][sS][aA][vV]",
    "",
    "# R data files #",
    "*.[rR][dD][aA][tT][aA]",
    "*.[rR][dD][sS]",
    "",
    "# MacOS folder attributes files #",
    ".DS_Store"
  )


  rproj_settings <- c(
    "Version: 1.0",
    "",
    "RestoreWorkspace: No",
    "SaveWorkspace: No",
    "AlwaysSaveHistory: Default",
    "",
    "EnableCodeIndexing: Yes",
    "UseSpacesForTab: Yes",
    "NumSpacesForTab: 2",
    "Encoding: UTF-8",
    "",
    "RnwWeave: Sweave",
    "LaTeX: pdfLaTeX"
  )

  # collect into single text string
  gitignore <- paste(gitignore, collapse = "\n")
  rproj_settings <- paste(rproj_settings, collapse = "\n")

  # write to index file
  if (!renv) {
    writeLines("", con = file.path(path, ".Rprofile"))
  }

  writeLines(gitignore, con = file.path(path, ".gitignore"))
  writeLines(rproj_settings, con = file.path(path, paste0(basename(path), ".Rproj")))


  ### Copy Template documents to project directory
  for ( template_document in dir( system.file("resources",package="CTutils") ) ) {
    file_location = system.file( "resources",  template_document,  package = "CTutils" )
    file_name = basename( file_location )
    cat( sprintf( "Creating template document: %s\n", file_name ) )
    text_template = readLines( file_location, warn=FALSE )
    text_specific = stringr::str_replace( text_template , "<<TRIAL_NAME>>", trial_name )
    writeLines( text_specific, file.path( path, file_name ))
  }


  if (git) {
    if (Sys.info()[["sysname"]] == "Windows") {
      shell(paste("cd", path, "&&", "git init"))
    } else {
      system(paste("cd", path, "&&", "git init"))
    }
  }

  # if (renv) {
  #   if (!"renv" %in% utils::installed.packages()[, 1]) {
  #     warning("renv is not installed. Now attempting to install...",
  #             immediate. = TRUE)
  #     utils::install.packages("renv")
  #   }
  # 
  #   if (Sys.info()[["sysname"]] != "Windows" && file.access("/conf/linkage/output/renv_cache", mode = 2) == 0) {
  #     renv_rprofile <- "Sys.setenv(RENV_PATHS_ROOT = \"/conf/linkage/output/renv_cache\")"
  #   } else if (Sys.info()[["sysname"]] == "Windows" && file.access("//stats/cl-out/renv_cache/Windows_test", mode = 2) == 0) {
  #     renv_rprofile <- "Sys.setenv(RENV_PATHS_ROOT = \"//stats/cl-out/renv_cache/Windows_test\")"
  #   } else {
  #     renv_rprofile <- ""
  #   }
  #   writeLines(renv_rprofile, con = file.path(path, ".Rprofile"))
  # 
  #   renv::init(project = file.path(getwd(), path))
  # }
}
