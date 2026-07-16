Index of /Linux-builds/Ubuntu-22.04.5 LTS (Jammy Jellyfish) x86_64/
  Name	Last Modified	Size
UpParent Directory

DirectoryVersion_26.02.06
2026-02-05 13:04	-
  DirectoryVersion_26.05.02
2026-05-02 14:39	-
  DirectoryVersion_26.05.10
2026-05-10 14:57	-
  DirectoryVersion_26.05.21
2026-06-04 07:28	-
  DirectoryVersion_26.06.07
2026-06-07 05:42	-
  DirectoryVersion_26.06.08
2026-06-08 10:18	-


#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_test_inla_server("test_inla_1")
}
