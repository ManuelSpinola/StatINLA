#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Asegura permisos de ejecucion para el binario de INLA en este entorno
  inla_bin_dir <- system.file("bin", "linux", package = "INLA")
  if (dir.exists(inla_bin_dir)) {
    Sys.chmod(list.files(inla_bin_dir, recursive = TRUE, full.names = TRUE), mode = "0755")
  }

  mod_test_inla_server("test_inla_1")
}
