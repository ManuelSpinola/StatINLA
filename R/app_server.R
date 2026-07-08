#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Solo en Linux (Connect Cloud): el binario de INLA que viene del repo
  # "testing" necesita una version de GLIBC mas nueva de la que trae
  # Ubuntu 22.04. Aqui lo reemplazamos por el build oficial compilado
  # especificamente para Ubuntu 22.04.5, que coincide con nuestra version
  # de INLA (26.06.08).
  if (Sys.info()[["sysname"]] == "Linux") {
    inla_bin_parent <- system.file("bin", "linux", package = "INLA")
    if (dir.exists(inla_bin_parent)) {
      tryCatch({
        tgz_url <- paste0(
          "https://inla.r-inla-download.org/Linux-builds/",
          "Ubuntu-22.04.5%20LTS%20(Jammy%20Jellyfish)%20x86_64/",
          "Version_26.06.08/64bit.tgz"
        )
        tmp <- tempfile(fileext = ".tgz")
        utils::download.file(tgz_url, tmp, mode = "wb", quiet = TRUE)
        utils::untar(tmp, exdir = inla_bin_parent)
        unlink(tmp)
        Sys.chmod(
          list.files(inla_bin_parent, recursive = TRUE, full.names = TRUE),
          mode = "0755"
        )
        message("Binario de INLA para Ubuntu 22.04 instalado correctamente.")
      }, error = function(e) {
        message("No se pudo instalar el binario de INLA para Ubuntu 22.04: ",
                conditionMessage(e))
      })
    }
  }

  mod_test_inla_server("test_inla_1")
}
