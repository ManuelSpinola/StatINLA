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
    # Intento adicional (no confirmado oficialmente, pero inofensivo):
    # desactivar MKL antes de que INLA elija el binario.
    Sys.setenv(INLA_DISABLE_MKL = "TRUE")

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

    # El binario optimizado para Intel MKL (inla.mkl.run) provoca
    # Segmentation fault en el entorno virtualizado de Connect Cloud.
    # Forzamos el uso del binario estandar (sin MKL), mas portable.
    inla_run <- file.path(inla_bin_parent, "64bit", "inla.run")
    if (file.exists(inla_run)) {
      Sys.chmod(inla_run, mode = "0755")
      INLA::inla.setOption(inla.call = inla_run)
      message("INLA configurado para usar el binario estandar: ", inla_run)
    }
  }

  mod_test_inla_server("test_inla_1")
}
