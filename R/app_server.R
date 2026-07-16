# --- Reemplazo automático del binario de INLA para Ubuntu 22.04 (Connect Cloud) ---
#
# QUÉ HACE: descarga el binario de INLA compilado específicamente para
# Ubuntu 22.04.5 LTS y reemplaza la carpeta "64bit" de la instalación,
# para evitar el error de GLIBC y el segfault del binario MKL.
#
# CUÁNDO CORRE: una sola vez por proceso de R (no una vez por cada
# usuario que abre la app). Por eso este bloque debe ir en R/app_server.R
# FUERA de la función app_server(), a nivel del archivo.
#
# CÓMO ELIGE LA VERSIÓN: la carpeta de Ubuntu-22.04.5 en el sitio de INLA
# tiene subcarpetas por versión (ej. "Version_26.06.08"). Este script
# primero intenta usar la carpeta que coincide exactamente con la
# versión de INLA instalada en este deploy. Si no la encuentra (porque
# salió una version nueva de INLA y el binario de Ubuntu todavia no se
# publica), usa como respaldo la última versión conocida que confirmamos
# manualmente (ver LATEST_KNOWN_VERSION abajo).
#
# IMPORTANTE: si en el futuro el respaldo deja de funcionar, hay que
# revisar a mano https://inla.r-inla-download.org/Linux-builds/Ubuntu-22.04.5%20LTS%20(Jammy%20Jellyfish)%20x86_64/
# y actualizar LATEST_KNOWN_VERSION con la carpeta más reciente que se vea ahi.

local({
  inla_bin_dir <- system.file("bin/linux/64bit", package = "INLA")
  marker_file  <- file.path(inla_bin_dir, ".ubuntu2204_ok")

  LATEST_KNOWN_VERSION <- "26.06.08"  # confirmado manualmente el 16-jul-2026

  if (nzchar(inla_bin_dir) && !file.exists(marker_file)) {
    message("StatINLA: instalando binario de INLA para Ubuntu 22.04...")

    base_url <- paste0(
      "https://inla.r-inla-download.org/Linux-builds/",
      "Ubuntu-22.04.5%20LTS%20(Jammy%20Jellyfish)%20x86_64/"
    )

    # Version de INLA instalada en este deploy (texto tal cual, sin
    # normalizar, para que coincida con el nombre de carpeta)
    instalada <- tryCatch(
      read.dcf(system.file("DESCRIPTION", package = "INLA"))[, "Version"],
      error = function(e) NA_character_
    )

    versiones_a_probar <- unique(stats::na.omit(c(instalada, LATEST_KNOWN_VERSION)))

    tmp_tgz <- tempfile(fileext = ".tgz")
    descargado <- FALSE
    version_usada <- NA_character_

    for (v in versiones_a_probar) {
      url <- paste0(base_url, "Version_", v, "/64bit.tgz")
      ok <- tryCatch({
        download.file(url, tmp_tgz, mode = "wb", quiet = TRUE)
        TRUE
      }, error = function(e) FALSE, warning = function(w) FALSE)
      if (ok) {
        descargado <- TRUE
        version_usada <- v
        break
      }
    }

    if (descargado) {
      # Reemplaza el contenido de la carpeta 64bit con el binario descargado
      utils::untar(tmp_tgz, exdir = inla_bin_dir)

      # Asegura permisos de ejecución en los binarios
      bin_files <- list.files(inla_bin_dir, full.names = TRUE)
      Sys.chmod(bin_files, mode = "0755")

      # Marca que ya se hizo, para no repetir la descarga en el mismo proceso
      file.create(marker_file)

      message("StatINLA: binario de INLA reemplazado correctamente (version ",
              version_usada, ").")
    } else {
      warning("StatINLA: no se pudo descargar el binario de Ubuntu 22.04 para ",
              "ninguna de estas versiones: ", paste(versiones_a_probar, collapse = ", "),
              ". Revisar manualmente: ", base_url)
    }
  }
})

#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  mod_test_inla_server("test_inla_1")
}
