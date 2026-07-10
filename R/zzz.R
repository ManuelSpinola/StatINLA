#' Ajustes del paquete al cargarse
#'
#' Confirmado con evidencia directa: en Linux (Posit Connect Cloud, que
#' corre Ubuntu 22.04 con GLIBC 2.35), el binario "inla.mkl" que trae el
#' paquete de fabrica requiere GLIBC 2.38, una version mas nueva de la que
#' tiene el sistema. Por eso descargamos el build compilado
#' especificamente para Ubuntu 22.04.5, que trae el binario real "inla"
#' (compatible con GLIBC 2.35) junto a su envoltorio "inla.run".
#'
#' Importante: el chequeo de "ya esta listo" revisa el binario REAL
#' ("inla"), no solo el envoltorio ("inla.run") -- el envoltorio viene
#' incluido de fabrica aunque el binario real no este, y eso hizo que la
#' primera version de este parche se saltara la descarga sin darse cuenta.
#'
#' @param libname,pkgname Parametros internos requeridos por R para el
#'   hook de carga de paquetes. No se usan directamente aqui.
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (Sys.info()[["sysname"]] != "Linux") {
    return(invisible(NULL))
  }

  inla_bin_parent <- system.file("bin", "linux", package = "INLA")
  if (!dir.exists(inla_bin_parent)) {
    return(invisible(NULL))
  }

  inla_run <- file.path(inla_bin_parent, "64bit", "inla.run")
  inla_bin <- file.path(inla_bin_parent, "64bit", "inla")

  # Solo si el binario REAL no esta, descargamos el build para Ubuntu
  # 22.04.5 (GLIBC 2.35), que es el que coincide con Connect Cloud.
  if (!file.exists(inla_bin)) {
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
      message("Binario de INLA para Ubuntu 22.04 (GLIBC 2.35) instalado correctamente.")
    }, error = function(e) {
      message("No se pudo instalar el binario de INLA para Ubuntu 22.04: ",
              conditionMessage(e))
    })
  }

  if (file.exists(inla_bin) && file.exists(inla_run)) {
    Sys.chmod(inla_run, mode = "0755")
    Sys.chmod(inla_bin, mode = "0755")
    INLA::inla.setOption(inla.call = inla_run)
    message("INLA configurado para usar el binario estandar (Ubuntu 22.04): ", inla_run)
  } else {
    message("No se encontro un binario de INLA compatible con GLIBC 2.35 en: ", inla_bin)
  }

  invisible(NULL)
}
