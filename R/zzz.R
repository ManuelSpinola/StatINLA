#' Ajustes del paquete al cargarse
#'
#' En Linux (Posit Connect Cloud), el binario de INLA que se instala junto
#' con el paquete para esta version de Ubuntu solo trae compilada la
#' variante optimizada con Intel MKL (inla.mkl / inla.mkl.run). El script
#' generico "inla.run" tambien viene incluido, pero es un envoltorio que
#' espera un binario "inla" que en este build no existe -- por eso
#' apuntamos a inla.mkl.run explicitamente, que si tiene su binario real.
#'
#' Nota: la sospecha original de que el binario MKL causaba un
#' segmentation fault en Connect Cloud nunca se confirmo con evidencia
#' directa -- la vamos a confirmar (o descartar) con este cambio, usando
#' el diagnostico con processx que ya tenemos en mod_test_inla.R.
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

  inla_mkl_run <- file.path(inla_bin_parent, "64bit", "inla.mkl.run")

  if (file.exists(inla_mkl_run)) {
    Sys.chmod(inla_mkl_run, mode = "0755")
    INLA::inla.setOption(inla.call = inla_mkl_run)
    message("INLA configurado para usar el binario MKL: ", inla_mkl_run)
  } else {
    message("No se encontro el binario MKL de INLA en: ", inla_mkl_run,
            " -- INLA usara lo que traiga por defecto.")
  }

  invisible(NULL)
}
