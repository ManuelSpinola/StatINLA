#' Ajustes del paquete al cargarse
#'
#' Confirmado con evidencia directa de dos formas distintas:
#' 1. GLIBC: en Linux (Posit Connect Cloud, Ubuntu 22.04, GLIBC 2.35), el
#'    binario "inla.mkl" de builds recientes (26.06.08) pedia GLIBC 2.38.
#' 2. Segfault real: incluso descargando el build especifico para Ubuntu
#'    22.04.5, el UNICO binario que INLA distribuye ahi es "inla.mkl" (la
#'    variante sin MKL, "inla" a secas, ya no viene incluida en ningun
#'    build reciente) -- y ese binario MKL muere con "Segmentation fault"
#'    en el entorno virtualizado de Connect Cloud. Es un problema conocido
#'    de MKL: al arrancar, intenta detectar automaticamente que
#'    instrucciones especiales soporta el procesador, y esa deteccion
#'    puede fallar en maquinas virtuales/contenedores.
#'
#' La solucion: usar directamente "inla.mkl.run" (ya no tiene caso buscar
#' una alternativa que no existe), pero forzando a MKL a usar un conjunto
#' de instrucciones mas conservador y compatible (SSE4_2) en vez de dejar
#' que intente detectar el procesador por su cuenta.
#'
#' @param libname,pkgname Parametros internos requeridos por R para el
#'   hook de carga de paquetes. No se usan directamente aqui.
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (Sys.info()[["sysname"]] != "Linux") {
    return(invisible(NULL))
  }

  # Le pedimos a MKL que use un set de instrucciones conservador y
  # compatible con casi cualquier procesador, en vez de auto-detectar
  # (la auto-deteccion es la que falla en el entorno virtualizado de
  # Connect Cloud y provoca el segmentation fault).
  Sys.setenv(MKL_ENABLE_INSTRUCTIONS = "SSE4_2")

  inla_bin_parent <- system.file("bin", "linux", package = "INLA")
  if (!dir.exists(inla_bin_parent)) {
    return(invisible(NULL))
  }

  inla_mkl_run <- file.path(inla_bin_parent, "64bit", "inla.mkl.run")

  if (file.exists(inla_mkl_run)) {
    Sys.chmod(inla_mkl_run, mode = "0755")
    INLA::inla.setOption(inla.call = inla_mkl_run)
    message("INLA configurado para usar inla.mkl.run con MKL_ENABLE_INSTRUCTIONS=SSE4_2: ",
            inla_mkl_run)
  } else {
    message("No se encontro ningun binario de INLA en: ", inla_mkl_run)
  }

  invisible(NULL)
}
