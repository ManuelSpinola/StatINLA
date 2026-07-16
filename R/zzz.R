#' Ajustes del paquete al cargarse
#'
#' HISTORIAL DE INTENTOS PARA RESOLVER EL SEGMENTATION FAULT EN POSIT
#' CONNECT CLOUD (Ubuntu 22.04, GLIBC 2.35):
#'
#' Intento 1 (ya probado, no resolvio el problema por si solo):
#'   Usar el binario que trae el paquete instalado (inla.mkl.run, la unica
#'   variante que INLA distribuye para Ubuntu 22.04.5), forzando
#'   MKL_ENABLE_INSTRUCTIONS=SSE4_2 para evitar la auto-deteccion de CPU
#'   que falla en el entorno virtualizado. El binario mkl.run tambien pide
#'   GLIBC 2.38, que no esta disponible (solo hay 2.35).
#'
#' Intento 2 (este archivo):
#'   Descargar un binario de INLA mas viejo (Ubuntu 20.04.6, version
#'   23.05.30-1, mayo 2023), que fue compilado contra una version de
#'   GLIBC mas baja y es mas probable que sea compatible. Este build es
#'   anterior al cambio de INLA hacia el paquete "fmesher" (agosto 2023),
#'   asi que existe una posibilidad real de que el "motor" viejo y la
#'   "app" moderna no se entiendan del todo -- si eso pasa, veremos un
#'   error DISTINTO al segmentation fault actual, no necesariamente peor,
#'   solo diferente. Es un experimento, no una solucion garantizada.
#'
#' Si la descarga o la configuracion del binario alternativo falla por
#' cualquier motivo, el codigo regresa automaticamente al Intento 1 como
#' respaldo, para no perder lo que ya se tenia funcionando parcialmente.
#'
#' @param libname,pkgname Parametros internos requeridos por R para el
#'   hook de carga de paquetes. No se usan directamente aqui.
#'
#' @noRd
.onLoad <- function(libname, pkgname) {
  if (Sys.info()[["sysname"]] != "Linux") {
    return(invisible(NULL))
  }

  # Se mantiene por si terminamos usando el binario MKL de respaldo.
  Sys.setenv(MKL_ENABLE_INSTRUCTIONS = "SSE4_2")

  used_alt_binary <- tryCatch(
    .use_alt_inla_binary(),
    error = function(e) {
      message("No se pudo usar el binario alternativo de INLA: ", conditionMessage(e))
      FALSE
    }
  )

  if (!used_alt_binary) {
    .use_default_mkl_binary()
  }

  invisible(NULL)
}

#' Descarga y activa un binario de INLA mas viejo (Ubuntu 20.04.6,
#' version 23.05.30-1), como alternativa al binario MKL que viene
#' instalado y que provoca segmentation fault en Connect Cloud.
#'
#' @return TRUE si se activo el binario alternativo, FALSE si no.
#' @noRd
.use_alt_inla_binary <- function() {
  alt_dir <- file.path(tempdir(), "inla_alt_binary_ubuntu2004")
  alt_tgz <- file.path(tempdir(), "inla_alt_binary_ubuntu2004.tgz")
  alt_url <- paste0(
    "https://inla.r-inla-download.org/Linux-builds/",
    "Ubuntu-20.04.6%20LTS%20(Focal%20Fossa)/Version_23.05.30-1/64bit.tgz"
  )

  ya_descargado <- dir.exists(alt_dir) &&
    length(list.files(alt_dir, recursive = TRUE)) > 0

  if (!ya_descargado) {
    dir.create(alt_dir, recursive = TRUE, showWarnings = FALSE)
    utils::download.file(alt_url, alt_tgz, mode = "wb", quiet = TRUE)
    utils::untar(alt_tgz, exdir = alt_dir)
  }

  todos_los_archivos <- list.files(alt_dir, recursive = TRUE, full.names = TRUE)

  # Buscamos el binario principal de INLA. Preferimos la variante SIN mkl
  # si viene incluida; si no, usamos la que haya (sigue siendo una
  # version distinta a la que ya sabemos que falla).
  candidatos_inla <- todos_los_archivos[
    grepl("(^|/)inla(\\.run)?$", todos_los_archivos) |
      grepl("(^|/)inla\\.mkl\\.run$", todos_los_archivos)
  ]
  candidatos_inla <- candidatos_inla[order(grepl("mkl", candidatos_inla))]

  if (length(candidatos_inla) == 0) {
    message("El paquete descargado no contiene ningun binario 'inla' reconocible.")
    return(FALSE)
  }

  inla_bin <- candidatos_inla[1]
  Sys.chmod(inla_bin, mode = "0755")
  INLA::inla.setOption(inla.call = inla_bin)
  message(
    "INLA configurado para usar binario alternativo ",
    "(Ubuntu 20.04.6, version 23.05.30-1): ", inla_bin
  )

  TRUE
}

#' Plan de respaldo: usar el inla.mkl.run que ya viene instalado con el
#' paquete, forzando instrucciones conservadoras de CPU.
#' @noRd
.use_default_mkl_binary <- function() {
  inla_bin_parent <- system.file("bin", "linux", package = "INLA")
  if (!dir.exists(inla_bin_parent)) {
    return(invisible(NULL))
  }

  inla_mkl_run <- file.path(inla_bin_parent, "64bit", "inla.mkl.run")

  if (file.exists(inla_mkl_run)) {
    Sys.chmod(inla_mkl_run, mode = "0755")
    INLA::inla.setOption(inla.call = inla_mkl_run)
    message(
      "INLA configurado para usar inla.mkl.run (respaldo) con ",
      "MKL_ENABLE_INSTRUCTIONS=SSE4_2: ", inla_mkl_run
    )
  } else {
    message("No se encontro ningun binario de INLA en: ", inla_mkl_run)
  }

  invisible(NULL)
}
