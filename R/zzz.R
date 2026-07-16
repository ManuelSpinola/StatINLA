#' Ajustes del paquete al cargarse
#'
#' HISTORIAL DE INTENTOS PARA RESOLVER EL SEGMENTATION FAULT EN POSIT
#' CONNECT CLOUD (Ubuntu 22.04, GLIBC 2.35):
#'
#' Intento 1 (el actual, primero en probarse):
#'   Descargar el binario de INLA compilado especificamente para Ubuntu
#'   22.04.5 LTS (la misma version de Ubuntu que usa Connect Cloud),
#'   eligiendo automaticamente la carpeta de version que coincide con el
#'   INLA instalado en este deploy (con respaldo a la ultima version
#'   confirmada manualmente si no hay coincidencia exacta). Esto evita
#'   tanto el segmentation fault del binario MKL como el error de
#'   seccion "stiles" que aparecia al usar un binario de otra version
#'   de INLA (confirmado en despliegue del 16-jul-2026).
#'
#' Intento 2 (respaldo, si el Intento 1 falla):
#'   Descargar un binario de INLA mas viejo (Ubuntu 20.04.6, version
#'   23.05.30-1, mayo 2023), que fue compilado contra una version de
#'   GLIBC mas baja y es mas probable que sea compatible. Este build es
#'   anterior al cambio de INLA hacia el paquete "fmesher" (agosto 2023),
#'   asi que existe una posibilidad real de que el "motor" viejo y la
#'   "app" moderna no se entiendan del todo.
#'
#' Intento 3 (ultimo respaldo):
#'   Usar el binario que trae el paquete instalado (inla.mkl.run, la
#'   unica variante que INLA distribuye para Ubuntu 22.04.5), forzando
#'   MKL_ENABLE_INSTRUCTIONS=SSE4_2 para evitar la auto-deteccion de CPU
#'   que falla en el entorno virtualizado.
#'
#' Si un intento falla por cualquier motivo, el codigo pasa
#' automaticamente al siguiente, para no perder lo que ya se tenia
#' funcionando parcialmente.
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

  used_2204 <- tryCatch(
    .use_ubuntu2204_binary(),
    error = function(e) {
      message("Intento 1 (Ubuntu 22.04) fallo: ", conditionMessage(e))
      FALSE
    }
  )

  if (!used_2204) {
    used_2004 <- tryCatch(
      .use_alt_inla_binary(),
      error = function(e) {
        message("Intento 2 (Ubuntu 20.04) fallo: ", conditionMessage(e))
        FALSE
      }
    )

    if (!used_2004) {
      .use_default_mkl_binary()
    }
  }

  invisible(NULL)
}

#' Intento 1: reemplaza la carpeta 64bit de la instalacion de INLA con el
#' binario compilado para Ubuntu 22.04.5 LTS. Elige la carpeta de version
#' que coincide con el INLA instalado en este deploy; si no la encuentra,
#' usa como respaldo la ultima version confirmada manualmente.
#'
#' @return TRUE si se activo el binario de Ubuntu 22.04, FALSE si no.
#' @noRd
.use_ubuntu2204_binary <- function() {
  inla_bin_dir <- system.file("bin", "linux", "64bit", package = "INLA")
  if (!nzchar(inla_bin_dir)) {
    return(FALSE)
  }

  marker_file <- file.path(inla_bin_dir, ".ubuntu2204_ok")

  # Si ya se reemplazo el binario antes en este mismo proceso, no hay
  # que descargar de nuevo -- solo apuntar inla.call ahi.
  if (file.exists(marker_file)) {
    INLA::inla.setOption(inla.call = file.path(inla_bin_dir, "inla.run"))
    return(TRUE)
  }

  # Ultima version confirmada manualmente visitando el listado del
  # sitio de INLA. Si el respaldo alguna vez deja de funcionar, revisar
  # a mano https://inla.r-inla-download.org/Linux-builds/Ubuntu-22.04.5%20LTS%20(Jammy%20Jellyfish)%20x86_64/
  # y actualizar este valor con la carpeta "Version_..." mas reciente.
  LATEST_KNOWN_VERSION <- "26.06.08"  # confirmado manualmente el 16-jul-2026

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
      utils::download.file(url, tmp_tgz, mode = "wb", quiet = TRUE)
      TRUE
    }, error = function(e) FALSE, warning = function(w) FALSE)
    if (ok) {
      descargado <- TRUE
      version_usada <- v
      break
    }
  }

  if (!descargado) {
    message("No se encontro binario de Ubuntu 22.04 para ninguna de estas ",
            "versiones: ", paste(versiones_a_probar, collapse = ", "),
            ". Revisar manualmente: ", base_url)
    return(FALSE)
  }

  # Descomprimimos en una carpeta temporal primero, NO directo en
  # inla_bin_dir. El .tgz de INLA puede traer los archivos envueltos en
  # uno o mas niveles de carpetas adentro (esto varia entre versiones,
  # ya lo confirmamos en despliegues del 16-jul-2026); si
  # descomprimieramos directo en inla_bin_dir (que ya se llama "64bit"),
  # podriamos terminar con carpetas anidadas y el script inla.run no
  # encontraria a su binario "inla" vecino.
  tmp_extract_dir <- tempfile("inla2204_")
  dir.create(tmp_extract_dir)
  utils::untar(tmp_tgz, exdir = tmp_extract_dir)

  # En vez de asumir cuantos niveles de carpetas hay, buscamos
  # directamente el archivo "inla" en TODO el arbol descomprimido, sin
  # importar que tan anidado este, y usamos su carpeta contenedora como
  # origen. Esto es robusto sin importar como este empaquetado el .tgz.
  archivos_extraidos <- list.files(tmp_extract_dir, recursive = TRUE,
                                    full.names = TRUE, all.files = TRUE)
  ruta_inla <- archivos_extraidos[basename(archivos_extraidos) == "inla"]

  if (length(ruta_inla) == 0) {
    message("El .tgz descargado no contiene un archivo 'inla' en ningun ",
            "nivel. Contenido real del .tgz: ",
            paste(basename(archivos_extraidos), collapse = ", "),
            ". Revisar manualmente: ", base_url)
    unlink(tmp_extract_dir, recursive = TRUE)
    return(FALSE)
  }

  origen <- dirname(ruta_inla[1])
  archivos_origen <- list.files(origen, full.names = TRUE, all.files = TRUE)
  file.copy(archivos_origen, inla_bin_dir, overwrite = TRUE, recursive = TRUE)
  unlink(tmp_extract_dir, recursive = TRUE)

  # Verificamos que el binario "inla" realmente haya quedado donde debe
  # antes de dar el reemplazo por exitoso -- si no, no marcamos exito,
  # para que el proximo intento (o el respaldo de Ubuntu 20.04) se use
  # en su lugar en vez de repetir este mismo error silenciosamente.
  if (!file.exists(file.path(inla_bin_dir, "inla"))) {
    message("El binario 'inla' no quedo donde se esperaba despues de ",
            "copiar. Revisar manualmente: ", base_url)
    return(FALSE)
  }

  # Asegura permisos de ejecucion en los binarios
  bin_files <- list.files(inla_bin_dir, full.names = TRUE)
  Sys.chmod(bin_files, mode = "0755")

  # Marca que ya se hizo, para no repetir la descarga en el mismo proceso
  file.create(marker_file)

  INLA::inla.setOption(inla.call = file.path(inla_bin_dir, "inla.run"))
  message(
    "INLA configurado para usar binario de Ubuntu 22.04.5 (version ",
    version_usada, "): ", file.path(inla_bin_dir, "inla.run")
  )

  TRUE
}

#' Intento 2 (respaldo): descarga y activa un binario de INLA mas viejo
#' (Ubuntu 20.04.6, version 23.05.30-1), como alternativa al binario MKL
#' que viene instalado y que provoca segmentation fault en Connect Cloud.
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

  # Buscamos el binario principal de INLA. IMPORTANTE: preferimos el
  # script "inla.run" sobre el binario "inla" en crudo. El script se
  # encarga de indicarle al programa donde estan sus piezas necesarias
  # (librerias compartidas en las subcarpetas "first" y "external");
  # sin el, el binario "inla" solo puede fallar con un error de tipo
  # "no encuentro tal libreria" (esto ya se confirmo: fallaba por no
  # encontrar libRmath.so.1).
  orden_preferencia <- c("inla.run", "inla", "inla.mkl.run", "inla.mkl")

  nombre_base <- basename(todos_los_archivos)
  candidatos_inla <- todos_los_archivos[nombre_base %in% orden_preferencia]

  if (length(candidatos_inla) == 0) {
    message("El paquete descargado no contiene ningun binario 'inla' reconocible.")
    return(FALSE)
  }

  # Ordenamos segun la preferencia de arriba (inla.run primero).
  candidatos_inla <- candidatos_inla[
    order(match(basename(candidatos_inla), orden_preferencia))
  ]

  inla_bin <- candidatos_inla[1]
  Sys.chmod(inla_bin, mode = "0755")
  INLA::inla.setOption(inla.call = inla_bin)
  message(
    "INLA configurado para usar binario alternativo ",
    "(Ubuntu 20.04.6, version 23.05.30-1): ", inla_bin
  )

  TRUE
}

#' Ultimo respaldo: usar el inla.mkl.run que ya viene instalado con el
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
