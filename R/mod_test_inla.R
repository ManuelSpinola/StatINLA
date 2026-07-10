#' test_inla UI Function
#'
#' @description Pantalla minima de prueba para confirmar que INLA
#' se instala y corre correctamente en el entorno de despliegue
#' (Posit Connect Cloud). No es un modulo didactico final, es solo
#' una prueba tecnica.
#'
#' @param id Internal parameter for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_test_inla_ui <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    shiny::h2("Prueba tecnica: INLA en Connect Cloud"),
    shiny::p("Esta pantalla corre un modelo inla() de juguete (ejemplo Seeds, ",
             "incluido en el paquete) para confirmar que INLA funciona en este ",
             "entorno de despliegue."),
    shiny::actionButton(ns("run_test"), "Correr modelo de prueba"),
    shiny::br(), shiny::br(),
    shiny::verbatimTextOutput(ns("result"))
  )
}

#' test_inla Server Functions
#'
#' @noRd
mod_test_inla_server <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$result <- shiny::renderPrint({
      shiny::req(input$run_test)

      shiny::withProgress(message = "Corriendo modelo INLA...", {

        # --- Prueba minima, antes que nada: puede R correr CUALQUIER
        # subproceso en este entorno? Esto no depende de INLA ni del
        # binario que zzz.R descarga -- si esto falla, el problema es
        # de la plataforma (Connect Cloud), no de INLA.
        cat("Prueba minima: puede R correr CUALQUIER subproceso?\n")
        prueba_echo <- tryCatch(
          system2("echo", "hola_mundo", stdout = TRUE, stderr = TRUE),
          error = function(e) paste("FALLO incluso con echo:", conditionMessage(e))
        )
        cat(paste(prueba_echo, collapse = "\n"))
        cat("\n\n---\n\n")

        data(Seeds, package = "INLA")

        formula <- r ~ x1 + x2

        bin_path <- tryCatch(INLA::inla.getOption("inla.call"), error = function(e) NA)
        cat("Binario INLA que se va a usar:\n")
        print(bin_path)

        cat("\nContenido completo del script (es pequeno, lo mostramos entero):\n")
        contenido <- tryCatch(
          readLines(bin_path, warn = FALSE),
          error = function(e) paste("No se pudo leer:", conditionMessage(e))
        )
        cat(paste(contenido, collapse = "\n"))
        cat("\n\n---\n\n")

        # --- Diagnostico de permisos, dueno y tipo de archivo ---
        cat("Existe el archivo?:", file.exists(bin_path), "\n")
        cat("R considera que tiene permiso de ejecucion?:",
            file.access(bin_path, mode = 1) == 0, "\n\n")

        cat("Detalle de permisos y dueno (ls -la):\n")
        ls_info <- tryCatch(
          system2("ls", c("-la", shQuote(bin_path)), stdout = TRUE, stderr = TRUE),
          error = function(e) paste("No se pudo correr ls:", conditionMessage(e))
        )
        cat(paste(ls_info, collapse = "\n"))
        cat("\n\n")

        cat("Tipo de archivo (file), para confirmar que es un binario Linux x86_64 valido:\n")
        file_info <- tryCatch(
          system2("file", shQuote(bin_path), stdout = TRUE, stderr = TRUE),
          error = function(e) paste("No se pudo correr file:", conditionMessage(e))
        )
        cat(paste(file_info, collapse = "\n"))
        cat("\n\n")

        # --- Es la CARPETA la que bloquea la ejecucion, no el archivo?
        # Creamos un script trivial ("echo hola") y probamos correrlo
        # desde dos carpetas distintas: una temporal (deberia poder
        # ejecutar) y la misma carpeta donde vive el binario de INLA.
        # Si el de tempdir() funciona pero el de INLA no, confirma que
        # esa carpeta especifica esta montada sin permiso de ejecucion
        # (comun en contenedores reforzados), sin importar el archivo.
        cat("Prueba de ejecucion por carpeta (es la carpeta la que bloquea?):\n\n")

        probar_ejecucion_en <- function(carpeta, etiqueta) {
          script_path <- file.path(carpeta, paste0("prueba_exec_", basename(tempfile()), ".sh"))
          resultado <- tryCatch({
            writeLines(c("#!/bin/bash", "echo hola_desde_script"), script_path)
            Sys.chmod(script_path, mode = "0755")
            system2(script_path, stdout = TRUE, stderr = TRUE)
          }, error = function(e) paste("FALLO:", conditionMessage(e)))
          cat(sprintf("Carpeta (%s): %s\n", etiqueta, carpeta))
          cat("Resultado:", paste(resultado, collapse = " | "), "\n\n")
          unlink(script_path)
        }

        probar_ejecucion_en(tempdir(), "temporal, tempdir()")
        probar_ejecucion_en(dirname(bin_path), "misma carpeta que el binario de INLA")

        cat("---\n\n")

        cat("Usuario con el que corre la app (whoami / id):\n")
        cat(paste(tryCatch(system2("whoami", stdout = TRUE, stderr = TRUE),
                            error = function(e) "No se pudo correr whoami"),
                  collapse = " "), "\n")
        cat(paste(tryCatch(system2("id", stdout = TRUE, stderr = TRUE),
                            error = function(e) "No se pudo correr id"),
                  collapse = " "), "\n\n---\n\n")

        cat("Existe /bin/bash?:", file.exists("/bin/bash"), "\n")
        cat("Sys.which('bash'):", Sys.which("bash"), "\n\n")

        # --- Intentos de ejecucion, ahora capturando advertencias reales ---
        # (antes se suprimian con suppressWarnings(), lo que escondia el
        # mensaje real del sistema operativo, p.ej. "Permission denied" o
        # "Exec format error")

        cat("Intentando ejecutar explicitamente via bash (sin depender del shebang):\n")
        diag_bash <- withCallingHandlers(
          tryCatch(
            system2("/bin/bash", args = shQuote(bin_path), stdout = TRUE, stderr = TRUE),
            error = function(e) paste("Error de R al invocar bash:", conditionMessage(e))
          ),
          warning = function(w) {
            cat("Advertencia real (bash):", conditionMessage(w), "\n")
            invokeRestart("muffleWarning")
          }
        )
        cat("Codigo de salida:", if (is.null(attr(diag_bash, "status"))) "NA" else attr(diag_bash, "status"), "\n")
        cat(paste(diag_bash, collapse = "\n"))
        cat("\n\n---\n\n")

        cat("Intentando ejecutar directamente (sin bash, dejando que el SO use el shebang):\n")
        diag <- withCallingHandlers(
          tryCatch(
            system2(bin_path, args = character(0), stdout = TRUE, stderr = TRUE),
            error = function(e) paste("Error de R al invocar system2:", conditionMessage(e))
          ),
          warning = function(w) {
            cat("Advertencia real (directo):", conditionMessage(w), "\n")
            invokeRestart("muffleWarning")
          }
        )
        estado <- attr(diag, "status")
        cat("Codigo de salida:", if (is.null(estado)) "NA (proceso no devolvio status)" else estado, "\n")
        cat(paste(diag, collapse = "\n"))
        cat("\n---\n\n")

        salida <- tryCatch(
          capture.output(
            result <- INLA::inla(
              formula,
              data = Seeds,
              family = "binomial",
              Ntrials = Seeds$n,
              verbose = TRUE,
              safe = FALSE
            )
          ),
          error = function(e) e
        )

        if (inherits(salida, "error")) {
          cat("ERROR al correr INLA:\n")
          cat(conditionMessage(salida))
        } else {
          cat("Salida detallada (verbose) de INLA:\n\n")
          cat(paste(salida, collapse = "\n"))
          cat("\n\n---\n\n")
          if (exists("result") && inherits(result, "inla")) {
            cat("INLA corrio correctamente en este entorno.\n\n")
            summary(result)
          } else {
            cat("INLA no devolvio un resultado valido.\n")
          }
        }
      })
    }) |> shiny::bindEvent(input$run_test)
  })
}
