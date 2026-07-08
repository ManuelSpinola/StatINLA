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
        data(Seeds, package = "INLA")

        formula <- r ~ x1 + x2

        # Informacion de diagnostico sobre el binario de INLA en este entorno
        bin_path <- tryCatch(INLA::inla.getOption("inla.call"), error = function(e) NA)
        cat("Binario INLA que se va a usar:\n")
        print(bin_path)

        cat("\nExiste el archivo?:", file.exists(bin_path), "\n")
        cat("Permiso de ejecucion (0 = si tiene, -1 = no tiene):",
            file.access(bin_path, mode = 1), "\n")
        info <- tryCatch(file.info(bin_path), error = function(e) NULL)
        cat("Tamano del archivo (bytes):",
            if (!is.null(info)) info$size else NA, "\n")
        cat("\nPrimeras 2 lineas del archivo (para ver si es un script con shebang):\n")
        primeras <- tryCatch(
          readLines(bin_path, n = 2, warn = FALSE),
          error = function(e) paste("No se pudo leer como texto:", conditionMessage(e))
        )
        print(primeras)

        cat("\nIntentando ejecutar el binario directamente (diagnostico crudo del SO):\n")
        diag <- suppressWarnings(
          tryCatch(
            system2(bin_path, args = character(0), stdout = TRUE, stderr = TRUE),
            error = function(e) paste("Error de R al invocar system2:", conditionMessage(e))
          )
        )
        estado <- attr(diag, "status")
        cat("Codigo de salida:", if (is.null(estado)) "NA (proceso no devolvio status)" else estado, "\n")
        cat("Salida cruda:\n")
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
