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

        cat("Existe /bin/bash?:", file.exists("/bin/bash"), "\n")
        cat("Sys.which('bash'):", Sys.which("bash"), "\n\n")

        cat("Intentando ejecutar explicitamente via bash (sin depender del shebang):\n")
        diag_bash <- suppressWarnings(tryCatch(
          system2("/bin/bash", args = shQuote(bin_path), stdout = TRUE, stderr = TRUE),
          error = function(e) paste("Error de R al invocar bash:", conditionMessage(e))
        ))
        cat("Codigo de salida:", if (is.null(attr(diag_bash, "status"))) "NA" else attr(diag_bash, "status"), "\n")
        cat(paste(diag_bash, collapse = "\n"))
        cat("\n\n---\n\n")

        cat("Intentando ejecutar directamente (sin bash, dejando que el SO use el shebang):\n")
        diag <- suppressWarnings(
          tryCatch(
            system2(bin_path, args = character(0), stdout = TRUE, stderr = TRUE),
            error = function(e) paste("Error de R al invocar system2:", conditionMessage(e))
          )
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
