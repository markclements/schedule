#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # Your application server logic

  rv <- reactiveValues()

  mod_upload_schedule_server("upload_schedule_1", rv)

  mod_course_select_server("course_select_1", rv)

  mod_schedule_plot_server("schedule_plot_1", rv)

  observe({
    print(rv$schedule)
  })

   if (interactive()) {
     cat("Running in interactive mode, will stop session at end.", "\n")
     session$onSessionEnded(stopApp)
   }
   
}
