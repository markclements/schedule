#' upload_schedule UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom attempt try_catch
#'
mod_upload_schedule_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      inputId = ns("file"),
      label = div("Upload Excel file"),
      multiple = FALSE,
      accept = c(
        "application/excel",
        ".xlsx"
      )
    )
  )
}
    
#' upload_schedule Server Functions
#' @param rv is the reactive values list that stores the rv$schedule tibble
#' @noRd 
mod_upload_schedule_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    observeEvent(input$file, {
      rv$schedule <- NULL

      ### check for any errors with uploaded file
      df <- try_catch({
          readxl::read_excel(input$file$datapath) |>
            prepare_data()
        },
        .e = ~ {
          return(NULL)
        }
      )
      if (!is.null(df)) {
        # showModal(
        #   modalDialog(
        #     tags$p("File uploaded successfully"),
        #     easyClose = TRUE
        #   )
        # )
        rv$schedule <- df ## TRUE, assign tibble to reactive value
      } else {
        showModal(
          modalDialog(
            tags$div(
              tags$p(
                "Unable to upload file. Download the",
              tags$a(
                  href = "example_course_file.xlsx",
                  "example file (click to download)"
                ),
              tags$p("and use as a template.")
              )
            )
          )
        )
      }
    })
  })
}
    
## To be copied in the UI
# mod_upload_schedule_ui("upload_schedule_1")
    
## To be copied in the server
# mod_upload_schedule_server("upload_schedule_1")
