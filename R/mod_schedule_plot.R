#' schedule_plot UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList
#' @import rlang
mod_schedule_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(
    plotly::plotlyOutput(
      ns("schedule")
    )
  )
}
    
#' schedule_plot Server Functions
#'
#' @noRd 
mod_schedule_plot_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    df <- eventReactive(rv$course_list, {
      df <- NULL
      if (!is.null(rv$course_list)) {
        rv$schedule |>
          dplyr::filter(
            course %in% rv$course_list
            ) |>
          dplyr::filter(
            !stringr::str_detect(day, "ON")
          ) -> filtered
        if (nrow(filtered) >= 1) {
          filtered |>
            prepare_plot_data() -> df
        } else {
          df <- NULL
        }
      }
      return(df)
    })

    output$schedule <- plotly::renderPlotly({
        validate(
          need(
            !is.null(rv$schedule),
            message = "Upload a schedule Excel file or add courses manually"
          )
        )
        validate(
          need(
            !is.null(rv$course_list),
            message = "Select courses from the drop down menu to
            display course schedule"
          )
        )
        plot_schedule(df(), fill = rv$fill_color) -> p
          plotly::ggplotly(
            p,
            height = 700
            ) |>
          plotly::style(
            textposition = "right"
          )
      })
    })
}
    
## To be copied in the UI
# mod_schedule_plot_ui("schedule_plot_1")
    
## To be copied in the server
# mod_schedule_plot_server("schedule_plot_1")
