#' time_picker UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#' @param am_pm_sel starting value for am/pm button. TRUE = AM and FALSE = PM
#' @param hr the starting value for hours
#' @param min the starting value for minutes
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_time_picker_ui <- function(id, am_pm_sel = TRUE, hr = "09", min = "00") {
  ns <- NS(id)
  tagList(
    div(
      class = "time_picker",
      numericInput(
        inputId = ns("hr"),
        label = NULL,
        value = hr,
        min = 1,
        max = 12,
        step = 1
      ) |> htmltools::tagAppendAttributes(class = "hours"),
      p(":"),
      numericInput(
        inputId = ns("min"),
        label = NULL,
        value = min,
        min = 0,
        max = 55,
        step = 5
      ) |> htmltools::tagAppendAttributes(class = "minutes"),
      # shinyWidgets::radioGroupButtons(
      #   inputId = ns("am_pm"),
      #   choices = c("AM", "PM"),
      #   size = "sm",
      #   selected = am_pm_sel
      # )
      shinyWidgets::prettyToggle(
        inputId = ns("am_pm"),
        label_on = "AM",
        label_off = "PM",
        value = am_pm_sel
      )
    )
  )
}
    
#' time_picker Server Functions
#'
#' @noRd 
mod_time_picker_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    time <- reactive({
      hour <- input$hr
      min <- input$min / 60
      am_pm <- input$am_pm
      if (!is.na(hour) && !is.na(min)) {
        if (!am_pm && hour < 12) {
          hour <- hour + 12
        }
        time <- hour + min
      }
      return(time)
    })
    return(time)
  })
}
    
## To be copied in the UI
# mod_time_picker_ui("time_picker_1")
    
## To be copied in the server
# mod_time_picker_server("time_picker_1")
