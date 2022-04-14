#' course_select UI Function
#'
#' @description A shiny Module. Allow user to select and visualize course shedules 
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
#' @importFrom shinyWidgets pickerInput updatePickerInput
mod_course_select_ui <- function(id) {
  ns <- NS(id)
  tagList(
    pickerInput(
      inputId = ns("course"),
      label = NULL,
      choices = c(),
      multiple = TRUE,
      options = list(
        `live-search` = TRUE,
        `actions-box` = TRUE,
        title = "Select Courses",
        `selected-text-format` = "count > 3"
      )
    ), # end course picker
    h4("Highlight"),
    shinyWidgets::pickerInput(
      inputId = ns("fill_color"),
      choices = c(
        "course",
        "instructor",
        "campus",
        "room"
      ),
      selected = "course"
    ),
    h4("Filters"),
    hr(),
    p("week day"),
    pickerInput(
      inputId = ns("days"),
      label = NULL,
      choices = list(
        "Monday" = "M",
        "Tuesday" = "T",
        "Wednesday" = "W",
        "Thursday" = "R",
        "Friday" = "F",
        "Saturday" = "S"
      ),
      selected = c(
        "M", "T", "W", "R", "F", "S"
      ),
      multiple = TRUE,
      options = list(
        title = "Filter: Day of Week"#,
       #`selected-text-format` = "count > 8"
      ) # end day picker
    ),
  p("start time"),
    mod_time_picker_ui(ns("time_picker_1"), TRUE, "06", "00"),
  p("end time"),
    mod_time_picker_ui(ns("time_picker_2"), FALSE, "09", "00"),
   p("instructor"),
   pickerInput(
     inputId = ns("instructor"),
     choices = c(),
     multiple = TRUE,
     options = list(
       `live-search` = TRUE,
       `actions-box` = TRUE,
       title = "Select Instructor",
       `selected-text-format` = "count > 3"
     )
   ),
   p("campus"),
   pickerInput(
     inputId = ns("campus"),
     choices = c(),
     multiple = TRUE,
   ),
   p("room"),
   pickerInput(
     inputId = ns("room"),
     choices = c(), 
     multiple = TRUE,
     options = list(
       `live-search` = TRUE,
       `actions-box` = TRUE,
       title = "Select Room",
       `selected-text-format` = "count > 3"
     )
   )
  )
}
    
#' course_select Server Functions
#' @param rv reactive valueslist that holds rv$schedule
#' @noRd 
mod_course_select_server <- function(id, rv) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns


  observe({
    req(rv$schedule)

    rv$schedule |>
      dplyr::pull(room) |>
      unique() -> room

    updatePickerInput(
      session = session,
      inputId = "room",
      selected = room,
      choices = sort(room)
    )
  })

  observe({
    req(rv$schedule)

    rv$schedule |>
      dplyr::pull(campus) |>
      unique() -> campus

    updatePickerInput(
      session = session,
      inputId = "campus",
      selected = campus,
      choices = sort(campus)
    )
  })

  observe({
    req(rv$schedule)

    rv$schedule |>
      dplyr::pull(instructor) |>
      unique() -> instructor

    updatePickerInput(
      session = session,
      inputId = "instructor",
      selected = instructor,
      choices = sort(instructor)
    )
  })

    stime <- mod_time_picker_server("time_picker_1")
    etime <- mod_time_picker_server("time_picker_2")

  observe({
    req(rv$schedule)

      rv$schedule |>
        dplyr::group_by(course) |>
        dplyr::filter(
          any(day %in% input$days) &
            # any(campus %in% input$campus) &
            stime >= stime() &
            etime <= etime() &
            any(instructor %in% input$instructor) &
            any(campus %in% input$campus) &
            any(room %in% input$room)
        ) -> filtered

      filtered |>
        dplyr::pull(course) |>
        unique() -> course_list

      updatePickerInput(
        session = session,
        inputId = "course",
        selected = NULL,
        choices = sort(course_list)
      )
    })
    observe({
      rv$course_list <- input$course
    })
    observe({
      rv$fill_color <- input$fill_color
    })
  })
}
    
## To be copied in the UI
# mod_course_select_ui("course_select_1")
    
## To be copied in the server
# mod_course_select_server("course_select_1")
