ui <- function(request){
  
  dashboardPagePlus(
    
  title = "Schedule Application",
  skin = "blue",
  collapse_sidebar = TRUE, 
  dashboardHeaderPlus(),
  dashboardSidebar(collapsed = TRUE,
                   width=100,
                   sidebarMenu(
                     menuItem("Upload", tabName = "upload", icon = icon("file-upload")),
                     menuItem(
                       "Visualize",
                       tabName = "visualize",
                       icon = icon("chart-line")
                     ),
                     menuItem("Help", tabName = "help", icon = icon("question"))
                   )),
  dashboardBody(
    tags$head( ###https://support.dominodatalab.com/hc/en-us/articles/360015932932-Increasing-the-timeout-for-Shiny-Server
      HTML(
        "
          <script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
          "
      )
    ),
    
    tabItems(
    tabItem(
      tabName = "upload",
      fluidRow(
        boxPlus(width=6,
                status = "info",
                title=div("Whats New"), 
                collapsible = TRUE,
                collapsed = FALSE,
                closable = FALSE,
                p("1. The input file format has been updated. Please download", 
                  a(href="example_course_file.xlsx","THE EXAMPLE FILE"),"and format input files to match."),
                p("2. You can now download schedule files to your desktop.")
              
        ),  
      boxPlus(width=6,
              status = "info",
              title=div("Getting Started"), 
              collapsible = TRUE,
              collapsed = FALSE,
              closable = FALSE,
              
              p("1. Upload a properly formatted course file", 
                a(href="example_course_file.xlsx","(EXAMPLE HERE)"), 
                "and then switch to the visualize module"),
              p("2. Switch to the visualize module and add courses manually."),
              p("Note: You may add courses manually to those uploaded from a file, 
                but you can not (yet) add courses from a file to those added manually. 
                Uploading a new file will start a new session.")
              
              )
      ),
      fluidRow(
      boxPlus(
        width = 6,
        status = "info",
        fileInput(
          inputId = "file",
          label = div("Upload Excel file"),
          multiple = FALSE,
          accept = c("application/excel",
                     ".xlsx")
          
        )
      )),
      fluidRow(
      boxPlus(
        width = 6,
        status = "info",
        title = "Summary", 
        closable = FALSE,
        collapsible = TRUE,
        collapsed = TRUE,
        h5("Number of course sections"),
        uiOutput("data"),
        textOutput("keepAlive")
      )
    )),
    tabItem(
      tabName = "visualize",
      column(
        width = 2,
        boxPlus(
          width = NULL,
          closable = FALSE,
          status = "info",
          collapsible = FALSE,
          #title = "Select Courses",
          #solidHeader = TRUE,
          pickerInput(inputId = "course",
                      label = NULL,
                      choices = NA,
                      multiple = TRUE,
                      options = list(
                        `live-search` = TRUE,
                        `actions-box` = TRUE,
                        title = "Select Courses",
                        `selected-text-format` = "count > 3"))),
        boxPlus(
          width = NULL,
          closable = FALSE,
          status = "info",
          actionButton(inputId = "add_course",
                       label = paste0("Add course"))
        ),
        boxPlus(
          width = NULL,
          collapsible = TRUE,
          collapsed = TRUE,
          closable = FALSE,
          title = "Course Filters",
          solidHeader = TRUE,
          status = "info",
          checkboxGroupInput(
            inputId = "days",
            label =
              "Week Day",
            choices =
              c("M", "T", "W", "R", "F", "S"),
            selected =
              c("M", "T", "W", "R", "F","S")
          ),
          hr(),
          checkboxGroupInput(
            inputId = "campus",
            label =
              "Campus",
            choices =
              c("Haverhill", "Lawrence", "Unknown"),
            selected =
              c("Haverhill", "Lawrence", "Unknown")
          ),
          hr(),
          sliderInput(
            inputId = "start_range",
            label = "Time of Day",
            min = ymd_hms("0000-01-01 00:05:00"),
            max = ymd_hms("0000-01-01 23:55:00"),
            value = c(
              ymd_hms("0000-01-01 00:05:00"),
              ymd_hms("0000-01-01 23:50:00")
            ),
            timezone = "+0000",
            timeFormat = "%R",
            step = 300,
            ticks = F
          )
        ),## end box
        boxPlus(
          title="Save",
          collapsible = TRUE,
          collapsed = TRUE,
          closable = FALSE,
          status = "info",
          width = NULL,
          solidHeader = TRUE,
          #bookmarkButton(label="Save schedule"),
          downloadButton(outputId="download_data",
                         label="Download File"),
          hr(),
          downloadButton(outputId="download_plot",
                      label="Download Image")
          
          #downloadButton(outputId="download_data",
           #            label="Data") ## after fix time rouding errors 
        )## end box
      ),
      ## end column
      column(
        width = 10,
        boxPlus(
          closable = FALSE,
          solidHeader = TRUE,
          collapsible = TRUE,
          title = "Course Schedule Display",
          background = NULL,
          width = NULL,
          status = "info",
          # enable_dropdown = TRUE,
          # dropdown_icon = "question-circle",
          # dropdown_menu = dropdownItemList(
          #   dropdownItem(url = "https://www.google.com", name = "Link to google"),
          #   dropdownItem(url = "#", name = "item 2"),
          #   dropdownDivider(),
          #   dropdownItem(url = "#", name = "item 3")
          # ),
          
          plotOutput("plot",
                     height=700,
                     width="100%",
                     click = "plot_click",
                     hover = "plot_hover"),
          verbatimTextOutput("text")
        )## end box
      )# end column
      
  ))
  )
  )  
}