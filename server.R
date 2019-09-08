
### remove course_id/row_id dependency (use row_id) or move both/one to prepare plot data function? 
### add ability to add course title
### add ability to modify course title

server<-function(input,output,session){
  
  
  if (interactive()) {
    cat('Running in interactive mode, so will stop session at end.')
    session$onSessionEnded(stopApp)
  }
  
  
  rv <- reactiveValues()
  
  observeEvent(input$file, {
    
    rv$schedule <<- NULL
    
   ### check for any errors with uploaded file 
    df <- try_catch({
      read_excel(input$file$datapath) %>%
        prepare_data(.)
    },
    .e = ~ {
      return(NULL)
    })
    
    
    if (!is.null(df)) {
      showModal(modalDialog(tags$p("File uploaded successfully")))
      rv$schedule <- df
    }
    
    else {
      showModal(modalDialog(tags$div(
        tags$p(
          "Unable to upload file. Download the",
          tags$a(href = "example_course_file.xlsx",
                 "example file (click to download)"),
          tags$p("and use as a template.")
        )
      )))
    }
    
  })
  
  output$data <- renderUI({
    validate(need(!is.null(rv$schedule), message = "Upload a file"))
 ### report some stats about file contents to user after successful file uplaod.    
    # rv$schedule %>%
    #   select(course, campus) %>%
    #   mutate(course = str_extract(course, "^.{6}")) %>%
    #   group_by(course, campus) %>%
    #   count() %>%
    #   spread(campus, n) -> summary
    
    # rv$schedule->summary
    # 
    # output$aa <- DT::renderDataTable(summary)
    # DT::dataTableOutput("aa")
    
    output$aa<-renderPrint(reactiveValuesToList(input))
    verbatimTextOutput("aa")
    
  })
  
  
  observe({
    req(rv$schedule)
    ### create list of courses from data, allow user to filter list
    start_time <-
      hour(input$start_range[1]) + minute(input$start_range[1]) / 60
    end_time <-
      hour(input$start_range[2]) + minute(input$start_range[2]) / 60
    
    
    rv$schedule %>%
      group_by(course) %>%
      filter(
        any(day %in% input$days) &
          any(campus %in% input$campus) &
          stime >= start_time &
          etime <= end_time
      ) %>%
      select(course) %>%
      unlist() %>%
      unique() %>%
      as.character() -> rv$class_list
  })
  
  
  observe({
    ### update course selections from course list
    course <- isolate(c(input$course,input$new_course))

    updatePickerInput(session = session,
                     inputId = "course",
                     selected=course,
                     choices = sort(rv$class_list))
  })
  
 
observe({
    req(rv$schedule)
    rv$schedule %>%
      filter(course %in% input$course)%>%
      prepare_plot_data(.)->rv$plot_data
  })
  
  output$plot <- renderPlot({
    validate(need(nrow(rv$schedule) > 0, message = "Upload a file or add courses manually"))
    
    validate(need(length(input$course) > 0, message = "Select courses from the drop down menu to display course schedule"))
    
    rv$plot_data %>%
      plot_schedule(.,fill=course)
  })
  

  observeEvent(input$plot_click, {
   
  
    #  A tibble: 1 x 14
    # day    indx course   campus        n    x2    x1 rowid title              room  stime etime duration course_id 
    # <fct> <dbl> <chr>    <chr>     <int> <dbl> <dbl> <int> <chr>              <chr> <dbl> <dbl>    <dbl> <chr>     
    #   1 M    0 ACC101-8 Haverhill     1     1     0     1 Intro Accounting I C107      8  9.25     1.25 ACC101-8_1     
  
    click<-input$plot_click

      rv$plot_data %>%
        filter(click$x >= x1 &
               click$x <= x2 &
               click$y >= stime &
               click$y <= etime &
               day == click$panelvar1 &
               campus == click$panelvar2
                 )->row

    rv$course_id<<-row$course_id
    
  
    
  if (length(row$course_id>1)){   
    
    hour<-floor(row$stime)
    min<-(row$stime-hour)*60
    
    if (row$stime>=13) hour<-hour-12
    
    if (row$stime<12) am_pm<-"AM"
    else  am_pm<-"PM"    
    
    dur<-abs(row$etime-row$stime)*60
    
 
   showModal( 
    #### Modify course attribute dynamic UI ### 
    modalDialog(size = "s",
                div(
                   textInput(inputId = "course_id",
                              label="Selected Course",
                              value=row$course),
                   textInput(inputId = "course_title",
                              label="Title",
                              value=row$title),
                 # h3(glue::glue("Modify and update ",{row$course})),
                    selectInput(inputId = "course_day",
                                label="Day",
                                choices = c("M","T","W","R","F","S"),
                                selected = row$day),
                    numericInput(inputId = "course_s_time_hr",
                                 label="Start (hr)",
                                 min = 1,
                                 max=12,
                                 step = 1,
                                 value=hour),
                    numericInput(inputId = "course_s_time_min",
                                 label="Start (min)",
                                 min = 0,
                                 max=59,
                                 step=1,
                                 value=min),
                    selectInput(inputId = "AM_PM",
                                label="AM/PM",
                                choices = c("AM","PM"),
                                selected = am_pm),
                    numericInput(inputId = "course_dur",
                                 label="Duration (min)",
                                 min = 1,
                                 max=600,
                                 step=1,
                                 value=dur), 
                   selectInput(inputId = "select_campus",
                               label="Campus",
                               choices = c("Haverhill","Lawrence", "Unknown"),
                               selected = row$campus)),
                
      footer = tagList(
        modalButton(label = "Cancel"),
        actionButton(inputId = 'apply', label = HTML('Apply <br/> Changes')),
        actionButton(inputId = "delete", label = HTML("Delete <br/> Section"))
      
      )
      )
   )}
   
  })
  
  observeEvent(input$apply,{
  
    #  A tibble: 1 x 14
    # day    indx course   campus        n    x2    x1 rowid title              room  stime etime duration course_id 
    # <fct> <dbl> <chr>    <chr>     <int> <dbl> <dbl> <int> <chr>              <chr> <dbl> <dbl>    <dbl> <chr>     
    #   1 M    0 ACC101-8 Haverhill     1     1     0     1 Intro Accounting I C107      8  9.25     1.25 ACC101-8_1     
    
    
    if(rv$course_id>0) {
      #x[x$day=="M","time"]
      # x[!is.na(x$a) & x$a==2,]$b <- 99
      # rv$schedule[rv$schedule$course_id == rv$course_id, "course"] <<-
      #   input$course_id
      # rv$schedule[rv$schedule$course_id == rv$course_id, "day"] <<-
      #   input$course_day
      # rv$schedule[rv$schedule$course_id == rv$course_id, "campus"] <<-
      #   input$select_campus
      # rv$schedule[rv$schedule$course_id == rv$course_id, "title"] <<-
      #   input$course_title
      
      hour <- isolate(input$course_s_time_hr)
      min <- isolate(input$course_s_time_min / 60)
      dur <- isolate(input$course_dur)
      AM_PM <- isolate(input$AM_PM)
      
      if (!is.na(hour) &
          !is.na(min) & 
          !is.na(dur)) {
        if (AM_PM == "PM" & hour < 12)
          hour <- hour + 12
        
        stime <- hour + min
        etime <- stime + (dur / 60)
        
        # rv$schedule[rv$schedule$course_id == rv$course_id, "stime"] <<-
        #   stime
        # rv$schedule[rv$schedule$course_id == rv$course_id, "etime"] <<-
        #   etime
        
        tibble(course_id=rv$course_id,
               course=input$course_id,
               day=input$course_day,
               campus=input$select_campus,
               title=input$course_title,
               stime=stime,
               etime=etime
               )->d
        
        rv$schedule %>% 
          filter(course_id!=rv$course_id) %>%
          bind_rows(.,d)->rv$schedule
        
        
        
      }
      removeModal()
      
    }
  })
  
  observeEvent(input$delete,{
      
    if(rv$course_id>0){
      rv$schedule %>%
        filter(course_id!=rv$course_id)->>rv$schedule
    }
    removeModal()
    
  })
  
  
  output$text<-renderPrint({
      
      click<-input$plot_hover
      
      validate(need(!is.null(click),"Mouse-over course to dislpay details. Click to edit."))
      
      rv$plot_data %>%
        filter(click$x >= x1 &
               click$x <= x2 &
               click$y >= stime &
               click$y <= etime &
               day == click$panelvar1 &
               campus == click$panelvar2
        )->row
      
      validate(need(row$course>0,"Mouse-over course to dislpay details. Click to edit."))
      
      hour<-floor(row$stime)
      min<-floor((row$stime-hour)*60)
      
      if (row$stime>=13) hour<-hour-12
      
      if (row$stime<12) am_pm<-"AM"
      else  am_pm<-"PM"    
      
      dur<-abs(row$etime-row$stime)*60
      
      glue::glue("Course: ",{row$course}," (",{row$title},")","\n",
                 "Start Time: ",{hour},":",{min}," ",{am_pm},"\n",
                 "Duration: ",{dur}," (mins)","\n",
                 "Campus: ",{row$campus})
         
      })
  
  observeEvent(input$add_course,{

    print("error add")
    showModal(
      modalDialog(size = "l",
        div(
          h4("Add new course"),
          numericInput(inputId = "meeting_num",
                       label = "How many in-class meetings?",
                       min=1,
                       max=10,
                       step=1,
                       value=1
                        ),
          textInput(inputId = "new_course",
                    label = "Unique course number and section (e.g., BIO111-E5)",
                    value = ""),
          textInput(inputId = "new_title",
                    label = "Course Title",
                    value=""),
          selectInput(inputId = "select_campus_new",
                      label="Campus",
                      choices = c("Haverhill","Lawrence", "Unknown"),
                      selected =""),
          uiOutput("new_course_input"),
          uiOutput("warning")
          
          ),
        footer = tagList(
        modalButton(label = "Cancel"),
        actionButton(inputId = 'add_course_to_data', HTML('Add course and close'))
     )
    )
    )
  })
  
  output$new_course_input<-renderUI({
    print("error render")
    map(1:input$meeting_num,~div(
                            fluidRow(
                              column(width=2,
                                selectInput(inputId = paste0("course_day",.),
                                             label=paste0("Day ",.),
                                             choices = c("M","T","W","R","F","S"),
                                             selected=NA)), ## end column 1
                              column(width=2,
                                 numericInput(inputId = paste0("course_s_time_hr",.),
                                              label="Start (hr)",
                                              min = 1,
                                              max=12,
                                              step = 1,
                                              value=9)), ## end column 2
                              column(width=2,
                                 numericInput(inputId = paste0("course_s_time_min",.),
                                              label="Start (min)",
                                              min = 0,
                                              max=59,
                                              step=1,
                                              value=15)), ## end column 3
                              column(width=2,
                                 selectInput(inputId = paste0("AM_PM",.),
                                             label="AM/PM",
                                             choices = c("AM","PM"),
                                             selected ="AM")), ## end column 4 
                              column(width=2,
                                 numericInput(inputId = paste0("course_dur",.),
                                              label="Duration (min)",
                                              min = 1,
                                              max=600,
                                              step=1,
                                              value=75)) ## end column 5
                                        ) ## end fluidRow
                                 
                                 ) ## end div
        )## end map
    
  })
  
  observeEvent(input$add_course_to_data,{
    
    print("error apply")
    # # A tibble: 1 x 9
    # rowid course     title           day   room  stime etime campus    course_id   
    # <int> <chr>      <chr>           <chr> <chr> <dbl> <dbl> <chr>     <chr>       
    #   1     1 BIO111-B1A Intro Biology I R     E356     14  15.7 Haverhill BIO111-B1A_1
    
    days<-map(1:input$meeting_num,~input[[paste0("course_day",.)]]) %>% unlist()
    hour<-map_dbl(1:input$meeting_num,~input[[paste0("course_s_time_hr",.)]])
    min<-map_dbl(1:input$meeting_num,~input[[paste0("course_s_time_min",.)]])
    am_pm<-map(1:input$meeting_num,~input[[paste0("AM_PM",.)]]) %>% unlist()
    dur<-map_dbl(1:input$meeting_num,~input[[paste0("course_dur",.)]])
    
    
    
    if(any(is.na(hour)) | any(is.na(min)) | any(is.na(dur)) | 
       any(hour>12) | any(hour<1) | any(min>59) | any(min<0) |
       any(dur<1) | any(dur>720) | input$new_course==""
    ){
    
    output$warning<-renderUI({
      div(h5(style="color:red","Please provide course number and section, reasonable values for start times (hour = 1-12; min = 1-59) and/or duration (1-720)"))
    })  
      
    }
    
    else {
    tibble(day=days,
           hour=hour,
           min=min,
           am_pm=am_pm,
           dur=dur
           ) %>%
      mutate(course=input$new_course,
             campus=input$select_campus_new,
             #campus=case_when(campus=="Haverhill" ~ "E",
            #                  campus=="Lawrence" ~ "L",
             #               TRUE ~ ""),
             hour=case_when(am_pm == "PM" & hour < 12 ~ hour+12,
             TRUE~hour),
             stime=hour+(min/60),
             etime=stime+(dur/60),
             title=input$new_title) %>%
             select(-hour,-min,-am_pm,-dur)->df
    
    
    if (!is.null(rv$schedule)){
      rv$schedule %>% 
        select(-rowid,-course_id) %>%
        bind_rows(.,df) %>% 
        rowid_to_column() %>%
        group_by(course) %>%
        mutate(course_id=str_c(course,1:n()))%>%
        ungroup()->>rv$schedule
    }
    
    else{df %>%
           rowid_to_column()%>%
           group_by(course) %>%
           mutate(course_id=str_c(course,1:n()))%>%
           ungroup()->>rv$schedule
      
      }
  
    removeModal()
    }
    
  })
  
  output$download_plot<-downloadHandler(
    
    filename = function() {
      paste('course_schedule-', Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      rv$plot_data %>%
        plot_schedule(.,fill=course)->plot
      ggsave(file,plot=plot,width = 7,height = 5)
      
    }
    
  )
  
  # Bookmarking code --------------------------
  onBookmark(function(state) {
    state$values$schedule <- rv$schedule
  })

  observe({setBookmarkExclude(names(input))}, priority = 100, autoDestroy = TRUE)
  
  onRestore(function(state) {
    rv$schedule <- state$values$schedule
    course <- state$input$course
    updatePickerInput(session = session, inputId = "course", selected = course)
  })

  # setBookmarkExclude(c("file",
  #                      "add_course",
  #                      "download_plot",
  #                      "download_data",
  #                      "add_course_to_data",
  #                      "apply",
  #                      "delete",
  #                      "new_course",
  #                      "new_title",
  #                      "meeting_num",
  #                      "count"))

  
  
  output$download_data<-downloadHandler(
    
    filename = function() {
      paste('course_schedule-', Sys.Date(), '.xlsx', sep='')
    },
    content = function(file) {
      rv$schedule %>%
        export_file(.) %>%
        write_xlsx(x = .,file)
    }

  )
  
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  session$allowReconnect(TRUE)
}