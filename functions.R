# prepare data

prepare_data<-function(df){
  df %>%
    rename_all(~str_to_lower(.)) %>%
    gather(day_of_week,day, ends_with("day"),na.rm = T)%>%
    select(course,title,times,day,room) %>%
    separate(col = times, into = c("s_time","e_time"),sep=" - ") %>%
    mutate(s_time=parse_date_time(s_time,'%I:%M %p'),
           e_time=parse_date_time(e_time,'%I:%M %p')) %>%
    mutate(stime=hour(s_time)+minute(s_time)/60,
           etime=hour(e_time)+minute(e_time)/60) %>%
    group_by(course) %>%
    mutate(campus = case_when(any(str_detect(room,"^A|B|C|E|TC"))~"Haverhill",
                              any(str_detect(room,"^L|LH|LC|LA|LRW"))~"Lawrence",
                              TRUE~"Unknown")) %>%
    #mutate(day=fct_relevel(day,c("M","T","W","R","F","S"))) %>%
    #group_by(course) %>%
    mutate(id = 1:n(),course_id=str_c(course,id,sep="_")) %>%
    select(-s_time,-e_time,-id) %>%
    arrange(course,day) %>%
    rowid_to_column() %>%
    ungroup()->df
  
  return(df)
}

prepare_plot_data <- function(df) {
  df %>%
    group_by(day, campus) %>%
    arrange(day, stime, etime) %>%
    mutate(indx = c(0, cumsum(lead(stime) >=
                                cummax(etime))[-n()])) %>%
    
    group_by(day, indx, course_id, campus) %>%
    arrange(day, indx, course_id, stime) %>%
    nest() %>%
    group_by(day, indx, campus) %>%
    mutate(n = n()) %>%
    mutate(x2 = cumsum(1 / n)) %>% ### right side of rectangle
    mutate(x1 = lag(x2, default = 0)) %>%  ### left side of rectangle
    unnest() %>%
    ungroup() %>%
    mutate(day=fct_relevel(day,c("M","T","W","R","F","S")))-> df
  
  return(df)
}

plot_schedule<- function(df){
  
  times <- c(str_c(1:11, " AM"), "12 PM", str_c(1:11, " PM"))
  
  ggplot(df) +
    geom_rect(
      aes(
        xmin = x1,
        xmax = x2,
        ymax = etime,
        ymin = stime,
        fill = course,
        group = course),
      color = "black",
      alpha = 1 
    ) +
    geom_text(
      aes(
        x = x1,
        y = (stime + abs(stime - etime) / 2),
        label = course,
        size = 1 / n
      ),
      hjust = 0,
      vjust = 0.5,
      nudge_x = 0.01
    ) +
    scale_size_area(max_size = 4) +
    scale_y_reverse(breaks = 1:23,
                    labels = times) +
    theme(
      legend.position = "none",
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()
    ) +
    xlab("") +
    ylab("") +
    guides(fill = FALSE) +
    facet_grid(campus ~ day)->p
  
  return(p)
    
}
