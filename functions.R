# prepare data

prepare_data<-function(df){
  df %>%
    rename_all(~str_to_lower(.)) %>%
    select(course,
           title,
           times,
           site,
           ends_with("day")) %>%
    mutate(on_day=case_when(site=="ON"~"ON",
                            TRUE~NA_character_))%>%
    mutate(stime=str_extract(times,"^\\d{1,2}:\\d{2}\\s*\\w{2}"),
           etime=str_extract(times,"\\d{1,2}:\\d{2}\\s*\\w{2}$")) %>%
    mutate(stime=parse_date_time(stime,'%I:%M %p'),
           etime=parse_date_time(etime,'%I:%M %p')) %>%
    mutate(stime=hour(stime)+minute(stime)/60,
           etime=hour(etime)+minute(etime)/60) %>%
    gather(day_of_week, day, ends_with("day"), na.rm = T) %>%
    select(-day_of_week,-times) %>% 
    mutate(id = 1:n(),course_id=str_c(course,id,sep="_")) %>%
    arrange(course,day) %>%
    rowid_to_column() %>%
    rename(campus=site)%>%
    mutate(campus=case_when(campus=="H"~"Haverhill",
                            campus=="L"~"Lawrence",
                            TRUE~"Other"))%>%
    ungroup()->df ### output to app ###
  return(df)
}

place_events <- function(d) {
    d <- mutate(d, col = 0)
    for (i in seq_along(d$col)) {
        if (i == 1) {
            d$col[i] <- 1
            next
        }
        placed <- FALSE
    for (j in 1:max(d$col)) {
           collide <- d$stime[i] < last(d$etime[d$col == j])
           if (!collide) {
                d$col[i] <- j
                placed <- TRUE
                 break
            }
        }
        if (!placed) d$col[i] <- j + 1
        }
       return(d)
    }


prepare_plot_data <- function(df) {
  df %>%
    filter(day != "ON") %>%
    group_by(day, campus) %>%
    arrange(day, stime, etime) %>%
    mutate(indx = c(0, cumsum(lead(stime) >=
                                cummax(etime))[-n()])) %>%
    group_by(day, indx, campus) %>%
    nest() %>%
    mutate(data = map(data, ~place_events(.))) %>%
    unnest(data) %>%
    mutate(col = col - 0.95) %>%
    mutate(colr = col + 0.95) %>%
    mutate(x1 = col / (max(col) + 1), x2 = colr / (max(col) + 1)) %>%
    ungroup() %>% 
    mutate(day = fct_relevel(day, c("M","T","W","R","F","S"))) -> df
  
  return(df)
}

plot_schedule <- function(df, fill) {

  times <- c(str_c(1:11, " AM"), "12 PM", str_c(1:11, " PM"))

ggplot(df) +
    geom_rect(
      aes(
        xmin = x1,
        xmax = x2,
        ymax = etime,
        ymin = stime,
        fill = {{fill}},
        group = {{fill}}
    ), 
    alpha = 0.6, 
    color = "black") +
    geom_text(
      aes(
        x = x1,
        y = (stime + abs(stime - etime) / 2),
        label = course,
        size = (x2 - x1) * 100
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
    facet_grid(campus ~ day) -> sched->p
  
  return(p)
    
}

export_file<-function(df){
 df %>% 
  select(day,course,title,campus,stime,etime)%>%
  mutate(day_title=case_when(day=="M"~"mon_day",
                             day=="T"~"tue_day",
                             day=="W"~"wed_day",
                             day=="R"~"thu_day",
                             day=="F"~"fri_day",
                             day=="S"~"sat_day",
                             day=="ON"~"on_day")) %>%
    
    spread(day_title,day) %>% 
    mutate(times= str_c(format(as.POSIXct(stime*3600,origin="2001-01-01", "GMT"),"%I:%M %p"),
                        e_time=format(as.POSIXct(etime*3600,origin="2001-01-01", "GMT"),"%I:%M %p"),sep = " - "))%>%
    rename(site=campus) %>%
    select(-stime,-etime) %>%
    mutate(site=case_when(site=="Haverhill"~"H",
                          site=="Lawrence"~"L",
                          site=="Other"~"ON"))->output
    #select(course,title,mon_day,tue_day,wed_day,thu_day,fri_day,sat_day,times,site)->output
  return(output)
}
