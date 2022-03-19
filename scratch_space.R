# # A tibble: 1 x 9
# rowid course     title           day   room  stime etime campus    course_id   
# <int> <chr>      <chr>           <chr> <chr> <dbl> <dbl> <chr>     <chr>       
#   1     1 BIO111-B1A Intro Biology I R     E356     14  15.7 Haverhill BIO111-B1A_1


# A tibble: 19 x 10                                                                                                                            
# COURSE     TITLE            MON_DAY TUE_DAY WED_DAY THU_DAY FRI_DAY SAT_DAY TIMES               ROOM 
# <chr>      <chr>            <chr>   <chr>   <chr>   <chr>   <chr>   <lgl>   <chr>               <chr>
# BIO111-B1A Intro Biology I  NA      NA      NA      R       NA      NA      02:00 PM - 03:40 PM E356 
# BIO111-B1A Intro Biology I  NA      NA      NA      NA      NA      NA      -                   ONON 

## select(course,title,times,day (gathred days),room)
## course, title, times, days, site/campus

### check for minimum columns ### 


df <- try_catch({
  read_excel("www/example_course_file.xlsx")
},
.e = ~ {
  return(NULL)
})


df %>% 
  prepare_data(.) %>%
  prepare_plot_data() %>%
  plot_schedule(.,course_id)
  

  
read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
  rename_all(~str_to_lower(.)) %>% 
  gather(day_of_week,day, ends_with("day"),na.rm = T) %>%
  View()

  
  read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data() %>%
    filter(str_detect(course,"ENG101|MAT|BIO")) %>%
    prepare_plot_data() %>%
    mutate(type=str_extract(course,"^\\w{3}")) %>%
    plot_schedule(.,fill=type)

  
  times <- c(str_c(1:11, " AM"), "12 PM", str_c(1:11, " PM"))
  read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data() %>%
    filter(str_detect(course,"ENG101|MAT|BIO")) %>%
    mutate(day=fct_relevel(day,c("M","T","W","R","F","S")))%>%
    mutate(type=str_extract(course,"^\\w{3}")) %>%
    ggplot()+
    geom_segment(aes(x=type,
                     xend=type,
                  y=stime,
                  yend=etime),
                 size=9,
              alpha=0.3)+
    scale_y_reverse(breaks = 1:23,
                    labels = times)+
    facet_grid(campus ~ day)
    
 
  
    
  step_plot<-function(df,type){  
    df %>%
    filter(str_detect(course,{{type}})) %>%
    group_by(day,campus) %>%
    gather(which_time,time,stime,etime)%>%
    arrange(campus,day,time)%>% 
    mutate(count_chg=ifelse(which_time=="stime",1,-1))%>%
    mutate(x=cumsum(count_chg))%>%
    group_by(day,campus,time)%>%
    summarise(count=sum(count_chg))%>%
    mutate(x=cumsum(count))%>%
    ungroup()->df
    
    return(df)
      
    }
  
  bind_rows(read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data()%>%
    step_plot(.,"ENG101") %>%
    mutate(type="ENG101"),
    
  read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data()%>%
    step_plot(.,"BIO")%>%
    mutate(type="BIO"),
  
  read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data()%>%
    step_plot(.,"MAT")%>%
    mutate(type="MAT")) %>%
    mutate(xx=lag(x))%>%
    gather(when,value,x,xx)%>%
    mutate(day=fct_relevel(day,c("M","T","W","R","F","S")))%>%
    arrange(campus,day,type,time,when)%>%
  ggplot()+
    #geom_ribbon(aes(x=time,ymin=0 ,ymax=value,group=type,fill=type))+
    #geom_line(aes(x=time,y=value))+
    #geom_step(aes(x=time,y=x,group=type,color=type),position = "stack")+
    #geom_step(aes(x=time,y=x),color="red")+
    #geom_point(aes(x=time,y=x))+
    #geom_point(aes(x=time,y=xx))+
    #geom_ridgeline(aes(x=time,y=type,height=value,fill=type))+
    geom_density_ridges(aes(x=time,y=type,height=value,group=type,fill=type),
                        stat = "identity", scale = 1.1)+
    coord_flip()+
    scale_x_reverse()+
    ylab("# course sections")+
    xlab("time of day (24hr)")+
    facet_grid(campus~day)
   
  
  
  
  
  
  bind_rows(read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
              prepare_data()%>%
              step_plot(.,"ENG101") %>%
              mutate(type="ENG101"),
            
            read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
              prepare_data()%>%
              step_plot(.,"BIO")%>%
              mutate(type="all_BIO"),
            
            read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
              prepare_data()%>%
              step_plot(.,"MAT")%>%
              mutate(type="all_MAT")) %>%
    mutate(xx=lag(x))%>%
    gather(when,value,x,xx)%>%
    mutate(day=fct_relevel(day,c("M","T","W","R","F","S")))%>%
    arrange(campus,day,type,time,when)%>%
    ggplot()+
    geom_ribbon(aes(x=time,ymin=0 ,ymax=value,group=type,fill=type))+
    #geom_line(aes(x=time,y=value))+
    #geom_step(aes(x=time,y=x,group=type,color=type),position = "stack")+
    #geom_step(aes(x=time,y=x),color="red")+
    #geom_point(aes(x=time,y=x))+
    #geom_point(aes(x=time,y=xx))+
    #geom_ridgeline(aes(x=time,y=type,height=value,fill=type))+
    #geom_density_ridges(aes(x=time,y=type,height=value,group=type,fill=type),
    #                    stat = "identity", scale = 1.1)+
    coord_flip()+
    scale_x_reverse()+
    ylab("# course sections")+
    xlab("time of day (24hr)")+
    facet_grid(campus~day+type)
  
  
  read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx",skip = 1) %>%
    prepare_data() %>%
    mutate(dur=etime-stime)%>%
    mutate(on_grid=case_when(stime %in% seq(8,by=1.5,length.out=7) & dur==1.25~"on grid",
                             TRUE~"off grid")) %>%
    filter(stime<17) %>%
    arrange(course,day,stime) %>%
    mutate(type=str_extract(course,"^\\w{3}")) %>%
    mutate(day=fct_relevel(day,c("M","T","W","R","F","S")))%>%
    group_by(day,type,on_grid) %>%
    summarise(n=n())%>%
    group_by(type) %>%
    mutate(total=sum(n))%>%
    ungroup()%>%
    mutate(type=fct_reorder(type,total))%>%
    ggplot()+
    geom_col(aes(type,n,fill=on_grid))+
    coord_flip()+
    facet_grid(~day)
    
    
     seq(8,by=1.5,length.out=7)
     
     
     
library(fs)     
     
dir_info("/var/log/shiny-server/")

dir_ls("/srv/shiny-server/")
       
#/var/log/shiny-server/schedule-shiny-20190907-210738-41687.log" = add file apply button crash

#/var/log/shiny-server/schedule-shiny-20190907-210513-42392.log = modify file apply button crash
     
       

read_xlsx("data/course_schedule-2019-09-08.xlsx") %>%
  prepare_data() %>% View()

sudo su - -c "R -e \"install.packages('shinyWidgets', lib ='/usr/lib/R/site-library',repos='http://cran.rstudio.com/')\""

    

  
  