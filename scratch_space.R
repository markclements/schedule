# # A tibble: 1 x 9
# rowid course     title           day   room  stime etime campus    course_id   
# <int> <chr>      <chr>           <chr> <chr> <dbl> <dbl> <chr>     <chr>       
#   1     1 BIO111-B1A Intro Biology I R     E356     14  15.7 Haverhill BIO111-B1A_1


# A tibble: 19 x 10                                                                                                                            
# COURSE     TITLE            MON_DAY TUE_DAY WED_DAY THU_DAY FRI_DAY SAT_DAY TIMES               ROOM 
# <chr>      <chr>            <chr>   <chr>   <chr>   <chr>   <chr>   <lgl>   <chr>               <chr>
# BIO111-B1A Intro Biology I  NA      NA      NA      R       NA      NA      02:00 PM - 03:40 PM E356 
# BIO111-B1A Intro Biology I  NA      NA      NA      NA      NA      NA      -                   ONON 

select(course,title,times,day (gathred days),room)

read_xlsx("www/example_course_file.xlsx",skip = 1) %>%
  rename_all(~str_to_lower(.)) %>%
  mutate(stime=str_extract(times,"^\\d{1,2}:\\d{2}\\s*\\w{2}"),
         etime=str_extract(times,"\\d{1,2}:\\d{2}\\s*\\w{2}$")) %>%
  mutate(stime=parse_date_time(stime,'%I:%M %p'),
         etime=parse_date_time(etime,'%I:%M %p')) %>%
  mutate(stime=hour(stime)+minute(stime)/60,
         etime=hour(etime)+minute(etime)/60)%>%
  mutate(new_times= str_c(format(as.POSIXct(stime*3600,origin="2001-01-01", "GMT"),"%I:%M %p"),
         e_time=format(as.POSIXct(etime*3600,origin="2001-01-01", "GMT"),"%I:%M %p"),sep = " - "))
        
  
  
  
read_xlsx("www/example_course_file.xlsx",skip = 1) %>%
  rename_all(~str_to_lower(.)) %>% 
  map(~.)
  
  