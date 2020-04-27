read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx") %>%
  prepare_data() %>%
  filter(day=="M" & str_detect(course,"BIO")) %>%
  arrange(stime,etime) %>% 
  select(rowid,day,course,stime,etime)%>%
  full_join(.,.,by="day") %>%
  filter(rowid.x!=rowid.y) %>%
  mutate(indx=stime.x>=stime.y & etime.x>=stime.y) %>%




d %>%
  rowid_to_column() %>%
  left_join(.,.,by=c("day")) %>%
  filter(rowid.x!=rowid.y & rowid.x<rowid.y) %>% ### only unique combonations
  mutate(overlap=stime.y >= stime.x & stime.y <etime.x |
           stime.y<=stime.x & etime.y>stime.x )


d %>%
  rowid_to_column() %>%
  left_join(.,.,by=c("day")) %>%
  filter(rowid.x!=rowid.y) %>% ### all combinations 
  mutate(overlap=stime.y >= stime.x & stime.y <etime.x |
           stime.y<=stime.x & etime.y>stime.x ) %>%
  group_by(name.x)%>%
  filter(overlap=="TRUE")%>%
  mutate(num_neighbors=sum(overlap))



#### linear in time overlap/step plot ####  
  d %>%
    rowid_to_column()%>%
    gather(x,y,stime,etime)%>%
    arrange(y,x) %>%
    mutate(sum=cumsum(x=="stime")-cumsum(x=="etime")) %>%
    group_by(y)%>%
    mutate(zz=last(sum)) %>%
    ggplot()+
    geom_step(aes(y,zz))


wq
    

d<-tibble(name=c("one","two","three","four","five"),
          day="M",
          stime=c(9 , 9,  9, 9,11),
          etime=c(10, 10, 12, 11,12)) %>%
    mutate(dur=etime-stime)%>%
    arrange(stime,desc(dur),etime) %>%
    mutate(col=NA)

read_xlsx("www/example_course_file.xlsx") %>%
  prepare_data() %>%
  filter(!is.na(stime)) %>%
  mutate(dur=etime-stime)%>%
  arrange(day,stime,desc(dur),etime) %>%
  mutate(col=NA) %>%
  mutate(column_grp=NA)%>%
  filter(day=="T")->d

#### algorithim to place time slots into non-overlapping columns ####  
  
  last_event_ending<-NA
  
  for (i in 1:length(d$stime)){
    
    if (!is.na(last_event_ending) & d$stime[i] >= last_event_ending){

      last_event_ending<-NA ### new group
    }
    
    if (all(is.na(d$col))) { ### first event
      d$col[i]<-1
      next
      }
    
    placed <- FALSE
    
    for (j in 1:max(d$col,na.rm=T)){
      
      d %>%
        filter(col=={{j}}) %>%
        summarise(last(etime)) %>%
        unlist()->etime_last_event
      
      if (d$stime[i]>=etime_last_event){
      
        d$col[i]<-j 
        placed<-TRUE
        break
      }
    }        
    
    if (!placed) d$col[i]<-j+1 
    
    if (is.na(last_event_ending) | d$etime[i] > last_event_ending) last_event_ending<-d$etime[i] 
  }  



d %>%
  rowid_to_column() %>%
  left_join(.,.,by=c("day")) %>%
  filter(rowid.x!=rowid.y & 
           #rowid.x<rowid.y  &
           col.x<col.y) %>% ### only unique combonations
  mutate(overlap=stime.y >= stime.x & stime.y <etime.x |
           stime.y<=stime.x & etime.y>stime.x ) %>%
  mutate(colspan=abs(col.y-col.x)) %>%
  filter(colspan>0) %>%
  group_by(name.x) %>%
  filter(!any(overlap==TRUE))
  

d %>%
  ggplot()+
  geom_segment(aes(x=col,xend=col,y=stime,yend=etime,color=course),size=40,alpha=0.5) +
  geom_text(aes(x=col,y=(stime+((etime-stime)/2)),label=course))+
  scale_y_reverse()+
  theme(legend.position = "none") 


read_xlsx("www/example_course_file.xlsx")%>%
  map(.,~.) %>%
  keep(.,~str_detect(.,"^[M,T,W,R,F,S]$") %>% any(.,na.rm = T))
  
read_xlsx("data/Master Schedule Reform Common Sections USE THIS ONE April 24 2019.xlsx")->raw_data

raw_data %>% glimpse()
  
raw_data %>%
  select_if(.,~str_detect(.,"^[M,T,W,R,F,S]$") %>% any(.,na.rm = T))

raw_data %>% 
  select_if(.,~str_detect(.,"^\\d{1,2}:\\d{2}\\s*\\w{2}") %>% any(.,na.rm = T))

raw_data %>% 
  select_if(.,~str_detect(.,"^\\w{3}\\d{3}-") %>% any(.,na.rm = T))

 
  
