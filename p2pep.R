#load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(broom)
library(intubate)
library(magrittr)
library(ggbeeswarm)
library(dplyr)
library(Hmisc)
library(reshape2)
library(knitr)
library(rmarkdown)

#clear environment 
rm(list=ls())

#Read in excel file
df<- read_excel("PH2017Mar23.xlsx", sheet=1)
df <- dplyr::rename(df, order=`Order #`)
df <- dplyr::rename(df, attendee=`Attendee #`)
df <- dplyr::rename(df, breakout=`Which break-out group best suits you?`)

#fill in some missing data in first 4 rows
df[1:4,33]<-"Other or prefer not to say"
df[1:2,23]<-"Infections in IBD"
df[1:4,24]<-"My Experience in Clinical Trials"
df[1:4,25]<-"Will My Baby Develop IBD?"
df[1:4,26]<-"Physical Therapy for Pain in IBD"
df[1:4,27]<-"Transitioning from Pediatric to Adult Care"
df[1:4,28]<-"Insurance Challenges with IBD" 
df[1:4,29]<-"Being Your Own Advocate in a Healthcare Setting"
df[1:4,30]<-"Travel with IBD"
df[1:4,31]<-"Fatigue Reduction Diet"
df[1:4,32]<-"PTSD in IBD"

#Now select columns with 10 ranked choices, rename them to be shorter
df %>% 
  select(1, 8, starts_with("Please pick")) ->df2

colnames(df2) <- c("order","attendee", "c1","c2", "c3","c4","c5","c6","c7","c8","c9","c10")

#need to replace TPN talk
df2[df2=="Living without Eating: The TPN Experience"]<- "Living with an Ostomy"

#update talk names in df2
df2[df2=="PTSD in IBD"]<- "Anxiety in IBD"
df2[df2=="The Patient Perspective: Testing, Scoping, and Scanning in IBD"]<-"Testing, Scanning, and Scoping in IBD"
df2[df2=="Transitions: Going to College & Going to Work with IBD"]<- "College and Work with IBD"
df2[df2=="My Experience with A Stem Cell Clinical Trial for Fistulas in IBD"]<- "Stem Cells for IBD"
df2[is.na(df2)]<- "Infections in IBD"
df2[df2=="My Experience with IBD Surgery"]<- "Experience of IBD Surgery"
df2[df2=="For Family Members: The Experience of Crohn's Disease"]<- "For Family Members: Life with IBD"
df2[df2=="For Family Members: The Experience of Ulcerative Colitis"]<- "For Family Members: Life with IBD"
df2[df2=="Managing School and 501 Plans with IBD (pediatric MD)"]<-  "Transitioning from Pediatric to Adult Care"
df2[df2=="What is IBD Remission: Adapting to a New Normal?"]<-  "A New Normal"

#Now gather them from wide to tall & tidy, then count and arrange in descending order.
## Use only top 4 choices
## Note that missing (NA) values are removed.
df2 %>% 
  gather(key="key", value="value", c1:c4) %>% 
  select(order, key, value) %>% 
  dplyr::rename(topic=value, choicenum=key) %>% 
  filter(!is.na(topic)) %>% 
  dplyr::count(topic) %>% 
  arrange(desc(n)) %>% 
  print(n=32) -> df3

#add breakouts
b1 <- c("Breakout for Ulcerative Colitis", 1, 4, "11:40 - 1:00")
b2 <- c("Breakout for Crohn's Disease", 1, 4, "11:40 - 1:00")
b3 <- c("Breakout for Family and Friends", 1, 4, "11:40 - 1:00")
b4 <- c("Breakout for J pouch and Ostomy", 1, 4, "11:40 - 1:00")

df3 <- rbind(df3,b1,b2,b3,b4)

# add topicnum
#NOTE first digit corresponds to session 1-5, 2nd digit to room(1-8)
df3<-df3 %>% arrange(topic) %>% print(n=Inf)
df3$topicnum<-0
df3$topicnum<-c(32,57,21,11,33,13,24,43,44,45,42,23,54,22,51,36,52,53,25,35,38,14,17,12,31,27,26,56,34,55,15,28,37,16)
  
  
#ADD IN SESSION numbers and times
df3$session<- floor(df3$topicnum/10)

#confirm sessions correct
df3 %>% arrange(session, desc(as.integer(n))) %>%  print(n=Inf)

df3$time<- "9:40 - 10:10"
df3$time[df3$session==2] <- "10:20 - 10:50"
df3$time[df3$session==3] <- "11:00 - 11:30"
df3$time[df3$session==4] <- "11:40 - 12:50"
df3$time[df3$session==5] <- "1:00 - 1:30"

#confirm times correct
table(df3$session, df3$time)

# add rooms
df3$room <- "Forum Hall"
df3$room[mod(df3$topicnum, 10)==2] <- "Great Lakes Central"
df3$room[mod(df3$topicnum, 10)==3] <- "Great Lakes North"
df3$room[mod(df3$topicnum, 10)==4] <- "Great Lakes South"
df3$room[mod(df3$topicnum, 10)==5] <- "Boardroom 1"
df3$room[mod(df3$topicnum, 10)==6] <- "Boardroom 2"
df3$room[mod(df3$topicnum, 10)==7] <- "Boardroom 3"
df3$room[mod(df3$topicnum, 10)==8] <- "Boardroom 4"

#check rooms
df3 %>% arrange(room, session) %>% print(n=Inf)

#bring in breakout from df
df2<- df2 %>% left_join(df, by="attendee") %>% 
    select(attendee, `First Name`, `Last Name`, Email, breakout, c1:c10) %>% 
    dplyr::rename(firstname= `First Name`, lastname=`Last Name`, email=Email)

#take df session choice data and melt it to tall
df4<- df2 %>% select(attendee, c1:c10) %>%  
    melt(id.vars= "attendee", variable.name = "choice", value.name = "topic") %>% 
    arrange(attendee, choice)

#join to bring in session data
df4 <- df4 %>% left_join(df3) %>% select( -n) %>% arrange(attendee, session, choice)

#remove rows with duplicate choices for the same session
df4 <- df4[ (df4$session != Lag(df4$session)) | (df4$attendee != Lag(df4$attendee)), ]

#some missing rows - some people with no choices for particular session
table(df4$session)

# fill in missing rows for patients who selected no choices in a particular session
df4 %>% complete(attendee, session) %>% arrange(attendee, session) -> df5

#map differences between df5 and df4
discrep <- mapply(setdiff, df5, df4)
table(df5$session)

#list filled in (but empty) rows
df5 %>% filter(is.na(time)) %>% arrange(session) %>% print(n=Inf)

#count which topics underfilled
df5 %>% dplyr::count(topic) %>% 
  arrange(desc(n)) %>% 
  print(n=Inf)

#fill missing sessions with low subscribed sessions
#session 1
df5$topic[is.na(df5$topicnum) & (df5$session==1)]<- "Will My Baby Develop IBD?"
df5$time[is.na(df5$topicnum) & (df5$session==1)]<- "9:40 - 10:10"
df5$room[is.na(df5$topicnum) & (df5$session==1)]<- "Boardroom 2"

#session 2
df5$topic[is.na(df5$topicnum) & (df5$session==2)]<- "Transitioning from Pediatric to Adult Care"
df5$time[is.na(df5$topicnum) & (df5$session==2)]<- "10:20 - 10:50"
df5$room[is.na(df5$topicnum) & (df5$session==2)]<- "Boardroom 4"

#session 3
df5$topic[is.na(df5$topicnum) & (df5$session==3)]<- "Infections in IBD"
df5$time[is.na(df5$topicnum) & (df5$session==3)]<- "11:00 - 11:30"
df5$room[is.na(df5$topicnum) & (df5$session==3)]<- "Boardroom 4"

#session 5
df5$topic[is.na(df5$topicnum) & (df5$session==5)]<- "Dating and Intimacy in IBD"
df5$time[is.na(df5$topicnum) & (df5$session==5)]<- "1:00 - 1:30"
df5$room[is.na(df5$topicnum) & (df5$session==5)]<- "Great Lakes South"


#did people get the choices they wanted?
table(df5$choice)

# set up breakouts
df %>% select(attendee, breakout) -> df6

#clean up names
df6$breakout[df6$breakout=="Crohn's Disease group"]<-"Breakout for Crohn's Disease"
df6$breakout[df6$breakout=="Ulcerative colitis group"]<-"Breakout for Ulcerative Colitis"
df6$breakout[df6$breakout=="Family/Friend group"]<-"Breakout for Family and Friends"
df6$breakout[df6$breakout=="Pediatric group"]<-"Breakout for Family and Friends"
df6$breakout[df6$breakout=="Ostomy group" | df6$breakout=="J pouch group"]<-"Breakout for J pouch and Ostomy"

#add rooms
df6$room<- ""
df6$room[df6$breakout=="Breakout for Crohn's Disease"]<-"Great Lakes North"
df6$room[df6$breakout=="Breakout for Ulcerative Colitis"]<-"Great Lakes Central"
df6$room[df6$breakout=="Breakout for Family and Friends"]<-"Great Lakes South"
df6$room[df6$breakout=="Breakout for J pouch and Ostomy"]<-"Boardroom 1"
df6$room[df6$breakout=="Other or prefer not to say"]<-"Atrium for Lunch"

#add other fields
df6$session <- 4
df6$time <- "11:40 - 1:00"

#fix names
names(df6)<-c("attendee", "topic", "room", "session", "time")
#reorder df6
df6<- df6 %>% select(attendee, session, topic, time, room)


#remove choice and topicnum column from df5
df5 %>% select(-choice, -topicnum) -> df5

#combine df5 and df6
df7 <- rbind(df5, df6)

df7 %>% arrange(attendee, session) -> df7
df7 %>% nest(-attendee) -> df7

df %>% select(attendee,`First Name`, `Last Name`) -> df8
colnames(df8) <- c("attendee", "firstname", "lastname")

# join with names
left_join(df7, df8) %>% arrange(lastname) -> df9


#now make printable schedules
# after Mine C-R at rmarkdown.rstudio.com/articles_mail_merge.html
#using df9 as input data

for (i in 1:nrow(df9)) {
  rmarkdown::render(
    input = "schedules.Rmd",
    output_format = "word_document",
    output_file = paste("schedule_", i, ".doc", sep = "")
  )
}


