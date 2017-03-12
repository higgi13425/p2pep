#load libraries
library(tidyverse)
library(readxl)
library(ggplot2)
library(broom)
library(intubate)
library(magrittr)
library(ggbeeswarm)

#clear environment and set working directory
rm(list=ls())

#Read in excel file
df<- read_excel("p2pepeventbrite.xlsx", sheet=1)

#Now select columns with 10 ranked choices, rename them to be shorter
df %>% 
  select(starts_with("Please pick")) ->df2

colnames(df2) <- c("c1","c2", "c3","c4","c5","c6","c7","c8","c9","c10")

#Now gather them from wide to tall & tidy, then count and arrange in descending order.
## Use only top 4 choices
## Note that missing (NA) values are removed.
df2 %>% 
  gather(1:4, key="rank", value="choice", na.rm=TRUE) %>% 
  count(choice) %>% 
  arrange(desc(n)) %>% 
  print(n=32) -> df3
## set up capacity & audience
df3$capacity<- 0
df3$capacity[1:4]<-140
df3$capacity[5:16]<-30
df3$capacity[17:31]<-15
df3$rank <- c(1:31)
df3$audience<-0

## what would top 4 choices look like?
options(tibble.print_max = Inf)
df2 %>% 
  group_by(c1) %>% 
  tally() %>% 
  rename(choice=c1, n1=n)-> df4

left_join(df3,df4, by="choice") %>% 
  select(choice, n1,capacity, audience) -> df3

###choice 2
options(tibble.print_max = Inf)
df2 %>% 
  group_by(c2) %>% 
  tally() %>% 
  rename(choice=c2, n2=n)-> df4

left_join(df3,df4, by="choice") %>% 
  select(choice, n1,n2,capacity, audience) -> df3

###choice 3
options(tibble.print_max = Inf)
df2 %>% 
  group_by(c3) %>% 
  tally() %>% 
  rename(choice=c3, n3=n)-> df4

left_join(df3,df4, by="choice") %>% 
  select(choice, n1,n2,n3, capacity, audience) -> df3

###choice 4
options(tibble.print_max = Inf)
df2 %>% 
  group_by(c4) %>% 
  tally() %>% 
  rename(choice=c4, n4=n)-> df4

left_join(df3,df4, by="choice") %>% 
  select(choice, n1,n2,n3,n4,capacity, audience) -> df3

df3$audience <- rowSums(df3[,2:5], na.rm = TRUE)



#Now part 2 - assigning individuals to sessions, limited by room capacity
## set up capacity & audience
df3$capacity[1:4]<-140
df3$capacity[5:16]<-30
df3$capacity[17:32]<-15
df3$audience<-0
