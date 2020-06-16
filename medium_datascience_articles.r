
###Load Packages
if (!require("pacman")) install.packages("pacman")

pacman::p_load(datasets,magnittr, pacman, vcd, rio, tidyverse)

#Load Data
df <- read.csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-12-04/medium_datasci.csv")



#Take a Look
glimpse(df)
view(df)

summary(df)

head(df)

#Select columns
df %>% select(-x1,-subtitle,-image) %>% glimpse()

#Want to check which author has highest number of claps
df %>% 
  select(author,claps) %>% 
  arrange(desc(claps)) %>% 
  top_n(10) %>% 
print()

##Sophia Ciocca author has highest number of claps

#Want to check which articles have highest claps

df %>% 
  select(title,claps) %>% 
  arrange(desc(claps)) %>% 
  top_n(10) %>% 
  print()

## How Does Spotify Know You So Well? has 60000 claps

#Want to check which is the top publication

glimpse(df)


##count publications

df %>% count(publication,sort=TRUE)

##count authors
df %>% count(author,sort=TRUE)





df %>% 
  select(publication,claps) %>% 
  arrange(desc(claps)) %>% 
  top_n(10) %>% 
  print()
###freeCodeCamp.org 39000 top publication. The one with 60000 claps do not have any publications


glimpse(df)
df_gather <- df %>% 
  gather(tag,value,starts_with("tag")) %>%
  mutate(tag = str_remove(tag,"tag_")) %>% 
  filter(value == 1)
  glimpse()

  df_gather %>% count(tag,sort = TRUE)
  
  library(tidytext)
df %>% 
select(title,subtitle,year,reading_time,claps)  %>% 
  unnest_tokens(word,title)


glimpse(df)
  

df_gather %>%
  group_by(tag) %>% 
summarize(median_claps = median(claps)) %>% 
  arrange(desc(median_claps))

df_gather %>%
group_by(tag) %>% 
summarise(read_time = mean(reading_time)) %>% 
arrange(desc(read_time))


glimpse(df_gather)

df_gather <- df_gather %>% 
  select(title,author,year,month,day,reading_time,claps,tag,url,author_url) %>% 
 glimpse()

export(df_gather,"medium_2018.csv")


getwd()

df_gather %>% 

###Lets remove the NA in the title
 df_gather <- df_gather %>% drop_na(title) %>% 
  glimpse()


 ###Lets add post id
 df_gather %>% 
 transmute(post_id = row_number(),title,year,reading_time,claps) %>% 
glimpse() 





