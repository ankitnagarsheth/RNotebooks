# Here we are checking if the package is installed
if(!require("rvest")){
  
  # If the package is not in the system then it will be install
  install.packages("rvest", dependencies = TRUE)
  
  # Here we are loading the package
  library("rvest")
}


# Here we are checking if the package is installed
if(!require("tidytext")){
  
  # If the package is not in the system then it will be install
  install.packages("tidytext", dependencies = TRUE)
  
  # Here we are loading the package
  library("tidytext")
}

# Here we are checking if the package is installed
if(!require("lubridate")){
  
  # If the package is not in the system then it will be install
  install.packages("lubridate", dependencies = TRUE)
  
  # Here we are loading the package
  library("lubridate")
}



# Here we are checking if the package is installed
if(!require("wordcloud")){
  
  # If the package is not in the system then it will be install
  install.packages("wordcloud", dependencies = TRUE)
  
  # Here we are loading the package
  library("wordcloud")
}


# Here we are checking if the package is installed
if(!require("tidyverse")){
  
  # If the package is not in the system then it will be install
  install.packages("tidyverse", dependencies = TRUE)
  
  # Here we are loading the package
  library("tidyverse")
}

doc <- "MyActivity.html"
search_archive <- read_html(doc)

date_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=<br>)(.*)(?<=PM|AM)") %>%
  mdy_hms()

text_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>%
  str_extract(pattern = '(?<=<a)(.*)(?=</a>)') %>% 
  str_extract(pattern = '(?<=\">)(.*)')



type_search <- search_archive %>% 
  html_nodes(xpath = '//div[@class="mdl-grid"]/div/div') %>% 
  str_extract(pattern = "(?<=mdl-typography--body-1\">)(.*)(?=<a)") %>% 
  str_extract(pattern = "(\\w+)(?=\\s)")


search_data <- tibble(timestamp = date_search,
                      date = as_date(date_search),
                      year = year(date_search),
                      month = month(date_search, label = TRUE),
                      day = weekdays(date_search),
                      hour = hour(date_search),
                      type = type_search,
                      search = text_search)


search_data$day <- factor( search_data$day , 
                           levels = c("Sunday", "Monday", "Tuesday",
                                      "Wednesday","Thursday",
                                      "Friday", "Saturday"))

search_data <- na.omit(search_data)

# View the Data
View(search_data)


#Filter by year

search_data_2015 <- filter(search_data,year == 2015)
View(search_data_2011)

# Volume of Google Searchers per Year
p <- ggplot(search_data, aes(year))
p + geom_bar() + labs(x="", title = "Volume of Google Searches Per Year")

ggsave("00-search_volume.png", 
       width = 10, height = 7,  
       dpi = 300, units = "in", device='png')


# Volume of Google Searches by Month
ggplot(monthly) + geom_bar(aes(x = month, group = year)) +
  theme(axis.text.x = element_text(angle=90)) +
  facet_grid(.~year, scales="free") 
+ labs(x="", title = "Volume of Google Searches By Month")

ggsave("01-search_volume.png", 
       width = 10, height = 7,  
       dpi = 300, units = "in", device='png')


# Volume of Google Searches by hour
p <- ggplot(search_data, aes(hour))
p + geom_bar() + labs(title = "Volume of Google Searches Per Hour")

ggsave("02-search_volume.png", 
       width = 10, height = 7,  
       dpi = 300, units = "in", device='png')

# Volume of Google Searches by weekday
p <- ggplot(search_data, aes(day))
p + geom_bar() + labs(x="", title = "Volume of Google Searches Per Weekday")

ggsave("03-search_volume.png", 
       width = 10, height = 7,  
       dpi = 300, units = "in", device='png')

#Wordcloud
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
replace_reg <- '(.*.)\\.com(.*.)\\S+\\s|[^[:alnum:]]|(http|https)\\S+\\s*|(#|@)\\S+\\s*|\\n|\\"'

#type_visited = "Visited"
#type_searched = "Searched"
#search_data[search_data$type == type_visited, ]$search

search <- search_data$search %>% 
  str_replace_all(pattern = replace_reg, replacement = " ") %>% 
  iconv(from = "ASCII", to = "UTF-8", sub = " ") %>% 
  tolower() %>% 
  trimws()

search <-  tibble(text = search) %>% 
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word, str_detect(word, "[a-z]"))

#remove words not required Wordcloud
remove_words <- c("top","www","site","canada","toronto","mississauga","milton","google")

my_stop_words <- bind_rows(data_frame(word = remove_words, lexicon = c("SMART")), stop_words)

min_freq = 11
fig_scale = c(4 , 0.5)
max_words = 200

#Display Wordcloud
search %>%
  anti_join(my_stop_words) %>%
  count(word) %>%
  with(wordcloud(word, n, 
                 scale = fig_scale,
                 min.freq = min_freq,
                 max.words = max_words))

