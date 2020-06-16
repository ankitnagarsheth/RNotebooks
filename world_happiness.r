#Load Packages
library(lubridate)
library(tidyverse)
library(stringr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(sqldf)

#Load Data
df <- read.csv("C:/Users/nagara/Desktop/GWL/Ankit/Learning/Datasets/World Happiness/world_happiness.csv")


#Take a Look
glimpse(df)
view(df)

summary(df)

head(df)
rename(df,c(Healthy.life.expectancy.at.birth = Lifeexpectancy,Freedom.to.make.life.choices= Freedom))
df

colnames(df)
df %>% as_tibble(df)

dfc <- df %>% filter(Country.name == "Canada") %>% select(Life.Ladder,Healthy.life.expectancy.at.birth,Freedom.to.make.life.choices,Year,Generosity) 
  
plot(dfc)

summary(dfc)

colnames(dfc)

cor(dfc)
round(cor(dfc), 2)
filter(dfc, Year > 2007)
hist(dfc$Freedom.to.make.life.choices)

hist(dfc$Healthy.life.expectancy.at.birth)

view(dfc)
arrange(dfc, desc(Healthy.life.expectancy.at.birth))

pl <- ggplot(data = dfc,aes(x=Year,y=Freedom.to.make.life.choices))

pl + geom_line(aes(color = "red")) +theme_bw()
hist(dfc$Life.Ladder)


pl <- ggplot(data = dfc,aes(x=Year,y=Freedom.to.make.life.choices))

pl + geom_boxplot()

typeof(dfc)

glimpse(dfc)
colnames(dfc)[2] <- "Lifeexpectancy"

dfc$Generosity <- as.numeric(dfc$Generosity)
unique(dfc)

dir()

# Install pacman ("package manager") if needed
if (!require("pacman")) install.packages("pacman")

pacman::p_load(datasets, pacman, psych, rio, tidyverse)

dfc %>% ggplot(aes(Ladder,fill = Lifeexpectancy)) +geom_bar

dfc %>% as_tibble() %>% mutate_all(as.factor) 


