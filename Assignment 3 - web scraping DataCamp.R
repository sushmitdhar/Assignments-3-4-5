rm(list=ls())

# loading packages
library(tidyverse)
library(rvest)
library (dplyr)


# Reading the HTML content of the URLs
browseURL("https://www.datacamp.com/courses/tech:r")
browseURL("https://www.datacamp.com/courses/tech:python")


# Assigning the web-page to an object called webpage_number
url_1 <- "https://www.datacamp.com/courses/tech:r"
scraped_1 <- Sys.time()
webpage_1 <- read_html(url_1)
webpage_1
str(webpage_1) # list 1

url_2 <- "https://www.datacamp.com/courses/tech:python"
scraped_2 <- Sys.time()
webpage_2 <- read_html(url_2)
webpage_2
str(webpage_2) # list 2

####html sessions

html_session("https://www.datacamp.com/courses/tech:r")

html_session("https://www.datacamp.com/courses/tech:python")



Course.R <- tibble(webpage_1 %>% html_nodes(".course-block__title") %>% html_text())%>% add_column(L = "R Language")
colnames(Course.R) <- c("Tech","Language")

Course.P <- tibble( webpage_2 %>% html_nodes(".course-block__title") %>% html_text())%>% add_column(L = "Python Language")
colnames(Course.P) <- c("Tech","Language")


Data_Camp <- rbind(Course.R,Course.P)
View(Data_Camp)

