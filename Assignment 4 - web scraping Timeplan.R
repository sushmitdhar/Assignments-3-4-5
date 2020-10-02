  rm(list=ls())
  
  # loading packages
  library(tidyverse)
  library(rvest)
  library (dplyr)

  
  
  # Reading the HTML content of the URLs
  #browseURL("http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list")
  
  
  
  # Assigning the web-page to an object called webpage_number
  url_1 <-"http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list"
  scraped_1 <- Sys.time()
  webpage_1 <- read_html(url_1)
  webpage_1
  str(webpage_1) # list 
  
  ####html sessions
  
  html_session("http://timeplan.uit.no/emne_timeplan.php?sem=20h&module%5B%5D=BED-2056-1&View=list")
  
  
  Calender <- tibble(webpage_1 %>% html_nodes("td:nth-child(1)") %>% html_text())
  colnames(Calender) <- c("Date")
  df<-str_remove_all(Calender$Date,"[Mandag]")
  BED_2056<-tibble(as.Date(df,"%d.%m.%Y"))
  colnames(BED_2056) <- c("Class Dates")
  View(BED_2056)
  
  
