
#Web Scraping party list for library

library(rvest)
library(RSelenium)
library(tidyverse)
library(data.table)
library(stringr)

######################################################################################################################################################

date = gsub("-", "_", Sys.Date())

url = "https://www.spr.gov.my/ms/pilihan-raya/penjalanan-pilihan-raya/calon#tab_tab-2"

driver = rsDriver(port = as.integer(sample(1000:10000, 1)), browser = "chrome", chromever = "105.0.5195.19")

remDr = driver[["client"]]

remDr$navigate(url)

Sys.sleep(.5)

######################################################################################################################################################

nodefxn = function(nodename)  {
  node1 = (nodename)
  node1 = gsub(" ",".",node1)
  node1 = gsub(":","\\\\:",node1)
  
  return(node1)
  
}

######################################################################################################################################################
# 
# views-table cols-2 table table-hover table-striped

nodename = nodefxn("views-table cols-2 table table-hover table-striped")
nodename = paste0("table.",nodename," tr")

extractlibrarylist = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text() 


length(extractlibrarylist)

partibertandinglist = str_squish(extractlibrarylist)



























