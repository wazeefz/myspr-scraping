
#Web Scraping mySPR

library(rvest)
library(RSelenium)
library(tidyverse)
library(data.table)

######################################################################################################################################################

date = gsub("-", "_", Sys.Date())

url = "https://mysprsemak.spr.gov.my/semakan/keputusan/pru"

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

#CLICKING ALL PRU SEASON LOOP

nodename = nodefxn("pruId-tabs.flex.flex-row.mb-0.list-none pt-3 pb-4 overflow-x-auto")
nodename = paste0("ul.",nodename, " li")
cat(nodename)

clickpru = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("ul.pruId-tabs.flex.flex-row.mb-0.list-none.pt-3.pb-4.overflow-x-auto li") %>% html_text()

if(length(clickpru)!=0){
  Sys.sleep(0.5)
  buttonpru = remDr$findElements(using = "css selector",
                             value = nodename)

}

for(i in 1:length(buttonpru)){
  buttonpru[[i]]$clickElement()
  Sys.sleep(1)
}


######################################################################################################################################################

#CLICKING PRU KE 14

# 
# id1 = ("7FC9A145-18BD-4431-BE14-6552152DCB65")
# node1 = (".w-32 text-sm font-bold uppercase px-5 py-3 shadow-lg rounded block leading-normal hover:text-white hover:bg-primary text-white bg-primary tab-active")
# 
# node1 = gsub(" ",".",node1)
# node1 = gsub(":","\\\\:",node1)
# node1 = paste0(init1,id1,node1)
# 
# node1
# 
# click1 = read_html(remDr$getPageSource()[[1]]) %>%
#   html_nodes("i.fas.fa-space-shuttle.text-base.mr-1")
# 
# click1
# 
# if(length(click1)!=0){
#   Sys.sleep(0.5)
#   x_btn = remDr$findElements(using = "id",
#                             value = "7FC9A145-18BD-4431-BE14-6552152DCB65")
#   
#   x_btn[[1]]$clickElement()
#   x_btn$clickElement()
#   
# }


######################################################################################################################################################

#CLICKING SILA PILIH NEGERI BUTTON

silapilihnegeri = function(){
  nodename = nodefxn("select2-selection select2-selection--single font-bold")
  nodename = paste0("span.",nodename)
  
  
  click2 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text()
  
  if(length(click2)!=0){
    Sys.sleep(0.5)
    x_btn = remDr$findElement(using = "css selector",
                              value = "span.select2-selection__arrow")
  }
  
  
  x_btn$clickElement()

}

silapilihnegeri()

######################################################################################################################################################

#CLICKING ALL NEGERI LOOP

nodename = nodefxn("select2-results__options")
nodename = paste0("ul.",nodename, " li")
cat(nodename)

silapilihnegeri()

click = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text()


if(length(click)!=0){
  Sys.sleep(0.5)
  buttonnegeri = remDr$findElements(using = "css selector",
                             value = nodename)
  
}

for(j in 1:length(buttonnegeri)){
  
  if(j==length(buttonnegeri)){break}
  
  if(j!=1){silapilihnegeri()}
  
  buttonnegeri = remDr$findElements(using = "css selector",
                                    value = nodename)
  buttonnegeri[[j+1]]$clickElement()
  
}

######################################################################################################################################################

#CLICKING BAHAGIAN PILIHAN RAYA LOOP

nodename = nodefxn("flex mb-0 list-none flex-wrap pt-3 pb-4 flex-row")
nodename = paste0("ul.",nodename, " li")
cat(nodename)

clickbahpru = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text()

if(length(clickbahpru)!=0){
  Sys.sleep(0.5)
  bahprubutton = remDr$findElements(using = "css selector",
                                 value = nodename)
  
}

for(k in 1:length(bahprubutton)){
  bahprubutton[[k]]$clickElement()
  Sys.sleep(1)
}

######################################################################################################################################################

#CLICKING ALL PARLIMEN LIST

nodename = nodefxn("text-sm md:text-base tab-parlimen-web accordion overflow-auto block")
nodename = paste0("div.",nodename, " div")
cat(nodename)

clickparlimenlist = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text()

if(length(clickparlimenlist)!=0){
  Sys.sleep(0.5)
  parlimenlistbutton = remDr$findElements(using = "css selector",
                                    value = nodename)
  
}

for(l in 1:length(parlimenlistbutton)){
  parlimenlistbutton[[l]]$clickElement()
  Sys.sleep(1)
}

######################################################################################################################################################

#CLICKING ALL DUN LIST

nodename = nodefxn("text-sm md:text-base space-y-2 mb-2 overflow-auto block")
nodename = paste0("div.",nodename, " div")
cat(nodename)

clickdunlist = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text()

if(length(clickdunlist)!=0){
  Sys.sleep(0.5)
  dunlistbutton = remDr$findElements(using = "css selector",
                                          value = nodename)
  
}

for(l in 1:length(dunlistbutton)){
  dunlistbutton[[l]]$clickElement()
  Sys.sleep(1)
}

######################################################################################################################################################










######################################################################################################################################################

#CLICKING DAERAH

nodename = nodefxn("text-sm md:text-base tab-parlimen-web accordion block overflow-auto")
nodename = paste0("div.",nodename)
cat(nodename)

# click = read_html(remDr$getPageSource()[[1]]) %>%
#   html_nodes(nodename)
nodename = "ul.select2-results__options li"
click = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes(nodename) %>% html_text()


# click

if(length(click)!=0){
  Sys.sleep(0.5)
  x_btn = remDr$findElements(using = "css selector",
                             value = nodename)
  
  x_btn[[10]]$clickElement()
  x_btn$clickElement()
  
}

######################################################################################################################################################

#EXTRACTING DATA

mysprdata = data.frame()

  nodename = nodefxn("sm:mx-16 sm:block hidden uppercase mx-auto text-md")
  nodename = paste0("span.",nodename)
  
  
  extract = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract)!=0){
    extract = extract
  }else{
    extract = NA 
  }
  
  Jenis = extract[1]

  

  nodename = nodefxn("mx-auto uppercase text-sm my-2 font-bold")
  nodename = paste0("span.",nodename)
  

  extract1 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract1)!=0){
    extract1 = extract1
  }else{
    extract1 = NA 
  }
  
  PRU = extract1[1]
  Tarikh = extract1[2]
  
  nodename = nodefxn("text-center mx-auto uppercase text-sm mb-4 sm:my-4 font-bold")
  nodename = paste0("span.",nodename)

  
  extract2 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract2)!=0){
    extract2 = extract2
  }else{
    extract2 = NA 
  }
  
  Menang = extract2
  
  nodename = nodefxn("text-center mx-auto text-4xl my-auto h-16")
  nodename = paste0("span.",nodename)
  
  
  extract3 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract3)!=0){
    extract3 = extract3
  }else{
    extract3 = NA 
  }
  
  MajoritiUndi = extract3
  
  nodename = nodefxn("bg-primary p-2 uppercase border-b border-white text-white font-bold rounded-t-lg")
  nodename = paste0("div.",nodename)
  
  extract4 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract4)!=0){
    extract4 = extract4
  }else{
    extract4 = NA 
  }
  
  KodDanNama = extract4
  
  nodename = nodefxn("select2-selection__rendered")
  nodename = paste0("span.",nodename)
  
  
  extract5 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text()
  if(length(extract5)!=0){
    extract5 = extract5
  }else{
    extract5 = NA 
  }
  
  Negeri = extract5
  
  nodename = nodefxn("flex flex-col w-full justify-center mr-4")
  nodename = paste0("div.",nodename)
  
  
  extract7 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text()
  if(length(extract7)!=0){
    extract7 = extract7
  }else{
    extract7 = NA 
  }
  
  extract7 = paste(str_squish(extract7),collapse = '/ ')
  
  nodename = nodefxn("grid grid-flow-row font-bold text-center items-center col-1 border-l border-t keputusan-nama-calon")
  nodename = paste0("span.",nodename)
  
  
  extract8 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename) %>% html_text() 
  if(length(extract8)!=0){
    extract8 = extract8
  }else{
    extract8 = NA 
  }
  
  extract8 = paste(str_squish(extract8),collapse = '/ ')


  df = data.frame(Jenis = Jenis,
                  PRU = PRU,
                  Negeri= Negeri,
                  KodDanNama = KodDanNama,
                  Tarikh = Tarikh,
                  Menang = Menang,
                  MajoritiUndi = MajoritiUndi,
                  NamaDanParti = extract7,
                  BilUndi = extract8)
                 

  
  mysprdata = rbind(mysprdata, df)
  
######################################################################################################################################################







######################################################################################################################################################
# init1 = ("i.")
# init1 = ("a.")
# init1 = ("li.")
# node1 = ("fas fa-space-shuttle text-base mr-1")
# node1 = ("mr-4 flex-row text-center cursor-pointer")
# node1 = ("w-32 text-sm font-bold uppercase px-5 py-3 shadow-lg rounded block leading-normal hover:text-white hover:bg-primary text-white bg-primary tab-active")
# node1 = gsub(" ",".",node1)
# node1 = gsub(":","\\\\:",node1)
# node1 = paste0(init1,node1)
# 
# node1
# 
# click1 = read_html(remDr$getPageSource()[[1]]) %>%
#   html_nodes(node1) %>% html_attr("href")
# 
# if(length(click1)!=0){
#   Sys.sleep(0.5)
#   x_btn = remDr$findElement(using = "css selector",
#                             value = node1)
# }                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                   
# 
# 
# x_btn$clickElement()

######################################################################################################################################################

c.1 = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("span.select2-selection__arrow") %>% html_text()

if(length(click1)!=0){
  Sys.sleep(0.5)
  x_btn = remDr$findElement(using = "css selector",
                            value = "span.select2-selection__arrow")
}


x_btn$clickElement()

click1.1 = read_html(remDr$getPageSource()[[1]]) %>%
  html_nodes("li.select2-results__option.select2-results__option--selectable.select2-results__option--highlighted") %>% html_text()
  
  

if(length(click1)!=0){
  Sys.sleep(0.5)
  x_btn = remDr$findElement(using = "css selector",
                            value = "li.select2-results__option.select2-results__option--selectable.select2-results__option--highlighted")
}


x_btn$clickElement()

init1 = ("div.")
node1 = ("parlimen-content grid grid-cols-2 justify-between font-semibold rounded-lg hover:bg-gray-300 p-2 mb-2 cursor-pointer")
node1 = gsub(" ",".",node1)
node1 = gsub(":","\\\\:",node1)
node1 = paste0(init1,node1)
node1

link1 = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes ("node1") %>%  html_text("")


link1

if(length(click1)!=0){
  Sys.sleep(0.5)
  x_btn = remDr$findElement(using = "css selector",
                            value = "node1")
}


x_btn$clickElement()

######################################################################################################################################################

kembali = function() {

  # browser()
  
  Sys.sleep(0.5)
  kembalibutton = remDr$findElements(using = "css selector",
                                     value = nodenamekembali)
  kembalibutton[[1]]$clickElement()
  
  clickpru = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes("ul.pruId-tabs.flex.flex-row.mb-0.list-none.pt-3.pb-4.overflow-x-auto li") %>% html_text()
  
  if(length(clickpru)!=0){
    Sys.sleep(0.1)
    buttonpru = remDr$findElements(using = "css selector",
                                   value = nodenamepru)
    buttonpru[[i]]$clickElement()
    
  }
  
  silapilihnegeri()
  
  click = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodenamenegeri) %>% html_text()
  

  if(length(click)!=0){  
    Sys.sleep(0.5)
    buttonnegeri = remDr$findElements(using = "css selector",
                                      value = nodenamenegeri)
    buttonnegeri[[j+1]]$clickElement()
  }

  nodename1 = nodefxn("flex mb-0 list-none flex-wrap pt-3 pb-4 flex-row")
  nodename1 = paste0("ul.",nodename1, " li")
  
  
  clickbahpru = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename1) %>% html_text()
  
  if(length(clickbahpru)!=0){
    Sys.sleep(0.1)
    bahprubutton = remDr$findElements(using = "css selector",
                                      value = nodename1)
    
    bahprubutton[[k]]$clickElement()
  }
  
  clickdunlist = read_html(remDr$getPageSource()[[1]]) %>%
    html_nodes(nodename3) %>% html_text()
  
  if(length(clickdunlist)!=0){
    Sys.sleep(0.1)
    dunlistbutton = remDr$findElements(using = "css selector",
                                       value = nodename3)
    
  }



}



######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################
######################################################################################################################################################