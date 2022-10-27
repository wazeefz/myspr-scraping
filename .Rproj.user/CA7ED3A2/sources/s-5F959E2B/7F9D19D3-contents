
library(rvest)
library(RSelenium)
library(tidyverse)
library(data.table)
library(stringr)



################################################################################################################################################

#Removing duplicates in the dataframe

mysprclean = mysprcomplete[!duplicated(mysprcomplete),]

################################################################################################################################################

mysprclean = mysprclean %>% mutate(numberofparty = (str_count(mysprclean$BilUndi,"/") + 1))

count(grepl("/", mysprclean$BilUndi[1]))

str_count(mysprclean$BilUndi[1],"/") + 1

################################################################################################################################################

mysprclean = mysprclean %>% mutate(Kod = str_split_fixed(mysprclean$KodDanNama, ' ',2)[,1])

mysprclean = mysprclean %>% mutate(`Nama Kawasan` = str_split_fixed(mysprclean$KodDanNama, ' ',2)[,2])

################################################################################################################################################

mysprclean = mysprclean %>% mutate(BilUndi1 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,1])
mysprclean = mysprclean %>% mutate(BilUndi2 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,2])
mysprclean = mysprclean %>% mutate(BilUndi3 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,3])
mysprclean = mysprclean %>% mutate(BilUndi4 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,4])
mysprclean = mysprclean %>% mutate(BilUndi5 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,5])
mysprclean = mysprclean %>% mutate(BilUndi6 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,6])
mysprclean = mysprclean %>% mutate(BilUndi7 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,7])
mysprclean = mysprclean %>% mutate(BilUndi8 = str_split_fixed(mysprclean$BilUndi, ' ',8)[,8])

################################################################################################################################################

mysprclean = mysprclean %>% mutate(BilUndi1 = str_replace(BilUndi1,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi2 = str_replace(BilUndi2,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi3 = str_replace(BilUndi3,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi4 = str_replace(BilUndi4,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi5 = str_replace(BilUndi5,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi6 = str_replace(BilUndi6,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi7 = str_replace(BilUndi7,'/',''))
mysprclean = mysprclean %>% mutate(BilUndi8 = str_replace(BilUndi8,'/',''))

################################################################################################################################################

mysprclean = mysprclean %>% mutate(NamaDanParti1 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,1])
mysprclean = mysprclean %>% mutate(NamaDanParti2 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,2])
mysprclean = mysprclean %>% mutate(NamaDanParti3 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,3])
mysprclean = mysprclean %>% mutate(NamaDanParti4 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,4])
mysprclean = mysprclean %>% mutate(NamaDanParti5 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,5])
mysprclean = mysprclean %>% mutate(NamaDanParti6 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,6])
mysprclean = mysprclean %>% mutate(NamaDanParti7 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,7])
mysprclean = mysprclean %>% mutate(NamaDanParti8 = str_split_fixed(mysprclean$NamaDanParti, '/',8)[,8])

################################################################################################################################################

partibertandinglist

contoh = mysprclean$NamaDanParti1
# 
# grepl(for(i in 1: length(partibertandinglist){
#   partibertandinglist[i]
# }),

contoh[1]

# mysprclean = mysprclean %>% mutate(Parti1 = ifelse(NamaDanParti1 %in% partibertandinglist, partibertandinglist, NA))


parti1 = str_match(contoh,"(\\s*(.*?)\\s*)")

id_loc_s = str_locate(link, "jobId=jobstreet-[:alpha:]{2}-job-")[,2]

id_loc_s = str_locate(contoh[1],"(")

id_loc_s = str_locate(contoh[1],"(")

cariparti = function(x){

  parti_s = str_locate(x,"\\(")[,2]
  
  parti_e = str_locate(x, "\\)")[,1]
  
  parti = str_sub(x, start = parti_s, end = parti_e) 

}


cariparti(contoh[1])


################################################################################################################################################

mysprclean = mysprclean %>% mutate(Parti1 = cariparti(NamaDanParti1))
mysprclean = mysprclean %>% mutate(Parti2 = cariparti(NamaDanParti2))
mysprclean = mysprclean %>% mutate(Parti3 = cariparti(NamaDanParti3))
mysprclean = mysprclean %>% mutate(Parti4 = cariparti(NamaDanParti4))
mysprclean = mysprclean %>% mutate(Parti5 = cariparti(NamaDanParti5))
mysprclean = mysprclean %>% mutate(Parti6 = cariparti(NamaDanParti6))
mysprclean = mysprclean %>% mutate(Parti7 = cariparti(NamaDanParti7))
mysprclean = mysprclean %>% mutate(Parti8 = cariparti(NamaDanParti8))

################################################################################################################################################

id_loc_s = str_locate(link, "jobId=jobstreet-[:alpha:]{2}-job-")[,2]

x = "PRU KE-14"

gsub("PRU KE-","",x)

mysprclean = mysprclean %>% mutate(PRU = gsub("PRU KE-","",PRU))



################################################################################################################################################

caritahunpru = function(x){
  
  tahun_s = str_locate(x,"\\(")[,2]
  
  tahun_e = str_locate(x, "\\)")[,1]
  
  tahun = str_sub(x, start = tahun_s+1, end = tahun_e-1) 
  
}

mysprclean = mysprclean %>% mutate(`Tahun Undi` = caritahunpru(PRU))

################################################################################################################################################

mysprclean = mysprclean %>% mutate(Tarikh = gsub("Tarikh Pengundian :","",Tarikh))

gsub("\\([:digit:]{4}\\)","",x)



################################################################################################################################################

date = gsub("-", "_", Sys.Date())

data.table::fwrite(mysprclean, paste0("//10.13.10.22/dataSandbox/scraping/misc/myspr/mysprclean", date, ".csv"))












