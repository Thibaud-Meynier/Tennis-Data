library(rvest)
library(xml2)
library(tidyverse)
library(sjmisc)

year=2024

list_tournament=function(year){
  
  # URL de la page a scraper
  url <- paste0("https://www.tennisexplorer.com/calendar/atp-men/",year,"/")
  
  # Fonction pour extraire le mois et le nom des tournois
  
  page <- read_html(url)
  
  list=data.frame("tournament"=html_text(elements <- page %>%
                                           html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody th.t-name")))
  
  # On extrait les noms des tournois de la page
  elements <- page %>%
    html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody tr th a")
  
  x=xml_attrs(elements)
  
  list[,2]=as.data.frame(matrix(nrow=length(elements),NA))
  n=length(elements)
  
  for(i in 1:n){
    
    y=as.data.frame(x[[i]])
    list[i,2]=as.character(y[,1])
    #print(i)
    
  }
  
  list=list %>% 
    mutate("Valid"=grepl("UTR Pro Tennis Series",list$tournament)) %>% 
    filter(Valid==F) %>% 
    select(1,2)
  
  list=list %>% filter(!tournament %in% c("Netflix Slam","Kooyong - exh.")) %>% 
    rename("URL"=V1)
  
  table=page %>% html_nodes("table#tournamentList") %>% html_table()
  table=table[[1]] %>% select(1,2)
  colnames(table)=table[1,]
  table=table[-1,]
  
  list=list %>% 
    left_join(table,by=c("tournament"="Tournament"))
  
  list$Started=dmy(gsub("['^.^']", "-", substr(list$Started,1,10)))
  
  list=list %>% rename("Date"=Started)
  
  list=unique(list) %>% arrange(Date)
  
  list$Categorie=NA
  
  for (i in 1:nrow(list)){
    
    list$Categorie[i]=ifelse(str_contains(list$tournament[i],c("chall.","challenger"),logic="or")==TRUE,
                          "Challenger","ATP")
  }
  
  return(list)
}



# 
# elements_prize <- page %>%
#   html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody td.tr")
# 
# list$prize=gsub("???","",
#           gsub(" ","",
#                gsub(",","",
#                     gsub("[$]","",html_text(elements_prize)))))
# 
# 
# list
# 
# colnames(list)[2]="ref_tournament"
# list$prize=as.numeric(list$prize)
# 
# list=na.omit(list[list$prize>=250000,])
# row=as.numeric(row.names(list))
# 
# elements_surface <- page %>%
#   html_nodes("div.box.lGray div.inner div.content table#tournamentList tbody th.t-name+td.s-color")
# 
# list$surface=NA
# 
# for (i in row){
# 
#   l=length(elements_surface[[i]]%>% html_nodes("span"))
#   list[rownames(list)==i,]$surface=ifelse(l>0,as.character(xml_attrs(elements_surface[[i]]%>% html_nodes("span"))[[1]][1]),NA)
#   print(i)
# 
# }
# 
# list$year=year

