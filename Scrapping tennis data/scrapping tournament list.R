library(rvest)
library(xml2)
library(tidyverse)
library(sjmisc)

#year=2020

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
  
  list=list %>% filter(!tournament %in% exclusion) %>% 
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
  
  list$URL=paste0("https://www.tennisexplorer.com",list$URL)
  
  return(list)
}


#tournament_list=list_tournament(year)

info_tournament=function(tournament_list){
  
calendar_info=data.frame()

for (i in tournament_list$tournament){
  
  url=tournament_list %>% filter(tournament==i) %>% select(URL) %>% as.character()
  
  page=read_html(url)
  
  tournament_info=page %>% 
    html_nodes("#center > div:nth-child(7) > div > div > table") %>% 
    html_table()
  
if (length(tournament_info)>0){
    
    tournament_info=tournament_info[[1]]
  
  if (nrow(tournament_info)>9|nrow(tournament_info)<3){
    tournament_info=data.frame("Round"=NA,"Prize money"=NA,"Ranking points"=NA)
    tournament_info$tournament=i
    tournament_info=tournament_info %>% rename("Prize money"=Prize.money,
                                               "Ranking points"=Ranking.points)
  }else{
    
    colnames(tournament_info)=tournament_info[1,]
    
    tournament_info=tournament_info[-1,]
    
    tournament_info$tournament=i
    
    tournament_info=tournament_info %>% 
      mutate(Round=case_when(Round=="1. round"~"1R",
                             Round=="2. round"~"2R",
                             Round=="3. round"~"3R",
                             Round=="round of 16"~"R16",
                             Round=="quarterfinal"~"QF",
                             Round=="semifinal"~"SF",
                             Round=="final"~"F",
                             TRUE~"Winner"))
    
  }
    calendar_info=rbind(calendar_info,tournament_info)
}else{
  tournament_info=data.frame("Round"=NA,"Prize money"=NA,"Ranking points"=NA)
  tournament_info$tournament=i
  tournament_info=tournament_info %>% rename("Prize money"=Prize.money,
                                             "Ranking points"=Ranking.points)
  
  calendar_info=rbind(calendar_info,tournament_info)
}

}

return(calendar_info)

}

#calendar_info=info_tournament(tournament_list)

# list=list %>% 
#   left_join(calendar_info %>% 
#                           group_by(tournament) %>% 
#                           mutate(Points=max(as.numeric(`Ranking points`))) %>% 
#                           select(tournament,Points) %>% 
#               unique(),by=c("tournament"="tournament"))

