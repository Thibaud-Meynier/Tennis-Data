get_tournament=function(tournament,year) {
  # import des librairies
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(xml2)
  
  url=paste('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/',sep='')
  # extraire les donn?es ? partir de la page Web
  page <- read_html(url)
  matches <- page %>% html_nodes("table.result") %>% html_table()
  # nettoyer et organiser les donn?es extraites
  matches <- matches[[1]]
  colnames(matches)=matches[1,]
  matches=matches[-1,]
  # rennomer toutes les colonnes
  colnames(matches)=c('Date','Round','Player','Score','Set1','Set2','Set3','Set4','Set5','Odd_W','Odd_L','Info')
  matches=matches[matches$Player!='',]
  # extraire les jeux des sets tb
  matches$Set1=as.numeric(substr(matches$Set1, 1, 1))
  matches$Set2=as.numeric(substr(matches$Set2, 1, 1))
  matches$Set3=as.numeric(substr(matches$Set3, 1, 1))
  # On transforme la date
  matches$Date=dmy(paste(gsub("['^.^']", "-", substr(matches$Date,1,5)),sep="-",year))
  matches$Player=str_replace(matches$Player,'\\s+\\((.*$)',"")
  matches$test=c(1:nrow(matches))%%2
  # un data set winner et undata set looser
  winner=matches[matches$test==1,]
  colnames(winner)=c('Date','Round','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Odd_W','Odd_L','Info')
  winner=winner[,c(1:10)]
  loser=matches[matches$test==0,]
  colnames(loser)=c('Date','Round','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Odd_W','Odd_L','Info')
  loser=loser[,c(1:9,11)]
  data_set=winner[,1:2]
  
  for (i in 1:8){
    data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
    n=ncol(data_set)
    colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
    #print(i)
  }
  data_set$Score_W=as.numeric(as.character(data_set$Score_W))
  data_set$Score_L=as.numeric(as.character(data_set$Score_L))
  data_set$Odd_W=as.numeric(as.character(data_set$Odd_W))
  data_set$Odd_L=as.numeric(as.character(data_set$Odd_L))
  data_set$info=ifelse(data_set$Score_W<=1,'Walkover or Retired','Completed')
  data_set$Outcome=ifelse(data_set$Odd_W<=data_set$Odd_L,'Fav_W','Out_W')
  data_set=cbind(tournament,data_set)
  #Passage du nom du tournoi en maj
  data_set$tournament=str_to_title(as.character(data_set$tournament))
  data_set$N_match=c(1:nrow(data_set))
  data_set=data_set[order(data_set$N_match,decreasing=T),]
  #ajout du lieu du tournoi et de la surface
  location=str_extract(page %>% html_nodes("h1") %>% html_text(), "(?<=\\().*?(?=\\))")
  data_set$Location=location
  surface=str_match(page %>% 
                      html_nodes("#center > div:nth-child(2)") %>% 
                      html_text(), "\\$\\s*,\\s*(\\w+)")[,2]
  data_set$Surface=str_to_title(surface)
  
  return(data_set)
}

get_tournament_qualif=function(tournament,year) {
  # import des librairies
  library(rvest)
  library(dplyr)
  library(tidyverse)
  library(stringr)
  library(lubridate)
  library(xml2)
  
  url=paste0('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/?phase=qualification')
  # extraire les donn?es ? partir de la page Web
  page <- read_html(url)
  matches <- page %>% html_nodes("table.result") %>% html_table()
  
  if (length(matches)<5){
  data_set=data.frame()  
  }else{
  # nettoyer et organiser les donn?es extraites
  matches <- matches[[1]]
  colnames(matches)=matches[1,]
  matches=matches[-1,]
  # rennomer toutes les colonnes
  colnames(matches)=c('Date','Round','Player','Score','Set1','Set2','Set3','Set4','Set5','Odd_W','Odd_L','Info')
  matches=matches[matches$Player!='',]
  # extraire les jeux des sets tb
  matches$Set1=as.numeric(substr(matches$Set1, 1, 1))
  matches$Set2=as.numeric(substr(matches$Set2, 1, 1))
  matches$Set3=as.numeric(substr(matches$Set3, 1, 1))
  # On transforme la date
  matches$Date=dmy(paste(gsub("['^.^']", "-", substr(matches$Date,1,5)),sep="-",year))
  matches$Player=str_replace(matches$Player,'\\s+\\((.*$)',"")
  matches$test=c(1:nrow(matches))%%2
  # un data set winner et undata set looser
  winner=matches[matches$test==1,]
  colnames(winner)=c('Date','Round','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Odd_W','Odd_L','Info')
  winner=winner[,c(1:10)]
  loser=matches[matches$test==0,]
  colnames(loser)=c('Date','Round','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Odd_W','Odd_L','Info')
  loser=loser[,c(1:9,11)]
  data_set=winner[,1:2]
  
  for (i in 1:8){
    data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
    n=ncol(data_set)
    colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
    #print(i)
  }
  data_set$Score_W=as.numeric(as.character(data_set$Score_W))
  data_set$Score_L=as.numeric(as.character(data_set$Score_L))
  data_set$Odd_W=as.numeric(as.character(data_set$Odd_W))
  data_set$Odd_L=as.numeric(as.character(data_set$Odd_L))
  data_set$info=ifelse(data_set$Score_W<=1,'Walkover or Retired','Completed')
  data_set$Outcome=ifelse(data_set$Odd_W<=data_set$Odd_L,'Fav_W','Out_W')
  data_set=cbind(tournament,data_set)
  #Passage du nom du tournoi en maj
  data_set$tournament=str_to_title(as.character(data_set$tournament))
  data_set$N_match=c(1:nrow(data_set))
  data_set=data_set[order(data_set$N_match,decreasing=T),]
  #ajout du lieu du tournoi et de la surface
  location=str_extract(page %>% html_nodes("h1") %>% html_text(), "(?<=\\().*?(?=\\))")
  data_set$Location=location
  surface=str_match(page %>% 
                      html_nodes("#center > div:nth-child(2)") %>% 
                      html_text(), "\\$\\s*,\\s*(\\w+)")[,2]
  data_set$Surface=str_to_title(surface)
  }
  return(data_set)
}

# Function that scrap tennis playner full name

get_players_name=function(tournament,year){
  
  url=paste('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/',sep='')
  # extraire les donn?es ? partir de la page Web
  page <- read_html(url)
  
  links <- page %>%
    html_nodes("td:not([class])>a") 
  
  players_id=data.frame("P1"=NA,"P2"=NA,"N_match"=NA)
  
  for (i in 1:length(links)){
    
    match_id=paste0("https://tennisexplorer.com",xml_attrs(links[[i]])[["href"]])
    
    page_match <- read_html(match_id)
    
    players=page_match %>% html_nodes("th.plName") %>% html_text() 
    
    players_id[i,1]=players[1]
    players_id[i,2]=players[2]
    players_id[i,3]=i
    #print(i)
    
  }
  
  return(players_id)
}


get_players_name_qualif=function(tournament,year){
  
  url=paste('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/?phase=qualification',sep='')
  # extraire les donn?es ? partir de la page Web
  page <- read_html(url)
  
  links <- page %>%
    html_nodes("td:not([class])>a") 
  
  if (length(links)<1){
    players_id=data.frame()
  }else{
  
  players_id=data.frame("P1"=NA,"P2"=NA,"N_match"=NA)
  
  for (i in 1:length(links)){
    
    match_id=paste0("https://tennisexplorer.com",xml_attrs(links[[i]])[["href"]])
    
    page_match <- read_html(match_id)
    
    players=page_match %>% html_nodes("th.plName") %>% html_text() 
    
    players_id[i,1]=players[1]
    players_id[i,2]=players[2]
    players_id[i,3]=i
    #print(i)
    
  }
}  
  return(players_id)
}

# Function that scrap player rank for a given date (only first 1000 players)

rank_scrap=function(date){
  
  rank_scrap_end=data.frame()
  
  for (i in 1:20){
    rank=paste0("https://www.tennisexplorer.com/ranking/atp-men/?date=",date,"&page=",i)
    
    page_rank=read_html(rank)
    
    rank_scrap <- page_rank %>%
      html_nodes("table.result") %>% html_table() 
    
    rank_scrap=rank_scrap[[2]]
    
    colnames(rank_scrap)=rank_scrap[1,]
    
    rank_scrap=rank_scrap[-1,]
    
    rank_scrap=rank_scrap %>% mutate(row_number=row_number())
    
    player_id=page_rank %>% 
    html_nodes("td.t-name>a") %>% 
      html_attr("href") %>% 
      as.data.frame() %>% 
      head(50) %>% 
      rename("URL_players"=".") %>% 
      mutate(URL_players=paste0("https://www.tennisexplorer.com",URL_players),
             row_number=row_number())
    
    rank_scrap=rank_scrap %>% 
      left_join(player_id,by=c("row_number"))
    
    rank_scrap_end=rbind(rank_scrap_end,rank_scrap)
  }
  
  rank_scrap_end$Rank=substr(rank_scrap_end$Rank,1,nchar(rank_scrap_end$Rank)-1)
  
  rank_scrap_end=rank_scrap_end %>% select(-row_number)
  return(rank_scrap_end)
  
}


race_scrap=function(date){
  
  race_scrap_end=data.frame()
  
  for (i in 1:20){
    race=paste0("https://www.tennisexplorer.com/ranking/atp-men/?t=race&date=",date,"&page=",i)
    
    page_race=read_html(race)
    
    race_scrap <- page_race %>%
      html_nodes("table.result") %>% html_table() 
    
    race_scrap=race_scrap[[2]]
    
    colnames(race_scrap)=race_scrap[1,]
    
    race_scrap=race_scrap[-1,]
    
    race_scrap_end=rbind(race_scrap_end,race_scrap)
  }
  
  race_scrap_end$Rank=substr(race_scrap_end$Rank,1,nchar(race_scrap_end$Rank)-1)
  
  race_scrap_end=race_scrap_end %>% 
    rename("Race_Rank"=Rank,"Race_Points"=Points)
  
  return(race_scrap_end)
  
}

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
  
  return(list)
}

# 
# year=2023
# tournament=c('hertogenbosch','stuttgart','halle',"queen-s-club","mallorca","eastbourne")
# df_final=data.frame(matrix(nrow=0,ncol=21,NA))
# 
# year=2021
# tournament='olympics-tokyo'
# get_tournament('olympics-tokyo',2021)
# 
# for (i in tournament){
#   df=get_tournament(i,year)
#   df_final=rbind(df_final,df)
#   rm(df)
#   print(i)
# }
# 
# df_bet=data.frame("Tournament"=df_final$tournament,"Round"=df_final$Round,"Outcome"=df_final$Outcome,
#                   "Odd_play"=ifelse(df_final$Odd_W<df_final$Odd_L,df_final$Odd_L,df_final$Odd_W))
# 
# df_bet$Gain=ifelse(df_bet$Outcome=='Out_W',(df_bet$Odd_play*1-1),-1)
# 
# plot(cumsum(na.omit(df_bet$Gain)),type='l')
# 
# table(na.omit(df_bet$Outcome))
# 
# mean(df_bet$Odd_play,na.rm=T)
# 
# odd_dist=hist(df_bet$Odd_play,breaks = 30)
# 
# n=length(odd_dist$breaks)
# 
# odd=matrix(nrow=n,ncol=4,NA)
# odd[,1][2:n]=odd_dist$breaks[1:(n-1)]
# odd[,2]=odd_dist$breaks
# odd[,3][2:n]=odd_dist$counts
# odd=odd[-1,]
# 
# n2=length(hist(df_bet[df_bet$Outcome=='Out_W',]$Odd_play,breaks = 20)$counts)
# 
# odd[,4][1:n2]=hist(df_bet[df_bet$Outcome=='Out_W',]$Odd_play,breaks = 20)$counts
# 
# # Bet odd between 2 and max_odd_winner
# obs=min(which(is.na(odd[,4])))-1
# odd_max=odd[obs,2]
# 
# df_bet2=df_bet[df_bet$Odd_play>=2&df_bet$Odd_play<=odd_max,]
# 
# plot(cumsum(na.omit(df_bet2$Gain)),type='l')
# 
# table(df_bet2$Outcome)
