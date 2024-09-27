

V_RANK=data.frame()

#i=2016

for (i in 2016:2023){

load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",i,".RData"))
  
V_RANK=rbind(V_RANK,rank)
     
print(i)  
     
}

V_RANK=V_RANK %>% 
  #select(-Move) %>% 
  rename("Player_name"="Player name")

save(V_RANK,file = paste0(getwd(),"/Scrapping tennis data/Rank/V_RANK.RData"))


V_MATCH=data.frame()

#i=2016

for (i in 2017:2023){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",i,"_Extraction.RData"))
  
  V_MATCH=rbind(V_MATCH,table_stock)
  
  print(i)  
  
}

save(V_MATCH,file = paste0(getwd(),"/Scrapping tennis data/Extraction/V_MATCH.RData"))

V_TOURNAMENT=data.frame()

for (i in 2017:2023){
  
  list=list_tournament(i)
  
  V_TOURNAMENT=rbind(V_TOURNAMENT,list)
  
  print(i)  
  
}

V_TOURNAMENT$Country_tournament=NA
V_TOURNAMENT$Surface_tournament=NA
V_TOURNAMENT$Points_tournament=NA

for (i in 1:nrow(V_TOURNAMENT)){
  
  url=V_TOURNAMENT$URL[i]
  
  page_info=read_html(url)
  
  country_tournament=page_info %>% 
    html_nodes("#center > h1") %>% 
    html_text()
  
  info <- sub(".*\\(([^)]+)\\).*", "\\1", country_tournament)
  
  surface_tournament=page_info %>% 
    html_nodes("#center > div:nth-child(2)") %>% 
    html_text()
  
  info2 <- extrait <- sub(".*\\, (\\w+)\\,.*", "\\1", surface_tournament)
  
  ranking_points_tournament=
    page_info %>% 
    html_nodes("#center > div:nth-child(7) > div > div > table") %>% 
    html_table() %>% 
    as.data.frame()
  
  names(ranking_points_tournament)=ranking_points_tournament[1,]
  ranking_points_tournament=ranking_points_tournament[-1,]
  
  points=max(as.numeric(ranking_points_tournament$`Ranking points`))
  
  V_TOURNAMENT$Country_tournament[i]=info
  V_TOURNAMENT$Surface_tournament[i]=info2
  V_TOURNAMENT$Points_tournament[i]=points
  
  print(url)
  
}


V_TOURNAMENT$Week_tournament=isoweek(V_TOURNAMENT$Date)

V_TOURNAMENT$Year=ifelse(V_TOURNAMENT$Week_tournament>=52 & month(V_TOURNAMENT$Date)==1,year(V_TOURNAMENT$Date)-1,
                         ifelse(V_TOURNAMENT$Week_tournament>=52 & month(V_TOURNAMENT$Date)==12,year(V_TOURNAMENT$Date),
                                year(V_TOURNAMENT$Date)))

V_TOURNAMENT= V_TOURNAMENT %>%  
  mutate(tournament = gsub("chall", "Chall", tournament, ignore.case = TRUE))

save(V_TOURNAMENT,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT.RData"))

V_PLAYERS=V_RANK %>% select(Player_name,Country,URL_players) %>% distinct()

V_PLAYERS$Size=NA
V_PLAYERS$Weight=NA
V_PLAYERS$Birth_date=NA
V_PLAYERS$Hand=NA


for (i in 1:nrow(V_PLAYERS)){
  
  url=V_PLAYERS$URL_players[i]
  
  page=read_html(url)

  data=page %>% html_nodes("#center > div.box.boxBasic.lGray > table > tbody > tr ") %>% 
  html_text()

  data_players=infos_players_scrap(data)


# On stock les infos
  V_PLAYERS$Size[i]=data_players$size
  V_PLAYERS$Weight[i]=data_players$weight
  V_PLAYERS$Birth_date[i]=as.character(data_players$date_naissance)
  V_PLAYERS$Hand[i]=data_players$plays
  
  print(i)

}

save(V_PLAYERS,file = paste0(getwd(),"/Scrapping tennis data/Info_players/V_PLAYERS.RData"))
