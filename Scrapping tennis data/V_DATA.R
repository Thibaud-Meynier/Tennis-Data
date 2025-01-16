library(tidyverse)
library(data.table)

V_RANK=data.frame()

#i=2016

for (i in 2009:2024){

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

for (i in 2012:2016){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",i,"_Extraction.RData"))
  
  table_stock$Season=i
  
  V_MATCH=rbind(V_MATCH,table_stock)
  
  print(i)  
  
}

save(V_MATCH,file = paste0(getwd(),"/Scrapping tennis data/Extraction/V_MATCH_2012_2016.RData"))

V_TOURNAMENT=data.frame()

for (i in 2002:2024){
  
  list=list_tournament(i)
  
  V_TOURNAMENT=rbind(V_TOURNAMENT,list)
  
  print(i)  
  
}

V_TOURNAMENT$Country_tournament=NA
V_TOURNAMENT$Surface_tournament=NA
V_TOURNAMENT$Points_tournament=NA

#i=15

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

   ranking_points_tournament = ranking_points_tournament %>% select(`Ranking points`,Round)
  
  if(ncol(ranking_points_tournament)<=4 & ncol(ranking_points_tournament)>0){

    ranking_points_tournament$tournament=V_TOURNAMENT$tournament[i]

    ranking_points_tournament=ranking_points_tournament %>%
      mutate(Round=case_when(Round=="1. round"~"1R",
                             Round=="2. round"~"2R",
                             Round=="3. round"~"3R",
                             Round=="round of 16"~"R16",
                             Round=="quarterfinal"~"QF",
                             Round=="semifinal"~"SF",
                             Round=="final"~"F",
                             Round=="-"~"",
                             TRUE~"Winner"))

    points=max(as.numeric(ranking_points_tournament$`Ranking points`))

    V_TOURNAMENT$Country_tournament[i]=info
    V_TOURNAMENT$Surface_tournament[i]=info2
    V_TOURNAMENT$Points_tournament[i]=points
    
  }else{
    
   points=0

    V_TOURNAMENT$Country_tournament[i]=info
    V_TOURNAMENT$Surface_tournament[i]=info2
    V_TOURNAMENT$Points_tournament[i]=points
    
  }
  

  print(i)
  
}


V_TOURNAMENT$Week_tournament=isoweek(V_TOURNAMENT$Date)

V_TOURNAMENT$Year=case_when(V_TOURNAMENT$Week_tournament>=52 & month(V_TOURNAMENT$Date)==1 ~year(V_TOURNAMENT$Date),
                            V_TOURNAMENT$Week_tournament>=52 & month(V_TOURNAMENT$Date)==12 ~ year(V_TOURNAMENT$Date)+1,
                            V_TOURNAMENT$Week_tournament==1 & month(V_TOURNAMENT$Date)==12 ~ year(V_TOURNAMENT$Date)+1,
                            TRUE ~ year(V_TOURNAMENT$Date))

V_TOURNAMENT=V_TOURNAMENT %>% 
  mutate(Categorie=paste(Categorie,ifelse(is.na(points)==T,"",points)))
  
V_TOURNAMENT= V_TOURNAMENT %>%  
  mutate(tournament = gsub("chall", "Chall", tournament, ignore.case = TRUE))


#save(V_TOURNAMENT,file=paste0(getwd(),"/Scrapping tennis data/Tournament/INFO_TOURNAMENT_2002_2024.RData"))

V_TOURNAMENT2=data.frame()

for (i in seq(2024,2024,by=1)){
  
  list=list_tournament(i)
  
  calendar_info=info_tournament(list)
  
  list=list %>% 
    left_join(calendar_info,by=c("tournament"="tournament")) %>% 
    group_by(tournament) %>% 
    mutate(Max_Pts=max(as.numeric(`Ranking points`))) %>% 
    ungroup() %>% 
    mutate(Categorie=paste(Categorie,ifelse(is.na(Max_Pts)==T,"",Max_Pts))) %>% 
    select(tournament,Date,Categorie,Round,`Ranking points`)
  
  V_TOURNAMENT2=rbind(V_TOURNAMENT2,list)
  
  print(i)
  
}

#save(V_TOURNAMENT,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT.RData"))

V_TOURNAMENT2=V_TOURNAMENT2 %>% 
  mutate(tournament2=toupper(tournament))

V_TOURNAMENT=V_TOURNAMENT %>% 
  mutate(tournament2=toupper(tournament))

V_TOURNAMENT3=V_TOURNAMENT2 %>% 
  left_join(V_TOURNAMENT %>% select(tournament2,Country_tournament,Date,Surface_tournament,Week_tournament,Year),
            by=c("tournament2","Date")) %>% 
  select(-tournament2)

V_TOURNAMENT3=V_TOURNAMENT3 %>% 
  mutate(tournament = gsub("chall", "Chall", tournament, ignore.case = TRUE)) %>% 
  mutate(tournament = gsub("Masters Cup ATP","Masters Cup Atp",tournament, ignore.case = TRUE)) %>% 
  mutate(tournament = gsub("Rio de Janeiro","Rio De Janeiro",tournament, ignore.case = TRUE)) %>% 
  mutate(tournament = gsub("ATP Cup","Atp Cup",tournament, ignore.case = TRUE)) %>% 
  mutate(tournament = gsub("US Open","Us Open",tournament, ignore.case = TRUE)) %>% 
  rename(Ranking_points=`Ranking points`)

save(V_TOURNAMENT3,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT3_2024.RData"))

##### V_PLAYERS #####

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

V_PLAYERS=V_PLAYERS %>% select(-Best_Rank)


V_PLAYERS=V_PLAYERS %>% 
  left_join(V_RANK %>% 
              group_by(Player_name) %>% 
              summarise(Best_Rank=min(as.numeric(Rank),na.rm=T)),by="Player_name")



save(V_PLAYERS,file = paste0(getwd(),"/Scrapping tennis data/Info_players/V_PLAYERS.RData"))
