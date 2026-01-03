library(data.table)
library(tidyverse)

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

year_enc=year(Sys.Date())
week_enc=isoweek(Sys.Date())
day_enc=day(Sys.Date())

rank_week=rank_scrap(Sys.Date())

list=list_tournament(year_enc)

list$Week=isoweek(list$Date)

list=list %>% filter(Categorie %like% "ATP")

MATCH_TO_PLAY=data.frame()

for (i in 1:nrow(list)){
  
  url=list$URL[i]
  
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
  
  if(ncol(ranking_points_tournament)<=4 & ncol(ranking_points_tournament)>0 & nrow(ranking_points_tournament)<=9){
    
    
    
    names(ranking_points_tournament)=ranking_points_tournament[1,]
    ranking_points_tournament=ranking_points_tournament[-1,]
    
    ranking_points_tournament = ranking_points_tournament[,c(1,3)]
    
    
    
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
    
    list$Country_tournament[i]=info
    list$Surface_tournament[i]=info2
    list$Points_tournament[i]=points
    
  }else{
    
    points=0
    
    list$Country_tournament[i]=info
    list$Surface_tournament[i]=info2
    list$Points_tournament[i]=points
    
  }
  
  
  print(i)
  
}

for (i in 1:nrow(list)){
  
  url=list$URL[i]
  
  page_info=read_html(url)
  
  table_match=page_info %>%
    html_nodes("table.result") %>% 
    html_table()
  
  
  table_match=table_match[[1]] %>% 
    as.data.frame() %>% 
    select(2,3,5,6,7)
  
  names(table_match)=table_match[1,]
  
  table_match=table_match[-1,]
  
  colnames(table_match)=c("Round","Match","H2H","H", "A")
  
  table_match$tournament=list$tournament[i]
  
  table_match$Country_tournament=list$Country_tournament[i]
  
  table_match$Surface_tournament=list$Surface_tournament[i]
  
  match_ref=data.frame()
  
  for (i in 1:nrow(table_match)){
    
    href=page_info %>%
      html_node(paste0("table.result>tbody>tr:nth-child(",i,")>td.t-name>a")) %>% 
      #html_nodes(paste0("#tournamentTabs-1-data > table > tbody > tr:nth-child(",i,") > td.t-name > a")) %>% 
      html_attr("href") 
    
    
    href=paste0("https://www.tennisexplorer.com",href)
    
    match_ref=rbind(match_ref,href)
    
    print(href)
  }
  
  colnames(match_ref)="url"
  
  table_match$url=match_ref$url
  
  players_id=data.frame("P1"=NA,"P2"=NA,"N_match"=NA)
  
  for (i in 1:nrow(table_match)){
    
    match_id=table_match$url[i]
    
    page_match <- read_html(match_id)
    
    players=page_match %>% html_nodes("th.plName") %>% html_text() 
    
    players_id[i,1]=players[1]
    players_id[i,2]=players[2]
    players_id[i,3]=i
    
    print(i)
  }
  
  table_match$P1=players_id$P1
  table_match$P2=players_id$P2
  
  
  table_match= table_match %>% 
    left_join(rank_week %>% select(`Player name`,Points,Rank,Country),by=c("P1"="Player name")) %>% 
    left_join(rank_week %>% select(`Player name`,Points,Rank,Country),by=c("P2"="Player name")) %>% 
    rename("Points P1"="Points.x",
           "Points P2"="Points.y",
           "Rank P1"="Rank.x",
           "Rank P2"="Rank.y",
           "Country P1"="Country.x",
           "Country P2"="Country.y")
  
  
  table_match=table_match %>% select(tournament,Country_tournament,Surface_tournament,Round,P1,P2,H2H,H,A,
                                     `Points P1`,`Rank P1`,`Country P1`,
                                     `Rank P2`,`Points P2` ,`Country P2`)
  
  table_match=table_match %>% 
    mutate(Surface_tournament=str_to_title(Surface_tournament))
  
  MATCH_TO_PLAY=rbind(MATCH_TO_PLAY,table_match)
  
  print(i)
  
  
}



##### AJOUT DU ELO #####

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"))

MATCH_TO_PLAY$Elo_P1=NA
MATCH_TO_PLAY$Elo_P2=NA
MATCH_TO_PLAY$Elo_P1_surface=NA
MATCH_TO_PLAY$Elo_P2_surface=NA
MATCH_TO_PLAY$Proba_p1=NA
MATCH_TO_PLAY$Proba_p2=NA
MATCH_TO_PLAY$Proba_p1_surface=NA
MATCH_TO_PLAY$Proba_p2_surface=NA

#i=1

penalty=function(diff_date){
  
  penalty=ifelse(between(diff_date,60,80),0.7,
                 ifelse(between(diff_date,81,180),0.85,
                        ifelse(diff_date>180,1,0)))
  
  return(penalty)
}


last_elo=function(elo_player,player_name,surface="all",Date_match){
  
  if (surface=="all"){
    
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player)
    
    return(elo_player)
    
  }else if (surface=="Grass"){
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_grass) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_grass)
    
    return(elo_player)
    
  }else if (surface=="Clay"){
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_clay) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_clay)
    
    return(elo_player)
    
  }else{
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_hard) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_hard)
    
    return(elo_player)
    
  }
}

for (i in 1:nrow(MATCH_TO_PLAY)){
  
  P1=MATCH_TO_PLAY$P1[i]
  
  P2=MATCH_TO_PLAY$P2[i]
  
  surface=MATCH_TO_PLAY$Surface_tournament[i]
  
  Date_match=as.Date(Sys.time())
  
  ##### ELO CLASSIC #####
  
  last_elo_p1=last_elo(ELO_RATING_PLAYERS,P1,"all",Date_match)
  
  last_elo_p2=last_elo(ELO_RATING_PLAYERS,P2,"all",Date_match)
  
  diff_date_p1=Date_match-last_elo_p1$Date
  
  diff_date_p2=Date_match-last_elo_p2$Date
  
  elo_p1=last_elo_p1$Elo_player-(penalty(diff_date_p1)*100)
  
  elo_p2=last_elo_p2$Elo_player-(penalty(diff_date_p2)*100)
  
  proba_p1 = 1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400))
  
  proba_p2 = 1 / (1 + 10 ^ ((elo_p1 - elo_p2)/400)) 
  
  ##### ELO SURFACE #####
  
  if (surface %in% c("Indoors","Various")){
    
    surface="Hard"
  }else{
    
    surface=surface
  }
  
  elo_p1_surface=last_elo(ELO_RATING_PLAYERS,P1,surface,Sys.time()) %>% pull(4)
  
  elo_p2_surface=last_elo(ELO_RATING_PLAYERS,P2,surface,Sys.time()) %>% pull(4)
  
  proba_p1_surface = 1 / (1 + 10 ^ ((elo_p2_surface - elo_p1_surface)/400))
  
  proba_p2_surface = 1 / (1 + 10 ^ ((elo_p1_surface - elo_p2_surface)/400)) 
  
  ##### assigniation
  
  MATCH_TO_PLAY$Elo_P1[i]=elo_p1
  MATCH_TO_PLAY$Elo_P2[i]=elo_p2
  MATCH_TO_PLAY$Elo_P1_surface[i]=elo_p1_surface
  MATCH_TO_PLAY$Elo_P2_surface[i]=elo_p2_surface
  MATCH_TO_PLAY$Proba_p1[i]=proba_p1
  MATCH_TO_PLAY$Proba_p2[i]=proba_p2
  MATCH_TO_PLAY$Proba_p1_surface[i]=proba_p1_surface
  MATCH_TO_PLAY$Proba_p2_surface[i]=proba_p2_surface
  
  print(i)
}

MATCH_TO_PLAY=MATCH_TO_PLAY %>% 
  mutate(across(starts_with("Proba_"), ~ round(., 3))) %>% 
  mutate(Proba_P1_G=round(((Proba_p1+Proba_p1_surface)/2),2),
         Proba_P2_G=round(((Proba_p2+Proba_p2_surface)/2),2)) %>% 
  mutate(Odd_P1_ELO=round((100/(Proba_P1_G*100)),2),
         Odd_P2_ELO=round((100/(Proba_P2_G*100)),2))

MATCH_TO_PLAY %>% select(tournament,P1,P2,H,A,Odd_P1_ELO,Odd_P2_ELO) %>% 
  rename(Odd_P1=H,
         Odd_P2=A) %>% 
  mutate(Odd_P1=as.numeric(Odd_P1),
         Odd_P2=as.numeric(Odd_P2)) %>% 
  mutate(value_bet=(Odd_P1>Odd_P1_ELO|Odd_P2>Odd_P2_ELO)&
           ((Odd_P1/Odd_P1_ELO)>=1.05|(Odd_P2/Odd_P2_ELO)>=1.05)) %>% 
  filter(value_bet==TRUE) %>% 
  mutate(Odd_to_play=case_when(Odd_P1>Odd_P1_ELO~Odd_P1,TRUE~Odd_P2)) %>% 
  filter(between(Odd_to_play,1.35,6)) %>% 
  select(-value_bet)


