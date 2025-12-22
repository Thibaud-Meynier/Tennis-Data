##### PREPA #####

library(data.table)
library(tidyverse)
library(here)

load(paste0(here(),"/Tennis-Data/Scrapping tennis data/Extraction/V_MATCH_2003_2008.RData"))

V_MATCH_2003_2008=V_MATCH

V_MATCH_2003_2008=V_MATCH_2003_2008 %>% 
  rename(Winner_id=P1,
         Loser_id=P2)

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Extraction/V_MATCH_2009_2016.RData"))

V_MATCH_2009_2016=V_MATCH

V_MATCH_2009_2016=V_MATCH_2009_2016 %>% 
  rename(Winner_id=P1,
         Loser_id=P2)

colnames(V_MATCH_2009_2016)

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Extraction/V_MATCH_2017_2023.RData"))

V_MATCH_2017_2023=V_MATCH %>% 
  select(colnames(V_MATCH_2009_2016))

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Extraction/ATP_2024_Extraction.RData"))

V_MATCH_2024=table_stock

V_MATCH_2024= V_MATCH_2024 %>% 
  rename(Winner_id=P1,
         Loser_id=P2) %>% 
  mutate(Season=2024)

V_MATCH=rbind(V_MATCH_2003_2008,V_MATCH_2009_2016,V_MATCH_2017_2023,V_MATCH_2024)

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT4_2002_2008.RData"))

V_TOURNAMENT_2003_2008=V_TOURNAMENT_4

V_TOURNAMENT_2003_2008=V_TOURNAMENT_2003_2008 %>% 
  select(-Categorie) %>% 
  rename(Categorie=Categorie_new)

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2009.RData"))

V_TOURNAMENT_2009=V_TOURNAMENT_F

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2010.RData"))

V_TOURNAMENT_2010=V_TOURNAMENT_F

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2011.RData"))

V_TOURNAMENT_2011=V_TOURNAMENT_F

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2012_2016.RData"))

V_TOURNAMENT_2012_2016=V_TOURNAMENT_F

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2017_2023.RData"))

V_TOURNAMENT_2017_2023=V_TOURNAMENT_F

load(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2024.RData"))

V_TOURNAMENT_2024=V_TOURNAMENT_F

V_TOURNAMENT_F=rbind(V_TOURNAMENT_2003_2008,V_TOURNAMENT_2009,V_TOURNAMENT_2010,V_TOURNAMENT_2011,
                     V_TOURNAMENT_2012_2016,V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024)


V_TOURNAMENT_INFO=V_TOURNAMENT_F %>% 
  select(tournament,Categorie,Country_tournament,Week_tournament,Year,Surface_tournament) %>% 
  unique() %>% 
  mutate(Categorie=case_when(Categorie=="ATP 2000"~"Grand Slam",
                             TRUE~Categorie)) %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament))

V_MATCH_t=V_MATCH %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_INFO %>% select(-tournament),by=c("CLE_TOURNAMENT","Season"="Year")) %>% 
  select(-CLE_TOURNAMENT) %>% 
  filter(!tournament %in% c("Riyadh - Exhibition","Next Gen Atp Finals")) %>% 
  mutate(Categorie=case_when(tournament %in% c("United Cup","Atp Cup","Davis Cup")~"Team",
                             tournament=="Masters Cup Atp"~"Masters",
                             tournament %like% "Olympics"~"Olympics",
                             tournament=="Reunion Challenger" & Season==2011~"Challenger 80",
                             TRUE~Categorie)) %>% 
  mutate(Surface_tournament=case_when(is.na(Surface_tournament)~Surface,
                                      Categorie=="Masters" & Season %in% c(2005,2006,2007,2008)~"Indoors",
                                      tournament=="Madrid" & Season %in% c(2003:2008)~"Indoors",
                                      tournament=="Bangkok" & Season %in% c(2003:2004)~"Indoors",
                                      TRUE~Surface_tournament)) %>% 
  mutate(Surface_tournament=case_when(Surface_tournament=="clay"~"Clay",
                                      Surface_tournament=="hard"~"Hard",
                                      Surface_tournament=="grass"~"Grass",
                                      Surface_tournament=="indoors"~"Indoors",
                                      TRUE~Surface_tournament))


surface=c("Grass")

source(paste0(getwd(),"/Tennis-Data/Scrapping tennis data/Elo rating surface.R"))


##### VERIF #####

Elo_player=function(player_name){
  
  Elo_player=tournament %>% 
    filter(Winner_id==player_name|Loser_id==player_name) %>% 
    mutate(Player_name=player_name) %>% 
    mutate(Elo_player=case_when(Winner_id==player_name~Elo_W_NEW,
                                TRUE~Elo_L_NEW)) %>% 
    select(tournament,Date,Elo_player,Player_name,Season)
  
  return(Elo_player)
}

elo_novak=Elo_player(player_name="Djokovic Novak") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_rafa=Elo_player(player_name="Nadal Rafael") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_roger=Elo_player(player_name="Federer Roger") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

elo_andy=Elo_player(player_name="Murray Andy") %>% 
  filter(Season<=2024) %>% 
  arrange(Date)

plot(elo_roger$Date,elo_roger$Elo_player,type='l',col='green',ylim=c(1300,2600),xlim = c(as.Date('2003-01-01'),as.Date('2024-12-31')))
lines(elo_rafa$Date,elo_rafa$Elo_player,type='l',col="brown")
lines(elo_novak$Date,elo_novak$Elo_player,type='l',col="steelblue")
lines(elo_andy$Date,elo_andy$Elo_player,type='l',col="orange")


elo_roger %>% slice(which.max(elo_roger$Elo_player)) %>% pull(Elo_player,Date)

elo_rafa %>% slice(which.max(elo_rafa$Elo_player)) %>% pull(Elo_player,Date)

elo_novak %>% slice(which.max(elo_novak$Elo_player)) %>% pull(Elo_player,Date)

elo_andy %>% slice(which.max(elo_andy$Elo_player)) %>% pull(Elo_player,Date)

### SAVE ELO RATING ###

ELO_RATING_GRASS=tournament %>% select(colnames(ELO_RATING))

save(ELO_RATING_GRASS,file=paste0(here(),"/Tennis-Data/Scrapping tennis data/Rank/ELO_RATING_GRASS.RData"))
