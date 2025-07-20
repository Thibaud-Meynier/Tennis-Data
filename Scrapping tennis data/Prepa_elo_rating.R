library(data.table)


load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/V_MATCH_2009_2016.RData")

V_MATCH_2009_2016=V_MATCH

V_MATCH_2009_2016=V_MATCH_2009_2016 %>% 
  rename(Winner_id=P1,
         Loser_id=P2)

colnames(V_MATCH_2009_2016)

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/V_MATCH_2017_2023.RData")

V_MATCH_2017_2023=V_MATCH %>% 
  select(colnames(V_MATCH_2009_2016))

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/ATP_2024_Extraction.RData")

V_MATCH_2024=table_stock

V_MATCH_2024= V_MATCH_2024 %>% 
                rename(Winner_id=P1,
                      Loser_id=P2) %>% 
                mutate(Season=2024)

V_MATCH=rbind(V_MATCH_2009_2016,V_MATCH_2017_2023,V_MATCH_2024)



load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2009.RData")

V_TOURNAMENT_2009=V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2010.RData")

V_TOURNAMENT_2010=V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2011.RData")

V_TOURNAMENT_2011=V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2012_2016.RData")

V_TOURNAMENT_2012_2016=V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2017_2023.RData")

V_TOURNAMENT_2017_2023=V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2024.RData")

V_TOURNAMENT_2024=V_TOURNAMENT_F


V_TOURNAMENT_F=rbind(V_TOURNAMENT_2009,V_TOURNAMENT_2010,V_TOURNAMENT_2011,
                    V_TOURNAMENT_2012_2016,V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024)


V_TOURNAMENT_INFO=V_TOURNAMENT_F %>% 
  select(tournament,Categorie,Country_tournament,Week_tournament,Year) %>% 
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
                             TRUE~Categorie))


tournament=V_MATCH_t

source(paste0(getwd(),"/Scrapping tennis data/Elo rating.R"))

tournament %>% 
  select(Date,tournament,Round,Winner_id,Loser_id,Elo_W_NEW,Elo_L_NEW,Season) %>% 
  filter(tournament=="Montreal" & Winner_id=="Nadal Rafael" & Season==2009)  


Elo_player=function(player_name){
  
  Elo_player=tournament %>% 
    filter(Winner_id==player_name|Loser_id==player_name) %>% 
    mutate(Player_name=player_name) %>% 
    mutate(Elo_player=case_when(Winner_id==player_name~Elo_W_NEW,
                                 TRUE~Elo_L_NEW)) %>% 
    select(tournament,Date,Elo_player,Player_name,Season)
  
  return(Elo_player)
}


Elo_player(player_name="Murray Andy") %>% 
  filter(Season==2009) %>% 
  arrange(Date)

plot(Elo_player(player_name="Djokovic Novak") %>% 
       filter(Season==2009) %>% 
       arrange(Date) %>% pull(Date),
     
     Elo_player(player_name="Djokovic Novak") %>% 
       filter(Season==2009) %>% 
       arrange(Date) %>% pull(Elo_player),type='l')
