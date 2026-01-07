source(paste0(getwd(),"/Scrapping tennis data/Stat function.R"))

load(paste0(getwd(),"/Scrapping tennis data/Info_players/V_PLAYERS_RED.RData"))

V_PLAYERS=V_PLAYERS %>% 
  group_by(Player_name,Birth_date,Hand,Size,Weight) %>% 
  mutate(CLE_JOUEUR=row_number()) %>% 
  filter(CLE_JOUEUR==1) %>% 
  select(-CLE_JOUEUR)

load(paste0(getwd(),"/Scrapping tennis data/Extraction/V_MATCH_2009_2016.RData"))

V_MATCH_2009_2016=V_MATCH

V_MATCH_2009_2016=V_MATCH_2009_2016 %>% 
  rename(Winner_id=P1,
         Loser_id=P2)

colnames=colnames(V_MATCH_2009_2016)

rm(V_MATCH)
rm(V_MATCH_2009_2016)

V_MATCH=data.frame()

#i=2016

for (i in 2020:2025){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",i,"_Extraction.RData"))
  
  table_stock$Season=i
  
  if ("P1" %in% names(table_stock)==T){
    
    table_stock=table_stock %>% 
      rename(Winner_id=P1,
             Loser_id=P2)
  }else{
    
    table_stock=table_stock=table_stock %>% select(colnames)
  }
  
  V_MATCH=rbind(V_MATCH,table_stock)
  
  print(i)  
  
}

rm(table_stock)


load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2017_2023.RData"))

V_TOURNAMENT_2017_2023=V_TOURNAMENT_F %>% filter(Year>=2020)

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2024.RData"))

V_TOURNAMENT_2024=V_TOURNAMENT_F

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2025.RData"))

V_TOURNAMENT_2025=V_TOURNAMENT_F

V_TOURNAMENT_F=rbind(V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024,V_TOURNAMENT_2025)

rm(V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024,V_TOURNAMENT_2025)


V_TOURNAMENT_INFO=V_TOURNAMENT_F %>% 
  select(tournament,Categorie,Country_tournament,Week_tournament,Year,Surface_tournament) %>% 
  unique() %>% 
  mutate(Categorie=case_when(Categorie=="ATP 2000"~"Grand Slam",
                             TRUE~Categorie)) %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament))

rm(V_TOURNAMENT_F)


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
                                      TRUE~Surface_tournament)) %>% 
  mutate(Week_tournament=isoweek(Date),
         Country_tournament=case_when(tournament=="Olympics - Paris"~"France",
                                      tournament=="Olympics - Tokyo"~"Japan",
                                      tournament=="Masters Cup Atp"~"Italy",
                                      TRUE~Country_tournament)) 

V_MATCH_t=V_MATCH_t %>% 
  group_by(tournament,Season,Phase,Round,Date,Week_tournament,Winner_id,Loser_id) %>% 
  mutate(CLE_LIGNE=row_number()) %>% 
  filter(CLE_LIGNE==1) %>% 
  select(-CLE_LIGNE)

#V_MATCH_t %>% group_by(Categorie) %>% count()

# "Challenger 125","Challenger 175",
# "Challenger 110","Challenger 100",
# "Challenger 90","Challenger 80",
# "Challenger 75","Challenger 50"

V_MATCH_HIST=V_MATCH_t

V_MATCH_t=V_MATCH_t %>% 
  filter(Phase=="Main Draw" & 
           (Categorie %in% c("Grand Slam","Olympics","Masters",
                             "ATP 1000","ATP 500",'ATP 250')|tournament %in% c("Atp Cup","United Cup")) & Season>=2021)

V_MATCH_t=V_MATCH_t %>% ungroup() %>% 
  select(tournament,Season,Date,Week_tournament,Categorie,Surface_tournament,Round,Winner_id,Loser_id,Odd_W,Odd_L,info,Winner,Loser)


V_RANK=data.frame()

for (i in 2020:2025){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",i,".RData"))
  
  V_RANK=rbind(V_RANK,rank)
  
  print(i)  
  
}

V_RANK=V_RANK %>% 
  select(-Move) %>% 
  rename("Player_name"="Player name") 


rm(rank)
rm(V_TOURNAMENT_INFO)
rm(V_MATCH)

V_MATCH_HIST=V_MATCH_HIST %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
              rename(Rank_W=Rank,Points_W=Points) %>% 
              mutate(Rank_W=as.numeric(Rank_W),
                     Points_W=as.numeric(Points_W)),
            by=c("Winner_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
              rename(Rank_L=Rank,Points_L=Points) %>% 
              mutate(Rank_L=as.numeric(Rank_L),
                     Points_L=as.numeric(Points_L)),
            by=c("Loser_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
  mutate(Rank_W=case_when(is.na(Rank_W)~1000,TRUE~Rank_W),
         Points_W=case_when(is.na(Points_W)~10,TRUE~Points_W),
         Rank_L=case_when(is.na(Rank_L)~1000,TRUE~Rank_L),
         Points_L=case_when(is.na(Points_W)~10,TRUE~Points_L))

V_MATCH_t=V_MATCH_t %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
              rename(Rank_W=Rank,Points_W=Points) %>% 
              mutate(Rank_W=as.numeric(Rank_W),
                     Points_W=as.numeric(Points_W)),
            by=c("Winner_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
              rename(Rank_L=Rank,Points_L=Points) %>% 
              mutate(Rank_L=as.numeric(Rank_L),
                     Points_L=as.numeric(Points_L)),
            by=c("Loser_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
  mutate(Rank_W=case_when(is.na(Rank_W)~1000,TRUE~Rank_W),
         Points_W=case_when(is.na(Points_W)~10,TRUE~Points_W),
         Rank_L=case_when(is.na(Rank_L)~1000,TRUE~Rank_L),
         Points_L=case_when(is.na(Points_W)~10,TRUE~Points_L))

rm(V_RANK)

## Ajout info joueurs 

V_MATCH_t=V_MATCH_t %>% 
  left_join(V_PLAYERS %>% select(-Best_Rank) %>% 
              rename(Country_W=Country,
                     URL_W=URL_players,
                     Size_W=Size,
                     Weight_W=Weight,
                     Birth_date_W=Birth_date,
                     Hand_W=Hand),
            by=c("Winner_id"="Player_name")) %>% 
  left_join(V_PLAYERS %>% select(-Best_Rank) %>% 
              rename(Country_L=Country,
                     URL_L=URL_players,
                     Size_L=Size,
                     Weight_L=Weight,
                     Birth_date_L=Birth_date,
                     Hand_L=Hand),
            by=c("Loser_id"="Player_name"))

V_MATCH_HIST=as.data.table(V_MATCH_HIST)

##### Stats players #####

# Global

V_MATCH_t$H2H_Winner_W=NA
V_MATCH_t$H2H_Loser_W=NA

V_MATCH_t$H2H_Winner_Set_Won=NA
V_MATCH_t$H2H_Loser_Set_Won=NA

V_MATCH_t$H2H_Winner_Games_Won=NA
V_MATCH_t$H2H_Loser_Games_Won=NA

# Surface

V_MATCH_t$H2H_s_Winner_W=NA
V_MATCH_t$H2H_s_Loser_W=NA

V_MATCH_t$H2H_s_Winner_Set_Won=NA
V_MATCH_t$H2H_s_Loser_Set_Won=NA

V_MATCH_t$H2H_s_Winner_Games_Won=NA
V_MATCH_t$H2H_s_Loser_Games_Won=NA

# Stats joueur encours global 4 dernières semaines

# 4 12 24 48

V_MATCH_t$Winner_N_Win_4=NA
V_MATCH_t$Winner_N_Loss_4=NA

V_MATCH_t$Loser_N_Win_4=NA
V_MATCH_t$Loser_N_Loss_4=NA

# Surface

V_MATCH_t$Winner_N_Win_s_4=NA
V_MATCH_t$Winner_N_Loss_s_4=NA

V_MATCH_t$Loser_N_Win_s_4=NA
V_MATCH_t$Loser_N_Loss_s_4=NA

pb= progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = nrow(V_MATCH_t),
  clear = FALSE,
  width = 60
)

i=13454

for (i in 5169:nrow(V_MATCH_t)){
  
  ### INFO function ####
  
  tournoi=V_MATCH_t$tournament[i]
  Season=V_MATCH_t$Season[i]
  Date_match=V_MATCH_t$Date[i]
  surface=V_MATCH_t$Surface_tournament[i]
  winner_url=V_MATCH_t$URL_W[i]
  loser_url=V_MATCH_t$URL_L[i]
  W=V_MATCH_t$Winner[i]
  L=V_MATCH_t$Loser[i]
  R=V_MATCH_t$Round[i]
  winner_id=V_MATCH_t$Winner_id[i]
  loser_id=V_MATCH_t$Loser_id[i]
  
  ### CALCUL Stats ###
  
  # h2h
  result=get_h2h(winner_url,loser_url)
  
  stat_g=get_stat_h2h(result,"all",Season,tournoi,W,L,R)
  
  stat_s=get_stat_h2h(result,surface,Season,tournoi,W,L,R)
  
  # global
  
  match_count_w_g_4=match_count(V_MATCH_HIST,winner_id,4,"all",Date_match)
  
  match_count_l_g_4=match_count(V_MATCH_HIST,loser_id,4,"all",Date_match)
  
  match_count_w_s_4=match_count(V_MATCH_HIST,winner_id,4,surface,Date_match)
  
  match_count_l_s_4=match_count(V_MATCH_HIST,loser_id,4,surface,Date_match)
  
  ### Assigniation des valeurs ###
  
  # Global
  
  V_MATCH_t$H2H_Winner_W[i]=stat_g$W$Number_Win
  V_MATCH_t$H2H_Loser_W[i]=stat_g$L$Number_Win
  
  V_MATCH_t$H2H_Winner_Set_Won[i]=stat_g$W$Number_Set_Won
  V_MATCH_t$H2H_Loser_Set_Won[i]=stat_g$L$Number_Set_Won
  
  V_MATCH_t$H2H_Winner_Games_Won[i]=stat_g$W$Number_Games_Won
  V_MATCH_t$H2H_Loser_Games_Won[i]=stat_g$L$Number_Games_Won

  # Surface
  
  V_MATCH_t$H2H_s_Winner_W[i]=stat_s$W$Number_Win
  V_MATCH_t$H2H_s_Loser_W[i]=stat_s$L$Number_Win
  
  V_MATCH_t$H2H_s_Winner_Set_Won[i]=stat_s$W$Number_Set_Won
  V_MATCH_t$H2H_s_Loser_Set_Won[i]=stat_s$L$Number_Set_Won
  
  V_MATCH_t$H2H_s_Winner_Games_Won[i]=stat_s$W$Number_Games_Won
  V_MATCH_t$H2H_s_Loser_Games_Won[i]=stat_s$L$Number_Games_Won
  
  # Stats joueur encours global 4 dernières semaines
  
  V_MATCH_t$Winner_N_Win_4[i]=match_count_w_g_4$N_Win
  V_MATCH_t$Winner_N_Loss_4[i]=match_count_w_g_4$N_Loss
    
  V_MATCH_t$Loser_N_Win_4[i]=match_count_l_g_4$N_Win
  V_MATCH_t$Loser_N_Loss_4[i]=match_count_l_g_4$N_Loss
  
  # Surface
  
  V_MATCH_t$Winner_N_Win_s_4[i]=match_count_w_s_4$N_Win
  V_MATCH_t$Winner_N_Loss_s_4[i]=match_count_w_s_4$N_Loss
    
  V_MATCH_t$Loser_N_Win_s_4[i]=match_count_l_s_4$N_Win
  V_MATCH_t$Loser_N_Loss_s_4[i]=match_count_l_s_4$N_Loss
  
  print(i)
  
}

