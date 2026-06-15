year=2026

source(paste0(getwd(),"/Scrapping tennis data/Stat function.R"))
load("~/work/Tennis-Data/Scrapping tennis data/ML_engenering/V_MATCH_HIST.RData")
load("~/work/Tennis-Data/Scrapping tennis data/ML_engenering/V_MATCH_t.RData")
load("~/work/Tennis-Data/Scrapping tennis data/ML_engenering/V_RANK.RData")
load(paste0("~/work/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_",year,".RData"))
load(paste0("~/work/Tennis-Data/Scrapping tennis data/Extraction/ATP_",year,"_Extraction.RData"))
load(paste0("~/work/Tennis-Data/Scrapping tennis data/Rank/RANK_ATP_",year,".RData"))
load("~/work/Tennis-Data/Scrapping tennis data/Info_players/V_PLAYERS_RED.RData")

V_PLAYERS=V_PLAYERS %>% 
  group_by(Player_name,Birth_date,Hand,Size,Weight) %>% 
  mutate(CLE_JOUEUR=row_number()) %>% 
  filter(CLE_JOUEUR==1) %>% 
  select(-CLE_JOUEUR)

V_TOURNAMENT_INFO=V_TOURNAMENT_F %>% 
  select(tournament,Categorie,Country_tournament,Week_tournament,Year,Surface_tournament) %>% 
  unique() %>% 
  mutate(Categorie=case_when(Categorie=="ATP 2000"~"Grand Slam",
                             TRUE~Categorie)) %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament))

rank = rank %>% 
  select(-Move) %>% 
  rename("Player_name"="Player name") %>% 
  mutate(Week_Rank = sapply(Date, get_tennis_week))

V_RANK = V_RANK %>% filter(Year<year) %>% bind_rows(rank)

save(V_RANK,file=paste0(here(),"/Scrapping tennis data/ML_engenering/V_RANK.RData"),
     compress="xz")

date_max = max(V_MATCH_HIST$Date)

#date_max = as.Date("2026-04-19")
  
table_stock_new = table_stock %>% 
  filter(Date>date_max) %>% 
  rename(Winner_id=P1,
         Loser_id=P2) %>% 
  mutate(Season=year)

table_stock_new = table_stock_new %>% 
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
                                      tournament=="Masters Cup Atp" & Season>=2021~"Italy",
                                      tournament=="Masters Cup Atp" & Season %in% c(2009:2020)~"Great Britain",
                                      TRUE~Country_tournament)) 

table_stock_new = table_stock_new %>% 
  mutate(Match_week=sapply(Date, get_tennis_week)) %>% 
  left_join(rank %>% select(Rank,Player_name,Points,Week_Rank) %>% 
              rename(Rank_W=Rank,Points_W=Points) %>% 
              mutate(Rank_W=as.numeric(Rank_W),
                     Points_W=as.numeric(Points_W)),
            by=c("Winner_id"="Player_name","Match_week"="Week_Rank")) %>% 
  left_join(rank %>% select(Rank,Player_name,Points,Week_Rank) %>% 
              rename(Rank_L=Rank,Points_L=Points) %>% 
              mutate(Rank_L=as.numeric(Rank_L),
                     Points_L=as.numeric(Points_L)),
            by=c("Loser_id"="Player_name","Match_week"="Week_Rank")) %>% 
  mutate(Rank_W=case_when(is.na(Rank_W)~1000,TRUE~Rank_W),
         Points_W=case_when(is.na(Points_W)~10,TRUE~Points_W),
         Rank_L=case_when(is.na(Rank_L)~1000,TRUE~Rank_L),
         Points_L=case_when(is.na(Points_L)~10,TRUE~Points_L))


table_stock_new = table_stock_new %>% 
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

table_stock_new = table_stock_new %>% 
  group_by(tournament,Country_tournament,Season,Round,Date,Week_tournament,Winner_id,Loser_id) %>% 
  mutate(CLE_LIGNE=row_number()) %>% 
  filter(CLE_LIGNE==1) %>% 
  select(-CLE_LIGNE)

V_MATCH_HIST = V_MATCH_HIST %>% 
  bind_rows(table_stock_new %>% 
             select(colnames(V_MATCH_HIST)))

save(V_MATCH_HIST,file=paste0(here(),"/Scrapping tennis data/ML_engenering/V_MATCH_HIST.RData"),
     compress="xz")

V_MATCH_t_new = table_stock_new %>% 
  filter(Phase=="Main Draw" & 
           (Categorie %in% c("Grand Slam","Olympics","Masters",
                             "ATP 1000","ATP 500",'ATP 250')|tournament %in% c("Atp Cup","United Cup")))

V_MATCH_t_new = V_MATCH_t_new %>% 
  mutate(
    
  # H2H
  H2H_Winner_W = NA,
  H2H_Loser_W = NA,
  H2H_Winner_Set_Won = NA,
  H2H_Loser_Set_Won = NA,
  H2H_Winner_Games_Won = NA,
  H2H_Loser_Games_Won = NA,
  # H2H 3Y
  H2H_Winner_W_3Y = NA,
  H2H_Loser_W_3Y = NA,
  H2H_Winner_Set_Won_3Y = NA,
  H2H_Loser_Set_Won_3Y = NA,
  H2H_Winner_Games_Won_3Y = NA,
  H2H_Loser_Games_Won_3Y = NA,
  
  # H2H Surface
  H2H_s_Winner_W = NA,
  H2H_s_Loser_W = NA,
  H2H_s_Winner_Set_Won = NA,
  H2H_s_Loser_Set_Won = NA,
  H2H_s_Winner_Games_Won = NA,
  H2H_s_Loser_Games_Won = NA,
  # H2H Surface 3Y
  H2H_s_Winner_W_3Y = NA,
  H2H_s_Loser_W_3Y = NA,
  H2H_s_Winner_Set_Won_3Y = NA,
  H2H_s_Loser_Set_Won_3Y = NA,
  H2H_s_Winner_Games_Won_3Y = NA,
  H2H_s_Loser_Games_Won_3Y = NA,
  
  # 4 Weeks
  Winner_N_Win_4 = NA,
  Winner_N_Loss_4 = NA,
  Winner_N_Win_Fav_Rank_4 = NA,
  Winner_N_Win_Out_Rank_4 = NA,
  Winner_N_Loss_Fav_Rank_4 = NA,
  Winner_N_Loss_Out_Rank_4 = NA,
  Loser_N_Win_4 = NA,
  Loser_N_Loss_4 = NA,
  Loser_N_Win_Fav_Rank_4 = NA,
  Loser_N_Win_Out_Rank_4 = NA,
  Loser_N_Loss_Fav_Rank_4 = NA,
  Loser_N_Loss_Out_Rank_4 = NA,
  
  # 12 Weeks
  Winner_N_Win_12 = NA,
  Winner_N_Loss_12 = NA,
  Winner_N_Win_Fav_Rank_12 = NA,
  Winner_N_Win_Out_Rank_12 = NA,
  Winner_N_Loss_Fav_Rank_12 = NA,
  Winner_N_Loss_Out_Rank_12 = NA,
  Loser_N_Win_12 = NA,
  Loser_N_Loss_12 = NA,
  Loser_N_Win_Fav_Rank_12 = NA,
  Loser_N_Win_Out_Rank_12 = NA,
  Loser_N_Loss_Fav_Rank_12 = NA,
  Loser_N_Loss_Out_Rank_12 = NA,
  
  # 52 Weeks
  Winner_N_Win_52 = NA,
  Winner_N_Loss_52 = NA,
  Winner_N_Win_Fav_Rank_52 = NA,
  Winner_N_Win_Out_Rank_52 = NA,
  Winner_N_Loss_Fav_Rank_52 = NA,
  Winner_N_Loss_Out_Rank_52 = NA,
  Loser_N_Win_52 = NA,
  Loser_N_Loss_52 = NA,
  Loser_N_Win_Fav_Rank_52 = NA,
  Loser_N_Win_Out_Rank_52 = NA,
  Loser_N_Loss_Fav_Rank_52 = NA,
  Loser_N_Loss_Out_Rank_52 = NA,
  
  # Surface 4 Weeks
  Winner_N_Win_s_4 = NA,
  Winner_N_Loss_s_4 = NA,
  Loser_N_Win_s_4 = NA,
  Loser_N_Loss_s_4 = NA,
  
  # Surface 12 Weeks
  Winner_N_Win_s_12 = NA,
  Winner_N_Loss_s_12 = NA,
  Loser_N_Win_s_12 = NA,
  Loser_N_Loss_s_12 = NA,
  
  # Surface 52 Weeks
  Winner_N_Win_s_52 = NA,
  Winner_N_Loss_s_52 = NA,
  Loser_N_Win_s_52 = NA,
  Loser_N_Loss_s_52 = NA,
  
  # ELO
  Elo_W = NA,
  Elo_L = NA,
  Elo_W_surface = NA,
  Elo_L_surface = NA,
  Elo_W_categorie = NA,
  Elo_L_categorie = NA
)

V_MATCH_t = V_MATCH_t %>% 
  bind_rows(V_MATCH_t_new %>% select(colnames(V_MATCH_t))) 

V_MATCH_t = V_MATCH_t %>% 
  group_by(tournament,Country_tournament,Season,Round,Date,Week_tournament,Winner_id,Loser_id) %>% 
  mutate(CLE_LIGNE=row_number()) %>% 
  filter(CLE_LIGNE==1) %>% 
  select(-CLE_LIGNE)

save(V_MATCH_t,file=paste0(here(),"/Scrapping tennis data/ML_engenering/V_MATCH_t.RData"),
     compress="xz")
