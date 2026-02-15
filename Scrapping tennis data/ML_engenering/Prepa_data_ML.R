library(tidyverse)
library(progress)

load(paste0(getwd(),"/Scrapping tennis data/MATCH_STATS.RData"))

V_MATCH_t=V_MATCH_t %>% 
  mutate(Country_tournament = case_when(Categorie=="Masters" & Season==2021~"Italy",
                                        Categorie=="Team"~"Australia",
                                        TRUE~Country_tournament))
p=0.5

proba_calcul=function(elo_p1,elo_p2){
  
  proba=1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400))
  
  return(proba)
}

get_tennis_week <- function(date) {
  year <- year(date)
  week_num <- isoweek(date)
  
  # Gérer le cas des premiers jours de janvier qui appartiennent 
  # à la dernière semaine de l'année précédente
  if (month(date) == 1 && week_num >= 52) {
    year <- year - 1
  }
  
  # Gérer le cas de fin décembre appartenant à la semaine 1 de l'année suivante
  if (month(date) == 12 && week_num == 1) {
    year <- year + 1
  }
  
  return(paste0(year, "-", sprintf("%02d", week_num)))
}

V_RANK=data.frame()

for (i in (year_lim-1):2025){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",i,".RData"))
  
  V_RANK=rbind(V_RANK,rank)
  
  print(i)  
  
}

V_RANK=V_RANK %>% 
  select(-Move) %>% 
  rename("Player_name"="Player name") %>% 
  mutate(Week_Rank = sapply(Date, get_tennis_week))

rm(rank)
rm(V_TOURNAMENT_INFO)
rm(V_MATCH)

V_MATCH_t=V_MATCH_t %>% 
select(-c(Rank_W,Rank_L,Points_W,Points_L) %>% 
  mutate(Match_week=sapply(Date, get_tennis_week)) %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week_Rank) %>% 
              rename(Rank_W=Rank,Points_W=Points) %>% 
              mutate(Rank_W=as.numeric(Rank_W),
                     Points_W=as.numeric(Points_W)),
            by=c("Winner_id"="Player_name","Match_week"="Week_Rank")) %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week_Rank) %>% 
              rename(Rank_L=Rank,Points_L=Points) %>% 
              mutate(Rank_L=as.numeric(Rank_L),
                     Points_L=as.numeric(Points_L)),
            by=c("Loser_id"="Player_name","Match_week"="Week_Rank")) %>% 
  mutate(Rank_W=case_when(is.na(Rank_W)~1000,TRUE~Rank_W),
         Points_W=case_when(is.na(Points_W)~10,TRUE~Points_W),
         Rank_L=case_when(is.na(Rank_L)~1000,TRUE~Rank_L),
         Points_L=case_when(is.na(Points_L)~10,TRUE~Points_L))

rm(V_RANK)

TABLE = V_MATCH_t %>% 
  mutate(
    Favori = case_when(Rank_W < Rank_L ~ Winner_id, TRUE ~ Loser_id),
    Outsider = case_when(Rank_L < Rank_W ~ Winner_id, TRUE ~ Loser_id),
    Issue = case_when(Favori == Winner_id ~ "Fav_W", TRUE ~ "Out_W"),
    Rank_F = case_when(Rank_W < Rank_L ~ Rank_W, TRUE ~ Rank_L),
    Rank_O = case_when(Rank_L < Rank_W ~ Rank_W, TRUE ~ Rank_L),
    Points_F = case_when(Rank_W < Rank_L ~ Points_W, TRUE ~ Points_L),
    Points_O = case_when(Rank_L < Rank_W ~ Points_W, TRUE ~ Points_L),
    Odd_F = case_when(Rank_W < Rank_L ~ Odd_W, TRUE ~ Odd_L),
    Odd_O = case_when(Rank_L < Rank_W ~ Odd_W, TRUE ~ Odd_L),
    
    # Informations personnelles
    Country_F = case_when(Rank_W < Rank_L ~ Country_W, TRUE ~ Country_L),
    Country_O = case_when(Rank_L < Rank_W ~ Country_W, TRUE ~ Country_L),
    Country_F_score = case_when(Country_F==Country_tournament~1,TRUE~0),
    Country_O_score = case_when(Country_O==Country_tournament~1,TRUE~0),
    
    Size_F = as.numeric(case_when(Rank_W < Rank_L ~ Size_W, TRUE ~ Size_L)),
    Size_O = as.numeric(case_when(Rank_L < Rank_W ~ Size_W, TRUE ~ Size_L)),
    Weight_F = as.numeric(case_when(Rank_W < Rank_L ~ Weight_W, TRUE ~ Weight_L)),
    Weight_O = as.numeric(case_when(Rank_L < Rank_W ~ Weight_W, TRUE ~ Weight_L)),
    IMC_F = Weight_F / ((Size_F / 100)^2),
    IMC_O = Weight_O / ((Size_O / 100)^2),
    Birth_date_F = case_when(Rank_W < Rank_L ~ Birth_date_W, TRUE ~ Birth_date_L),
    Birth_date_O = case_when(Rank_L < Rank_W ~ Birth_date_W, TRUE ~ Birth_date_L),
    
    # Calcul des âges
    Age_F = as.numeric(difftime(Date, Birth_date_F, units = "days")) / 365.25,
    Age_O = as.numeric(difftime(Date, Birth_date_O, units = "days")) / 365.25,
    
    Hand_F = case_when(Rank_W < Rank_L ~ Hand_W, TRUE ~ Hand_L),
    Hand_O = case_when(Rank_L < Rank_W ~ Hand_W, TRUE ~ Hand_L),
    Hand_Score_F = case_when(Hand_F == "left" ~ 1, TRUE~0),
    Hand_Score_O = case_when(Hand_O == "left" ~ 1, TRUE~0),
    
    # Proba et ELO rate 
    
    Elo_F = case_when(Rank_W < Rank_L ~ Elo_W, TRUE ~ Elo_L),
    Elo_s_F = case_when(Rank_W < Rank_L ~ Elo_W_surface, TRUE ~ Elo_L_surface),
    Elo_O = case_when(Rank_L < Rank_W ~ Elo_W, TRUE ~ Elo_L),
    Elo_s_O = case_when(Rank_L < Rank_W ~  Elo_W_surface, TRUE ~ Elo_L_surface),
    P_F = proba_calcul(Elo_F,Elo_O),
    P_s_F = proba_calcul(Elo_s_F,Elo_s_O),
    P_O = 1-P_F,
    P_s_O = 1-P_s_F,
    P_F_comb=(p * P_s_F) + ((1-p) * P_F),
    
    # H2H - Historique complet
    
    H2H_F_W = case_when(Rank_W < Rank_L ~ H2H_Winner_W, TRUE ~ H2H_Loser_W),
    H2H_O_W = case_when(Rank_L < Rank_W ~ H2H_Winner_W, TRUE ~ H2H_Loser_W),
    H2H_F_Set_Won = case_when(Rank_W < Rank_L ~ H2H_Winner_Set_Won, TRUE ~ H2H_Loser_Set_Won),
    H2H_O_Set_Won = case_when(Rank_L < Rank_W ~ H2H_Winner_Set_Won, TRUE ~ H2H_Loser_Set_Won),
    H2H_F_Games_Won = case_when(Rank_W < Rank_L ~ H2H_Winner_Games_Won, TRUE ~ H2H_Loser_Games_Won),
    H2H_O_Games_Won = case_when(Rank_L < Rank_W ~ H2H_Winner_Games_Won, TRUE ~ H2H_Loser_Games_Won),
    
    H2H_F_Win_Rate = ifelse((H2H_F_W + H2H_O_W) > 0, (H2H_F_W / (H2H_F_W + H2H_O_W)) * 100, 0),
    H2H_O_Win_Rate = ifelse((H2H_F_W + H2H_O_W) > 0, (H2H_O_W / (H2H_F_W + H2H_O_W)) * 100, 0),
    H2H_F_Set_Win_Rate = ifelse((H2H_F_Set_Won + H2H_O_Set_Won) > 0, (H2H_F_Set_Won / (H2H_F_Set_Won + H2H_O_Set_Won)) * 100, 0),
    H2H_O_Set_Win_Rate = ifelse((H2H_F_Set_Won + H2H_O_Set_Won) > 0, (H2H_O_Set_Won / (H2H_F_Set_Won + H2H_O_Set_Won)) * 100, 0),
    H2H_F_Games_Win_Rate = ifelse((H2H_F_Games_Won + H2H_O_Games_Won) > 0, (H2H_F_Games_Won / (H2H_F_Games_Won + H2H_O_Games_Won)) * 100, 0),
    H2H_O_Games_Win_Rate = ifelse((H2H_F_Games_Won + H2H_O_Games_Won) > 0, (H2H_O_Games_Won / (H2H_F_Games_Won + H2H_O_Games_Won)) * 100, 0),

    # H2H - Même surface
    
    H2H_s_F_W = case_when(Rank_W < Rank_L ~ H2H_s_Winner_W, TRUE ~ H2H_s_Loser_W),
    H2H_s_O_W = case_when(Rank_L < Rank_W ~ H2H_s_Winner_W, TRUE ~ H2H_s_Loser_W),
    H2H_s_F_Set_Won = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Set_Won, TRUE ~ H2H_s_Loser_Set_Won),
    H2H_s_O_Set_Won = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Set_Won, TRUE ~ H2H_s_Loser_Set_Won),
    H2H_s_F_Games_Won = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Games_Won, TRUE ~ H2H_s_Loser_Games_Won),
    H2H_s_O_Games_Won = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Games_Won, TRUE ~ H2H_s_Loser_Games_Won),
    
    H2H_s_F_Win_Rate = ifelse((H2H_s_F_W + H2H_s_O_W) > 0, (H2H_s_F_W / (H2H_s_F_W + H2H_s_O_W)) * 100, 0),
    H2H_s_O_Win_Rate = ifelse((H2H_s_F_W + H2H_s_O_W) > 0, (H2H_s_O_W / (H2H_s_F_W + H2H_s_O_W)) * 100, 0),
    H2H_s_F_Set_Win_Rate = ifelse((H2H_s_F_Set_Won + H2H_s_O_Set_Won) > 0, (H2H_s_F_Set_Won / (H2H_F_Set_Won + H2H_s_O_Set_Won)) * 100, 0),
    H2H_s_O_Set_Win_Rate = ifelse((H2H_s_F_Set_Won + H2H_s_O_Set_Won) > 0, (H2H_s_O_Set_Won / (H2H_s_F_Set_Won + H2H_s_O_Set_Won)) * 100, 0),
    H2H_s_F_Games_Win_Rate = ifelse((H2H_s_F_Games_Won + H2H_s_O_Games_Won) > 0, (H2H_s_F_Games_Won / (H2H_s_F_Games_Won + H2H_s_O_Games_Won)) * 100, 0),
    H2H_s_O_Games_Win_Rate = ifelse((H2H_s_F_Games_Won + H2H_s_O_Games_Won) > 0, (H2H_s_O_Games_Won / (H2H_s_F_Games_Won + H2H_s_O_Games_Won)) * 100, 0),
    
    # H2H - 3 dernières années
    H2H_F_W_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_W_3Y, TRUE ~ H2H_Loser_W_3Y),
    H2H_O_W_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_W_3Y, TRUE ~ H2H_Loser_W_3Y),
    H2H_F_Set_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_Set_Won_3Y, TRUE ~ H2H_Loser_Set_Won_3Y),
    H2H_O_Set_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_Set_Won_3Y, TRUE ~ H2H_Loser_Set_Won_3Y),
    H2H_F_Games_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_Games_Won_3Y, TRUE ~ H2H_Loser_Games_Won_3Y),
    H2H_O_Games_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_Games_Won_3Y, TRUE ~ H2H_Loser_Games_Won_3Y),
    
    H2H_F_Win_Rate_3Y = ifelse((H2H_F_W_3Y + H2H_O_W_3Y) > 0, (H2H_F_W_3Y / (H2H_F_W_3Y + H2H_O_W_3Y)) * 100, 0),
    H2H_O_Win_Rate_3Y = ifelse((H2H_F_W_3Y + H2H_O_W_3Y) > 0, (H2H_O_W_3Y / (H2H_F_W_3Y + H2H_O_W_3Y)) * 100, 0),
    H2H_F_Set_Win_Rate_3Y = ifelse((H2H_F_Set_Won_3Y + H2H_O_Set_Won_3Y) > 0, (H2H_F_Set_Won_3Y / (H2H_F_Set_Won_3Y + H2H_O_Set_Won_3Y)) * 100, 0),
    H2H_O_Set_Win_Rate_3Y = ifelse((H2H_F_Set_Won_3Y + H2H_O_Set_Won_3Y) > 0, (H2H_O_Set_Won_3Y / (H2H_F_Set_Won_3Y + H2H_O_Set_Won_3Y)) * 100, 0),
    H2H_F_Games_Win_Rate_3Y = ifelse((H2H_F_Games_Won_3Y + H2H_O_Games_Won_3Y) > 0, (H2H_F_Games_Won_3Y / (H2H_F_Games_Won_3Y + H2H_O_Games_Won_3Y)) * 100, 0),
    H2H_O_Games_Win_Rate_3Y = ifelse((H2H_F_Games_Won_3Y + H2H_O_Games_Won_3Y) > 0, (H2H_O_Games_Won_3Y / (H2H_F_Games_Won_3Y + H2H_O_Games_Won_3Y)) * 100, 0),
    
    # H2H - Même surface, 3 dernières années
    H2H_s_F_W_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_W_3Y, TRUE ~ H2H_s_Loser_W_3Y),
    H2H_s_O_W_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_W_3Y, TRUE ~ H2H_s_Loser_W_3Y),
    H2H_s_F_Set_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Set_Won_3Y, TRUE ~ H2H_s_Loser_Set_Won_3Y),
    H2H_s_O_Set_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Set_Won_3Y, TRUE ~ H2H_s_Loser_Set_Won_3Y),
    H2H_s_F_Games_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Games_Won_3Y, TRUE ~ H2H_s_Loser_Games_Won_3Y),
    H2H_s_O_Games_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Games_Won_3Y, TRUE ~ H2H_s_Loser_Games_Won_3Y),
    
    H2H_s_F_Win_Rate_3Y = ifelse((H2H_s_F_W_3Y + H2H_s_O_W_3Y) > 0, (H2H_s_F_W_3Y / (H2H_s_F_W_3Y + H2H_s_O_W_3Y)) * 100, 0),
    H2H_s_O_Win_Rate_3Y = ifelse((H2H_s_F_W_3Y + H2H_s_O_W_3Y) > 0, (H2H_s_O_W_3Y / (H2H_s_F_W_3Y + H2H_s_O_W_3Y)) * 100, 0),
    H2H_s_F_Set_Win_Rate_3Y = ifelse((H2H_s_F_Set_Won_3Y + H2H_s_O_Set_Won_3Y) > 0, (H2H_s_F_Set_Won_3Y / (H2H_s_F_Set_Won_3Y + H2H_s_O_Set_Won_3Y)) * 100, 0),
    H2H_s_O_Set_Win_Rate_3Y = ifelse((H2H_s_F_Set_Won_3Y + H2H_s_O_Set_Won_3Y) > 0, (H2H_s_O_Set_Won_3Y / (H2H_s_F_Set_Won_3Y + H2H_s_O_Set_Won_3Y)) * 100, 0),
    H2H_s_F_Games_Win_Rate_3Y = ifelse((H2H_s_F_Games_Won_3Y + H2H_s_O_Games_Won_3Y) > 0, (H2H_s_F_Games_Won_3Y / (H2H_s_F_Games_Won_3Y + H2H_s_O_Games_Won_3Y)) * 100, 0),
    H2H_s_O_Games_Win_Rate_3Y = ifelse((H2H_s_F_Games_Won_3Y + H2H_s_O_Games_Won_3Y) > 0, (H2H_s_O_Games_Won_3Y / (H2H_s_F_Games_Won_3Y + H2H_s_O_Games_Won_3Y)) * 100, 0),
    
     # Forme récente - 4 derniers matchs
    F_N_Win_4 = case_when(Rank_W < Rank_L ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    O_N_Win_4 = case_when(Rank_L < Rank_W ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    F_N_Loss_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    O_N_Loss_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    F_Win_Rate_4 = (F_N_Win_4/(F_N_Win_4+F_N_Loss_4)),
    F_Win_Rate_4 = ifelse(is.na(F_Win_Rate_4),0,F_Win_Rate_4)*100,
    O_Win_Rate_4 = (O_N_Win_4/(O_N_Win_4+O_N_Loss_4)),
    O_Win_Rate_4 = ifelse(is.na(O_Win_Rate_4),0,O_Win_Rate_4)*100,

  #  Performance as Favoris (Joueurs mieux classés) 
    F_N_Win_as_Fav_12  = case_when(Rank_W < Rank_L ~ Winner_N_Win_Fav_Rank_12,  TRUE ~ Loser_N_Win_Fav_Rank_12),
    O_N_Win_as_Fav_12  = case_when(Rank_L < Rank_W ~ Winner_N_Win_Fav_Rank_12,  TRUE ~ Loser_N_Win_Fav_Rank_12),
    F_N_Loss_as_Fav_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_Fav_Rank_12, TRUE ~ Loser_N_Loss_Fav_Rank_12),
    O_N_Loss_as_Fav_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_Fav_Rank_12, TRUE ~ Loser_N_Loss_Fav_Rank_12),
        
  #  Performance as Outsiders (Joueurs moins bien classés) 
    F_N_Win_as_Out_12  = case_when(Rank_W < Rank_L ~ Winner_N_Win_Out_Rank_12,  TRUE ~ Loser_N_Win_Out_Rank_12),
    O_N_Win_as_Out_12  = case_when(Rank_L < Rank_W ~ Winner_N_Win_Out_Rank_12,  TRUE ~ Loser_N_Win_Out_Rank_12),
    F_N_Loss_as_Out_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_Out_Rank_12, TRUE ~ Loser_N_Loss_Out_Rank_12),
    O_N_Loss_as_Out_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_Out_Rank_12, TRUE ~ Loser_N_Loss_Out_Rank_12),
        
  #  Calcul des Win Rates Specifiques 
    F_as_Fav_12_Win_rate = ifelse((F_N_Win_as_Fav_12 + F_N_Loss_as_Fav_12) == 0, 0, (F_N_Win_as_Fav_12 / (F_N_Win_as_Fav_12 + F_N_Loss_as_Fav_12)) * 100),
    O_as_Fav_12_Win_rate = ifelse((O_N_Win_as_Fav_12 + O_N_Loss_as_Fav_12) == 0, 0, (O_N_Win_as_Fav_12 / (O_N_Win_as_Fav_12 + O_N_Loss_as_Fav_12)) * 100),
    
    F_as_Out_12_Win_rate = ifelse((F_N_Win_as_Out_12 + F_N_Loss_as_Out_12) == 0, 0, (F_N_Win_as_Out_12 / (F_N_Win_as_Out_12 + F_N_Loss_as_Out_12)) * 100),
    O_as_Out_12_Win_rate = ifelse((O_N_Win_as_Out_12 + O_N_Loss_as_Out_12) == 0, 0, (O_N_Win_as_Out_12 / (O_N_Win_as_Out_12 + O_N_Loss_as_Out_12)) * 100),

  #  Performance en tant que Favori (Statut basé sur le Rank) - 4 
    F_N_Win_as_Fav_4  = case_when(Rank_W < Rank_L ~ Winner_N_Win_Fav_Rank_4,  TRUE ~ Loser_N_Win_Fav_Rank_4),
    O_N_Win_as_Fav_4  = case_when(Rank_L < Rank_W ~ Winner_N_Win_Fav_Rank_4,  TRUE ~ Loser_N_Win_Fav_Rank_4),
    F_N_Loss_as_Fav_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_Fav_Rank_4, TRUE ~ Loser_N_Loss_Fav_Rank_4),
    O_N_Loss_as_Fav_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_Fav_Rank_4, TRUE ~ Loser_N_Loss_Fav_Rank_4),
      
    #  Performance en tant qu'Outsider (Statut basé sur le Rank) - 4 
    F_N_Win_as_Out_4  = case_when(Rank_W < Rank_L ~ Winner_N_Win_Out_Rank_4,  TRUE ~ Loser_N_Win_Out_Rank_4),
    O_N_Win_as_Out_4  = case_when(Rank_L < Rank_W ~ Winner_N_Win_Out_Rank_4,  TRUE ~ Loser_N_Win_Out_Rank_4),
    F_N_Loss_as_Out_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_Out_Rank_4, TRUE ~ Loser_N_Loss_Out_Rank_4),
    O_N_Loss_as_Out_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_Out_Rank_4, TRUE ~ Loser_N_Loss_Out_Rank_4),
      
    #  Calcul des Win Rates Spécifiques - 4 
    F_as_Fav_4_Win_rate = ifelse((F_N_Win_as_Fav_4 + F_N_Loss_as_Fav_4) == 0, 0, 
                                   (F_N_Win_as_Fav_4 / (F_N_Win_as_Fav_4 + F_N_Loss_as_Fav_4)) * 100),
      
    O_as_Fav_4_Win_rate = ifelse((O_N_Win_as_Fav_4 + O_N_Loss_as_Fav_4) == 0, 0, 
                                   (O_N_Win_as_Fav_4 / (O_N_Win_as_Fav_4 + O_N_Loss_as_Fav_4)) * 100),
      
    F_as_Out_4_Win_rate = ifelse((F_N_Win_as_Out_4 + F_N_Loss_as_Out_4) == 0, 0, 
                                   (F_N_Win_as_Out_4 / (F_N_Win_as_Out_4 + F_N_Loss_as_Out_4)) * 100),
      
    O_as_Out_4_Win_rate = ifelse((O_N_Win_as_Out_4 + O_N_Loss_as_Out_4) == 0, 0, 
                                   (O_N_Win_as_Out_4 / (O_N_Win_as_Out_4 + O_N_Loss_as_Out_4)) * 100),
    
    # Forme récente sur surface - 4 derniers matchs
    F_N_Win_s_4 = case_when(Rank_W < Rank_L ~ Winner_N_Win_s_4, TRUE ~ Loser_N_Win_s_4),
    O_N_Win_s_4 = case_when(Rank_L < Rank_W ~ Winner_N_Win_s_4, TRUE ~ Loser_N_Win_s_4),
    F_N_Loss_s_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_s_4, TRUE ~ Loser_N_Loss_s_4),
    O_N_Loss_s_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_s_4, TRUE ~ Loser_N_Loss_s_4),
    F_Win_Rate_s_4 = (F_N_Win_s_4/(F_N_Win_s_4+F_N_Loss_s_4)),
    F_Win_Rate_s_4 = ifelse(is.na(F_Win_Rate_s_4),0,F_Win_Rate_s_4)*100,
    O_Win_Rate_s_4 = (O_N_Win_s_4/(O_N_Win_s_4+O_N_Loss_s_4)),
    O_Win_Rate_s_4 = ifelse(is.na(O_Win_Rate_s_4),0,O_Win_Rate_s_4)*100,
    
    # Forme récente - 12 derniers matchs
    F_N_Win_12 = case_when(Rank_W < Rank_L ~ Winner_N_Win_12, TRUE ~ Loser_N_Win_12),
    O_N_Win_12 = case_when(Rank_L < Rank_W ~ Winner_N_Win_12, TRUE ~ Loser_N_Win_12),
    F_N_Loss_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_12, TRUE ~ Loser_N_Loss_12),
    O_N_Loss_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_12, TRUE ~ Loser_N_Loss_12),
    F_Win_Rate_12 = (F_N_Win_12/(F_N_Win_12+F_N_Loss_12)),
    F_Win_Rate_12 = ifelse(is.na(F_Win_Rate_12),0,F_Win_Rate_12)*100,
    O_Win_Rate_12 = (O_N_Win_12/(O_N_Win_12+O_N_Loss_12)),
    O_Win_Rate_12 = ifelse(is.na(O_Win_Rate_12),0,O_Win_Rate_12)*100,
    
    # Forme récente sur surface - 12 derniers matchs
    F_N_Win_s_12 = case_when(Rank_W < Rank_L ~ Winner_N_Win_s_12, TRUE ~ Loser_N_Win_s_12),
    O_N_Win_s_12 = case_when(Rank_L < Rank_W ~ Winner_N_Win_s_12, TRUE ~ Loser_N_Win_s_12),
    F_N_Loss_s_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_s_12, TRUE ~ Loser_N_Loss_s_12),
    O_N_Loss_s_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_s_12, TRUE ~ Loser_N_Loss_s_12),
    F_Win_Rate_s_12 = (F_N_Win_s_12/(F_N_Win_s_12+F_N_Loss_s_12)),
    F_Win_Rate_s_12 = ifelse(is.na(F_Win_Rate_s_12),0,F_Win_Rate_s_12)*100,
    O_Win_Rate_s_12 = (O_N_Win_s_12/(O_N_Win_s_12+O_N_Loss_s_12)),
    O_Win_Rate_s_12 = ifelse(is.na(O_Win_Rate_s_12),0,O_Win_Rate_s_12)*100,
    
    # Categorie tournoi
    Categ_Elite = case_when(Categorie %in% c("Grand Slam","Olympics","Team")~1,TRUE~0),
    Categ_Mid = case_when(Categorie %in% c("ATP 1000","ATP 500","Masters")~1,TRUE~0),
    Categ_low = case_when(Categorie=="ATP 250"~1,TRUE~0),
    
    Round_RR = case_when(Round=="-"~1,TRUE~0),
    Round_R1 = case_when(Round=="1R"~1,TRUE~0),
    Round_R2 = case_when(Round=="2R"~1,TRUE~0),
    Round_R3 = case_when(Round=="3R"~1,TRUE~0),
    Round_R16 = case_when(Round=="R16"~1,TRUE~0),
    Round_QF = case_when(Round=="QF"~1,TRUE~0),
    Round_SF = case_when(Round=="SF"~1,TRUE~0),
    Round_F = case_when(Round=="F"~1,TRUE~0)

    
  ) %>% 
  filter(info=="Completed") %>% 
  select(
    tournament,
    Categorie,
    Round,
    Season,
    Date,
    Surface_tournament,
    Country_tournament,
    Favori,
    Outsider,
    Issue,
    Rank_F,
    Rank_O,
    Points_F,
    Points_O,
    Odd_F,
    Odd_O,
    Country_F_score,
    Country_O_score,
    Size_F,
    Size_O,
    Weight_F,
    Weight_O,
    IMC_F,
    IMC_O,
    Birth_date_F,
    Birth_date_O,
    Age_F,
    Age_O,
    Hand_Score_F,
    Hand_Score_O,
    Elo_F,
    Elo_s_F,
    Elo_O,
    Elo_s_O,
    P_F,
    P_s_F,
    P_O,
    P_s_O,
    P_F_comb,
    H2H_F_W,
    H2H_O_W,
    H2H_F_Set_Won,
    H2H_O_Set_Won,
    H2H_F_Games_Won,
    H2H_O_Games_Won,
    H2H_F_Win_Rate,
    H2H_O_Win_Rate,
    H2H_F_Set_Win_Rate,
    H2H_O_Set_Win_Rate,
    H2H_F_Games_Win_Rate,
    H2H_O_Games_Win_Rate,
    H2H_s_F_W,
    H2H_s_O_W,
    H2H_s_F_Set_Won,
    H2H_s_O_Set_Won,
    H2H_s_F_Games_Won,
    H2H_s_O_Games_Won,
    H2H_s_F_Win_Rate,
    H2H_s_O_Win_Rate,
    H2H_s_F_Set_Win_Rate,
    H2H_s_O_Set_Win_Rate,
    H2H_s_F_Games_Win_Rate,
    H2H_s_O_Games_Win_Rate,
    H2H_F_W_3Y,
    H2H_O_W_3Y,
    H2H_F_Set_Won_3Y,
    H2H_O_Set_Won_3Y,
    H2H_F_Games_Won_3Y,
    H2H_O_Games_Won_3Y,
    H2H_F_Win_Rate_3Y,
    H2H_O_Win_Rate_3Y,
    H2H_F_Set_Win_Rate_3Y,
    H2H_O_Set_Win_Rate_3Y,
    H2H_F_Games_Win_Rate_3Y,
    H2H_O_Games_Win_Rate_3Y,
    H2H_s_F_W_3Y,
    H2H_s_O_W_3Y,
    H2H_s_F_Set_Won_3Y,
    H2H_s_O_Set_Won_3Y,
    H2H_s_F_Games_Won_3Y,
    H2H_s_O_Games_Won_3Y,
    H2H_s_F_Win_Rate_3Y,
    H2H_s_O_Win_Rate_3Y,
    H2H_s_F_Set_Win_Rate_3Y,
    H2H_s_O_Set_Win_Rate_3Y,
    H2H_s_F_Games_Win_Rate_3Y,
    H2H_s_O_Games_Win_Rate_3Y,
    F_N_Win_4,
    O_N_Win_4,
    F_N_Loss_4,
    O_N_Loss_4,
    F_Win_Rate_4,
    F_Win_Rate_4,
    O_Win_Rate_4,
    O_Win_Rate_4,
    F_N_Win_as_Fav_12,
    O_N_Win_as_Fav_12,
    F_N_Loss_as_Fav_12,
    O_N_Loss_as_Fav_12,
    F_N_Win_as_Out_12,
    O_N_Win_as_Out_12,
    F_N_Loss_as_Out_12,
    O_N_Loss_as_Out_12,
    F_as_Fav_12_Win_rate,
    O_as_Fav_12_Win_rate,
    F_as_Out_12_Win_rate,
    O_as_Out_12_Win_rate,
    F_N_Win_as_Fav_4,
    O_N_Win_as_Fav_4,
    F_N_Loss_as_Fav_4,
    O_N_Loss_as_Fav_4,
    F_N_Win_as_Out_4,
    O_N_Win_as_Out_4,
    F_N_Loss_as_Out_4,
    O_N_Loss_as_Out_4,
    F_as_Fav_4_Win_rate,
    F_N_Win_as_Fav_4,
    O_as_Fav_4_Win_rate,
    O_N_Win_as_Fav_4,
    F_as_Out_4_Win_rate,
    F_N_Win_as_Out_4,
    O_as_Out_4_Win_rate,
    O_N_Win_as_Out_4,
    F_N_Win_s_4,
    O_N_Win_s_4,
    F_N_Loss_s_4,
    O_N_Loss_s_4,
    F_Win_Rate_s_4,
    F_Win_Rate_s_4,
    O_Win_Rate_s_4,
    O_Win_Rate_s_4,
    F_N_Win_12,
    O_N_Win_12,
    F_N_Loss_12,
    O_N_Loss_12,
    F_Win_Rate_12,
    F_Win_Rate_12,
    O_Win_Rate_12,
    O_Win_Rate_12,
    F_N_Win_s_12,
    O_N_Win_s_12,
    F_N_Loss_s_12,
    O_N_Loss_s_12,
    F_Win_Rate_s_12,
    F_Win_Rate_s_12,
    O_Win_Rate_s_12,
    O_Win_Rate_s_12,
    Categ_Elite,
    Categ_Mid,
    Categ_low,
    Round_RR,
    Round_R1,
    Round_R2,
    Round_R3,
    Round_R16,
    Round_QF,
    Round_SF,
    Round_F
    
  )

# save(TABLE,file="TABLE_RAW.RData",compress="xz")
# write.csv(TABLE, "TABLE_RAW.csv", row.names = FALSE,sep = ";")

##### TABLE DIFF 

TABLE_ML_DIFF=TABLE %>% 
  mutate(
    # --- DIFFÉRENCES - Profils Joueurs ---
    Diff_Rank = Rank_F - Rank_O,
    Diff_Points = Points_F - Points_O,
    Diff_Age = Age_F - Age_O,
    Diff_IMC = IMC_F - IMC_O,
    Diff_Size = Size_F- Size_O,
    Diff_Weight = Weight_F - Weight_O,
    Diff_Hand_Score = Hand_Score_F - Hand_Score_O,
    Diff_Country_score = Country_F_score - Country_O_score,
    
    # --- DIFFÉRENCES - ELO & Probabilités ---
    Diff_Elo = Elo_F - Elo_O,
    Diff_Elo_s = Elo_s_F - Elo_s_O,
    #P_F_comb,  # Déjà une probabilité relative
    
    # --- DIFFÉRENCES - H2H (Historique Face à Face) ---
    
    Diff_H2H_Win_Rate = H2H_F_Win_Rate - H2H_O_Win_Rate,
    Diff_H2H_Set_Win_Rate = H2H_F_Set_Win_Rate - H2H_O_Set_Win_Rate,
    Diff_H2H_Games_Win_Rate = H2H_F_Games_Win_Rate - H2H_O_Games_Win_Rate,
    
    Diff_H2H_s_Win_Rate = H2H_s_F_Win_Rate - H2H_s_O_Win_Rate,
    Diff_H2H_s_Set_Win_Rate = H2H_s_F_Set_Win_Rate - H2H_s_O_Set_Win_Rate,
    Diff_H2H_s_Games_Win_Rate = H2H_s_F_Games_Win_Rate - H2H_s_O_Games_Win_Rate,
    
    Diff_H2H_Win_Rate_3Y = H2H_F_Win_Rate_3Y - H2H_O_Win_Rate_3Y,
    Diff_H2H_Set_Win_Rate_3Y = H2H_F_Set_Win_Rate_3Y - H2H_O_Set_Win_Rate_3Y,
    Diff_H2H_Games_Win_Rate_3Y = H2H_F_Games_Win_Rate_3Y - H2H_O_Games_Win_Rate_3Y,
    
    Diff_H2H_s_Win_Rate_3Y = H2H_s_F_Win_Rate_3Y - H2H_s_O_Win_Rate_3Y,
    Diff_H2H_s_Set_Win_Rate_3Y = H2H_s_F_Set_Win_Rate_3Y - H2H_s_O_Set_Win_Rate_3Y,
    Diff_H2H_s_Games_Win_Rate_3Y = H2H_s_F_Games_Win_Rate_3Y - H2H_s_O_Games_Win_Rate_3Y,
    
    # --- DIFFÉRENCES - Forme Récente (4 matchs) ---
    Diff_Win_Rate_4 = F_Win_Rate_4 - O_Win_Rate_4,
    Diff_Win_Rate_s_4 = F_Win_Rate_s_4 - O_Win_Rate_s_4,
    
    # --- DIFFÉRENCES - Forme Récente (12 matchs) ---
    Diff_Win_Rate_12 = F_Win_Rate_12 - O_Win_Rate_12,
    Diff_Win_Rate_s_12 = F_Win_Rate_s_12 - O_Win_Rate_s_12,
    
    # --- DIFFÉRENCES - Performance As Fav (12 mois) ---
    Diff_as_Fav_12_Win_rate = F_as_Fav_12_Win_rate - O_as_Fav_12_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Out (12 mois) ---
    Diff_as_Out_12_Win_rate = F_as_Out_12_Win_rate - O_as_Out_12_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Fav (4 mois) ---
    Diff_as_Fav_4_Win_rate = F_as_Fav_4_Win_rate - O_as_Fav_4_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Out (4 mois) ---
    Diff_as_Out_4_Win_rate = F_as_Out_4_Win_rate - O_as_Out_4_Win_rate
  ) %>% 
  select(
    # --- Identification & Backtest (non-features) ---
    tournament, Season, Date, Favori, Outsider, 
    Odd_F, Odd_O,
    
    # --- VARIABLE CIBLE ---
    Issue, 
    
    # --- Caractéristiques du Match ---
    Categorie, Surface_tournament, Round,
    Categ_Elite, Categ_Mid, Categ_low,
    Round_RR, Round_R1, Round_R2, Round_R3, Round_R16, Round_QF, Round_SF, Round_F,
    
    # --- DIFFÉRENCES - Profils Joueurs ---
    Diff_Rank,
    Diff_Points,
    Diff_Age,
    Diff_IMC,
    Diff_Size,
    Diff_Weight,
    Diff_Hand_Score,
    Diff_Country_score,
    
    # --- DIFFÉRENCES - ELO & Probabilités ---
    Diff_Elo,
    Diff_Elo_s,
    P_F_comb,  # Déjà une probabilité relative
    
    # --- DIFFÉRENCES - H2H (Historique Face à Face) ---
    Diff_H2H_Win_Rate,
    Diff_H2H_Set_Win_Rate,
    Diff_H2H_Games_Win_Rate,
    
    Diff_H2H_s_Win_Rate,
    Diff_H2H_s_Set_Win_Rate,
    Diff_H2H_s_Games_Win_Rate,
    
    Diff_H2H_Win_Rate_3Y,
    Diff_H2H_Set_Win_Rate_3Y,
    Diff_H2H_Games_Win_Rate_3Y,
    
    Diff_H2H_s_Win_Rate_3Y,
    Diff_H2H_s_Set_Win_Rate_3Y,
    Diff_H2H_s_Games_Win_Rate_3Y,
    
    
    # --- DIFFÉRENCES - Forme Récente (4 matchs) ---
    Diff_Win_Rate_4,
    Diff_Win_Rate_s_4,
    
    # --- DIFFÉRENCES - Forme Récente (12 matchs) ---
    Diff_Win_Rate_12,
    Diff_Win_Rate_s_12,
    
    # --- DIFFÉRENCES - Performance As Fav (12 mois) ---
    Diff_as_Fav_12_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Out (12 mois) ---
    Diff_as_Out_12_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Fav (4 mois) ---
    Diff_as_Fav_4_Win_rate,
    
    # --- DIFFÉRENCES - Performance As Out (4 mois) ---
    Diff_as_Out_4_Win_rate
  )


#### TABLE MOMENTUM #####

  TABLE_MOMENTUM = TABLE_ML_DIFF %>%
  mutate(
    # --- Momentum H2H (Matchs, Sets, Games) ---
    Mom_H2H               = sign(Diff_H2H_Win_Rate),
    Mom_H2H_Set           = sign(Diff_H2H_Set_Win_Rate),
    Mom_H2H_Games         = sign(Diff_H2H_Games_Win_Rate),
    
    Mom_H2H_s             = sign(Diff_H2H_s_Win_Rate),
    Mom_H2H_s_Set         = sign(ifelse(is.na(Diff_H2H_s_Set_Win_Rate), 0, Diff_H2H_s_Set_Win_Rate)),
    Mom_H2H_s_Games       = sign(ifelse(is.na(Diff_H2H_s_Games_Win_Rate), 0, Diff_H2H_s_Games_Win_Rate)),
    
    Mom_H2H_3Y            = sign(Diff_H2H_Win_Rate_3Y),
    Mom_H2H_Set_3Y        = sign(Diff_H2H_Set_Win_Rate_3Y),
    Mom_H2H_Games_3Y      = sign(Diff_H2H_Games_Win_Rate_3Y),
    
    Mom_H2H_s_3Y          = sign(Diff_H2H_s_Win_Rate_3Y),
    Mom_H2H_s_Set_3Y      = sign(Diff_H2H_s_Set_Win_Rate_3Y),
    Mom_H2H_s_Games_3Y    = sign(Diff_H2H_s_Games_Win_Rate_3Y),
    
    # --- Momentum Forme Récente ---
    Mom_WR_4              = sign(Diff_Win_Rate_4),
    Mom_WR_s_4            = sign(Diff_Win_Rate_s_4),
    Mom_WR_12             = sign(Diff_Win_Rate_12),
    Mom_WR_s_12           = sign(Diff_Win_Rate_s_12),
    
    # --- Momentum Statut ---
    Mom_as_Fav_12         = sign(Diff_as_Fav_12_Win_rate),
    Mom_as_Out_12         = sign(Diff_as_Out_12_Win_rate),
    Mom_as_Fav_4          = sign(Diff_as_Fav_4_Win_rate),
    Mom_as_Out_4          = sign(Diff_as_Out_4_Win_rate),
    
    # --- Score Global ---
    Global_Momentum_Score = rowSums(across(starts_with("Mom_")), na.rm = TRUE)
  )
