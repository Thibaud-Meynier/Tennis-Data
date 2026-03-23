library(tidyverse)
library(progress)
library(dbplyr)
library(conflicted)
library(furrr)
library(here)

# Définir la priorité explicitement

conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")
conflict_prefer("year", "lubridate")
conflict_prefer("isoweek", "lubridate")
conflict_prefer("month", "lubridate")
conflicts_prefer(dplyr::between)
conflicts_prefer(sjmisc::is_empty)

year_lim=2017

load(paste0(getwd(),"/Scrapping tennis data/ML_engenering/MATCH_STATS.RData"))

V_MATCH_t = V_MATCH_final %>% filter(Season>=year_lim)

rm(V_MATCH_final)

proba_calcul=function(elo_p1,elo_p2){
  
  proba=1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400))
  
  return(proba)
}

p=0.5

TABLE = V_MATCH_t %>% 
  mutate(
    Categorie2 = case_when(Categorie %in% c("Team","Olympics")~"Country",TRUE~Categorie),
    Favori = case_when(Rank_W < Rank_L ~ Winner_id, TRUE ~ Loser_id),
    Outsider = case_when(Rank_L < Rank_W ~ Winner_id, TRUE ~ Loser_id),
    Issue = case_when(Favori == Winner_id ~ "Fav_W", TRUE ~ "Out_W"),
    Rank_F = case_when(Rank_W < Rank_L ~ Rank_W, TRUE ~ Rank_L),
    Rank_O = case_when(Rank_L < Rank_W ~ Rank_W, TRUE ~ Rank_L),
    Rank_F_score = case_when(
      Rank_F >= 1 & Rank_F <= 5 ~ 8,
      Rank_F >= 6 & Rank_F <= 10 ~ 7,
      Rank_F >= 11 & Rank_F <= 20 ~ 6,
      Rank_F >= 21 & Rank_F <= 30 ~ 5,
      Rank_F >= 31 & Rank_F <= 50 ~ 4,
      Rank_F >= 51 & Rank_F <= 100 ~ 3,
      Rank_F >= 101 & Rank_F <= 200 ~ 2,
      Rank_F >= 201 ~ 1,
      TRUE ~ NA
    ),
    Rank_O_score = case_when(
      Rank_O >= 1 & Rank_O <= 5 ~ 8,
      Rank_O >= 6 & Rank_O <= 10 ~ 7,
      Rank_O >= 11 & Rank_O <= 20 ~ 6,
      Rank_O >= 21 & Rank_O <= 30 ~ 5,
      Rank_O >= 31 & Rank_O <= 50 ~ 4,
      Rank_O >= 51 & Rank_O <= 100 ~ 3,
      Rank_O >= 101 & Rank_O <= 200 ~ 2,
      Rank_O >= 201 ~ 1,
      TRUE ~ NA
    ),
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
    
    
    # Forme récente - 4 derniers matchs
    F_N_Win_4 = case_when(Rank_W < Rank_L ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    O_N_Win_4 = case_when(Rank_L < Rank_W ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    F_N_Loss_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    O_N_Loss_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    F_Win_Rate_4 = (F_N_Win_4/(F_N_Win_4+F_N_Loss_4)),
    F_Win_Rate_4 = ifelse(is.na(F_Win_Rate_4),0,F_Win_Rate_4)*100,
    O_Win_Rate_4 = (O_N_Win_4/(O_N_Win_4+O_N_Loss_4)),
    O_Win_Rate_4 = ifelse(is.na(O_Win_Rate_4),0,O_Win_Rate_4)*100,
    
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
    
    # Indice de fatigue 
    F_N_match_12 = F_N_Win_12+F_N_Loss_12,
    O_N_match_12 = O_N_Win_12+O_N_Loss_12,
    F_N_match_4 = F_N_Win_4+F_N_Loss_4,
    O_N_match_4 = O_N_Win_4+O_N_Loss_4,
    
    P4_P12_F = ifelse(is.na(F_N_match_4/F_N_match_12),0,F_N_match_4/F_N_match_12),
    P4_P12_O = ifelse(is.na(O_N_match_4/O_N_match_12),0,O_N_match_4/O_N_match_12),
    
    F_Fatigue_index = F_N_match_12/21*(1+P4_P12_F),
    O_Fatigue_index = O_N_match_12/21*(1+P4_P12_O),
    
    # Categorie tournoi
    Grand_Slam = case_when(Categorie %in% c("Grand Slam")~1,TRUE~0),
    Country_Champ = case_when(Categorie %in% c("Olympics","Team")~1,TRUE~0),
    ATP_1000 = case_when(Categorie %in%"ATP 1000"~1,TRUE~0),
    ATP_500 = case_when(Categorie %in%"ATP 500"~1,TRUE~0),
    ATP_250 = case_when(Categorie %in%"ATP 250"~1,TRUE~0),
    Masters = case_when(Categorie=="Masters"~1,TRUE~0),
    
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
    Categorie2,
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
    Rank_F_score,
    Rank_O_score,
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
    Grand_Slam,
    Country_Champ,
    Masters,
    ATP_1000,
    ATP_500,
    ATP_250,
    Round_RR,
    Round_R1,
    Round_R2,
    Round_R3,
    Round_R16,
    Round_QF,
    Round_SF,
    Round_F,
    F_Fatigue_index,
    O_Fatigue_index
  )

##### TABLE DIFF 

TABLE_ML_DIFF=TABLE %>% 
  mutate(
    # --- DIFFÉRENCES - Profils Joueurs ---
    Diff_Rank = abs(Rank_F - Rank_O),
    Diff_Rank_Class = case_when(
      Diff_Rank >= 1 & Diff_Rank <= 5 ~ 1,
      Diff_Rank >= 6 & Diff_Rank <= 10 ~ 2,
      Diff_Rank >= 11 & Diff_Rank <= 20 ~ 3,
      Diff_Rank >= 21 & Diff_Rank <= 30 ~ 4,
      Diff_Rank >= 31 & Diff_Rank <= 50 ~ 5,
      Diff_Rank >= 51 & Diff_Rank <= 100 ~ 6,
      Diff_Rank >= 101 & Diff_Rank <= 200 ~ 7,
      Diff_Rank >= 201 ~ 8,
      TRUE ~ NA
    ),
    
    Diff_Score_Rank = Rank_F_score - Rank_O_score,
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
    Diff_H2H = H2H_F_W - H2H_O_W,
    Diff_H2H_set = H2H_F_Set_Won - H2H_O_Set_Won,
    Diff_H2H_games = H2H_F_Games_Won - H2H_O_Games_Won,
    
    Diff_H2H_Win_Rate = H2H_F_Win_Rate - H2H_O_Win_Rate,
    Diff_H2H_Set_Win_Rate = H2H_F_Set_Win_Rate - H2H_O_Set_Win_Rate,
    Diff_H2H_Games_Win_Rate = H2H_F_Games_Win_Rate - H2H_O_Games_Win_Rate,
    
    Diff_H2H_s = H2H_s_F_W - H2H_s_O_W,
    Diff_H2H_set_s = H2H_s_F_Set_Won - H2H_s_O_Set_Won,
    Diff_H2H_games_s = H2H_s_F_Games_Won - H2H_s_O_Games_Won,
    
    Diff_H2H_s_Win_Rate = H2H_s_F_Win_Rate - H2H_s_O_Win_Rate,
    Diff_H2H_s_Set_Win_Rate = H2H_s_F_Set_Win_Rate - H2H_s_O_Set_Win_Rate,
    Diff_H2H_s_Games_Win_Rate = H2H_s_F_Games_Win_Rate - H2H_s_O_Games_Win_Rate,
    
    Diff_H2H_3Y = H2H_F_W_3Y - H2H_O_W_3Y,
    Diff_H2H_set_3Y = H2H_F_Set_Won_3Y - H2H_O_Set_Won_3Y,
    Diff_H2H_games_3Y = H2H_F_Games_Won_3Y - H2H_O_Games_Won_3Y,
    
    Diff_H2H_Win_Rate_3Y = H2H_F_Win_Rate_3Y - H2H_O_Win_Rate_3Y,
    Diff_H2H_Set_Win_Rate_3Y = H2H_F_Set_Win_Rate_3Y - H2H_O_Set_Win_Rate_3Y,
    Diff_H2H_Games_Win_Rate_3Y = H2H_F_Games_Win_Rate_3Y - H2H_O_Games_Win_Rate_3Y,
    
    Diff_H2H_s_3Y =H2H_s_F_W_3Y - H2H_s_O_W_3Y,
    Diff_H2H_s_set_3Y = H2H_s_F_Set_Won_3Y - H2H_s_O_Set_Won_3Y,
    Diff_H2H_s_games_3Y = H2H_s_F_Games_Won_3Y - H2H_s_O_Games_Won_3Y,
    
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
    Diff_as_Out_4_Win_rate = F_as_Out_4_Win_rate - O_as_Out_4_Win_rate,
    
    # --- DIFFERENCE - Fatigue
    Diff_Fatigue = F_Fatigue_index - O_Fatigue_index,
    
    # --- DIFFERENCE - en match
    
    Diff_N_Win_4 = F_N_Win_4-O_N_Win_4,
    Diff_N_Win_s_4 = F_N_Win_s_4-O_N_Win_s_4,
    Diff_N_Win_12 = F_N_Win_12-O_N_Win_12,
    Diff_N_Win_s_12 = F_N_Win_s_12-O_N_Win_s_12,
    
    Diff_N_Loss_4 = F_N_Loss_4-O_N_Loss_4,
    Diff_N_Loss_s_4 = F_N_Loss_s_4-O_N_Loss_s_4,
    Diff_N_Loss_12 = F_N_Loss_12-O_N_Loss_12,
    Diff_N_Loss_s_12 = F_N_Loss_s_12-O_N_Loss_s_12
    
  ) %>% 
  select(
    # --- Identification & Backtest (non-features) ---
    tournament, Season, Date, Favori, Outsider, 
    Odd_F, Odd_O,
    
    # --- VARIABLE CIBLE ---
    Issue, 
    
    # --- Caractéristiques du Match ---
    Categorie, Categorie2, Surface_tournament, Round,
    Grand_Slam,
    Country_Champ,
    Masters,
    ATP_1000,
    ATP_500,
    ATP_250,
    Round_RR, Round_R1, Round_R2, Round_R3, Round_R16, Round_QF, Round_SF, Round_F,
    
    # --- DIFFÉRENCES - Profils Joueurs ---
    Diff_Rank_Class,
    Diff_Rank,
    Diff_Points,
    Diff_Score_Rank,
    Diff_Age,
    Diff_IMC,
    Diff_Size,
    Diff_Weight,
    Diff_Hand_Score,
    Diff_Country_score,
    
    # --- DIFFÉRENCES - ELO & Probabilités ---
    P_F,
    P_s_F,
    P_F_comb,
    Diff_Elo,
    Diff_Elo_s,
    
    # --- DIFFÉRENCES - H2H (Historique Face à Face) ---
    
    Diff_H2H,
    Diff_H2H_s,
    Diff_H2H_3Y,
    Diff_H2H_s_3Y,
    Diff_H2H_s_set_3Y,
    Diff_H2H_s_games_3Y,
    Diff_H2H_set_3Y,
    Diff_H2H_games_3Y,
    Diff_H2H_set_s,
    Diff_H2H_games_s,
    Diff_H2H_set,
    Diff_H2H_games,
    
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
    Diff_as_Out_4_Win_rate,
    
    # --- DIFFERENCE - Fatigue
    Diff_Fatigue ,
    
    Diff_N_Win_4,
    Diff_N_Win_s_4,
    Diff_N_Win_12,
    Diff_N_Win_s_12 ,
    
    Diff_N_Loss_4,
    Diff_N_Loss_s_4,
    Diff_N_Loss_12,
    Diff_N_Loss_s_12
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
    
    Mom_Fatigue = sign(Diff_Fatigue),
  ) %>%
  mutate(
    Mom_H2H_global = Mom_H2H+Mom_H2H_s+Mom_H2H_3Y+Mom_H2H_s_3Y,
    Mom_WR_global = Mom_WR_4+Mom_WR_s_4+Mom_WR_12+Mom_WR_s_12,
    Mom_as_Fav = Mom_as_Fav_12+Mom_as_Fav_4,
    Mom_as_Out = Mom_as_Out_4+Mom_as_Out_12,  
    Clay = ifelse(Surface_tournament=="Clay",1,0),
    Hard = ifelse(Surface_tournament=="Hard",1,0),
    Grass = ifelse(Surface_tournament=="Grass",1,0),
    Indoors = ifelse(Surface_tournament=="Indoors",1,0)
  )

##### DATA COMPLEMENTAIRES #####

load(paste0(getwd(),"/Scrapping tennis data/ML_engenering/V_MATCH_HIST.RData"))

plan(multisession, workers = 25)

rm(list=setdiff(ls(),c("V_MATCH_HIST","TABLE_MOMENTUM")))

source(paste0(getwd(),"/Scrapping tennis data/Stat function.R"))

Start=Sys.time()

Test = TABLE_MOMENTUM %>% 
  mutate(
    Fav_Q = future_pmap_chr(
      list(
        Player_name = Favori,
        Date_match  = Date,
        tournoi     = tournament,
        Year        = Season
      ),
      is_qualies
    ),
    
    Out_Q = future_pmap_chr(
      list(
        Player_name = Outsider,
        Date_match  = Date,
        tournoi     = tournament,
        Year        = Season
      ),
      is_qualies
    ) ,
    
    Fav_Giant_Killer = future_pmap_int(
      list(
        Player_name = Favori,
        Date_match  = Date
      ),
      is_giant_killer
    ),
    
    Out_Giant_Killer = future_pmap_int(
      list(
        Player_name = Outsider,
        Date_match  = Date
      ),
      is_giant_killer
    ),
    
    Fav_is_finalist = future_pmap_int(
      list(
        Player_name = Favori,
        Date_match  = Date
      ),
      is_finalist
    ),
    
    Out_is_finalist = future_pmap_dbl(
      list(
        Player_name = Outsider,
        Date_match  = Date
      ),
      is_finalist
    ) ,
    
    Fav_WR_career = future_pmap_dbl(
      list(
        Player_name = Favori,
        Date_match  = Date,
        Categ = Categorie
      ),
      get_player_win_rate
    ) ,
    
    Out_WR_career = future_pmap_dbl(
      list(
        Player_name = Outsider,
        Date_match  = Date,
        Categ = Categorie
      ),
      get_player_win_rate
    ) ,
    
    Fav_WR_career_surface = future_pmap_dbl(
      list(
        Player_name = Favori,
        Date_match  = Date,
        Surf = Surface_tournament
      ),
      get_player_win_rate_surface
    ) ,
    
    Out_WR_career_surface = future_pmap_dbl(
      list(
        Player_name = Outsider,
        Date_match  = Date,
        Surf = Surface_tournament
      ),
      get_player_win_rate_surface
    ) ,
    
    Fav_best_run = future_pmap_chr(
      list(
        Player_name = Favori,
        Date_match  = Date,
        Categ     = Categorie
      ),
      get_player_best_run
    ),
    
    Out_best_run = future_pmap_chr(
      list(
        Player_name = Outsider,
        Date_match  = Date,
        Categ     = Categorie
      ),
      get_player_best_run
    )
    
  )


End=Sys.time()-Start

End

score_map <- c("0" = 0, "LL" = 1, "Q" = 2)

score_map_round <- c(
  "-"  = 1,  
  "1R" = 1,
  "2R" = 2,
  "3R" = 3,
  "R16"= 4,
  "QF" = 5,
  "SF" = 6,
  "F"  = 7,
  "W"  = 8
)


TABLE_MOMENTUM=Test %>% 
  mutate(Diff_Q = score_map[Fav_Q] - score_map[Out_Q],
         Diff_Giant_Kill = Fav_Giant_Killer - Out_Giant_Killer,
         Diff_Final=Fav_is_finalist - Out_is_finalist,
         Diff_Run = coalesce(score_map_round[Fav_best_run],0)-coalesce(score_map_round[Out_best_run],0),
         Diff_WR = coalesce(Fav_WR_career,0) - coalesce(Out_WR_career,0),
         Diff_WR_Surface = coalesce(Fav_WR_career_surface,0) - coalesce(Out_WR_career_surface,0),
         
         Mom_Q = sign(Diff_Q),
         Mom_Giant_Kill = sign(Diff_Giant_Kill),
         Mom_Final=sign(Diff_Final),
         Mom_Run = sign(Diff_Run),
         Mom_WR = sign(Diff_WR),
         Mom_WR_surface = sign(Diff_WR_Surface),
         Odd_S = ifelse(Odd_F>=Odd_O,1,0),
         Mom_Physical=sign(Diff_Size)+sign(Diff_Age)+sign(Diff_Weight),
         Mom_Form=sign(Diff_Win_Rate_4)+sign(Diff_Win_Rate_12)+sign(Diff_Final)+sign(Diff_Fatigue),
         Mom_Career=sign(Diff_Run)+sign(Diff_WR)+sign(Diff_WR_Surface),
         Mom_Mental=sign(Diff_H2H)+sign(Diff_H2H_3Y)+sign(Diff_as_Out_12_Win_rate)+sign(Diff_as_Fav_12_Win_rate)) %>% 
  select(-c("Fav_WR_career","Out_WR_career",        
            "Fav_WR_career_surface","Out_WR_career_surface",
            "Fav_best_run" ,"Out_best_run",
            "Fav_Q","Out_Q",
            "Fav_Giant_Killer","Out_Giant_Killer",
            "Fav_is_finalist","Out_is_finalist"))

closeAllConnections()
