library(tidyverse)
library(progress)

load("~/work/Tennis-Data/Scrapping tennis data/MATCH_STATS.RData")

p=0.5

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
    
    # Momentum Favori
    H2H_F_Momentum = case_when(H2H_F_Win_Rate_3Y > H2H_F_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_F_Momentum = case_when(H2H_s_F_Win_Rate_3Y > H2H_s_F_Win_Rate ~ 1, TRUE ~ 0),
    
    H2H_F_Set_Momentum = case_when(H2H_F_Set_Win_Rate_3Y > H2H_F_Set_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_F_Set_Momentum = case_when(H2H_s_F_Set_Win_Rate_3Y > H2H_s_F_Set_Win_Rate ~ 1, TRUE ~ 0),
    
    H2H_F_Games_Momentum = case_when(H2H_F_Games_Win_Rate_3Y > H2H_F_Games_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_F_Games_Momentum = case_when(H2H_s_F_Games_Win_Rate_3Y > H2H_s_F_Games_Win_Rate ~ 1, TRUE ~ 0),
    
    # Momentum Outsider
    H2H_O_Momentum = case_when(H2H_O_Win_Rate_3Y > H2H_O_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_O_Momentum = case_when(H2H_s_O_Win_Rate_3Y > H2H_s_O_Win_Rate ~ 1, TRUE ~ 0),
    
    H2H_O_Set_Momentum = case_when(H2H_O_Set_Win_Rate_3Y > H2H_O_Set_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_O_Set_Momentum = case_when(H2H_s_O_Set_Win_Rate_3Y > H2H_s_O_Set_Win_Rate ~ 1, TRUE ~ 0),
    
    H2H_O_Games_Momentum = case_when(H2H_O_Games_Win_Rate_3Y > H2H_O_Games_Win_Rate ~ 1, TRUE ~ 0),
    H2H_s_O_Games_Momentum = case_when(H2H_s_O_Games_Win_Rate_3Y > H2H_s_O_Games_Win_Rate ~ 1, TRUE ~ 0),
    
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
    
    # Momentum 
    F_Momentum = case_when(F_Win_Rate_4>F_Win_Rate_12~1,TRUE~0),
    F_Momentum_s = case_when(F_Win_Rate_s_4>F_Win_Rate_s_12~1,TRUE~0),
    O_Momentum = case_when(O_Win_Rate_4>O_Win_Rate_12~1,TRUE~0),
    O_Momentum_s = case_when(O_Win_Rate_s_4>O_Win_Rate_s_12~1,TRUE~0),
    
    
    # Categorie tournoi
    Categ_Elite = case_when(Categorie %in% c("Grand Slam","Olympics","Team")~1,TRUE~0),
    Categ_Mid = case_when(Categorie %in% c("ATP 1000","ATP 500")~1,TRUE~0),
    Categ_low = case_when(Categorie=="ATP 250"~1,TRUE~0),
    
    Round_RR = case_when(Round=="-"~1,TRUE~0),
    Round_R1 = case_when(Round=="1R"~1,TRUE~0),
    Round_R2 = case_when(Round=="2R"~1,TRUE~0),
    Round_R3 = case_when(Round=="3R"~1,TRUE~0),
    Round_R16 = case_when(Round=="R16"~1,TRUE~0),
    Round_QF = case_when(Round=="QF"~1,TRUE~0),
    Round_SF = case_when(Round=="SF"~1,TRUE~0),
    Round_F = case_when(Round=="F"~1,TRUE~0),
    
    # Variable mixte
    
    Categ_Round = paste0(Categorie,"_",Round)
    
  ) %>% 
  filter(info=="Completed") %>% 
  select(
    tournament, Season, Date, Week_tournament, Categorie, Surface_tournament, Round,
    Favori, Outsider, Winner_id, Issue,
    Rank_F, Points_F, Odd_F,
    Rank_O, Points_O, Odd_O,
    Country_F, Country_O, 
    Size_F, Size_O, Weight_F, Weight_O,
    IMC_F, IMC_O,
    Birth_date_F, Birth_date_O, Age_F, Age_O, 
    Hand_F, Hand_O, Hand_Score_F, Hand_Score_O,
    Elo_F, Elo_s_F, Elo_O, Elo_s_O, 
    P_F, P_s_F, P_O, P_s_O, P_F_comb,
    
    # H2H historique complet
    H2H_F_W, H2H_O_W, 
    H2H_F_Set_Won, H2H_O_Set_Won,
    H2H_F_Games_Won, H2H_O_Games_Won,
    H2H_F_Win_Rate, H2H_O_Win_Rate,
    H2H_F_Set_Win_Rate, H2H_O_Set_Win_Rate,
    H2H_F_Games_Win_Rate, H2H_O_Games_Win_Rate,
    
    # H2H même surface
    H2H_s_F_W, H2H_s_O_W, 
    H2H_s_F_Set_Won, H2H_s_O_Set_Won,
    H2H_s_F_Games_Won, H2H_s_O_Games_Won,
    H2H_s_F_Win_Rate, H2H_s_O_Win_Rate,
    H2H_s_F_Set_Win_Rate, H2H_s_O_Set_Win_Rate,
    H2H_s_F_Games_Win_Rate, H2H_s_O_Games_Win_Rate,
    
    # H2H 3 dernières années
    H2H_F_W_3Y, H2H_O_W_3Y, 
    H2H_F_Set_Won_3Y, H2H_O_Set_Won_3Y,
    H2H_F_Games_Won_3Y, H2H_O_Games_Won_3Y,
    H2H_F_Win_Rate_3Y, H2H_O_Win_Rate_3Y,
    H2H_F_Set_Win_Rate_3Y, H2H_O_Set_Win_Rate_3Y,
    H2H_F_Games_Win_Rate_3Y, H2H_O_Games_Win_Rate_3Y,
    
    # H2H même surface 3 dernières années
    H2H_s_F_W_3Y, H2H_s_O_W_3Y, 
    H2H_s_F_Set_Won_3Y, H2H_s_O_Set_Won_3Y,
    H2H_s_F_Games_Won_3Y, H2H_s_O_Games_Won_3Y,
    H2H_s_F_Win_Rate_3Y, H2H_s_O_Win_Rate_3Y,
    H2H_s_F_Set_Win_Rate_3Y, H2H_s_O_Set_Win_Rate_3Y,
    H2H_s_F_Games_Win_Rate_3Y, H2H_s_O_Games_Win_Rate_3Y,
    
    # Momentum H2H
    H2H_F_Momentum, H2H_s_F_Momentum,
    H2H_F_Set_Momentum, H2H_s_F_Set_Momentum,
    H2H_F_Games_Momentum, H2H_s_F_Games_Momentum,
    H2H_O_Momentum, H2H_s_O_Momentum,
    H2H_O_Set_Momentum, H2H_s_O_Set_Momentum,
    H2H_O_Games_Momentum, H2H_s_O_Games_Momentum,
    
    # Forme récente 4 semaines
    F_N_Win_4, O_N_Win_4, F_N_Loss_4, O_N_Loss_4,
    F_Win_Rate_4, O_Win_Rate_4,
    
    # Forme récente surface 4 semaines
    F_N_Win_s_4, O_N_Win_s_4, F_N_Loss_s_4, O_N_Loss_s_4,
    F_Win_Rate_s_4, O_Win_Rate_s_4,
    
    # Forme récente 12 semaines
    F_N_Win_12, O_N_Win_12, F_N_Loss_12, O_N_Loss_12,
    F_Win_Rate_12, O_Win_Rate_12,
    
    # Forme récente surface 12 semaines
    F_N_Win_s_12, O_N_Win_s_12, F_N_Loss_s_12, O_N_Loss_s_12,
    F_Win_Rate_s_12, O_Win_Rate_s_12,
    
    # Momentum forme
    F_Momentum, F_Momentum_s, O_Momentum, O_Momentum_s,
    
    # Info stade match
    Categ_Elite,Categ_Mid,Categ_low,Round_RR,Round_R1,Round_R2,Round_R3,Round_R16,Round_QF,Round_SF,Round_F,
    
    Categ_Round
    
  )


##### TABLE ML DIFF #####

# TABLE_ML_DIFF - Différences entre Favori et Outsider
TABLE_ML_DIFF = TABLE %>%
  mutate(
    
    Issue = as.factor(Issue),
    
    # Différences de classement et points
    Diff_Rank = abs(Rank_F - Rank_O),
    Diff_Points = log(Points_F) - log(Points_O),
    
    S_Odd = case_when(Odd_F >= 1.9 ~ 1, TRUE ~ 0),
    
    # Différences physiques
    Diff_Size = Size_F - Size_O,
    Diff_Weight = Weight_F - Weight_O,
    Diff_IMC = IMC_F - IMC_O,
    Diff_Age = Age_F - Age_O,
    Diff_Log_Age = log(Age_F) - log(Age_O),
    Diff_Hand_Score = Hand_Score_F - Hand_Score_O,

    
    # Différences H2H - Historique complet
    Diff_H2H_W = H2H_F_W - H2H_O_W,
    Diff_H2H_Set_Won = H2H_F_Set_Won - H2H_O_Set_Won,
    Diff_H2H_Games_Won = H2H_F_Games_Won - H2H_O_Games_Won,
    Diff_H2H_Win_Rate = H2H_F_Win_Rate - H2H_O_Win_Rate,
    Diff_H2H_Set_Win_Rate = H2H_F_Set_Win_Rate - H2H_O_Set_Win_Rate,
    Diff_H2H_Games_Win_Rate = H2H_F_Games_Win_Rate - H2H_O_Games_Win_Rate,
    
    # Différences H2H - Même surface
    Diff_H2H_s_W = H2H_s_F_W - H2H_s_O_W,
    Diff_H2H_s_Set_Won = H2H_s_F_Set_Won - H2H_s_O_Set_Won,
    Diff_H2H_s_Games_Won = H2H_s_F_Games_Won - H2H_s_O_Games_Won,
    Diff_H2H_s_Win_Rate = H2H_s_F_Win_Rate - H2H_s_O_Win_Rate,
    Diff_H2H_s_Set_Win_Rate = H2H_s_F_Set_Win_Rate - H2H_s_O_Set_Win_Rate,
    Diff_H2H_s_Games_Win_Rate = H2H_s_F_Games_Win_Rate - H2H_s_O_Games_Win_Rate,
    
    # Différences H2H - 3 dernières années
    Diff_H2H_W_3Y = H2H_F_W_3Y - H2H_O_W_3Y,
    Diff_H2H_Set_Won_3Y = H2H_F_Set_Won_3Y - H2H_O_Set_Won_3Y,
    Diff_H2H_Games_Won_3Y = H2H_F_Games_Won_3Y - H2H_O_Games_Won_3Y,
    Diff_H2H_Win_Rate_3Y = H2H_F_Win_Rate_3Y - H2H_O_Win_Rate_3Y,
    Diff_H2H_Set_Win_Rate_3Y = H2H_F_Set_Win_Rate_3Y - H2H_O_Set_Win_Rate_3Y,
    Diff_H2H_Games_Win_Rate_3Y = H2H_F_Games_Win_Rate_3Y - H2H_O_Games_Win_Rate_3Y,
    
    # Différences H2H - Même surface, 3 dernières années
    Diff_H2H_s_W_3Y = H2H_s_F_W_3Y - H2H_s_O_W_3Y,
    Diff_H2H_s_Set_Won_3Y = H2H_s_F_Set_Won_3Y - H2H_s_O_Set_Won_3Y,
    Diff_H2H_s_Games_Won_3Y = H2H_s_F_Games_Won_3Y - H2H_s_O_Games_Won_3Y,
    Diff_H2H_s_Win_Rate_3Y = H2H_s_F_Win_Rate_3Y - H2H_s_O_Win_Rate_3Y,
    Diff_H2H_s_Set_Win_Rate_3Y = H2H_s_F_Set_Win_Rate_3Y - H2H_s_O_Set_Win_Rate_3Y,
    Diff_H2H_s_Games_Win_Rate_3Y = H2H_s_F_Games_Win_Rate_3Y - H2H_s_O_Games_Win_Rate_3Y,
    
    # Différences Momentum H2H (dummy)
    Diff_H2H_Momentum = H2H_F_Momentum - H2H_O_Momentum,
    Diff_H2H_s_Momentum = H2H_s_F_Momentum - H2H_s_O_Momentum,
    Diff_H2H_Set_Momentum = H2H_F_Set_Momentum - H2H_O_Set_Momentum,
    Diff_H2H_s_Set_Momentum = H2H_s_F_Set_Momentum - H2H_s_O_Set_Momentum,
    Diff_H2H_Games_Momentum = H2H_F_Games_Momentum - H2H_O_Games_Momentum,
    Diff_H2H_s_Games_Momentum = H2H_s_F_Games_Momentum - H2H_s_O_Games_Momentum,
    
    # First_meeting
    
    First_meeting = case_when((H2H_F_W+H2H_O_W)==0~1,TRUE~0),
    First_meeting_s = case_when((H2H_s_F_W+H2H_s_O_W)==0~1,TRUE~0),
    
    # Différences forme récente - 4 derniers matchs
    Diff_N_Win_4 = F_N_Win_4 - O_N_Win_4,
    Diff_N_Loss_4 = F_N_Loss_4 - O_N_Loss_4,
    Diff_Win_Rate_4 = F_Win_Rate_4 - O_Win_Rate_4,
    
    # Différences forme récente sur surface - 4 derniers matchs
    Diff_N_Win_s_4 = F_N_Win_s_4 - O_N_Win_s_4,
    Diff_N_Loss_s_4 = F_N_Loss_s_4 - O_N_Loss_s_4,
    Diff_Win_Rate_s_4 = F_Win_Rate_s_4 - O_Win_Rate_s_4,
    
    # Différences forme récente - 12 derniers matchs
    Diff_N_Win_12 = F_N_Win_12 - O_N_Win_12,
    Diff_N_Loss_12 = F_N_Loss_12 - O_N_Loss_12,
    Diff_Win_Rate_12 = F_Win_Rate_12 - O_Win_Rate_12,
    
    # Différences forme récente sur surface - 12 derniers matchs
    Diff_N_Win_s_12 = F_N_Win_s_12 - O_N_Win_s_12,
    Diff_N_Loss_s_12 = F_N_Loss_s_12 - O_N_Loss_s_12,
    Diff_Win_Rate_s_12 = F_Win_Rate_s_12 - O_Win_Rate_s_12,
    
    # Indic_play
    Indic_F_play_4 = case_when((F_N_Win_4+F_N_Loss_4)==0~1,TRUE~0),
    Indic_O_play_4 = case_when((O_N_Win_4+O_N_Loss_4)==0~1,TRUE~0),
    Indic_F_play_12 = case_when((F_N_Win_12+F_N_Loss_12)==0~1,TRUE~0),
    Indic_O_play_12 = case_when((O_N_Win_12+O_N_Loss_12)==0~1,TRUE~0),
    
    # Diff_Play
    
    Diff_Indic_Play_4 = Indic_F_play_4 - Indic_O_play_4,
    Diff_Indic_Play_12 = Indic_F_play_12 - Indic_O_play_12,
    
    # Différences Momentum forme (dummy)
    Diff_Momentum = F_Momentum - O_Momentum,
    Diff_Momentum_s = F_Momentum_s - O_Momentum_s
    
  ) %>%
  select(
    tournament, Season, Date, Week_tournament, Categorie, Surface_tournament, Round,
    Favori, Outsider, Winner_id, Issue, Odd_F, Odd_O, S_Odd,
    Country_F, Country_O, Hand_F, Hand_O,
    
    # Différences physiques et classement
    Diff_Rank, Diff_Points,
    Diff_Size, Diff_Weight, Diff_IMC, 
    Diff_Age, Diff_Log_Age, 
    Diff_Hand_Score,
    
    # Différences ELO et proba
     P_F,P_s_F,P_F_comb,
    
    # Différences H2H historique complet
    Diff_H2H_W, Diff_H2H_Set_Won, Diff_H2H_Games_Won,
    Diff_H2H_Win_Rate, Diff_H2H_Set_Win_Rate, Diff_H2H_Games_Win_Rate,
    
    # Différences H2H même surface
    Diff_H2H_s_W, Diff_H2H_s_Set_Won, Diff_H2H_s_Games_Won,
    Diff_H2H_s_Win_Rate, Diff_H2H_s_Set_Win_Rate, Diff_H2H_s_Games_Win_Rate,
    
    # Différences H2H 3 ans
    Diff_H2H_W_3Y, Diff_H2H_Set_Won_3Y, Diff_H2H_Games_Won_3Y,
    Diff_H2H_Win_Rate_3Y, Diff_H2H_Set_Win_Rate_3Y, Diff_H2H_Games_Win_Rate_3Y,
    
    # Différences H2H même surface 3 ans
    Diff_H2H_s_W_3Y, Diff_H2H_s_Set_Won_3Y, Diff_H2H_s_Games_Won_3Y,
    Diff_H2H_s_Win_Rate_3Y, Diff_H2H_s_Set_Win_Rate_3Y, Diff_H2H_s_Games_Win_Rate_3Y,
    
    # Différences Momentum H2H
    Diff_H2H_Momentum, Diff_H2H_s_Momentum,
    Diff_H2H_Set_Momentum, Diff_H2H_s_Set_Momentum,
    Diff_H2H_Games_Momentum, Diff_H2H_s_Games_Momentum,
    
    First_meeting,First_meeting_s,
    
    # Différences forme 4 semaines
    Diff_N_Win_4, Diff_N_Loss_4, Diff_Win_Rate_4,
    
    # Différences forme surface 4 semaines
    Diff_N_Win_s_4, Diff_N_Loss_s_4, Diff_Win_Rate_s_4,
    
    # Différences forme 12 semaines
    Diff_N_Win_12, Diff_N_Loss_12, Diff_Win_Rate_12,
    
    # Différences forme surface 12 semaines
    Diff_N_Win_s_12, Diff_N_Loss_s_12, Diff_Win_Rate_s_12,
    
    # Différences Momentum forme
    Diff_Momentum, Diff_Momentum_s,
    
    Diff_Indic_Play_4, Diff_Indic_Play_12,
    # Info stade match
    Categ_Elite,Categ_Mid,Categ_low,Round_RR,Round_R1,Round_R2,Round_R3,Round_R16,Round_QF,Round_SF,Round_F
    
    #,Categ_Round
  )



