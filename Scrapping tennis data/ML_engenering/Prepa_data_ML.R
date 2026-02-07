library(tidyverse)
library(progress)

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"))

last_elo=function(base,player_name,surface="all",Date_match,tournoi){
  
  if (surface=="all"){
    
    
    elo_player=base %>% 
      filter(Player_name==player_name & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player)
    
    return(elo_player)
    
  }else if (surface=="Grass"){
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_grass) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_grass)
    
    return(elo_player)
    
  }else if (surface=="Clay"){
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_clay) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_clay)
    
    return(elo_player)
    
  }else{
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_hard) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_hard)
    
    return(elo_player)
    
  }
}

penalty=function(diff_date){
  
  penalty=ifelse(between(diff_date,60,80),0.7,
                 ifelse(between(diff_date,81,180),0.85,
                        ifelse(diff_date>180,1,0)))
  
  return(penalty)
}

pb= progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = nrow(V_MATCH_t),
  clear = FALSE,
  width = 60
)

V_MATCH_t$Elo_W=NA
V_MATCH_t$Elo_L=NA
V_MATCH_t$Elo_W_surface=NA
V_MATCH_t$Elo_L_surface=NA

for (i in 1:nrow(V_MATCH_t)){
  
  P1=V_MATCH_t$Winner_id[i]
  
  P2=V_MATCH_t$Loser_id[i]
  
  surface=V_MATCH_t$Surface_tournament[i]
  
  Date_match=V_MATCH_t$Date[i]
  
  tournoi=V_MATCH_t$tournament[i]
  
  ##### ELO CLASSIC #####
  
  last_elo_p1=last_elo(ELO_RATING_PLAYERS,P1,"all",Date_match,tournoi)
  
  last_elo_p2=last_elo(ELO_RATING_PLAYERS,P2,"all",Date_match,tournoi)
  
  diff_date_p1=as.numeric(Date_match-last_elo_p1$Date)
  
  diff_date_p2=as.numeric(Date_match-last_elo_p2$Date)
  
  covid=ifelse(between(Date_match,as.Date("2020-08-01"),as.Date("2021-02-01")) ,0,1)
  
  elo_p1=ifelse(is_empty(last_elo_p1$Elo_player)==T,1500,last_elo_p1$Elo_player-(penalty(diff_date_p1)*covid*100))
  
  elo_p2=ifelse(is_empty(last_elo_p2$Elo_player)==T,1500,last_elo_p2$Elo_player-(penalty(diff_date_p2)*covid*100))
  
  ##### ELO SURFACE #####
  
  if (surface %in% c("Indoors","Various")){
    
    surface="Hard"
    
  }else{
    
    surface=surface
  }
  
  last_elo_p1_surface=last_elo(ELO_RATING_PLAYERS,P1,surface,Date_match,tournoi) %>% pull(4)
  
  last_elo_p2_surface=last_elo(ELO_RATING_PLAYERS,P2,surface,Date_match,tournoi) %>% pull(4)
  
  elo_p1_surface=ifelse(is_empty(last_elo_p1_surface)==T,1500,last_elo_p1_surface)
  
  elo_p2_surface=ifelse(is_empty(last_elo_p2_surface)==T,1500,last_elo_p2_surface)
  
  ##### assigniation
  
  V_MATCH_t$Elo_W[i]=elo_p1
  V_MATCH_t$Elo_L[i]=elo_p2
  V_MATCH_t$Elo_W_surface[i]=elo_p1_surface
  V_MATCH_t$Elo_L_surface[i]=elo_p2_surface
  
  pb$tick()
  
}

proba_calcul=function(elo_p1,elo_p2){
  
  proba=1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400))
  
  return(proba)
}

proba_calcul(1821.4,1651.5)

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
    
    # H2H - Même surface
    H2H_s_F_W = case_when(Rank_W < Rank_L ~ H2H_s_Winner_W, TRUE ~ H2H_s_Loser_W),
    H2H_s_O_W = case_when(Rank_L < Rank_W ~ H2H_s_Winner_W, TRUE ~ H2H_s_Loser_W),
    H2H_s_F_Set_Won = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Set_Won, TRUE ~ H2H_s_Loser_Set_Won),
    H2H_s_O_Set_Won = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Set_Won, TRUE ~ H2H_s_Loser_Set_Won),
    H2H_s_F_Games_Won = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Games_Won, TRUE ~ H2H_s_Loser_Games_Won),
    H2H_s_O_Games_Won = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Games_Won, TRUE ~ H2H_s_Loser_Games_Won),
    
    # H2H - 3 dernières années
    H2H_F_W_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_W_3Y, TRUE ~ H2H_Loser_W_3Y),
    H2H_O_W_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_W_3Y, TRUE ~ H2H_Loser_W_3Y),
    H2H_F_Set_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_Set_Won_3Y, TRUE ~ H2H_Loser_Set_Won_3Y),
    H2H_O_Set_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_Set_Won_3Y, TRUE ~ H2H_Loser_Set_Won_3Y),
    H2H_F_Games_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_Winner_Games_Won_3Y, TRUE ~ H2H_Loser_Games_Won_3Y),
    H2H_O_Games_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_Winner_Games_Won_3Y, TRUE ~ H2H_Loser_Games_Won_3Y),
    
    # H2H - Même surface, 3 dernières années
    H2H_s_F_W_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_W_3Y, TRUE ~ H2H_s_Loser_W_3Y),
    H2H_s_O_W_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_W_3Y, TRUE ~ H2H_s_Loser_W_3Y),
    H2H_s_F_Set_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Set_Won_3Y, TRUE ~ H2H_s_Loser_Set_Won_3Y),
    H2H_s_O_Set_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Set_Won_3Y, TRUE ~ H2H_s_Loser_Set_Won_3Y),
    H2H_s_F_Games_Won_3Y = case_when(Rank_W < Rank_L ~ H2H_s_Winner_Games_Won_3Y, TRUE ~ H2H_s_Loser_Games_Won_3Y),
    H2H_s_O_Games_Won_3Y = case_when(Rank_L < Rank_W ~ H2H_s_Winner_Games_Won_3Y, TRUE ~ H2H_s_Loser_Games_Won_3Y),
    
    # Forme récente - 4 derniers matchs
    F_N_Win_4 = case_when(Rank_W < Rank_L ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    O_N_Win_4 = case_when(Rank_L < Rank_W ~ Winner_N_Win_4, TRUE ~ Loser_N_Win_4),
    F_N_Loss_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    O_N_Loss_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_4, TRUE ~ Loser_N_Loss_4),
    
    # Forme récente sur surface - 4 derniers matchs
    F_N_Win_s_4 = case_when(Rank_W < Rank_L ~ Winner_N_Win_s_4, TRUE ~ Loser_N_Win_s_4),
    O_N_Win_s_4 = case_when(Rank_L < Rank_W ~ Winner_N_Win_s_4, TRUE ~ Loser_N_Win_s_4),
    F_N_Loss_s_4 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_s_4, TRUE ~ Loser_N_Loss_s_4),
    O_N_Loss_s_4 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_s_4, TRUE ~ Loser_N_Loss_s_4),
    
    # Forme récente - 12 derniers matchs
    F_N_Win_12 = case_when(Rank_W < Rank_L ~ Winner_N_Win_12, TRUE ~ Loser_N_Win_12),
    O_N_Win_12 = case_when(Rank_L < Rank_W ~ Winner_N_Win_12, TRUE ~ Loser_N_Win_12),
    F_N_Loss_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_12, TRUE ~ Loser_N_Loss_12),
    O_N_Loss_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_12, TRUE ~ Loser_N_Loss_12),
    
    # Forme récente sur surface - 12 derniers matchs
    F_N_Win_s_12 = case_when(Rank_W < Rank_L ~ Winner_N_Win_s_12, TRUE ~ Loser_N_Win_s_12),
    O_N_Win_s_12 = case_when(Rank_L < Rank_W ~ Winner_N_Win_s_12, TRUE ~ Loser_N_Win_s_12),
    F_N_Loss_s_12 = case_when(Rank_W < Rank_L ~ Winner_N_Loss_s_12, TRUE ~ Loser_N_Loss_s_12),
    O_N_Loss_s_12 = case_when(Rank_L < Rank_W ~ Winner_N_Loss_s_12, TRUE ~ Loser_N_Loss_s_12)
  ) %>% 
  select(
    tournament, Season, Date, Week_tournament, Categorie, Surface_tournament, Round,
    Favori, Outsider,Winner_id, Issue,
    Rank_F, Points_F, Odd_F,
    Rank_O, Points_O, Odd_O,
    Country_F, Country_O, 
    Size_F, Size_O, Weight_F, Weight_O,
    IMC_F,IMC_O,
    Birth_date_F, Birth_date_O, Age_F,Age_O,Hand_F, Hand_O,Hand_Score_F,Hand_Score_O,
    Elo_F,Elo_s_F,Elo_O,Elo_s_O,P_F,P_s_F,P_O,P_s_O,P_F_comb,
    H2H_F_W, H2H_O_W, H2H_F_Set_Won, H2H_O_Set_Won,
    H2H_F_Games_Won, H2H_O_Games_Won,
    H2H_s_F_W, H2H_s_O_W, H2H_s_F_Set_Won, H2H_s_O_Set_Won,
    H2H_s_F_Games_Won, H2H_s_O_Games_Won,
    H2H_F_W_3Y, H2H_O_W_3Y, H2H_F_Set_Won_3Y, H2H_O_Set_Won_3Y,
    H2H_F_Games_Won_3Y, H2H_O_Games_Won_3Y,
    H2H_s_F_W_3Y, H2H_s_O_W_3Y, H2H_s_F_Set_Won_3Y, H2H_s_O_Set_Won_3Y,
    H2H_s_F_Games_Won_3Y, H2H_s_O_Games_Won_3Y,
    F_N_Win_4, O_N_Win_4, F_N_Loss_4, O_N_Loss_4,
    F_N_Win_s_4, O_N_Win_s_4, F_N_Loss_s_4, O_N_Loss_s_4,
    F_N_Win_12, O_N_Win_12, F_N_Loss_12, O_N_Loss_12,
    F_N_Win_s_12, O_N_Win_s_12, F_N_Loss_s_12, O_N_Loss_s_12
  )


TABLE_ML=TABLE %>%
  mutate(
    
    Issue = as.factor(Issue),
    
    # Différences de classement et points
    Diff_Rank = abs(Rank_F - Rank_O),
    Diff_Points = log(Points_F) - log(Points_O),
    
    S_Odd = case_when(Odd_F>=1.9~1,TRUE~0),
    # Différences physiques
    Diff_Size = Size_F - Size_O,
    Diff_Weight = Weight_F - Weight_O,
    Diff_IMC = IMC_F - IMC_O,
    Diff_Age = Age_F - Age_O,
    Diff_Log_Age = log(Age_F) - log(Age_O),
    Diff_Dist_Peak_Age = abs(Age_F - 26) - abs(Age_O - 26),
    Diff_Hand_Score = Hand_Score_F - Hand_Score_O,

    
    # Différences H2H - Historique complet
    Diff_H2H_W = H2H_F_W - H2H_O_W,
    Diff_H2H_Set_Won = H2H_F_Set_Won - H2H_O_Set_Won,
    Diff_H2H_Games_Won = H2H_F_Games_Won - H2H_O_Games_Won,
    
    # Différences H2H - Même surface
    Diff_H2H_s_W = H2H_s_F_W - H2H_s_O_W,
    Diff_H2H_s_Set_Won = H2H_s_F_Set_Won - H2H_s_O_Set_Won,
    Diff_H2H_s_Games_Won = H2H_s_F_Games_Won - H2H_s_O_Games_Won,
    
    # Différences H2H - 3 dernières années
    Diff_H2H_W_3Y = H2H_F_W_3Y - H2H_O_W_3Y,
    Diff_H2H_Set_Won_3Y = H2H_F_Set_Won_3Y - H2H_O_Set_Won_3Y,
    Diff_H2H_Games_Won_3Y = H2H_F_Games_Won_3Y - H2H_O_Games_Won_3Y,
    
    # Différences H2H - Même surface, 3 dernières années
    Diff_H2H_s_W_3Y = H2H_s_F_W_3Y - H2H_s_O_W_3Y,
    Diff_H2H_s_Set_Won_3Y = H2H_s_F_Set_Won_3Y - H2H_s_O_Set_Won_3Y,
    Diff_H2H_s_Games_Won_3Y = H2H_s_F_Games_Won_3Y - H2H_s_O_Games_Won_3Y,
    
    # Différences forme récente - 4 derniers matchs
    Diff_N_Win_4 = F_N_Win_4 - O_N_Win_4,
    Diff_N_Loss_4 = F_N_Loss_4 - O_N_Loss_4,
    
    # Différences forme récente sur surface - 4 derniers matchs
    Diff_N_Win_s_4 = F_N_Win_s_4 - O_N_Win_s_4,
    Diff_N_Loss_s_4 = F_N_Loss_s_4 - O_N_Loss_s_4,
    
    # Différences forme récente - 12 derniers matchs
    Diff_N_Win_12 = F_N_Win_12 - O_N_Win_12,
    Diff_N_Loss_12 = F_N_Loss_12 - O_N_Loss_12,
    
    # Différences forme récente sur surface - 12 derniers matchs
    Diff_N_Win_s_12 = F_N_Win_s_12 - O_N_Win_s_12,
    Diff_N_Loss_s_12 = F_N_Loss_s_12 - O_N_Loss_s_12
  ) %>%
  select(
    tournament, Season, Date, Week_tournament, Categorie, Surface_tournament, Round,
    Favori, Outsider,Winner_id, Issue, Odd_F,Odd_O,S_Odd,
    Country_F, Country_O, Hand_F, Hand_O,Diff_Hand_Score,
    Diff_Rank, Diff_Points,
    Diff_Size, Diff_Weight,Diff_IMC, Diff_Age,Diff_Log_Age,Diff_Dist_Peak_Age,
    P_F,P_s_F,P_F_comb,
    Diff_H2H_W, Diff_H2H_Set_Won, Diff_H2H_Games_Won,
    Diff_H2H_s_W, Diff_H2H_s_Set_Won, Diff_H2H_s_Games_Won,
    Diff_H2H_W_3Y, Diff_H2H_Set_Won_3Y, Diff_H2H_Games_Won_3Y,
    Diff_H2H_s_W_3Y, Diff_H2H_s_Set_Won_3Y, Diff_H2H_s_Games_Won_3Y,
    Diff_N_Win_4, Diff_N_Loss_4,
    Diff_N_Win_s_4, Diff_N_Loss_s_4,
    Diff_N_Win_12, Diff_N_Loss_12,
    Diff_N_Win_s_12, Diff_N_Loss_s_12
  )
