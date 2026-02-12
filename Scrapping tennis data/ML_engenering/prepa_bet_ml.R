
# Global

V_MATCH_t$H2H_Winner_W=NA
V_MATCH_t$H2H_Loser_W=NA

V_MATCH_t$H2H_Winner_Set_Won=NA
V_MATCH_t$H2H_Loser_Set_Won=NA

V_MATCH_t$H2H_Winner_Games_Won=NA
V_MATCH_t$H2H_Loser_Games_Won=NA

# 3Y

V_MATCH_t$H2H_Winner_W_3Y=NA
V_MATCH_t$H2H_Loser_W_3Y=NA

V_MATCH_t$H2H_Winner_Set_Won_3Y=NA
V_MATCH_t$H2H_Loser_Set_Won_3Y=NA

V_MATCH_t$H2H_Winner_Games_Won_3Y=NA
V_MATCH_t$H2H_Loser_Games_Won_3Y=NA

# Surface

V_MATCH_t$H2H_s_Winner_W=NA
V_MATCH_t$H2H_s_Loser_W=NA

V_MATCH_t$H2H_s_Winner_Set_Won=NA
V_MATCH_t$H2H_s_Loser_Set_Won=NA

V_MATCH_t$H2H_s_Winner_Games_Won=NA
V_MATCH_t$H2H_s_Loser_Games_Won=NA

# Surface 3Y

V_MATCH_t$H2H_s_Winner_W_3Y=NA
V_MATCH_t$H2H_s_Loser_W_3Y=NA

V_MATCH_t$H2H_s_Winner_Set_Won_3Y=NA
V_MATCH_t$H2H_s_Loser_Set_Won_3Y=NA

V_MATCH_t$H2H_s_Winner_Games_Won_3Y=NA
V_MATCH_t$H2H_s_Loser_Games_Won_3Y=NA

# Stats joueur encours global 4 dernières semaines

# 4 Weeks 

V_MATCH_t$Winner_N_Win_4=NA
V_MATCH_t$Winner_N_Loss_4=NA

V_MATCH_t$Winner_N_Win_Fav_Rank_4=NA
V_MATCH_t$Winner_N_Win_Out_Rank_4=NA
V_MATCH_t$Winner_N_Loss_Fav_Rank_4=NA
V_MATCH_t$Winner_N_Loss_Out_Rank_4=NA

V_MATCH_t$Loser_N_Win_4=NA
V_MATCH_t$Loser_N_Loss_4=NA

V_MATCH_t$Loser_N_Win_Fav_Rank_4=NA
V_MATCH_t$Loser_N_Win_Out_Rank_4=NA
V_MATCH_t$Loser_N_Loss_Fav_Rank_4=NA
V_MATCH_t$Loser_N_Loss_Out_Rank_4=NA

# 12 weeks 

V_MATCH_t$Winner_N_Win_12=NA
V_MATCH_t$Winner_N_Loss_12=NA

V_MATCH_t$Winner_N_Win_Fav_Rank_12=NA
V_MATCH_t$Winner_N_Win_Out_Rank_12=NA
V_MATCH_t$Winner_N_Loss_Fav_Rank_12=NA
V_MATCH_t$Winner_N_Loss_Out_Rank_12=NA

V_MATCH_t$Loser_N_Win_12=NA
V_MATCH_t$Loser_N_Loss_12=NA

V_MATCH_t$Loser_N_Win_Fav_Rank_12=NA
V_MATCH_t$Loser_N_Win_Out_Rank_12=NA
V_MATCH_t$Loser_N_Loss_Fav_Rank_12=NA
V_MATCH_t$Loser_N_Loss_Out_Rank_12=NA

# Surface 4 weeks 

V_MATCH_t$Winner_N_Win_s_4=NA
V_MATCH_t$Winner_N_Loss_s_4=NA

V_MATCH_t$Winner_N_Win_Fav_Rank_s_4=NA
V_MATCH_t$Winner_N_Win_Out_Rank_s_4=NA
V_MATCH_t$Winner_N_Loss_Fav_Rank_s_4=NA
V_MATCH_t$Winner_N_Loss_Out_Rank_s_4=NA

V_MATCH_t$Loser_N_Win_s_4=NA
V_MATCH_t$Loser_N_Loss_s_4=NA

V_MATCH_t$Loser_N_Win_Fav_Rank_s_4=NA
V_MATCH_t$Loser_N_Win_Out_Rank_s_4=NA
V_MATCH_t$Loser_N_Loss_Fav_Rank_s_4=NA
V_MATCH_t$Loser_N_Loss_Out_Rank_s_4=NA

# Surface 12 weeks 

V_MATCH_t$Winner_N_Win_s_12=NA
V_MATCH_t$Winner_N_Loss_s_12=NA

V_MATCH_t$Winner_N_Win_Fav_Rank_s_12=NA
V_MATCH_t$Winner_N_Win_Out_Rank_s_12=NA
V_MATCH_t$Winner_N_Loss_Fav_Rank_s_12=NA
V_MATCH_t$Winner_N_Loss_Out_Rank_s_12=NA

V_MATCH_t$Loser_N_Win_s_12=NA
V_MATCH_t$Loser_N_Loss_s_12=NA

V_MATCH_t$Loser_N_Win_Fav_Rank_s_12=NA
V_MATCH_t$Loser_N_Win_Out_Rank_s_12=NA
V_MATCH_t$Loser_N_Loss_Fav_Rank_s_12=NA
V_MATCH_t$Loser_N_Loss_Out_Rank_s_12=NA


pb= progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = nrow(V_MATCH_t),
  clear = FALSE,
  width = 60
)

#i=13454

for (i in 1:nrow(V_MATCH_t)){
  
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
  
  stat_g_3y=get_stat_h2h(result,"all",Season,tournoi,W,L,R,YB=Season-3)
  
  stat_s=get_stat_h2h(result,surface,Season,tournoi,W,L,R)
  
  stat_s_3y=get_stat_h2h(result,surface,Season,tournoi,W,L,R,YB=Season-3)
  
  # global
  
  # 4 w
  match_count_w_g_4=match_count(V_MATCH_HIST,winner_id,4,"all",Date_match)
  
  match_count_l_g_4=match_count(V_MATCH_HIST,loser_id,4,"all",Date_match)
  
  match_count_w_s_4=match_count(V_MATCH_HIST,winner_id,4,surface,Date_match)
  
  match_count_l_s_4=match_count(V_MATCH_HIST,loser_id,4,surface,Date_match)
  
  # 12 w
  
  match_count_w_g_12=match_count(V_MATCH_HIST,winner_id,12,"all",Date_match)
  
  match_count_l_g_12=match_count(V_MATCH_HIST,loser_id,12,"all",Date_match)
  
  match_count_w_s_12=match_count(V_MATCH_HIST,winner_id,12,surface,Date_match)
  
  match_count_l_s_12=match_count(V_MATCH_HIST,loser_id,12,surface,Date_match)
  
  ### Assigniation des valeurs ###
  
  # Global
  V_MATCH_t$H2H_Winner_W[i] = stat_g$W$Number_Win
  V_MATCH_t$H2H_Loser_W[i] = stat_g$L$Number_Win
  
  V_MATCH_t$H2H_Winner_Set_Won[i] = stat_g$W$Number_Set_Won
  V_MATCH_t$H2H_Loser_Set_Won[i] = stat_g$L$Number_Set_Won
  
  V_MATCH_t$H2H_Winner_Games_Won[i] = stat_g$W$Number_Games_Won
  V_MATCH_t$H2H_Loser_Games_Won[i] = stat_g$L$Number_Games_Won
  
  # Global 3Y
  V_MATCH_t$H2H_Winner_W_3Y[i] = stat_g_3y$W$Number_Win
  V_MATCH_t$H2H_Loser_W_3Y[i] = stat_g_3y$L$Number_Win
  
  V_MATCH_t$H2H_Winner_Set_Won_3Y[i] = stat_g_3y$W$Number_Set_Won
  V_MATCH_t$H2H_Loser_Set_Won_3Y[i] = stat_g_3y$L$Number_Set_Won
  
  V_MATCH_t$H2H_Winner_Games_Won_3Y[i] = stat_g_3y$W$Number_Games_Won
  V_MATCH_t$H2H_Loser_Games_Won_3Y[i] = stat_g_3y$L$Number_Games_Won
  
  # Surface
  V_MATCH_t$H2H_s_Winner_W[i] = stat_s$W$Number_Win
  V_MATCH_t$H2H_s_Loser_W[i] = stat_s$L$Number_Win
  
  V_MATCH_t$H2H_s_Winner_Set_Won[i] = stat_s$W$Number_Set_Won
  V_MATCH_t$H2H_s_Loser_Set_Won[i] = stat_s$L$Number_Set_Won
  
  V_MATCH_t$H2H_s_Winner_Games_Won[i] = stat_s$W$Number_Games_Won
  V_MATCH_t$H2H_s_Loser_Games_Won[i] = stat_s$L$Number_Games_Won
  
  # Surface 3Y
  V_MATCH_t$H2H_s_Winner_W_3Y[i] = stat_s_3y$W$Number_Win
  V_MATCH_t$H2H_s_Loser_W_3Y[i] = stat_s_3y$L$Number_Win
  
  V_MATCH_t$H2H_s_Winner_Set_Won_3Y[i] = stat_s_3y$W$Number_Set_Won
  V_MATCH_t$H2H_s_Loser_Set_Won_3Y[i] = stat_s_3y$L$Number_Set_Won
  
  V_MATCH_t$H2H_s_Winner_Games_Won_3Y[i] = stat_s_3y$W$Number_Games_Won
  V_MATCH_t$H2H_s_Loser_Games_Won_3Y[i] = stat_s_3y$L$Number_Games_Won
  
  # Stats joueur en cours global 4 semaines
  V_MATCH_t$Winner_N_Win_4[i] = match_count_w_g_4$N_Win
  V_MATCH_t$Winner_N_Loss_4[i] = match_count_w_g_4$N_Loss
  V_MATCH_t$Winner_N_Win_Fav_Rank_4[i] = match_count_w_g_4$N_Win_Fav_Rank
  V_MATCH_t$Winner_N_Win_Out_Rank_4[i] = match_count_w_g_4$N_Win_Out_Rank
  V_MATCH_t$Winner_N_Loss_Fav_Rank_4[i] = match_count_w_g_4$N_Loss_Fav_Rank
  V_MATCH_t$Winner_N_Loss_Out_Rank_4[i] = match_count_w_g_4$N_Loss_Out_Rank
  
  V_MATCH_t$Loser_N_Win_4[i] = match_count_l_g_4$N_Win
  V_MATCH_t$Loser_N_Loss_4[i] = match_count_l_g_4$N_Loss
  V_MATCH_t$Loser_N_Win_Fav_Rank_4[i] = match_count_l_g_4$N_Win_Fav_Rank
  V_MATCH_t$Loser_N_Win_Out_Rank_4[i] = match_count_l_g_4$N_Win_Out_Rank
  V_MATCH_t$Loser_N_Loss_Fav_Rank_4[i] = match_count_l_g_4$N_Loss_Fav_Rank
  V_MATCH_t$Loser_N_Loss_Out_Rank_4[i] = match_count_l_g_4$N_Loss_Out_Rank
  
  # Stats joueur en cours global 12 semaines
  V_MATCH_t$Winner_N_Win_12[i] = match_count_w_g_12$N_Win
  V_MATCH_t$Winner_N_Loss_12[i] = match_count_w_g_12$N_Loss
  V_MATCH_t$Winner_N_Win_Fav_Rank_12[i] = match_count_w_g_12$N_Win_Fav_Rank
  V_MATCH_t$Winner_N_Win_Out_Rank_12[i] = match_count_w_g_12$N_Win_Out_Rank
  V_MATCH_t$Winner_N_Loss_Fav_Rank_12[i] = match_count_w_g_12$N_Loss_Fav_Rank
  V_MATCH_t$Winner_N_Loss_Out_Rank_12[i] = match_count_w_g_12$N_Loss_Out_Rank
  
  V_MATCH_t$Loser_N_Win_12[i] = match_count_l_g_12$N_Win
  V_MATCH_t$Loser_N_Loss_12[i] = match_count_l_g_12$N_Loss
  V_MATCH_t$Loser_N_Win_Fav_Rank_12[i] = match_count_l_g_12$N_Win_Fav_Rank
  V_MATCH_t$Loser_N_Win_Out_Rank_12[i] = match_count_l_g_12$N_Win_Out_Rank
  V_MATCH_t$Loser_N_Loss_Fav_Rank_12[i] = match_count_l_g_12$N_Loss_Fav_Rank
  V_MATCH_t$Loser_N_Loss_Out_Rank_12[i] = match_count_l_g_12$N_Loss_Out_Rank
  
  # Surface 4 semaines
  V_MATCH_t$Winner_N_Win_s_4[i] = match_count_w_s_4$N_Win
  V_MATCH_t$Winner_N_Loss_s_4[i] = match_count_w_s_4$N_Loss
  V_MATCH_t$Winner_N_Win_Fav_Rank_s_4[i] = match_count_w_s_4$N_Win_Fav_Rank
  V_MATCH_t$Winner_N_Win_Out_Rank_s_4[i] = match_count_w_s_4$N_Win_Out_Rank
  V_MATCH_t$Winner_N_Loss_Fav_Rank_s_4[i] = match_count_w_s_4$N_Loss_Fav_Rank
  V_MATCH_t$Winner_N_Loss_Out_Rank_s_4[i] = match_count_w_s_4$N_Loss_Out_Rank
  
  V_MATCH_t$Loser_N_Win_s_4[i] = match_count_l_s_4$N_Win
  V_MATCH_t$Loser_N_Loss_s_4[i] = match_count_l_s_4$N_Loss
  V_MATCH_t$Loser_N_Win_Fav_Rank_s_4[i] = match_count_l_s_4$N_Win_Fav_Rank
  V_MATCH_t$Loser_N_Win_Out_Rank_s_4[i] = match_count_l_s_4$N_Win_Out_Rank
  V_MATCH_t$Loser_N_Loss_Fav_Rank_s_4[i] = match_count_l_s_4$N_Loss_Fav_Rank
  V_MATCH_t$Loser_N_Loss_Out_Rank_s_4[i] = match_count_l_s_4$N_Loss_Out_Rank
  
  # Surface 12 semaines
  V_MATCH_t$Winner_N_Win_s_12[i] = match_count_w_s_12$N_Win
  V_MATCH_t$Winner_N_Loss_s_12[i] = match_count_w_s_12$N_Loss
  V_MATCH_t$Winner_N_Win_Fav_Rank_s_12[i] = match_count_w_s_12$N_Win_Fav_Rank
  V_MATCH_t$Winner_N_Win_Out_Rank_s_12[i] = match_count_w_s_12$N_Win_Out_Rank
  V_MATCH_t$Winner_N_Loss_Fav_Rank_s_12[i] = match_count_w_s_12$N_Loss_Fav_Rank
  V_MATCH_t$Winner_N_Loss_Out_Rank_s_12[i] = match_count_w_s_12$N_Loss_Out_Rank
  
  V_MATCH_t$Loser_N_Win_s_12[i] = match_count_l_s_12$N_Win
  V_MATCH_t$Loser_N_Loss_s_12[i] = match_count_l_s_12$N_Loss
  V_MATCH_t$Loser_N_Win_Fav_Rank_s_12[i] = match_count_l_s_12$N_Win_Fav_Rank
  V_MATCH_t$Loser_N_Win_Out_Rank_s_12[i] = match_count_l_s_12$N_Win_Out_Rank
  V_MATCH_t$Loser_N_Loss_Fav_Rank_s_12[i] = match_count_l_s_12$N_Loss_Fav_Rank
  V_MATCH_t$Loser_N_Loss_Out_Rank_s_12[i] = match_count_l_s_12$N_Loss_Out_Rank
  
  pb$tick()
  
}

##### AJOUT DU ELO #####

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

save(V_MATCH_t,file="MATCH_STATS.RData",compress = "xz")
