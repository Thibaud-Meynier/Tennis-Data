library(progressr)
library(foreach)
library(doParallel)
library(parallel)
library(furrr)
library(future)
# Activer la progress bar
handlers(global = TRUE)
handlers("progress")  # ou "cli" pour un rendu plus joli

# Fonction qui traite UNE année
process_year <- function(year_data,prog=NULL) {
  
  for (i in 1:nrow(year_data)) {
    
    tournoi    <- year_data$tournament[i]
    Season     <- year_data$Season[i]
    Date_match <- year_data$Date[i]
    surface    <- year_data$Surface_tournament[i]
    winner_url <- year_data$URL_W[i]
    loser_url  <- year_data$URL_L[i]
    W          <- year_data$Winner[i]
    L          <- year_data$Loser[i]
    R          <- year_data$Round[i]
    winner_id  <- year_data$Winner_id[i]
    loser_id   <- year_data$Loser_id[i]
    
    result  <- as.data.table(get_h2h(winner_url, loser_url))
    
    stat_g    <- get_stat_h2h(result, "all",   Season, tournoi, W, L, R)
    stat_g_3y <- get_stat_h2h(result, "all",   Season, tournoi, W, L, R, YB = 3)
    stat_s    <- get_stat_h2h(result, surface, Season, tournoi, W, L, R)
    stat_s_3y <- get_stat_h2h(result, surface, Season, tournoi, W, L, R, YB = 3)
    
    match_count_w_g_4  <- match_count(V_MATCH_HIST, winner_id,  4,  "all",   Date_match)
    match_count_l_g_4  <- match_count(V_MATCH_HIST, loser_id,   4,  "all",   Date_match)
    match_count_w_s_4  <- match_count(V_MATCH_HIST, winner_id,  4,  surface, Date_match)
    match_count_l_s_4  <- match_count(V_MATCH_HIST, loser_id,   4,  surface, Date_match)
    match_count_w_g_12 <- match_count(V_MATCH_HIST, winner_id,  12, "all",   Date_match)
    match_count_l_g_12 <- match_count(V_MATCH_HIST, loser_id,   12, "all",   Date_match)
    match_count_w_s_12 <- match_count(V_MATCH_HIST, winner_id,  12, surface, Date_match)
    match_count_l_s_12 <- match_count(V_MATCH_HIST, loser_id,   12, surface, Date_match)
    
    # Assignation
    year_data$H2H_Winner_W[i]            = stat_g$W$Number_Win
    year_data$H2H_Loser_W[i]             = stat_g$L$Number_Win
    year_data$H2H_Winner_Set_Won[i]      = stat_g$W$Number_Set_Won
    year_data$H2H_Loser_Set_Won[i]       = stat_g$L$Number_Set_Won
    year_data$H2H_Winner_Games_Won[i]    = stat_g$W$Number_Games_Won
    year_data$H2H_Loser_Games_Won[i]     = stat_g$L$Number_Games_Won
    
    year_data$H2H_Winner_W_3Y[i]         = stat_g_3y$W$Number_Win
    year_data$H2H_Loser_W_3Y[i]          = stat_g_3y$L$Number_Win
    year_data$H2H_Winner_Set_Won_3Y[i]   = stat_g_3y$W$Number_Set_Won
    year_data$H2H_Loser_Set_Won_3Y[i]    = stat_g_3y$L$Number_Set_Won
    year_data$H2H_Winner_Games_Won_3Y[i] = stat_g_3y$W$Number_Games_Won
    year_data$H2H_Loser_Games_Won_3Y[i]  = stat_g_3y$L$Number_Games_Won
    
    year_data$H2H_s_Winner_W[i]            = stat_s$W$Number_Win
    year_data$H2H_s_Loser_W[i]             = stat_s$L$Number_Win
    year_data$H2H_s_Winner_Set_Won[i]      = stat_s$W$Number_Set_Won
    year_data$H2H_s_Loser_Set_Won[i]       = stat_s$L$Number_Set_Won
    year_data$H2H_s_Winner_Games_Won[i]    = stat_s$W$Number_Games_Won
    year_data$H2H_s_Loser_Games_Won[i]     = stat_s$L$Number_Games_Won
    
    year_data$H2H_s_Winner_W_3Y[i]         = stat_s_3y$W$Number_Win
    year_data$H2H_s_Loser_W_3Y[i]          = stat_s_3y$L$Number_Win
    year_data$H2H_s_Winner_Set_Won_3Y[i]   = stat_s_3y$W$Number_Set_Won
    year_data$H2H_s_Loser_Set_Won_3Y[i]    = stat_s_3y$L$Number_Set_Won
    year_data$H2H_s_Winner_Games_Won_3Y[i] = stat_s_3y$W$Number_Games_Won
    year_data$H2H_s_Loser_Games_Won_3Y[i]  = stat_s_3y$L$Number_Games_Won
    
    year_data$Winner_N_Win_4[i]            = match_count_w_g_4$N_Win
    year_data$Winner_N_Loss_4[i]           = match_count_w_g_4$N_Loss
    year_data$Winner_N_Win_Fav_Rank_4[i]   = match_count_w_g_4$N_Win_Fav_Rank
    year_data$Winner_N_Win_Out_Rank_4[i]   = match_count_w_g_4$N_Win_Out_Rank
    year_data$Winner_N_Loss_Fav_Rank_4[i]  = match_count_w_g_4$N_Loss_Fav_Rank
    year_data$Winner_N_Loss_Out_Rank_4[i]  = match_count_w_g_4$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_4[i]             = match_count_l_g_4$N_Win
    year_data$Loser_N_Loss_4[i]            = match_count_l_g_4$N_Loss
    year_data$Loser_N_Win_Fav_Rank_4[i]    = match_count_l_g_4$N_Win_Fav_Rank
    year_data$Loser_N_Win_Out_Rank_4[i]    = match_count_l_g_4$N_Win_Out_Rank
    year_data$Loser_N_Loss_Fav_Rank_4[i]   = match_count_l_g_4$N_Loss_Fav_Rank
    year_data$Loser_N_Loss_Out_Rank_4[i]   = match_count_l_g_4$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_12[i]           = match_count_w_g_12$N_Win
    year_data$Winner_N_Loss_12[i]          = match_count_w_g_12$N_Loss
    year_data$Winner_N_Win_Fav_Rank_12[i]  = match_count_w_g_12$N_Win_Fav_Rank
    year_data$Winner_N_Win_Out_Rank_12[i]  = match_count_w_g_12$N_Win_Out_Rank
    year_data$Winner_N_Loss_Fav_Rank_12[i] = match_count_w_g_12$N_Loss_Fav_Rank
    year_data$Winner_N_Loss_Out_Rank_12[i] = match_count_w_g_12$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_12[i]            = match_count_l_g_12$N_Win
    year_data$Loser_N_Loss_12[i]           = match_count_l_g_12$N_Loss
    year_data$Loser_N_Win_Fav_Rank_12[i]   = match_count_l_g_12$N_Win_Fav_Rank
    year_data$Loser_N_Win_Out_Rank_12[i]   = match_count_l_g_12$N_Win_Out_Rank
    year_data$Loser_N_Loss_Fav_Rank_12[i]  = match_count_l_g_12$N_Loss_Fav_Rank
    year_data$Loser_N_Loss_Out_Rank_12[i]  = match_count_l_g_12$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_s_4[i]           = match_count_w_s_4$N_Win
    year_data$Winner_N_Loss_s_4[i]          = match_count_w_s_4$N_Loss
    year_data$Winner_N_Win_Fav_Rank_s_4[i]  = match_count_w_s_4$N_Win_Fav_Rank
    year_data$Winner_N_Win_Out_Rank_s_4[i]  = match_count_w_s_4$N_Win_Out_Rank
    year_data$Winner_N_Loss_Fav_Rank_s_4[i] = match_count_w_s_4$N_Loss_Fav_Rank
    year_data$Winner_N_Loss_Out_Rank_s_4[i] = match_count_w_s_4$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_s_4[i]            = match_count_l_s_4$N_Win
    year_data$Loser_N_Loss_s_4[i]           = match_count_l_s_4$N_Loss
    year_data$Loser_N_Win_Fav_Rank_s_4[i]   = match_count_l_s_4$N_Win_Fav_Rank
    year_data$Loser_N_Win_Out_Rank_s_4[i]   = match_count_l_s_4$N_Win_Out_Rank
    year_data$Loser_N_Loss_Fav_Rank_s_4[i]  = match_count_l_s_4$N_Loss_Fav_Rank
    year_data$Loser_N_Loss_Out_Rank_s_4[i]  = match_count_l_s_4$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_s_12[i]           = match_count_w_s_12$N_Win
    year_data$Winner_N_Loss_s_12[i]          = match_count_w_s_12$N_Loss
    year_data$Winner_N_Win_Fav_Rank_s_12[i]  = match_count_w_s_12$N_Win_Fav_Rank
    year_data$Winner_N_Win_Out_Rank_s_12[i]  = match_count_w_s_12$N_Win_Out_Rank
    year_data$Winner_N_Loss_Fav_Rank_s_12[i] = match_count_w_s_12$N_Loss_Fav_Rank
    year_data$Winner_N_Loss_Out_Rank_s_12[i] = match_count_w_s_12$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_s_12[i]            = match_count_l_s_12$N_Win
    year_data$Loser_N_Loss_s_12[i]           = match_count_l_s_12$N_Loss
    year_data$Loser_N_Win_Fav_Rank_s_12[i]   = match_count_l_s_12$N_Win_Fav_Rank
    year_data$Loser_N_Win_Out_Rank_s_12[i]   = match_count_l_s_12$N_Win_Out_Rank
    year_data$Loser_N_Loss_Fav_Rank_s_12[i]  = match_count_l_s_12$N_Loss_Fav_Rank
    year_data$Loser_N_Loss_Out_Rank_s_12[i]  = match_count_l_s_12$N_Loss_Out_Rank
    
    if (!is.null(prog)) {
      prog(message = paste0("Année ", year(year_data$Date[i]), 
                            " — Match ", i, "/", nrow(year_data),
                            " — ", year_data$Winner[i], " vs ", year_data$Loser[i]))
    }
  
  }
  
  return(year_data)  # Retourne le df complet de l'année
  
}

# Parallélisation sur les années

handlers("cli")

year_list <- unique(V_MATCH_t$Season)

#cl <- makeCluster(10)

closeAllConnections()

showConnections(all=T)

plan(multisession, workers = 10)

with_progress({
  
  p <- progressor(steps = nrow(V_MATCH_t))
  
  V_MATCH_final <- future_map_dfr(year_list, function(y) {
    
    year_data <- V_MATCH_t %>% filter(year(Date) == y)
    process_year(year_data  = year_data, 
                 #V_MATCH_HIST = V_MATCH_HIST,  # ← passe explicitement dans le lambda
                 prog          = p)
  },
  .options = furrr_options(
    globals  = c("V_MATCH_t", "V_MATCH_HIST",
                 "process_year", "get_h2h",
                 "get_stat_h2h", "match_count","data_set_cn","exclusion","p","V_TOURNAMENT"),
    packages = c("dplyr", "data.table", "lubridate",
                 "rvest", "httr", "stringr","stringdist")
  ))
})

plan(sequential)

##### AJOUT DU ELO #####

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"))

ELO_RATING_PLAYERS = as.data.table(ELO_RATING_PLAYERS)

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

V_MATCH_final$Elo_W=NA
V_MATCH_final$Elo_L=NA
V_MATCH_final$Elo_W_surface=NA
V_MATCH_final$Elo_L_surface=NA

for (i in 1:nrow(V_MATCH_final)){
  
  P1=V_MATCH_final$Winner_id[i]
  
  P2=V_MATCH_final$Loser_id[i]
  
  surface=V_MATCH_final$Surface_tournament[i]
  
  Date_match=V_MATCH_final$Date[i]
  
  tournoi=V_MATCH_final$tournament[i]
  
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
  
  V_MATCH_final$Elo_W[i]=elo_p1
  V_MATCH_final$Elo_L[i]=elo_p2
  V_MATCH_final$Elo_W_surface[i]=elo_p1_surface
  V_MATCH_final$Elo_L_surface[i]=elo_p2_surface
  
  pb$tick()
  
}

class(ELO_RATING_PLAYERS)
