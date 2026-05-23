load("~/work/Tennis-Data/Scrapping tennis data/ML_engenering/MATCH_STATS.RData")

load(paste0(getwd(),"/Scrapping tennis data/ML_engenering/V_MATCH_HIST.RData"))

load(paste0(getwd(),"/Scrapping tennis data/ML_engenering/V_RANK.RData"))

load(paste0(getwd(),"/Scrapping tennis data/ML_engenering/V_MATCH_t.RData"))

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"))

source(paste0(getwd(),"/Scrapping tennis data/Stat function.R"))

handlers(global = TRUE)

handlers("cli") 

# Fonction qui traite UNE année
process_year <- function(year_data,prog=NULL) {
  
  for (i in 1:nrow(year_data)) {
    
    tournoi    <- year_data$tournament[i]
    Season     <- year_data$Season[i]
    Categ      <- year_data$Categorie[i]
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
    
    match_count_w_g_4  <- match_count(V_MATCH_HIST, winner_id,  4,  "all",   Date_match, tournoi, Categ, Season)
    match_count_l_g_4  <- match_count(V_MATCH_HIST, loser_id,   4,  "all",   Date_match, tournoi, Categ, Season)
    match_count_w_s_4  <- match_count(V_MATCH_HIST, winner_id,  4,  surface, Date_match, tournoi, Categ, Season)
    match_count_l_s_4  <- match_count(V_MATCH_HIST, loser_id,   4,  surface, Date_match, tournoi, Categ, Season)
    match_count_w_g_12 <- match_count(V_MATCH_HIST, winner_id,  12, "all",   Date_match, tournoi, Categ, Season)
    match_count_l_g_12 <- match_count(V_MATCH_HIST, loser_id,   12, "all",   Date_match, tournoi, Categ, Season)
    match_count_w_s_12 <- match_count(V_MATCH_HIST, winner_id,  12, surface, Date_match, tournoi, Categ, Season)
    match_count_l_s_12 <- match_count(V_MATCH_HIST, loser_id,   12, surface, Date_match, tournoi, Categ, Season)
    match_count_w_g_52 <- match_count(V_MATCH_HIST, winner_id,  52, "all",   Date_match, tournoi, Categ, Season)
    match_count_l_g_52 <- match_count(V_MATCH_HIST, loser_id,   52, "all",   Date_match, tournoi, Categ, Season)
    match_count_w_s_52 <- match_count(V_MATCH_HIST, winner_id,  52, surface, Date_match, tournoi, Categ, Season)
    match_count_l_s_52 <- match_count(V_MATCH_HIST, loser_id,   52, surface, Date_match, tournoi, Categ, Season)
    
    ##### ELO CLASSIC #####
    
    last_elo_p1=last_elo(ELO_RATING_PLAYERS,winner_id,"all",Date_match,tournoi)
    
    last_elo_p2=last_elo(ELO_RATING_PLAYERS,loser_id,"all",Date_match,tournoi)
    
    diff_date_p1=as.numeric(Date_match-last_elo_p1$Date)
    
    diff_date_p2=as.numeric(Date_match-last_elo_p2$Date)
    
    covid=ifelse(between(Date_match,as.Date("2020-08-01"),as.Date("2021-02-01")) ,0,1)
    
    elo_p1=ifelse(is_empty(last_elo_p1$Elo_player)==T,1500,last_elo_p1$Elo_player-(penalty(diff_date_p1)*covid*100))
    
    elo_p2=ifelse(is_empty(last_elo_p2$Elo_player)==T,1500,last_elo_p2$Elo_player-(penalty(diff_date_p2)*covid*100))
    
    
    ##### ELO SURFACE #####
    
    if (surface %in% c("Indoors","Various")){
      
      surface="Indoors"
      
    }else{
      
      surface=surface
    }
    
    last_elo_p1_surface=last_elo(ELO_RATING_PLAYERS,winner_id,surface,Date_match,tournoi) %>% pull(4)
    
    last_elo_p2_surface=last_elo(ELO_RATING_PLAYERS,loser_id,surface,Date_match,tournoi) %>% pull(4)
    
    elo_p1_surface=ifelse(is_empty(last_elo_p1_surface)==T,1500,last_elo_p1_surface)
    
    elo_p2_surface=ifelse(is_empty(last_elo_p2_surface)==T,1500,last_elo_p2_surface)
    
    ##### ELO CATEG #####
    
    last_elo_p1_categ=last_elo2(ELO_RATING_PLAYERS,winner_id,Categ,Date_match,tournoi) %>% pull(4)
    
    last_elo_p2_categ=last_elo2(ELO_RATING_PLAYERS,loser_id,Categ,Date_match,tournoi) %>% pull(4)
    
    elo_p1_categ=ifelse(is_empty(last_elo_p1_categ)==T,1500,last_elo_p1_categ)
    
    elo_p2_categ=ifelse(is_empty(last_elo_p2_categ)==T,1500,last_elo_p2_categ)
    
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
    # year_data$Winner_N_Win_Fav_Rank_s_4[i]  = match_count_w_s_4$N_Win_Fav_Rank
    # year_data$Winner_N_Win_Out_Rank_s_4[i]  = match_count_w_s_4$N_Win_Out_Rank
    # year_data$Winner_N_Loss_Fav_Rank_s_4[i] = match_count_w_s_4$N_Loss_Fav_Rank
    # year_data$Winner_N_Loss_Out_Rank_s_4[i] = match_count_w_s_4$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_s_4[i]            = match_count_l_s_4$N_Win
    year_data$Loser_N_Loss_s_4[i]           = match_count_l_s_4$N_Loss
    # year_data$Loser_N_Win_Fav_Rank_s_4[i]   = match_count_l_s_4$N_Win_Fav_Rank
    # year_data$Loser_N_Win_Out_Rank_s_4[i]   = match_count_l_s_4$N_Win_Out_Rank
    # year_data$Loser_N_Loss_Fav_Rank_s_4[i]  = match_count_l_s_4$N_Loss_Fav_Rank
    # year_data$Loser_N_Loss_Out_Rank_s_4[i]  = match_count_l_s_4$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_s_12[i]           = match_count_w_s_12$N_Win
    year_data$Winner_N_Loss_s_12[i]          = match_count_w_s_12$N_Loss
    # year_data$Winner_N_Win_Fav_Rank_s_12[i]  = match_count_w_s_12$N_Win_Fav_Rank
    # year_data$Winner_N_Win_Out_Rank_s_12[i]  = match_count_w_s_12$N_Win_Out_Rank
    # year_data$Winner_N_Loss_Fav_Rank_s_12[i] = match_count_w_s_12$N_Loss_Fav_Rank
    # year_data$Winner_N_Loss_Out_Rank_s_12[i] = match_count_w_s_12$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_s_12[i]            = match_count_l_s_12$N_Win
    year_data$Loser_N_Loss_s_12[i]           = match_count_l_s_12$N_Loss
    # year_data$Loser_N_Win_Fav_Rank_s_12[i]   = match_count_l_s_12$N_Win_Fav_Rank
    # year_data$Loser_N_Win_Out_Rank_s_12[i]   = match_count_l_s_12$N_Win_Out_Rank
    # year_data$Loser_N_Loss_Fav_Rank_s_12[i]  = match_count_l_s_12$N_Loss_Fav_Rank
    # year_data$Loser_N_Loss_Out_Rank_s_12[i]  = match_count_l_s_12$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_52[i]           = match_count_w_g_52$N_Win
    year_data$Winner_N_Loss_52[i]          = match_count_w_g_52$N_Loss
    year_data$Winner_N_Win_Fav_Rank_52[i]  = match_count_w_g_52$N_Win_Fav_Rank
    year_data$Winner_N_Win_Out_Rank_52[i]  = match_count_w_g_52$N_Win_Out_Rank
    year_data$Winner_N_Loss_Fav_Rank_52[i] = match_count_w_g_52$N_Loss_Fav_Rank
    year_data$Winner_N_Loss_Out_Rank_52[i] = match_count_w_g_52$N_Loss_Out_Rank
    
    year_data$Loser_N_Win_52[i]            = match_count_l_g_52$N_Win
    year_data$Loser_N_Loss_52[i]           = match_count_l_g_52$N_Loss
    year_data$Loser_N_Win_Fav_Rank_52[i]   = match_count_l_g_52$N_Win_Fav_Rank
    year_data$Loser_N_Win_Out_Rank_52[i]   = match_count_l_g_52$N_Win_Out_Rank
    year_data$Loser_N_Loss_Fav_Rank_52[i]  = match_count_l_g_52$N_Loss_Fav_Rank
    year_data$Loser_N_Loss_Out_Rank_52[i]  = match_count_l_g_52$N_Loss_Out_Rank
    
    year_data$Winner_N_Win_s_52[i]           = match_count_w_s_52$N_Win
    year_data$Winner_N_Loss_s_52[i]          = match_count_w_s_52$N_Loss
    year_data$Loser_N_Win_s_52[i]            = match_count_l_s_52$N_Win
    year_data$Loser_N_Loss_s_52[i]           = match_count_l_s_52$N_Loss
    
    year_data$Elo_W[i]=elo_p1
    year_data$Elo_L[i]=elo_p2
    year_data$Elo_W_surface[i]=elo_p1_surface
    year_data$Elo_L_surface[i]=elo_p2_surface
    year_data$Elo_W_categorie[i]=elo_p1_categ
    year_data$Elo_L_categorie[i]=elo_p2_categ
    
    
    if (!is.null(prog)) {
      prog(message = paste0("Année ", year(year_data$Date[i]), 
                            " — Match ", i, "/", nrow(year_data),
                            " — ", year_data$Winner[i], " vs ", year_data$Loser[i]))
    }
    
  }
  
  return(year_data)  # Retourne le df complet de l'année
  
}

max_date = max(V_MATCH_final$Date)

year_list = V_MATCH_t %>% filter(Date>max_date) %>% pull(Season) %>% unique()

closeAllConnections()

showConnections(all=T)

plan(multisession, workers = 20)

with_progress({
  
  p <- progressor(steps = nrow(V_MATCH_t %>% filter(Date>max_date)))
  
  V_MATCH_final_new <- future_map_dfr(year_list, function(y) {
    
    year_data <- V_MATCH_t %>% filter(Date>max_date)
    process_year(year_data  = year_data, 
                 prog          = p)
  },
  .options = furrr_options(
    globals  = c("V_MATCH_t", "V_MATCH_HIST",
                 "process_year", "get_h2h",
                 "get_stat_h2h", "match_count",
                 "data_set_cn","exclusion","p",
                 "V_TOURNAMENT","ELO_RATING_PLAYERS","get_tennis_week",
                 "last_elo","penalty","last_elo2","max_date"),
    packages = c("dplyr", "data.table", "lubridate",
                 "rvest", "httr", "stringr","stringdist")
  ))
})

plan(sequential)

closeAllConnections()

V_MATCH_final = rbind(V_MATCH_final,V_MATCH_final_new)

save(V_MATCH_final,file = paste0(here(),"/Scrapping tennis data/ML_engenering/MATCH_STATS.RData"),compress = "xz")

save(V_MATCH_final_new,file = paste0(here(),"/Scrapping tennis data/ML_engenering/MATCH_STATS_DELTA.RData"),compress = "xz")
