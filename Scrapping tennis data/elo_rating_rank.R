library(tidyverse)
library(zoo)
library(progress)

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_CLAY.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_GRASS.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_HARD.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING.RData"))


ELO_RATING_G=ELO_RATING %>% 
  left_join(ELO_RATING_HARD %>% 
              rename(Elo_W_NEW_HARD=Elo_W_NEW,
                     Elo_L_NEW_HARD=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_CLAY %>% 
              rename(Elo_W_NEW_CLAY=Elo_W_NEW,
                     Elo_L_NEW_CLAY=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_GRASS %>% 
              rename(Elo_W_NEW_GRASS=Elo_W_NEW,
                     Elo_L_NEW_GRASS=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  group_by(tournament,Round,Week_tournament,Date,Season,Winner_id,Loser_id) %>% 
  mutate(ordre=row_number()) %>% 
  filter(ordre==1) %>% 
  select(-ordre)


#### ALL PLAYERS ####

all_players=ELO_RATING %>% 
  select(Winner_id) %>% 
  rename(Player_id=Winner_id) %>% 
  bind_rows(ELO_RATING %>% 
              select(Loser_id) %>% 
              rename(Player_id=Loser_id)) %>% 
  unique() %>% 
  pull(Player_id)


adjust_to_last_sunday <- function(dates) {
  sapply(dates, function(date) {
    if (is.na(date)) {
      return(NA) # Conserver les valeurs NA
    } else if (weekdays(date) == "dimanche") {
      return(date) # Si c'est un dimanche, on garde la date
    } else if (weekdays(date) == "samedi") {
      return(date + 1) # Si c'est un samedi, on ajoute une journée
    } else if (weekdays(date) %in% c("mercredi", "mardi")) {
      return(date + as.difftime(7 - lubridate::wday(date, week_start = 1), units = "days")) # Ajuster au premier dimanche après
    } else {
      return(date + as.difftime(7 - lubridate::wday(date, week_start = 1), units = "days")) # Ajuster au dernier dimanche
    }
  })
}


elo_players=function(player_name){
  
  ELO_PLAYER=ELO_RATING_G %>% 
    filter(Winner_id==player_name|Loser_id==player_name) %>% 
    mutate(Player_name=player_name) %>% 
    mutate(Elo_player=case_when(Winner_id==player_name~Elo_W_NEW,
                                TRUE~Elo_L_NEW),
           Elo_player_hard=case_when(Winner_id==player_name~Elo_W_NEW_HARD,
                                     TRUE~Elo_L_NEW_HARD),
           Elo_player_clay=case_when(Winner_id==player_name~Elo_W_NEW_CLAY,
                                     TRUE~Elo_L_NEW_CLAY),
           Elo_player_grass=case_when(Winner_id==player_name~Elo_W_NEW_GRASS,
                                      TRUE~Elo_L_NEW_GRASS)) %>% 
    
    mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
    ungroup() %>% 
    select(Player_name,Season,tournament,Date,Week_tournament,Round,
           Elo_player,Elo_player_hard,Elo_player_clay,Elo_player_grass) %>% 
    group_by(Player_name,Season,tournament) %>% 
    arrange(desc(Date), desc(Round)) %>%
    mutate(ORDRE_DESC_ELO = row_number()) %>% 
    filter(ORDRE_DESC_ELO==1) %>% 
    select(-ORDRE_DESC_ELO) %>% 
    arrange(Date,Season,Week_tournament) %>% 
    ungroup() %>% 
    mutate(Date=as.Date(ifelse(Round=="F" & weekdays(Date)=="Monday",(Date-1),
                               adjust_to_last_sunday(Date)))) %>% 
    mutate(Week_tournament=isoweek(Date)) %>% 
    mutate(Week2=(Week_tournament+1))
  
  return(ELO_PLAYER)
}


pb <- progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = length(all_players),
  clear = FALSE,
  width = 60
)

Start=Sys.time()

ELO_RATING_PLAYERS=data.frame()

for (i in all_players){
  
  ELO=elo_players(i)
  
  ELO_RATING_PLAYERS=rbind(ELO_RATING_PLAYERS,ELO)
  
  pb$tick()
  
}

Sys.time()-Start

save(ELO_RATING_PLAYERS,file=paste0(here(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"),
     compress = "xz")


last_elo=function(elo_player,player_name,surface="all",Date_match){
  
  if (surface=="all"){
    
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player)
    
    return(elo_player)
    
  }else if (surface=="Grass"){
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_grass) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_grass)
    
    return(elo_player)
    
  }else if (surface=="Clay"){
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_clay) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_clay)
    
    return(elo_player)
    
  }else{
    
    elo_player=elo_player %>% 
      filter(Player_name==player_name & !is.na(Elo_player_hard) & Date<Date_match) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_hard)
    
    return(elo_player)
    
  }
}

# player_name="Alcaraz Carlos"
# 
# last_elo(ELO_RATING_PLAYERS,player_name,Date_match = as.Date("2025-11-17"))
# 
# calculate_elo_points <- function(player_name) {
#   
#   period=ELO_RATING_G %>%
#     filter(Winner_id==player_name|Loser_id==player_name) %>%
#     mutate(Player_name=player_name)  %>%
#     group_by(Player_name) %>%
#     summarise(period_min=min(Season,na.rm=T),
#               period_max=max(Season,na.rm=T)) %>%
#     ungroup() %>%
#     unique()
#   
#   period=c(period$period_min:period$period_max)
#   
#   Elo_player=data.frame(Season=rep(min(period):max(period), each = 52),
#                         Week=rep(c(1:52),length(period))
#   )
#   
#   Elo_player=Elo_player %>% 
#     left_join(ELO_RATING_G %>% 
#                 filter(Winner_id==player_name|Loser_id==player_name) %>% 
#                 mutate(Player_name=player_name) %>% 
#                 mutate(Elo_player=case_when(Winner_id==player_name~Elo_W_NEW,
#                                             TRUE~Elo_L_NEW),
#                        Elo_player_hard=case_when(Winner_id==player_name~Elo_W_NEW_HARD,
#                                                  TRUE~Elo_L_NEW_HARD),
#                        Elo_player_clay=case_when(Winner_id==player_name~Elo_W_NEW_CLAY,
#                                                  TRUE~Elo_L_NEW_CLAY),
#                        Elo_player_grass=case_when(Winner_id==player_name~Elo_W_NEW_GRASS,
#                                                   TRUE~Elo_L_NEW_GRASS)) %>% 
#                 
#                 mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
#                 ungroup() %>% 
#                 select(Player_name,Season,tournament,Date,Week_tournament,Round,
#                        Elo_player,Elo_player_hard,Elo_player_clay,Elo_player_grass) %>% 
#                 group_by(Player_name,Season,tournament) %>% 
#                 arrange(desc(Date), desc(Round)) %>%
#                 mutate(ORDRE_DESC_ELO = row_number()) %>% 
#                 filter(ORDRE_DESC_ELO==1) %>% 
#                 select(-ORDRE_DESC_ELO) %>% 
#                 arrange(Date,Season,Week_tournament) %>% 
#                 ungroup() %>% 
#                 mutate(Date=as.Date(ifelse(Round=="F" & weekdays(Date)=="Monday",(Date-1),
#                                            adjust_to_last_sunday(Date)))) %>% 
#                 mutate(Week_tournament=isoweek(Date)) %>% 
#                 mutate(Week2=(Week_tournament+1)) %>% 
#                 mutate(
#                   Elo_player = ifelse(row_number() == 1 & is.na(Elo_player), 1500, Elo_player),
#                   Elo_player_hard = ifelse(row_number() == 1 & is.na(Elo_player_hard), 1500, Elo_player_hard),
#                   Elo_player_clay = ifelse(row_number() == 1 & is.na(Elo_player_clay), 1500, Elo_player_clay),
#                   Elo_player_grass = ifelse(row_number() == 1 & is.na(Elo_player_grass), 1500, Elo_player_grass)
#                 ) %>%
#                 mutate(across(starts_with("Elo_player"), ~na.locf(., na.rm = FALSE))),
#               by=c("Week"="Week2","Season"="Season")) %>% 
#     mutate(
#       Player_name = ifelse(row_number() == 1 & is.na(Player_name), na.locf(Player_name, na.rm = FALSE), Player_name),
#       Elo_player = ifelse(row_number() == 1 & is.na(Elo_player), 1500, Elo_player),
#       Elo_player_hard = ifelse(row_number() == 1 & is.na(Elo_player_hard), 1500, Elo_player_hard),
#       Elo_player_clay = ifelse(row_number() == 1 & is.na(Elo_player_clay), 1500, Elo_player_clay),
#       Elo_player_grass = ifelse(row_number() == 1 & is.na(Elo_player_grass), 1500, Elo_player_grass)
#     ) %>%
#     mutate(across(starts_with("Elo_player"), ~na.locf(., na.rm = FALSE))) %>% 
#     mutate(Player_name=na.locf(Player_name, na.rm = FALSE))
#   
#   return(Elo_player)
# }
# 
# pb <- progress_bar$new(
#   format = "[:bar] :current/:total (:percent) ETA: :eta",
#   total = length(all_players),
#   clear = FALSE,
#   width = 60
# )
# 
# Start=Sys.time()
# 
# Players_elo_rank=data.frame()
# 
# for (i in all_players){
#   
#   Elo_player_i=calculate_elo_points(i)
#   
#   Players_elo_rank=rbind(Players_elo_rank,Elo_player_i)
#   
#   print(i)
#   
#   pb$tick()
#   
# }
# 
# End=Sys.time()-Start
# 
# End
# 
# lead()
# 
# Players_elo_rank=Players_elo_rank %>% 
#   mutate(Player_name=ifelse(Week==1 & is.na(Player_name),lead(Player_name,1),Player_name))
# 
# 
# Players_elo_rank=Players_elo_rank %>% 
#   group_by(Season,Week) %>% 
#   mutate(Elo_Rank=dense_rank(desc(Elo_player)),
#          Elo_Rank_hard=dense_rank(desc(Elo_player_hard)),
#          Elo_Rank_clay=dense_rank(desc(Elo_player_clay)),
#          Elo_Rank_grass=dense_rank(desc(Elo_player_grass)))
# 
# for (i in c(2003:2024)){
#   
#   # Créer le nom de l'objet avec la variable
#   name <- paste0("Players_elo_rank_", i)
#   
#   # Filtrer les données
#   Players_elo_rank_filtered <- Players_elo_rank %>%
#     filter(Season == i)
#   
#   # Assigner les données à l'objet avec le nom variabilisé
#   assign(name, Players_elo_rank_filtered)
#   
#   save(list=name,file=paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RANK_PLAYERS_",i,".RData"))
#   
#   print(i)
#   
# }
# 
# 
# Players_elo_rank %>% 
#   filter(Season==2024 & Week==45 & between(Elo_Rank,1,10)) %>% 
#   select(Player_name,Elo_Rank,Elo_Rank_hard,Elo_Rank_clay,Elo_Rank_grass,
#          Elo_player,Elo_player_hard,Elo_player_clay,Elo_player_grass) %>% 
#   arrange(Elo_Rank)
