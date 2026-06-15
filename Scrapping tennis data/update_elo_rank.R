
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_CLAY.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_GRASS.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_HARD.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_INDOORS.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_MAJOR.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_ATP_1000.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_ATP_500.RData"))
load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_ATP_250.RData"))
load("~/work/Tennis-Data/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData")

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
  left_join(ELO_RATING_INDOORS %>% 
              rename(Elo_W_NEW_INDOORS=Elo_W_NEW,
                     Elo_L_NEW_INDOORS=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_MAJOR %>% 
              rename(Elo_W_NEW_MAJOR=Elo_W_NEW,
                     Elo_L_NEW_MAJOR=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_ATP_1000 %>% 
              rename(Elo_W_NEW_ATP_1000=Elo_W_NEW,
                     Elo_L_NEW_ATP_1000=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_ATP_500 %>% 
              rename(Elo_W_NEW_ATP_500=Elo_W_NEW,
                     Elo_L_NEW_ATP_500=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_ATP_250 %>% 
              rename(Elo_W_NEW_ATP_250=Elo_W_NEW,
                     Elo_L_NEW_ATP_250=Elo_L_NEW) %>% 
              select(-c(Categorie,Surface_tournament,N_match,info)),
            by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  group_by(tournament,Round,Week_tournament,Date,Season,Winner_id,Loser_id) %>% 
  mutate(ordre=row_number()) %>% 
  filter(ordre==1) %>% 
  select(-ordre)

ELO_RATING_G=as.data.table(ELO_RATING_G)

date_max=max(ELO_RATING_PLAYERS$Date,na.rm = T)

ELO_RATING_G_NEW=ELO_RATING_G %>% filter(Date>date_max)

#### ALL PLAYERS ####

all_players=ELO_RATING_G_NEW %>% 
  select(Winner_id) %>% 
  rename(Player_id=Winner_id) %>% 
  bind_rows(ELO_RATING_G_NEW %>% 
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

#player_name="Djokovic Novak"

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
                                      TRUE~Elo_L_NEW_GRASS),
           Elo_player_indoors=case_when(Winner_id==player_name~Elo_W_NEW_INDOORS,
                                        TRUE~Elo_L_NEW_INDOORS),
           Elo_player_major=case_when(Winner_id==player_name~Elo_W_NEW_MAJOR,
                                      TRUE~Elo_L_NEW_MAJOR),
           Elo_player_atp_1000=case_when(Winner_id==player_name~Elo_W_NEW_ATP_1000,
                                         TRUE~Elo_L_NEW_ATP_1000),
           Elo_player_atp_500=case_when(Winner_id==player_name~Elo_W_NEW_ATP_500,
                                        TRUE~Elo_L_NEW_ATP_500),
           Elo_player_atp_250=case_when(Winner_id==player_name~Elo_W_NEW_ATP_250,
                                        TRUE~Elo_L_NEW_ATP_250)) %>% 
    
    mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
    ungroup() %>% 
    select(Player_name,Season,tournament,Date,Week_tournament,Round,
           Elo_player,Elo_player_hard,Elo_player_clay,Elo_player_grass,Elo_player_indoors,
           Elo_player_major,Elo_player_atp_1000,Elo_player_atp_500,Elo_player_atp_250) %>% 
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

ELO_RATING_PLAYERS_NEW=data.frame()

for (i in all_players){
  
  ELO=elo_players(i,ELO_RATING_G_NEW)
  
  ELO_RATING_PLAYERS_NEW=rbind(ELO_RATING_PLAYERS_NEW,ELO)
  
  pb$tick()
  
}

Sys.time()-Start

ELO_RATING_PLAYERS=rbind(ELO_RATING_PLAYERS,ELO_RATING_PLAYERS_NEW)

save(ELO_RATING_PLAYERS,file=paste0(here(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"),
     compress = "xz")

