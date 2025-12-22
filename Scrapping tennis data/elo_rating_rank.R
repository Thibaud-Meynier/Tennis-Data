library(tidyverse)
library(zoo)

ELO_RATING_G=ELO_RATING %>% 
  left_join(ELO_RATING_HARD %>% 
              rename(Elo_W_NEW_HARD=Elo_W_NEW,
                     Elo_L_NEW_HARD=Elo_L_NEW),
             by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_CLAY %>% 
              rename(Elo_W_NEW_CLAY=Elo_W_NEW,
                     Elo_L_NEW_CLAY=Elo_L_NEW),
             by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  left_join(ELO_RATING_GRASS %>% 
              rename(Elo_W_NEW_GRASS=Elo_W_NEW,
                     Elo_L_NEW_GRASS=Elo_L_NEW),
             by=c("tournament","Round","Week_tournament","Date","Season","Winner_id","Loser_id")) %>% 
  group_by(tournament,Round,Week_tournament,Date,Season,Winner_id,Loser_id) %>% 
  mutate(ordre=row_number()) %>% 
  filter(ordre==1) %>% 
  select(-ordre)


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
      return(date - as.difftime(lubridate::wday(date, week_start = 1), units = "days")) # Ajuster au dernier dimanche
    }
  })
}

player_name="Djokovic Novak"

# Creer un calendrier depuis 2003 avec les semaines pour avoir un historique complet de tous les elo,
# se calcul à la suite, pas d'une année sur l'autre comme la race

calculate_elo_points <- function(player_id) {
  
Elo_player=ELO_RATING_G %>% 
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
  mutate(Week_tournament=isoweek(Date)) %>% 
  ungroup() %>% 
  select(Player_name,Season,tournament,Date,Week_tournament,Round,
         Elo_player,Elo_player_hard,Elo_player_clay,Elo_player_grass) %>% 
  group_by(Player_name,Season,tournament) %>% 
  mutate(ORDRE_DESC_ELO=row_number(desc(Date)),
         Week2=(Week_tournament+1)) %>% 
  filter(ORDRE_DESC_ELO==1) %>% 
  select(-ORDRE_DESC_ELO) %>% 
  ungroup() %>% 
  mutate(
    Elo_player = ifelse(row_number() == 1 & is.na(Elo_player), 1500, Elo_player),
    Elo_player_hard = ifelse(row_number() == 1 & is.na(Elo_player_hard), 1500, Elo_player_hard),
    Elo_player_clay = ifelse(row_number() == 1 & is.na(Elo_player_clay), 1500, Elo_player_clay),
    Elo_player_grass = ifelse(row_number() == 1 & is.na(Elo_player_grass), 1500, Elo_player_grass)
  ) %>%
  mutate(across(starts_with("Elo_player"), ~na.locf(., na.rm = FALSE)))

}


