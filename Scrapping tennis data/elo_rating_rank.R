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


adjust_to_last_sunday <- function(dates,Round) {
  sapply(dates, function(date) {
    if (is.na(date)) {
      return(NA) # Conserver les valeurs NA
    } else if (weekdays(date) == "Sunday") {
      return(date) # Si c'est un dimanche, on garde la date
    } else if (weekdays(date) == "Saturday") {
      return(date + 1) # Si c'est un samedi, on ajoute une journée
    } else if (weekdays(date) %in% c("Wednesday", "Tuesday")) {
      return(date + as.difftime(7 - lubridate::wday(date, week_start = 1), units = "days")) # Ajuster au premier dimanche après
    } else if {
      return(date + as.difftime(7 - lubridate::wday(date, week_start = 1), units = "days")) # Ajuster au dernier dimanche
    }
  })
}

weekdays(as.Date("2010-10-16"))=="Saturday"

as.Date(adjust_to_last_sunday(as.Date("2004-11-11")))

player_name="Djokovic Novak"

# Creer un calendrier depuis 2003 avec les semaines pour avoir un historique complet de tous les elo,
# se calcul à la suite, pas d'une année sur l'autre comme la race

calculate_elo_points <- function(player_id) {
  
  period=ELO_RATING_G %>%
    filter(Winner_id==player_name|Loser_id==player_name) %>%
    mutate(Player_name=player_name)  %>%
    group_by(Player_name) %>%
    summarise(period_min=min(Season,na.rm=T),
              period_max=max(Season,na.rm=T)) %>%
    ungroup() %>%
    unique()
  
  period=c(period$period_min:period$period_max)
  
  Elo_player=data.frame(Season=rep(min(period):max(period), each = 52),
                        Week=rep(c(1:52),length(period))
  )
  
  Elo_player=Elo_player %>% 
    left_join(ELO_RATING_G %>% 
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
                mutate(Date=as.Date(adjust_to_last_sunday(Date))) %>% 
                mutate(Week_tournament=isoweek(Date)) %>% 
                mutate(Week2=(Week_tournament+1)) %>% 
                mutate(
                  Elo_player = ifelse(row_number() == 1 & is.na(Elo_player), 1500, Elo_player),
                  Elo_player_hard = ifelse(row_number() == 1 & is.na(Elo_player_hard), 1500, Elo_player_hard),
                  Elo_player_clay = ifelse(row_number() == 1 & is.na(Elo_player_clay), 1500, Elo_player_clay),
                  Elo_player_grass = ifelse(row_number() == 1 & is.na(Elo_player_grass), 1500, Elo_player_grass)
                ) %>%
                mutate(across(starts_with("Elo_player"), ~na.locf(., na.rm = FALSE))),
              by=c("Week"="Week2","Season"="Season")) %>% 
    mutate(
      Player_name = ifelse(row_number() == 1 & is.na(Player_name), 1500, Player_name),
      Elo_player = ifelse(row_number() == 1 & is.na(Elo_player), 1500, Elo_player),
      Elo_player_hard = ifelse(row_number() == 1 & is.na(Elo_player_hard), 1500, Elo_player_hard),
      Elo_player_clay = ifelse(row_number() == 1 & is.na(Elo_player_clay), 1500, Elo_player_clay),
      Elo_player_grass = ifelse(row_number() == 1 & is.na(Elo_player_grass), 1500, Elo_player_grass)
    ) %>%
    mutate(across(starts_with("Elo_player"), ~na.locf(., na.rm = FALSE))) %>% 
    mutate(Player_name=na.locf(Player_name, na.rm = FALSE))
  
  
  Elo_player %>% 
    group_by(Season,Week) %>% 
    filter(row_number()>1)
}


