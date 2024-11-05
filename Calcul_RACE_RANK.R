load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/V_TABLE_MATCH.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Info_players/V_PLAYERS.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/RANK/V_RANK.RData")

##### Calcul classement race ####

V_TABLE_MATCH_TEST=V_TABLE_MATCH_TEST %>% 
  mutate(tournamentU=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_F %>% 
              mutate(tournamentU=toupper(tournament)) %>% 
              select(tournamentU,Categorie,Surface_tournament,Year) %>% 
              distinct(),
            by=c("tournamentU","Season"="Year")) %>% 
  select(-tournamentU)


V_RACE_RANK=V_TABLE_MATCH_TEST %>% 
  select(tournament,Categorie,Week,Season,Round,N_match,Phase,Winner_id,Loser_id,Rank_W,Rank_L) %>% 
  distinct()

table(V_RACE_RANK$Round)
# Gerer les Q-SF en Q-QF

V_RACE_RANK=V_RACE_RANK %>% 
  pivot_longer(
    cols = c(Winner_id, Loser_id),       # Colonnes à transformer
    names_to = "Issue",                  # Nouvelle colonne pour l'origine
    values_to = "Player_ID"              # Nouvelle colonne pour les ID des joueurs
  ) %>%
  mutate(Issue = ifelse(Issue == "Winner_id", "W", "L")) %>% 
  mutate(Round = ifelse(Round =="Q-SF", "Q-QF",Round))


ATP_CUP=V_RACE_RANK_t %>% filter(tournament %in% c("Atp Cup","United Cup"))

V_RACE_RANK_t=V_RACE_RANK %>% 
  mutate(Round=case_when(Phase=="Main Draw" & Round=="F" & Issue=="W"~"Winner",
                         Phase=="Qualification" & Categorie!="Grand Slam" & Round=="Q-QF" & Issue=="W"~"Q-QW",
                         Phase=="Qualification" & Categorie=="Grand Slam" & Round=="Q-3R" & Issue=="W"~"Q-QW",
                         Categorie=="Masters Cup" & Round=="-" & Issue=="W"~"RRW",
                         Categorie=="Masters Cup" & Round=="-" & Issue=="L"~"RR",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(1,2) & Issue=="W"~"Winner",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(1,2) & Issue=="L"~"F",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(3:6) & Issue=="W"~"SFW",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(3:6) & Issue=="L"~"SF",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(7:14) & Issue=="W"~"QFW",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match %in% c(7:14) & Issue=="L"~"QF",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match>=15 & Issue=="W"~"RRW",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2020) & N_match>=15 & Issue=="L"~"RR",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match %in% c(1,2) & Issue=="W"~"Winner",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match %in% c(1,2) & Issue=="L"~"F",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match %in% c(3:6) & Issue=="W"~"SFW",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match %in% c(3:6) & Issue=="L"~"SF",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match>6 & Issue=="W"~"RRW",
                         tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & N_match>6 & Issue=="L"~"RR",
                         TRUE~Round))

#V_RACE_RANK=V_RACE_RANK %>% filter(is.na(Categorie_tournament))

V_TABLE_MATCH_TEST %>% filter(tournament=="United Cup")

table(V_RACE_RANK$tournament)

V_RACE_RANK_t2=V_RACE_RANK_t %>% 
  mutate(tournamentU=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_F %>% 
              mutate(tournamentU=toupper(tournament)) %>% 
              select(tournamentU,Year,Round,Ranking_points,Categorie) %>% 
              distinct(),
            by=c("tournamentU","Season"="Year","Round","Categorie")) %>% 
  select(-tournamentU) %>% 
  distinct()

