library(tidyverse)


# ATP 500
# ATP 250

# Challenger 175
# Challenger 125
# Challenger 110
# Challenger 100
# Challenger 90
# Challenger 80
# Challenger 75
# Challenger 50


# V_TOURNAMENT4$Ranking_points[V_TOURNAMENT4$tournament=="Australian Open" & 
#                                V_TOURNAMENT4$Year==2021 & 
#                                V_TOURNAMENT4$Round=="R16"]=180

# V_TOURNAMENT4$Ranking_points[V_TOURNAMENT4$tournament=="Kitzbühel" & 
#                              V_TOURNAMENT4$Year==2023 & 
#                              V_TOURNAMENT4$Round=="QF"]=45
# 
# V_TOURNAMENT4$Ranking_points[V_TOURNAMENT4$tournament=="Santiago" & 
#                                V_TOURNAMENT4$Year==2022 & 
#                                V_TOURNAMENT4$Round=="SF"]=90

#Santiago(SF) Kitzbuel (QF)
V_1000=V_TOURNAMENT4 %>% filter(Categorie %in% c("ATP 1000"))

V_TOURNAMENT_RED = V_1000 %>% filter(N >=95)

# V_TOURNAMENT_RED2 = V_1000 %>% 
# filter(N <95) %>% 
# select(tournament,Categorie, Round, Ranking_points) %>%
# unique()

V_TOURNAMENT_RED2 = V_1000 %>% 
  filter(N >=95) %>%
  select(tournament,Year,Week_tournament, Round, Ranking_points) %>%
  unique()

n=3

# Créer les nouvelles lignes avec 3 lignes par tournoi
new_rows <- V_TOURNAMENT_RED2 %>%
  distinct(tournament,Year,Week_tournament,Round) %>%              # Obtenir les tournois uniques
  slice(rep(1:n(), each = n)) %>%        # Répéter chaque tournoi 3 fois
  mutate(Round = rep(c("Q-1R","Q-2R","Q-R2RW"), times = n()/n),
         Ranking_points = rep(c(0,8,16), times = n()/n)) %>% 
  distinct()

# Combiner avec les lignes existantes
V_TOURNAMENT_RED2 = rbind(new_rows, V_TOURNAMENT_RED2)

# Effectuer la jointure gauche avec les autres colonnes du dataset initial
V_TOURNAMENT_RED_P2 = V_TOURNAMENT_RED %>%
  select(tournament, Date, Categorie, Country_tournament, Surface_tournament, Week_tournament, Year) %>%
  left_join(V_TOURNAMENT_RED2, by = c("tournament","Year","Week_tournament")) %>%
  unique()

#V_50=V_TOURNAMENT_RED_P2

#dir.create(paste0(getwd(),"/Scrapping tennis data/Tournament/RED"))

V_1000=rbind(V_TOURNAMENT_RED_P1,V_TOURNAMENT_RED_P2) %>% arrange(Date)

save(V_1000,file = paste0(getwd(),"/Scrapping tennis data/Tournament/RED/V_1000.RData"))

# Compilation de V_TOURNAMENT_F

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_2000.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_1000.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_500.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_250.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_175.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_90_125.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_75_80.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data//Scrapping tennis data/Tournament/RED/V_50.RData")

V_TOURNAMENT_F=rbind(V_50,V_75_80,V_90_125,V_175,V_250,V_500,V_1000,V_2000)

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData"))

# RED V_TOURNAMENT_F

TOURNAMENT_RED=V_TOURNAMENT_F %>% filter(tournament=="Bergamo Challenger" & Year==2021)

new_rows=data.frame(
  tournament = TOURNAMENT_RED$tournament[1],
  Date = "2020-02-22",
  Categorie = TOURNAMENT_RED$Categorie[1],
  Country_tournament = TOURNAMENT_RED$Country_tournament[1],
  Surface_tournament = TOURNAMENT_RED$Surface_tournament[1],
  Week_tournament = 8,
  Year = 2020,
  Round = c("Q-R16","Q-QF","Q-QFW","1R","2R","R16","QF","SF","F","Winner"),              # Valeur manquante pour Round
  Ranking_points = c(0,2,4,0,4,7,15,29,48,80)      # Valeur manquante pour Ranking_points
)


TOURNAMENT_RED2=rbind(TOURNAMENT_RED,new_rows)

V_TOURNAMENT_F=V_TOURNAMENT_F %>% filter(!(tournament=="Bergamo Challenger" & Year==2020))

V_TOURNAMENT_F=rbind(V_TOURNAMENT_F,new_rows)

V_TOURNAMENT_F=V_TOURNAMENT_F %>% 
  mutate(Round=case_when(Categorie=="ATP 1000" & Round=="Q-R16W"~"Q-QW",
                         Categorie=="ATP 1000" & Round=="Q-2RW"~"Q-QW",
                         TRUE~Round
                         ))

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData"))
