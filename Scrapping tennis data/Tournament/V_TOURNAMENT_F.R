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
V_50=V_TOURNAMENT4 %>% filter(Categorie %in% c("Challenger 50"))

V_TOURNAMENT_RED = V_50
#%>% filter(N > 31)

V_TOURNAMENT_RED2 = V_50 %>% 
#   filter(N <= 31) %>%
select(tournament,Categorie, Round, Ranking_points) %>%
unique()

V_TOURNAMENT_RED2 = V_50 %>% 
  #filter(N > 31) %>%
  select(tournament,Year,Week_tournament, Round, Ranking_points) %>%
  unique()

n=3

# Créer les nouvelles lignes avec 3 lignes par tournoi
new_rows <- V_TOURNAMENT_RED2 %>%
  distinct(tournament,Year,Week_tournament,Round) %>%              # Obtenir les tournois uniques
  slice(rep(1:n(), each = n)) %>%        # Répéter chaque tournoi 3 fois
  mutate(Round = rep(c("Q-R16","Q-QF","Q-QW"), times = n()/n),
         Ranking_points = rep(c(0,1,3), times = n()/n)) %>% 
  distinct()

# Combiner avec les lignes existantes
V_TOURNAMENT_RED2 = rbind(new_rows, V_TOURNAMENT_RED2)

# Effectuer la jointure gauche avec les autres colonnes du dataset initial
V_TOURNAMENT_RED_P2 = V_TOURNAMENT_RED %>%
  select(tournament, Date, Categorie, Country_tournament, Surface_tournament, Week_tournament, Year) %>%
  left_join(V_TOURNAMENT_RED2, by = c("tournament","Year","Week_tournament")) %>%
  unique()

V_50=V_TOURNAMENT_RED_P2

#dir.create(paste0(getwd(),"/Scrapping tennis data/Tournament/RED"))

V_250=rbind(V_TOURNAMENT_RED_P1,V_TOURNAMENT_RED_P2) %>% arrange(Date)

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData"))

V_TOURNAMENT_F=rbind(V_50,V_75_80,V_90_125,V_175,V_250,V_500,V_1000,V_2000)

V_TABLE_MATCH=V_TABLE_MATCH %>% unique()

V_TABLE_MATCH %>% filter(Categorie)
