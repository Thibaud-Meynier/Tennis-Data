load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/V_TABLE_MATCH.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Info_players/V_PLAYERS.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/RANK/V_RANK.RData")

V_TABLE_MATCH_TEST=sqldf("select distinct 
                         --f.Country_tournament
                        --,f.Categorie
                        --,f.Surface_tournament
                        --,
                        a.*
                        ,b.Rank as Rank_W
                        ,b.Points as Points_W
                        ,d.Size as Size_W
                        ,d.Weight as Weight_W
                        ,d.Birth_date as Birth_date_W
                        ,d.Hand as Hand_W
                        

                        ,c.Rank as Rank_L
                        ,c.Points as Points_L
                        ,e.Size as Size_L
                        ,e.Weight as Weight_L
                        ,e.Birth_date as Birth_date_L
                        ,e.Hand as Hand_L
       
                       from V_TABLE_MATCH a
                       
                       left join V_RANK b on b.Player_name=a.Winner_id 
                        and b.Week=a.Week 
                        and b.Year=a.Season
                       
                       left join V_RANK c on c.Player_name=a.Loser_id 
                        and c.Week=a.Week 
                        and c.Year=a.Season
                            
                       left join V_PLAYERS d on d.Player_name=a.Winner_id   
                       
                       left join V_PLAYERS e on e.Player_name=a.Loser_id
                         
                      -- left join V_TOURNAMENT_F f on f.tournament=a.tournament 
")

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
  select(tournament,Categorie,Week,Season,Date,Round,N_match,Phase,Winner_id,Loser_id,Rank_W,Rank_L) %>% 
  distinct()

#table(V_RACE_RANK$Round)
# Gerer les Q-SF en Q-QF

V_RACE_RANK=V_RACE_RANK %>% 
  pivot_longer(
    cols = c(Winner_id, Loser_id),       # Colonnes à transformer
    names_to = "Issue",                  # Nouvelle colonne pour l'origine
    values_to = "Player_ID"              # Nouvelle colonne pour les ID des joueurs
  ) %>%
  mutate(Issue = ifelse(Issue == "Winner_id", "W", "L")) %>% 
  mutate(Round = ifelse(Round =="Q-SF", "Q-QF",Round))


#ATP_CUP=V_RACE_RANK_t %>% filter(tournament %in% c("Atp Cup","United Cup"))

V_RACE_RANK_t=V_RACE_RANK %>% 
  mutate(Round=case_when(tournament=="Bordeaux Chall." & Phase=="Qualification" & Season==2022 & Round=="Q-R16" & Issue=="W"~"Q-QW",
                         tournament=="Bordeaux Chall." & Phase=="Qualification" & Season==2022 & Round=="Q-R16" & Issue=="L"~"Q-QF",
                         tournament=="Bordeaux Chall." & Phase=="Qualification" & Season==2022 & Round=="Q-2R" & Issue=="W"~"Q-QW",
                         tournament=="Bordeaux Chall." & Phase=="Qualification" & Season==2022 & Round=="Q-2R" & Issue=="L"~"Q-QF",
                         tournament=="Bordeaux Chall." & Phase=="Qualification" & Season==2022 & Round=="Q-1R"~"Q-R16",
                         
                                                  
                         tournament=="Potchefstroom Chall." & Phase=="Qualification" & Season==2020 & Round=="Q-1R"~"Q-R16",
                         tournament=="Potchefstroom Chall." & Phase=="Qualification" & Season==2020 & Round=="Q-R16" & Issue=="W"~"Q-QW",
                         tournament=="Potchefstroom Chall." & Phase=="Qualification" & Season==2020 & Round=="Q-R16" & Issue=="L"~"Q-QF",
    
                         Phase=="Main Draw" & Round=="F" & Issue=="W"~"Winner",
                         Phase=="Qualification" & Categorie!="Grand Slam" & Round=="Q-QF" & Issue=="W"~"Q-QW",
                         Phase=="Qualification" & Categorie=="Grand Slam" & Round=="Q-3R" & Issue=="W"~"Q-QW",
                         tournament=="Masters Cup Atp" & Round=="-" & Issue=="W"~"RRW",
                         tournament=="Masters Cup Atp" & Round=="-" & Issue=="L"~"RR",
                         tournament=="Masters Cup Atp" & Round=="SF" & Issue=="W"~"SFW",
                         tournament=="Masters Cup Atp" & Round=="SF" & Issue=="L"~"SF",
                         Phase=="Qualification" & !Categorie %in% c("ATP 1000","ATP 2000") & Round=="Q-1R"~"Q-R16",
                         tournament=="Cincinnati Masters (New York)" & Phase=="Qualification" & Round=="Q-2R" & Issue=="W"~"Q-QW",
                         tournament=="Cincinnati Masters (New York)" & Phase=="Qualification" & Round=="Q-2R" & Issue=="L"~"Q-R16",
                         
                         Categorie=="ATP 1000" & Phase=="Qualification" & Round=="Q-R16" & Issue=="W"~"Q-QW",
                         Categorie=="ATP 1000" & Phase=="Qualification" & Round=="Q-R16" & Issue=="L"~"Q-R16",
                         
                         Categorie=="ATP 1000" & Phase=="Qualification" & Round=="Q-2R" & Issue=="W"~"Q-QW",
                         Categorie=="ATP 1000" & Phase=="Qualification" & Round=="Q-2R" & Issue=="L"~"Q-2R",
                         
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
                         
                         grepl("Olympics",tournament)==TRUE & Round=="-"~"BM",
                         
                         TRUE~Round))

#V_RACE_RANK=V_RACE_RANK %>% filter(is.na(Categorie_tournament))

#V_TABLE_MATCH_TEST %>% filter(tournament=="United Cup")

#table(V_RACE_RANK$tournament)

V_RACE_RANK_t2=V_RACE_RANK_t %>% 
  mutate(tournamentU=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_F %>% 
              mutate(tournamentU=toupper(tournament)) %>% 
              select(tournamentU,Year,Round,Ranking_points,Categorie) %>% 
              distinct(),
            by=c("tournamentU","Season"="Year","Round","Categorie")) %>% 
  select(-tournamentU) %>% 
  distinct()

V_RACE_RANK_t2$Rank_L=as.numeric(V_RACE_RANK_t2$Rank_L)
V_RACE_RANK_t2$Rank_W=as.numeric(V_RACE_RANK_t2$Rank_W)
V_RACE_RANK_t2$Ranking_points=as.numeric(V_RACE_RANK_t2$Ranking_points)

V_RACE_RANK_t2=V_RACE_RANK_t2 %>% 
  mutate(Rank_W=ifelse(is.na(Rank_W)==TRUE,1000,Rank_W),
         Rank_L=ifelse(is.na(Rank_L)==TRUE,1000,Rank_L))

V_RACE_RANK_t2=V_RACE_RANK_t2 %>% 
  mutate(Ranking_points=case_when(tournament=="Atp Cup" & Season==2020 & Round %in% c("RR","QF","SF","F")~0,
                                  tournament=="Atp Cup" & Season==2020 & Round=="RRW" & between(Rank_L,1,10)~75,
                                  tournament=="Atp Cup" & Season==2020 & Round=="RRW" & between(Rank_L,11,25)~65,
                                  tournament=="Atp Cup" & Season==2020 & Round=="RRW" & between(Rank_L,26,50)~50,
                                  tournament=="Atp Cup" & Season==2020 & Round=="RRW" & between(Rank_L,51,100)~25,
                                  tournament=="Atp Cup" & Season==2020 & Round=="RRW" & Rank_L>=101~20,
                                  
                                  tournament=="Atp Cup" & Season==2020 & Round=="QFW" & between(Rank_L,1,10)~120,
                                  tournament=="Atp Cup" & Season==2020 & Round=="QFW" & between(Rank_L,11,25)~100,
                                  tournament=="Atp Cup" & Season==2020 & Round=="QFW" & between(Rank_L,26,50)~75,
                                  tournament=="Atp Cup" & Season==2020 & Round=="QFW" & between(Rank_L,51,100)~35,
                                  tournament=="Atp Cup" & Season==2020 & Round=="QFW" & Rank_L>=101~25,
                                  
                                  tournament=="Atp Cup" & Season==2020 & Round=="SFW" & between(Rank_L,1,10)~180,
                                  tournament=="Atp Cup" & Season==2020 & Round=="SFW" & between(Rank_L,11,25)~140,
                                  tournament=="Atp Cup" & Season==2020 & Round=="SFW" & between(Rank_L,26,50)~105,
                                  tournament=="Atp Cup" & Season==2020 & Round=="SFW" & between(Rank_L,51,100)~50,
                                  tournament=="Atp Cup" & Season==2020 & Round=="SFW" & Rank_L>=101~35,
                                  
                                  tournament=="Atp Cup" & Season==2020 & Round=="Winner" & between(Rank_L,1,10)~250,
                                  tournament=="Atp Cup" & Season==2020 & Round=="Winner" & between(Rank_L,11,25)~200,
                                  tournament=="Atp Cup" & Season==2020 & Round=="Winner" & between(Rank_L,26,50)~150,
                                  tournament=="Atp Cup" & Season==2020 & Round=="Winner" & between(Rank_L,51,100)~75,
                                  tournament=="Atp Cup" & Season==2020 & Round=="Winner" & Rank_L>=101~50,
                                  
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round %in% c("RR","SF","F")~0,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,1,10)~75,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,11,20)~65,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,21,30)~50,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,31,50)~35,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,51,100)~25,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & between(Rank_L,101,250)~20,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="RRW" & Rank_L>=251~15,
                                
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,1,10)~150,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,11,20)~130,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,21,30)~100,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,31,50)~70,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,51,100)~45,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & between(Rank_L,101,250)~30,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="SFW" & Rank_L>=251~20,
                                  
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,1,10)~220,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,11,20)~180,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,21,30)~140,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,31,50)~100,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,51,100)~75,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & between(Rank_L,101,250)~45,
                                  tournament %in% c("Atp Cup","United Cup") & Season %in% c(2021,2022,2023) & Round=="Winner" & Rank_L>=251~30,
                                  
                                  tournament=="Masters Cup Atp" & Round %in% c("RR","SF","F") ~0,
                                  tournament=="Masters Cup Atp" & Round=="RRW" ~200,
                                  tournament=="Masters Cup Atp" & Round=="SFW" ~400,
                                  tournament=="Masters Cup Atp" & Round=="Winner" ~500,
                                  
                                  grepl("Olympics",tournament)==TRUE|tournament %in% c("Davis Cup","Next Gen Atp Finals")~0,
                                  
                                  TRUE~Ranking_points))

V_RACE_RANK_t2=V_RACE_RANK_t2 %>% 
  mutate(Categorie=case_when(is.na(Categorie)==TRUE & tournament %in% c("Davis Cup","United Cup","Atp Cup")~"Team Cup",
                             is.na(Categorie)==TRUE & tournament %in% c("Masters Cup Atp","Next Gen Atp Finals")~"Masters Cup",
                             is.na(Categorie)==TRUE & grepl("Olympics",tournament)==TRUE~"Olympics",
                             TRUE~Categorie))

         
# Miss_cat=V_RACE_RANK_t2 %>% 
#   filter(is.na(Categorie)==TRUE) %>% 
#   group_by(tournament) %>% 
#   summarise(N=n())
# 
# Miss=V_RACE_RANK_t2 %>% filter(is.na(Ranking_points)==TRUE) %>% 
#   group_by(tournament,Season,Phase,Categorie,Round) %>% 
#   summarise(N=n())

# V_RACE_RANK_t2 %>% 
#   filter(tournament %in% c("Masters Cup Atp") & Round=="-" & Season==2017) 
#  

##### Calcul Race ####

V_RACE_RANK_1=V_RACE_RANK_t2 %>% filter(!Categorie %in% c("Masters Cup","Team Cup"))

V_RACE_RANK_2017_1=V_RACE_RANK_1 %>% 
  filter(Season==2017) %>% 
  group_by(tournament,Season) %>% 
  mutate(Week=max(Week)) %>% 
  group_by(tournament,Week,Season,Player_ID,Phase) %>% 
  summarise(Race_points=max(Ranking_points))

V_RACE_RANK_2017_1=V_RACE_RANK_2017_1 %>% 
  group_by(tournament,Week,Season,Player_ID) %>% 
  summarise(Race_points=sum(Race_points,na.rm = T))

V_RACE_RANK_2=V_RACE_RANK_t2 %>% filter(Categorie %in% c("Masters Cup","Team Cup"))

V_RACE_RANK_2017_2=V_RACE_RANK_2 %>% 
  filter(Season==2017) %>% 
  group_by(tournament,Season) %>% 
  mutate(Week=max(Week)) %>% 
  group_by(tournament,Week,Season,Player_ID) %>% 
  summarise(Race_points=sum(Ranking_points,na.rm=T))

V_RACE_RANK_2017=rbind(V_RACE_RANK_2017_1,V_RACE_RANK_2017_2)

V_RACE_RANK_2017 %>% filter(Player_ID=="Federer Roger") %>% 
  arrange(Week) %>% 
  mutate(Week2=Week+1)

Race_player=data.frame(Week=c(1:52))

Race_player = Race_player %>% 
  left_join(
    V_RACE_RANK_2017 %>% filter(Player_ID=="Federer Roger") %>% 
      arrange(Week) %>% 
      mutate(Week2=Week+1),by=c("Week"="Week2")) %>% 
    mutate(Race_points=ifelse(is.na(Race_points)==TRUE,0,Race_points)) %>% 
  mutate(Cum_Race_points=cumsum(Race_points))

Race_player %>% 
  select(Week,tournament,Player_ID,Race_points) %>% 
  mutate(Race_points=ifelse(is.na(na.locf(Race_points, na.rm = FALSE))==TRUE,0,
                            na.locf(Race_points, na.rm = FALSE))) %>% 
  mutate()
library(zoo)

# Fonction pour calculer le classement
calculate_race_points <- function(player_id) {
  Race_player <- V_RACE_RANK_2017 %>%
    filter(Player_ID == player_id) %>%
    arrange(Week) %>%
    mutate(Week2 = Week + 1) %>%
    left_join(V_RACE_RANK_2017 %>% filter(Player_ID == player_id) %>% arrange(Week), by = c("Week" = "Week2")) %>%
    mutate(Race_points = ifelse(is.na(Race_points), 0, Race_points)) %>%
    mutate(Cum_Race_points = cumsum(Race_points))

  return(Race_player)
}

# Appliquer la fonction à chaque joueur et concaténer les résultats
all_players <- unique(V_RACE_RANK_2017$Player_ID)
RACE_RANK <- map_dfr(all_players, calculate_race_points)

# Ordonner la table finale par semaine en ordre croissant
RACE_RANK <- RACE_RANK %>%
  arrange(Week)
