library(sqldf)
library(tidyverse)
library(zoo)

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/V_TABLE_MATCH.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2017_2023.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Info_players/V_PLAYERS_RED.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/RANK/V_RANK.RData")


V_TABLE_MATCH=V_TABLE_MATCH %>% 
  mutate(Week=isoweek(Date),
         Year=case_when(Week>=52 & month(Date)==1 ~year(Date),
                        Week>=52 & month(Date)==12 ~ year(Date),
                        Week==1 & month(Date)==12 ~ year(Date)+1,
                        TRUE ~ year(Date)),
         Month=month(Date))

V_PLAYERS=V_PLAYERS %>% group_by(Player_name,Birth_date) %>% 
  mutate(R_N=row_number()) %>% 
  mutate(R_R_N=row_number(desc(R_N)))

V_RANK=V_RANK %>% 
  mutate(Week=isoweek(Date),
         Month=month(Date),
         Year=case_when(Week>=52 & month(Date)==1 ~year(Date),
                        Week>=52 & month(Date)==12 ~ year(Date),
                        Week==1 & month(Date)==12 ~ year(Date)+1,
                        TRUE ~ year(Date)))

V_TABLE_MATCH_TEST=sqldf("select distinct 
                        -- f.Country_tournament
                        --,f.Categorie
                        --,f.Surface_tournament
                       -- ,
                        a.*
                        ,b.Rank as Rank_W
                        ,b.Points as Points_W
                     --   ,d.Size as Size_W
                      --  ,d.Weight as Weight_W
                      --  ,d.Birth_date as Birth_date_W
                       -- ,d.Hand as Hand_W
                        

                        ,c.Rank as Rank_L
                        ,c.Points as Points_L
                        --,e.Size as Size_L
                        --,e.Weight as Weight_L
                        --,e.Birth_date as Birth_date_L
                        --,e.Hand as Hand_L
       
                       from V_TABLE_MATCH a
                       
                       left join V_RANK b on b.Player_name=a.Winner_id 
                        and b.Week=a.Week 
                        and b.Year=a.Year
                        --and b.Month=a.Month
                       
                       left join V_RANK c on c.Player_name=a.Loser_id 
                        and c.Week=a.Week 
                        and c.Year=a.Year
                        --and c.Month=a.Month
                        
                       left join V_PLAYERS d on d.Player_name=a.Winner_id and d.R_R_N=1 
                       
                       left join V_PLAYERS e on e.Player_name=a.Loser_id and e.R_R_N=1
                         
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
  select(-tournamentU) %>% 
  unique()


V_RACE_RANK=V_TABLE_MATCH_TEST %>% 
  select(tournament,Categorie,Week,Season,Date,Round,N_match,Phase,Winner_id,Loser_id,Rank_W,Rank_L) %>% 
  distinct()


V_RACE_RANK=V_RACE_RANK %>% 
  pivot_longer(
    cols = c(Winner_id, Loser_id),       # Colonnes à transformer
    names_to = "Issue",                  # Nouvelle colonne pour l'origine
    values_to = "Player_ID"              # Nouvelle colonne pour les ID des joueurs
  ) %>%
  mutate(Issue = ifelse(Issue == "Winner_id", "W", "L")) %>% 
  mutate(Round = ifelse(Round =="Q-SF", "Q-QF",Round))


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


V_RACE_RANK_t2=V_RACE_RANK_t %>% 
  mutate(tournamentU=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_F %>% 
              mutate(tournamentU=toupper(tournament)) %>% 
              select(tournamentU,Year,Round,Ranking_points,Week_tournament,Categorie) %>% 
              distinct(),
            by=c("tournamentU","Season"="Year","Round","Categorie")) %>% 
  select(-tournamentU) %>% 
  unique()


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


##### Calcul Race ####

V_RACE_RANK_1=V_RACE_RANK_t2 %>% filter(!Categorie %in% c("Masters Cup","Team Cup"))

V_RACE_RANK_2=V_RACE_RANK_t2 %>% filter(Categorie %in% c("Masters Cup","Team Cup"))

year <- c(2017:2023)

# Fonction pour redresser les dates aux dimanches

adjust_to_last_sunday <- function(dates) {
  sapply(dates, function(date) {
    if (is.na(date)) {
      return(NA) # Conserver les valeurs NA
    } else if (weekdays(date) == "dimanche") {
      return(date) # Si c'est un dimanche, on garde la date
    } else if (weekdays(date) == "samedi") {
      return(date + 1) # Si c'est un samedi, on ajoute une journée
    } else if (weekdays(date) %in% c("mercredi", "mardi")) {
      return(date + as.difftime(7 - wday(date, week_start = 1), units = "days")) # Ajuster au premier dimanche après
    } else {
      return(date - as.difftime(wday(date, week_start = 1), units = "days")) # Ajuster au dernier dimanche
    }
  })
}

# Fonction pour calculer le classement
calculate_race_points <- function(player_id) {
  
  Race_player_y=data.frame(Week=c(1:52),Season=i)
  
  Race_player=data.frame(Week=c(1:52))
  
  Race_player = Race_player %>% 
    left_join(
      V_RACE_RANK %>% filter(Player_ID==player_id & Season==i) %>% 
        arrange(Date,Week) %>% 
        mutate(Week2=Week+1),by=c("Week"="Week2")) %>% 
    rename(Week_orig="Week.y") %>% 
    mutate(Race_points=ifelse(is.na(Race_points)==TRUE,0,Race_points)) %>% 
    mutate(Cum_Race_points=cumsum(Race_points)) %>% 
    mutate(Player_ID=na.locf(Player_ID, na.rm = FALSE)) %>% 
    mutate(Player_ID=ifelse(is.na(Player_ID)==TRUE,na.omit(Player_ID),Player_ID)) %>% 
    mutate(Season=na.locf(Season, na.rm = FALSE)) %>% 
    mutate(Season=ifelse(is.na(Season)==TRUE,na.omit(Season),Season)) %>% 
    mutate(Date=as.Date(adjust_to_last_sunday(Date))) %>% 
    mutate(Week=isoweek(Date)+1) %>% 
    select(1,2,4,5,6,7,8) %>% 
    mutate(Ordre_l=row_number()) %>% 
    mutate(Test=ifelse(is.na(tournament)==T,0,1)) %>% 
    group_by(Player_ID,Season,Week) %>% 
    mutate(Test2=max(Test),
           N_l=row_number(),
           N_l2=max(row_number())
    ) %>% 
    #mutate(Week=ifelse(Test==Test2 & N_l==N_l2,Ordre_l,Week)) %>% 
    #group_by(Player_ID,Season,Week) %>% 
    # mutate(N_l=row_number(),
    #        N_l2=max(row_number())) %>% 
    filter(Test==Test2 & N_l==N_l2) %>% 
    select(-c(Test,Test2,N_l,N_l2,Ordre_l))

  
  Race_player_y=Race_player_y %>% 
    left_join(Race_player,by=c("Week","Season")) %>% 
    mutate(Race_points=ifelse(is.na(Race_points)==TRUE,0,Race_points)) %>% 
    mutate(Cum_Race_points=cumsum(Race_points)) %>% 
    mutate(Player_ID=na.locf(Player_ID, na.rm = FALSE)) %>% 
    mutate(Player_ID=ifelse(is.na(Player_ID)==TRUE,na.omit(Player_ID),Player_ID))
  
  return(Race_player_y)
  
}
# 
# i=2021
# 
# calculate_race_points("Medvedev Daniil")

V_RACE_RANK_F=data.frame()

for (i in year){
  
  Start=Sys.time()
  
  V_RACE_RANK_1=V_RACE_RANK_t2 %>% 
    filter(!Categorie %in% c("Masters Cup","Team Cup")) %>% 
    filter(Season==i)
  
  V_RACE_RANK_2=V_RACE_RANK_t2 %>% 
    filter(Categorie %in% c("Masters Cup","Team Cup")) %>% 
    filter(Season==i)
  
  
  V_RACE_RANK_p1=V_RACE_RANK_1 %>% 
    #filter(Season==i) %>% 
    group_by(tournament,Season) %>% 
    mutate(Date=max(Date)
           #Week=max(Week)
    ) %>% 
    mutate(Week=isoweek(Date)) %>%
    group_by(tournament,Week,Date,Season,Player_ID,Phase) %>% 
    summarise(Race_points=max(Ranking_points))
  
  V_RACE_RANK_p1=V_RACE_RANK_p1 %>% 
    group_by(tournament,Week,Date,Season,Player_ID) %>% 
    summarise(Race_points=sum(Race_points,na.rm = T))
  
  V_RACE_RANK_p1=V_RACE_RANK_p1 %>% 
    mutate(Week=ifelse(month(Date)==1 & Week>=52,1,Week))
  
  V_RACE_RANK_p2=V_RACE_RANK_2 %>% 
    #filter(Season==year) %>% 
    group_by(tournament,Season) %>% 
    mutate(Date=max(Date)
           #,Week=max(Week)
    ) %>% 
    mutate(Week=isoweek(Date)) %>%
    group_by(tournament,Week,Date,Season,Player_ID) %>% 
    summarise(Race_points=sum(Ranking_points,na.rm=T))
  
  V_RACE_RANK=rbind(V_RACE_RANK_p1,V_RACE_RANK_p2)
  
  all_players <- unique(V_RACE_RANK$Player_ID)
  
  RACE_RANK <- map_dfr(all_players, calculate_race_points)
  
  RACE_RANK=RACE_RANK %>% 
    group_by(Week) %>% 
    mutate(Rank=dense_rank(desc(Cum_Race_points)))
  
  V_RACE_RANK_F=rbind(V_RACE_RANK_F,RACE_RANK)
  
  End=Sys.time()-Start
  
  print(End)
  print(i)
  
  browser()
}

# print(n=52,V_RACE_RANK_F %>% filter(Player_ID=="Djokovic Novak" & Season==2023))

save(V_RACE_RANK_F,file = paste0(getwd(),"/Scrapping tennis data/Rank/V_RACE_RANK_2017_2023.RData"))
