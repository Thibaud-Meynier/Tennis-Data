library(ggplot2)
library(glue)
library(janitor)

load(paste0(getwd(),"/Scrapping tennis data/Extraction/V_MATCH_2009_2016.RData"))

V_MATCH_2009_2016=V_MATCH

V_MATCH_2009_2016=V_MATCH_2009_2016 %>% 
  rename(Winner_id=P1,
         Loser_id=P2)

colnames=colnames(V_MATCH_2009_2016)

rm(V_MATCH)
rm(V_MATCH_2009_2016)

V_MATCH=data.frame()

#i=2016

for (i in 2021:2025){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",i,"_Extraction.RData"))
  
  table_stock$Season=i
  
  if ("P1" %in% names(table_stock)==T){
    
    table_stock=table_stock %>% 
      rename(Winner_id=P1,
             Loser_id=P2)
  }else{
    
    table_stock=table_stock=table_stock %>% select(colnames)
  }
  
  V_MATCH=rbind(V_MATCH,table_stock)
  
  print(i)  
  
}

rm(table_stock)


load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2017_2023.RData"))

V_TOURNAMENT_2017_2023=V_TOURNAMENT_F %>% filter(Year>=2021)

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2024.RData"))

V_TOURNAMENT_2024=V_TOURNAMENT_F

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2025.RData"))

V_TOURNAMENT_2025=V_TOURNAMENT_F

V_TOURNAMENT_F=rbind(V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024,V_TOURNAMENT_2025)

rm(V_TOURNAMENT_2017_2023,V_TOURNAMENT_2024,V_TOURNAMENT_2025)


V_TOURNAMENT_INFO=V_TOURNAMENT_F %>% 
  select(tournament,Categorie,Country_tournament,Week_tournament,Year,Surface_tournament) %>% 
  unique() %>% 
  mutate(Categorie=case_when(Categorie=="ATP 2000"~"Grand Slam",
                             TRUE~Categorie)) %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament))

rm(V_TOURNAMENT_F)


V_MATCH_t=V_MATCH %>% 
  mutate(CLE_TOURNAMENT=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT_INFO %>% select(-tournament),by=c("CLE_TOURNAMENT","Season"="Year")) %>% 
  select(-CLE_TOURNAMENT) %>% 
  filter(!tournament %in% c("Riyadh - Exhibition","Next Gen Atp Finals")) %>% 
  mutate(Categorie=case_when(tournament %in% c("United Cup","Atp Cup","Davis Cup")~"Team",
                             tournament=="Masters Cup Atp"~"Masters",
                             tournament %like% "Olympics"~"Olympics",
                             tournament=="Reunion Challenger" & Season==2011~"Challenger 80",
                             TRUE~Categorie)) %>% 
  mutate(Surface_tournament=case_when(is.na(Surface_tournament)~Surface,
                                      Categorie=="Masters" & Season %in% c(2005,2006,2007,2008)~"Indoors",
                                      tournament=="Madrid" & Season %in% c(2003:2008)~"Indoors",
                                      tournament=="Bangkok" & Season %in% c(2003:2004)~"Indoors",
                                      TRUE~Surface_tournament)) %>% 
  mutate(Surface_tournament=case_when(Surface_tournament=="clay"~"Clay",
                                      Surface_tournament=="hard"~"Hard",
                                      Surface_tournament=="grass"~"Grass",
                                      Surface_tournament=="indoors"~"Indoors",
                                      TRUE~Surface_tournament)) %>% 
  mutate(Week_tournament=isoweek(Date),
         Country_tournament=case_when(tournament=="Olympics - Paris"~"France",
                                      tournament=="Olympics - Tokyo"~"Japan",
                                      tournament=="Masters Cup Atp"~"Italy",
                                      TRUE~Country_tournament)) 

V_MATCH_t=V_MATCH_t %>% 
  group_by(tournament,Season,Phase,Round,Date,Week_tournament,Winner_id,Loser_id) %>% 
  mutate(CLE_LIGNE=row_number()) %>% 
  filter(CLE_LIGNE==1) %>% 
  select(-CLE_LIGNE)

#V_MATCH_t %>% group_by(Categorie) %>% count()

# "Challenger 125","Challenger 175",
# "Challenger 110","Challenger 100",
# "Challenger 90","Challenger 80",
# "Challenger 75","Challenger 50"

V_MATCH_t=V_MATCH_t %>% 
  filter(Phase=="Main Draw" & 
           (Categorie %in% c("Grand Slam","Olympics","Masters",
                             "ATP 1000","ATP 500",'ATP 250')|tournament %in% c("Atp Cup","United Cup")))

V_MATCH_t=V_MATCH_t %>% ungroup() %>% 
  select(tournament,Season,Date,Week_tournament,Categorie,Surface_tournament,Round,Winner_id,Loser_id,Odd_W,Odd_L,info)


V_RANK=data.frame()

for (i in 2021:2025){
  
  load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",i,".RData"))
  
  V_RANK=rbind(V_RANK,rank)
  
  print(i)  
  
}

V_RANK=V_RANK %>% 
  select(-Move) %>% 
  rename("Player_name"="Player name") 


rm(rank)
rm(V_TOURNAMENT_INFO)
rm(V_MATCH)


V_MATCH_t=V_MATCH_t %>% 
  left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
               rename(Rank_W=Rank,Points_W=Points) %>% 
              mutate(Rank_W=as.numeric(Rank_W),
                     Points_W=as.numeric(Points_W)),
             by=c("Winner_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
left_join(V_RANK %>% select(Rank,Player_name,Points,Week,Year) %>% 
             rename(Rank_L=Rank,Points_L=Points) %>% 
            mutate(Rank_L=as.numeric(Rank_L),
                   Points_L=as.numeric(Points_L)),
           by=c("Loser_id"="Player_name","Week_tournament"="Week","Season"="Year")) %>% 
  mutate(Rank_W=case_when(is.na(Rank_W)~1000,TRUE~Rank_W),
         Points_W=case_when(is.na(Points_W)~10,TRUE~Points_W),
         Rank_L=case_when(is.na(Rank_L)~1000,TRUE~Rank_L),
         Points_L=case_when(is.na(Points_W)~10,TRUE~Points_L))

rm(V_RANK)

##### ELO PRED #####

load(paste0(getwd(),"/Scrapping tennis data/Rank/ELO_RATING_PLAYERS.RData"))

last_elo=function(base,player_name,surface="all",Date_match,tournoi){
  
  if (surface=="all"){
    
    
    elo_player=base %>% 
      filter(Player_name==player_name & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player)
    
    return(elo_player)
    
  }else if (surface=="Grass"){
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_grass) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_grass)
    
    return(elo_player)
    
  }else if (surface=="Clay"){
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_clay) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_clay)
    
    return(elo_player)
    
  }else{
    
    elo_player=base %>% 
      filter(Player_name==player_name & !is.na(Elo_player_hard) & Date<Date_match & tournament!=tournoi) %>% 
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
      arrange(desc(Date),desc(Round)) %>% 
      mutate(ORDRE_ELO=row_number()) %>% 
      filter(ORDRE_ELO==1) %>% 
      select(Player_name,tournament,Date,Elo_player_hard)
    
    return(elo_player)
    
  }
}

penalty=function(diff_date){
  
  penalty=ifelse(between(diff_date,60,80),0.7,
                 ifelse(between(diff_date,81,180),0.85,
                        ifelse(diff_date>180,1,0)))
  
  return(penalty)
}

pb= progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = nrow(V_MATCH_t),
  clear = FALSE,
  width = 60
)

V_MATCH_t$Elo_P1=NA
V_MATCH_t$Elo_P2=NA
V_MATCH_t$Elo_P1_surface=NA
V_MATCH_t$Elo_P2_surface=NA


for (i in 1:nrow(V_MATCH_t)){
  
  P1=V_MATCH_t$Winner_id[i]
  
  P2=V_MATCH_t$Loser_id[i]
  
  surface=V_MATCH_t$Surface_tournament[i]
  
  Date_match=V_MATCH_t$Date[i]
  
  tournoi=V_MATCH_t$tournament[i]
  
  ##### ELO CLASSIC #####
  
  last_elo_p1=last_elo(ELO_RATING_PLAYERS,P1,"all",Date_match,tournoi)
  
  last_elo_p2=last_elo(ELO_RATING_PLAYERS,P2,"all",Date_match,tournoi)
  
  diff_date_p1=Date_match-last_elo_p1$Date
  
  diff_date_p2=Date_match-last_elo_p2$Date
  
  covid=ifelse(between(Date_match,as.Date("2020-08-01"),as.Date("2021-02-01")) ,0,1)
  
  elo_p1=ifelse(is_empty(last_elo_p1$Elo_player)==T,1500,last_elo_p1$Elo_player-(penalty(diff_date_p1)*covid*100))
  
  elo_p2=ifelse(is_empty(last_elo_p2$Elo_player)==T,1500,last_elo_p2$Elo_player-(penalty(diff_date_p2)*covid*100))
  
  ##### ELO SURFACE #####
  
  if (surface %in% c("Indoors","Various")){
    
    surface="Hard"
    
  }else{
    
    surface=surface
  }
  
  last_elo_p1_surface=last_elo(ELO_RATING_PLAYERS,P1,surface,Date_match,tournoi) %>% pull(4)
  
  last_elo_p2_surface=last_elo(ELO_RATING_PLAYERS,P2,surface,Date_match,tournoi) %>% pull(4)
  
  elo_p1_surface=ifelse(is_empty(last_elo_p1_surface)==T,1500,last_elo_p1_surface)
  
  elo_p2_surface=ifelse(is_empty(last_elo_p2_surface)==T,1500,last_elo_p2_surface)

##### assigniation

V_MATCH_t$Elo_P1[i]=elo_p1
V_MATCH_t$Elo_P2[i]=elo_p2
V_MATCH_t$Elo_P1_surface[i]=elo_p1_surface
V_MATCH_t$Elo_P2_surface[i]=elo_p2_surface
  
pb$tick()

}

proba_calcul=function(elo_p1,elo_p2){
  
  proba=1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400))
  
  return(proba)
}


##### CALCUL ACCURACY PRED #####

accuracy=data.frame()

for (i in seq(0, 1, by = 0.01)){
  
  p=i
  
  res=V_MATCH_t %>% 
    group_by(Categorie,Surface_tournament) %>% 
    mutate(
      Proba_P1 = proba_calcul(Elo_P1, Elo_P2),
      Proba_P1_surface = proba_calcul(Elo_P1_surface, Elo_P2_surface),
      Proba_P2 = 1 - Proba_P1,
      Proba_P2_surface = 1 - Proba_P1_surface,
      Proba_P1_W = (p * Proba_P1 + (1-p) * Proba_P1_surface) * 100,
      Proba_P2_W = (p * Proba_P2 + (1-p) * Proba_P2_surface) * 100,
      Predicted_Winner = ifelse(Proba_P1_W > Proba_P2_W, Winner_id, Loser_id),
      Result = ifelse(Predicted_Winner == Winner_id, 1, 0)
    ) %>%
    summarise(accuracy = mean(Result) * 100,
              N=n()) %>% 
    mutate(P=p)
  
  accuracy=rbind(accuracy,res)
  
  print(i)
  
}

p_optimaux <- accuracy %>%
  group_by(Categorie, Surface_tournament) %>%
  filter(accuracy == max(accuracy, na.rm = TRUE)) %>%
  slice(1) %>%  # Si plusieurs p donnent le même max, prendre le premier
  rename(P_optimal = P, accuracy_max = accuracy) %>%
  ungroup()


ggplot(accuracy,aes(x=P,y=accuracy,group=Surface_tournament,color=Surface_tournament))+
  geom_line()+
  # Ajouter les points optimaux
  geom_point(data = p_optimaux,
             aes(x = P_optimal, y = accuracy_max),
             size = 3, shape = 21, fill = "white", stroke = 1.5) +
  # Ajouter des lignes verticales aux optimaux
  geom_vline(data = p_optimaux,
             aes(xintercept = P_optimal, color = Surface_tournament),
             linetype = "dashed", alpha = 0.5) +
  facet_wrap(~Categorie,scales="free")+
  theme_classic()

V_MATCH_t=V_MATCH_t %>% 
  left_join(p_optimaux %>% 
              select(Categorie,Surface_tournament,P_optimal),
            by=c("Categorie","Surface_tournament")) 

V_MATCH_t=V_MATCH_t %>% 
  mutate(
    Proba_P1 = proba_calcul(Elo_P1, Elo_P2),
    Proba_P1_surface = proba_calcul(Elo_P1_surface, Elo_P2_surface),
    Proba_P2 = 1 - Proba_P1,
    Proba_P2_surface = 1 - Proba_P1_surface,
    Proba_P1_W = (P_optimal * Proba_P1 + (1-P_optimal) * Proba_P1_surface) * 100,
    Proba_P2_W = (P_optimal * Proba_P2 + (1-P_optimal) * Proba_P2_surface) * 100,
    Predicted_Winner_elo = ifelse(Proba_P1_W > Proba_P2_W, Winner_id, Loser_id),
    Result = ifelse(Predicted_Winner_elo == Winner_id, 1, 0)
  ) 

V_MATCH_t=V_MATCH_t %>% select(-c(P_optimal,Proba_P1,Proba_P1_surface,Proba_P2,Proba_P2_surface))  

V_MATCH_t=V_MATCH_t %>% 
  mutate(Proba_P1_W=round(Proba_P1_W,2),
         Proba_P2_W=round(Proba_P2_W,2))

# Global accuracy 

V_MATCH_t %>%
  group_by(Result,Categorie) %>% 
  count() %>% 
  arrange(Categorie) %>% 
  ungroup() %>% 
  group_by(Categorie) %>% 
  mutate(Total=sum(n)) %>% 
  ungroup() %>% 
  mutate(prct=n/Total*100) %>% 
  filter(Result==1) %>% 
  select(-Result)



##### VALUE BET DETECTION #####

V_MATCH_value=V_MATCH_t %>% 
  mutate(Favori=ifelse(Rank_W<Rank_L,Winner_id,Loser_id),
         Outsider = ifelse(Favori == Winner_id, Loser_id, Winner_id),
         Outcome=ifelse(Rank_W<Rank_L,"Fav_W","Out_W")) %>% 
  rename(Predicted_Winner_elo=Predicted_Winner) %>% 
  mutate(Odd_F_market = ifelse(Rank_W < Rank_L, Odd_W, Odd_L),
         Odd_O_market = ifelse(Rank_W < Rank_L, Odd_L, Odd_W),
         Odd_P1=round(100/Proba_P1_W,2),
         Odd_P2=round(100/Proba_P2_W,2),
         Odd_F_elo = ifelse(Rank_W < Rank_L, Odd_P1, Odd_P2),
         Odd_O_elo = ifelse(Rank_W < Rank_L, Odd_P2, Odd_P1),
         Predicted_Winner_market=case_when(Odd_F_market<Odd_O_market~Favori,
                                           Odd_O_market<Odd_F_market~Outsider)) %>% 
  select(tournament,Categorie,Date,Round,Rank_W,Rank_L,Winner_id,Loser_id,Favori,Outsider,Predicted_Winner_elo,
         Predicted_Winner_market,
         Outcome,Odd_W,Odd_L,Odd_F_market,Odd_O_market,Odd_F_elo,Odd_O_elo)

### DETECT VALUE #####

M=10

V_MATCH_value=V_MATCH_value %>% 
  mutate(
# CRITÈRE 1 : Accord entre ELO et Marché
Accord_Elo_Market = (Predicted_Winner_elo == Predicted_Winner_market),

# CRITÈRE 2 & 3 : Value sur le FAVORI
Value_F = (Odd_F_market > Odd_F_elo) & 
  ((Odd_F_market / Odd_F_elo) >= 1.05) & Odd_F_market>=1.35,

Ratio_Value_F = ifelse(Value_F, Odd_F_market / Odd_F_elo, NA),

# CRITÈRE 2 & 3 : Value sur l'OUTSIDER
Value_O = (Odd_O_market > Odd_O_elo) & 
  ((Odd_O_market / Odd_O_elo) >= 1.05) & Odd_O_market>=1.35,

Ratio_Value_O = ifelse(Value_O, Odd_O_market / Odd_O_elo, NA),

# DÉTECTION FINALE DE VALUE BET
Has_Value_Bet = Accord_Elo_Market & (Value_F | Value_O),

# QUEL JOUEUR JOUER ?
Value_Bet_On = case_when(
  !Has_Value_Bet ~ "Aucun",
  Value_F & !Value_O ~ "Favori",
  !Value_F & Value_O ~ "Outsider",
  Value_F & Value_O & Ratio_Value_F > Ratio_Value_O ~ "Favori",
  Value_F & Value_O & Ratio_Value_F <= Ratio_Value_O ~ "Outsider",
  TRUE ~ "Aucun"
),

# RATIO DE VALUE RETENU
Ratio_Value_Bet = case_when(
  Value_Bet_On == "Favori" ~ Ratio_Value_F,
  Value_Bet_On == "Outsider" ~ Ratio_Value_O,
  TRUE ~ NA_real_
),

# COTE JOUÉE
Cote_Jouee = case_when(
  Value_Bet_On == "Favori" ~ Odd_F_market,
  Value_Bet_On == "Outsider" ~ Odd_O_market,
  TRUE ~ NA_real_
),

# RÉSULTAT DU PARI
Pari_Gagnant = case_when(
  Value_Bet_On == "Aucun" ~ NA,
  Value_Bet_On == "Favori" & Winner_id == Favori ~ TRUE,
  Value_Bet_On == "Outsider" & Winner_id == Outsider ~ TRUE,
  TRUE ~ FALSE
),

# GAIN/PERTE (mise de 1)
Cash = case_when(
  is.na(Pari_Gagnant) ~ 0,
  Pari_Gagnant ~ M*Cote_Jouee - M,
  !Pari_Gagnant ~ -M
),

  Tranche_Cote = cut(Cote_Jouee,
                     breaks = c(1.35, 1.5, 1.75, 2, 2.5, 3, 4, 5, Inf),
                     labels = c("1.35-1.50", "1.50-1.75", 
                                "1.75-2.00", "2.00-2.50", "2.50-3.00", 
                                "3.00-4.00", "4.00-5.00", "5.00+"),
                     include.lowest = TRUE,
                     right = FALSE),


Tranche_ratio = cut(Ratio_Value_Bet,
                   breaks = c(1.05,1.1,1.15,1.2,1.25,Inf),
                   labels = c("1.05-1.1", "1.1-1.15", 
                              "1.15-1.2", "1.2-1.25", "1.25+"),
                   include.lowest = TRUE,
                   right = FALSE)

) %>% select(-c(Value_F,Value_O,Ratio_Value_F,Ratio_Value_O,Has_Value_Bet))
