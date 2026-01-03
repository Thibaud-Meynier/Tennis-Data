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


p=0.55

V_MATCH_t=V_MATCH_t %>% 
  mutate(Proba_P1=proba_calcul(Elo_P1,Elo_P2),
         Proba_P1_surface=proba_calcul(Elo_P1_surface,Elo_P2_surface)) %>% 
  mutate(Proba_P2=(1-Proba_P1),
         Proba_P2_surface=(1-Proba_P1_surface)) %>% 
  mutate(Proba_P1=round(Proba_P1,2),
         Proba_P2=round(Proba_P2,2),
         Proba_P1_surface=round(Proba_P1_surface,2),
         Proba_P2_surface=round(Proba_P2_surface,2)) %>% 
  mutate(Proba_P1_W=(p*Proba_P1+(1-p)*Proba_P1_surface)*100,
         Proba_P2_W=(p*Proba_P2+(1-p)*Proba_P2_surface)*100) %>% 
  select(-c(Proba_P1,Proba_P2,Proba_P1_surface,Proba_P2_surface)) %>% 
  mutate(Favori=ifelse(Rank_W<Rank_L,Winner_id,Loser_id),
         Outcome=ifelse(Rank_W<Rank_L,"Fav_W","Out_W")) %>% 
  mutate(Predicted_Winner=ifelse(Proba_P1_W>Proba_P2_W,Winner_id,Loser_id)) %>% 
  mutate(Type_Predicted_elo=ifelse(Predicted_Winner==Favori,"Fav_W","Out_W")) %>% 
  select(-c(Predicted_Winner)) %>% 
  select(-c(Predicted_Winner_Odd))

V_MATCH_value=V_MATCH_t %>% 
  select(tournament,Categorie,Date,Round,Winner_id,Loser_id,Favori,Rank_W,Rank_L,Points_W,Points_L,
         Outcome,Odd_W,Odd_L,Proba_P1_W,Proba_P2_W,Type_Predicted_elo) %>% 
  mutate(Odd_P1=round(100/Proba_P1_W,2),
         Odd_P2=round(100/Proba_P2_W,2)) %>% 
  select(-c(Proba_P1_W,Proba_P2_W))

MISE=5

V_MATCH_value <- V_MATCH_t %>% 
  select(tournament, Categorie,Date, Round, Winner_id, Loser_id, Favori, Rank_W, Rank_L, 
         Points_W, Points_L, Outcome, Odd_W, Odd_L, Proba_P1_W, Proba_P2_W, 
         Type_Predicted_elo) %>% 
  mutate(
    # Identifier l'outsider
    Outsider = ifelse(Favori == Winner_id, Loser_id, Winner_id),
    
    # Calculer les cotes ELO pour P1 et P2
    Odd_P1_elo = round(100/Proba_P1_W, 2),
    Odd_P2_elo = round(100/Proba_P2_W, 2),
    
    # COTES MARCHÉ : Transformer en Favori/Outsider
    Odd_F_market = ifelse(Rank_W < Rank_L, Odd_W, Odd_L),
    Odd_O_market = ifelse(Rank_W < Rank_L, Odd_L, Odd_W),
    
    # COTES ELO : Transformer en Favori/Outsider
    # Si Winner est le favori : Odd_P1_elo = Favori, Odd_P2_elo = Outsider
    # Si Loser est le favori : Odd_P2_elo = Favori, Odd_P1_elo = Outsider
    Odd_F_elo = ifelse(Rank_W < Rank_L, Odd_P1_elo, Odd_P2_elo),
    Odd_O_elo = ifelse(Rank_W < Rank_L, Odd_P2_elo, Odd_P1_elo)
    
  ) %>% 
  select(tournament,Categorie, Date, Round, 
         Winner_id, Loser_id, 
         Favori, Outsider,
         Rank_W, Rank_L, 
         Points_W, Points_L,
         Outcome,
         # Cotes marché
         Odd_F_market, Odd_O_market,
         # Cotes ELO
         Odd_F_elo, Odd_O_elo,
         Type_Predicted_elo) %>% 
  mutate(VALUE=case_when(Odd_F_market>Odd_F_elo~"Value Fav",
                         Odd_O_market>Odd_O_elo~"Value Out",
                         TRUE~"No value")) %>% 
  mutate(ODD_VALUE=case_when(Odd_F_market>Odd_F_elo~Odd_F_market,
                         Odd_O_market>Odd_O_elo~Odd_O_market,
                         TRUE~NA)) %>% 
  mutate(RATIO_VALUE=case_when(VALUE=="Value Fav"~round(Odd_F_market/Odd_F_elo,2),
                               VALUE=="Value Out"~round(Odd_O_market/Odd_O_elo,2),
                               TRUE~NA)) %>% 
  mutate(Strategy_Win=case_when(VALUE=="Value Fav" & Winner_id==Favori~1,
                                    VALUE=="Value Out" & Winner_id==Outsider~1,
                                    VALUE=="No value"~NA,
                                    TRUE~0)) %>% 
  mutate(Cash=case_when(Strategy_Win==1~(ODD_VALUE-1)*MISE,
                        Strategy_Win==0~-MISE,
                        TRUE~0)) %>% 
  filter(RATIO_VALUE>=1.05 & RATIO_VALUE<=1.25 & between(ODD_VALUE,1.35,4.5))
  

# Créer des tranches de ratio
performance_par_ratio <- V_MATCH_value %>%
  filter(VALUE != "No value") %>%
  mutate(
    Tranche_ratio = cut(RATIO_VALUE,
                        breaks = c(1, 1.05, 1.10, 1.15, 1.20, 1.30, Inf),
                        labels = c("1.00-1.05", "1.05-1.10", "1.10-1.15", 
                                   "1.15-1.20", "1.20-1.30", "1.30+"),
                        include.lowest = TRUE)
  ) %>%
  group_by(Tranche_ratio,VALUE) %>%
  summarise(
    N_paris = n(),
    Taux_reussite = mean(Strategy_Win, na.rm = TRUE) * 100,
    N_gagnants = sum(Strategy_Win, na.rm = TRUE),
    Profit_net = sum(Cash),
    ROI = (sum(Cash) / n()) * 100,
    Cote_moyenne = mean(ODD_VALUE, na.rm = TRUE)
  ) %>%
  arrange(Tranche_ratio)

cat("\n=== PERFORMANCE PAR TRANCHE DE RATIO ===\n")
print(performance_par_ratio)


ggplot(V_MATCH_value %>% 
         filter(ODD_VALUE>2) %>% 
         group_by(Categorie) %>% 
         mutate(Obs=row_number(),
                cumul_cash=cumsum(Cash)) %>% 
         arrange(Date),aes(x=Obs,y=cumul_cash))+
  geom_line()+
  facet_wrap(~Categorie,scales="free")

# Fonction pour calculer l'accuracy
calculer_accuracy <- function(data, p) {
  data %>%
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
    summarise(accuracy = mean(Result) * 100) %>%
    pull(accuracy)
}

# Tester différents seuils
seuils_p <- seq(0, 1, by = 0.01)

resultats <- data.frame(
  p = seuils_p,
  accuracy = sapply(seuils_p, calculer_accuracy, data = V_MATCH_t)
)

# Meilleur p
meilleur_p <- resultats[which.max(resultats$accuracy), ]

# Visualiser
ggplot(resultats, aes(x = p, y = accuracy)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(data = meilleur_p, 
             aes(x = p, y = accuracy), 
             color = "red", size = 4) +
  geom_vline(xintercept = meilleur_p$p, 
             linetype = "dashed", color = "red", alpha = 0.5) +
  geom_hline(yintercept = meilleur_p$accuracy,
             linetype = "dashed", color = "red", alpha = 0.5) +
  annotate("text", 
           x = meilleur_p$p, 
           y = meilleur_p$accuracy + 1,
           label = paste0("p = ", meilleur_p$p, "\n", 
                          round(meilleur_p$accuracy, 2), "%"),
           color = "red", fontface = "bold") +
  labs(
    title = "Optimisation du seuil p",
    subtitle = paste0("Meilleur p = ", meilleur_p$p, 
                      " (Accuracy = ", round(meilleur_p$accuracy, 2), "%)"),
    x = "Seuil p (poids ELO général)",
    y = "Accuracy (%)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

