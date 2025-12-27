

V_MATCH=table_stock %>% 
  mutate(Season=2025)

count=V_MATCH %>% 
  filter(Phase=="Main Draw") %>% 
  group_by(tournament,Season) %>% 
  summarise(N=n(),Date=min(Date))


count$tournament=toupper(count$tournament)

V_TOURNAMENT3$tournament2=toupper(V_TOURNAMENT3$tournament)
# V_TOURNAMENT3=V_TOURNAMENT3 %>% 
#   mutate(Year=case_when(V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==1 ~year(V_TOURNAMENT3$Date),
#                         V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
#                         V_TOURNAMENT3$Week_tournament==1 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
#                           TRUE ~ year(V_TOURNAMENT3$Date)))

V_TOURNAMENT3=V_TOURNAMENT3 %>% 
  left_join(count %>% select(tournament,Season,N),by=c("tournament2"="tournament",
                                                       "Year"="Season")) %>% 
  select(-tournament2)

NO=V_TOURNAMENT3 %>% 
  filter(Categorie %in% c("ATP ","ATP 0","Challenger ","Challenger 0")) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Olympics - Paris","Masters Cup Atp"))


V_TOURNAMENT3 %>% 
  filter(is.na(Round) & is.na(Categorie))

V_TOURNAMENT3=V_TOURNAMENT3 %>% rename(Ranking_points="Ranking points")

#NO=NO %>% filter(is.na(N)) # Tournoi à supprimer de la base de reférence

NO=NO %>% filter(!is.na(N))

NO=NO %>% select(tournament,Date,Categorie) %>% unique()

NO=NO %>% 
  mutate(Points=c(125,75,50,75,75))

V_TOURNAMENT4=V_TOURNAMENT3 %>% 
  filter(!is.na(N))

# list_red=c("Canberra 2 Chall.","New Delhi Chall.","Hersonissos 6 Chall.","Lima 3 Challenger","Knoxville Chall.")

for (i in 1:nrow(NO)){
  
  tournament_to_red=NO$tournament[i]
  
  Date_tournement_to_red=NO$Date[i]
  
  Points=NO$Points[i]
  
  Ranking_points=Ranking_points <- switch(as.character(Points),
                                          "125" = c(0, 8, 16, 35, 64, 125),
                                          "75" = c(0, 6, 12, 22, 44, 75),
                                          c(0, 4, 8, 14, 25, 50))  # valeur par défaut
  
  Tournament_red=V_TOURNAMENT3 %>% 
    filter((tournament==tournament_to_red & Date==Date_tournement_to_red)) %>% 
    select(tournament,Date,Categorie,Country_tournament,Surface_tournament,Week_tournament,Year,N)
  
  Red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                        Ranking_points=Ranking_points,
                        tournament=rep(tournament_to_red,6))
  
  Tournament_red=Tournament_red %>% 
    left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
    select("tournament","Date","Categorie","Round",           
           "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
           "Year","N") %>% 
    unique()
  
  V_TOURNAMENT4=V_TOURNAMENT4 %>% 
    filter(!(tournament==tournament_to_red & Date==Date_tournement_to_red)) %>% 
    rbind(Tournament_red)
  
}


V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  mutate(Categorie=case_when(tournament=="Canberra 2 chall."~"Challenger 125",
                             tournament=="New Delhi chall."~"Challenger 75",
                             tournament=="Hersonissos 6 Chall."~"Challenger 50",
                             tournament=="Lima 3 Challenger"~"Challenger 75",
                             tournament=="Knoxville Chall."~"Challenger 75",
                             TRUE~Categorie))  


V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  mutate(Categorie = case_when(
    tournament %in% c('Australian Open', 'French Open', 'Wimbledon', 'Us Open') ~ 'Grand Slam',
    grepl('Olympics', tournament) ~ 'Olympics',
    tournament == 'Davis Cup' ~ 'Davis Cup',
    tournament %in% c('United Cup', 'Atp Cup') ~ 'Team Cup',
    tournament == 'Masters Cup Atp' ~ 'Masters Cup',
    TRUE ~ Categorie            # Cas par défaut : utilise la valeur de f.Categorie
  ))

save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2025.RData"))

V_ATP=function(dt,vect_a,vect_b,m,categorie,rule){
  
  V_i=V_TOURNAMENT4 %>% filter(Categorie %in% categorie)
  
  # step 1 pour les petits tableaux
  V_TOURNAMENT_RED = V_i %>% 
    filter(!!rlang::parse_expr(rule)) 
  
  V_TOURNAMENT_RED2 = V_i %>% 
    filter(!!rlang::parse_expr(rule)) %>%
    select(tournament, Round, Ranking_points) %>%
    unique()
  
  n=length(vect_a)
  
  # Créer les nouvelles lignes avec 3 lignes par tournoi
  new_rows <- V_TOURNAMENT_RED2 %>%
    distinct(tournament) %>%              # Obtenir les tournois uniques
    slice(rep(1:n(), each = n)) %>%        # Répéter chaque tournoi 3 fois
    mutate(Round = rep(vect_a, 
                       times = n_distinct(tournament)),
           Ranking_points = rep(vect_b, 
                                times = n_distinct(tournament))) # Ajouter les rounds et points
  
  V_TOURNAMENT_RED2 = rbind(new_rows, V_TOURNAMENT_RED2)
  
  V_TOURNAMENT_RED_P1 = V_TOURNAMENT_RED %>%
    select(tournament, Date, Categorie, Country_tournament, Surface_tournament, Week_tournament, Year) %>%
    left_join(V_TOURNAMENT_RED2, by = "tournament") %>%
    unique()
  
  return(V_TOURNAMENT_RED_P1)
}


V_2000=V_ATP(V_TOURNAMENT4,
             vect_a=c("Q-1R","Q-2R","Q-3R","Q-QW"),
             vect_b=c(0,8,16,30),
             m=128,
             "Grand Slam",
             rule="N<m")

V_1000_P1=V_ATP(V_TOURNAMENT4,
                vect_a=c("Q-1R","Q-R16","Q-R16W"),
                vect_b=c(0,16,30),
                m=56,
                "ATP 1000",
                rule="N<m")

V_1000_P2=V_ATP(V_TOURNAMENT4,
                vect_a=c("Q-1R","Q-2R","Q-2RW"),
                vect_b=c(0,10,20),
                m=95,
                "ATP 1000",
                rule="N>=m")

V_1000=rbind(V_1000_P1,V_1000_P2)

V_500_P1=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,13,25),
               m=32,
               "ATP 500",
               rule="N<m")

V_500_P2=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,8,16),
               m=47,
               "ATP 500",
               rule="N>=m")

V_500=rbind(V_500_P1,V_500_P2)

V_250_P1=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,7,13),
               m=32,
               "ATP 250",
               rule="N<m")

V_250_P2=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,4,8),
               m=47,
               "ATP 250",
               rule="N>=m")

V_250=rbind(V_250_P1,V_250_P2)


V_175=V_ATP(V_TOURNAMENT4,
            vect_a=c("Q-R16","Q-QF","Q-QW"),
            vect_b=c(0,3,6),
            m=32,
            "Challenger 175",
            rule="N<=m")

V_125=V_ATP(V_TOURNAMENT4,
            vect_a=c("Q-R16","Q-QF","Q-QW"),
            vect_b=c(0,3,5),
            m=47,
            "Challenger 125",
            rule="N<=m")

V_75_100=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,2,4),
               m=32,
               c("Challenger 75","Challenger 100"),
               rule="N<m")

V_50=V_ATP(V_TOURNAMENT4,
           vect_a=c("Q-R16","Q-QF","Q-QW"),
           vect_b=c(0,1,3),
           m=32,
           "Challenger 50",
           rule="N<m")

V_TOURNAMENT_F=rbind(V_50,V_75_100,V_125,V_175,V_250,V_500,V_1000,V_2000) %>% arrange(Date) %>% 
  unique()

V_TOURNAMENT_F=V_TOURNAMENT_F %>% 
  group_by(tournament,Date,Categorie,Round,Week_tournament,Year,Country_tournament,Surface_tournament) %>% 
  mutate(Ranking_points=max(Ranking_points,na.rm = T)) %>% 
  ungroup() %>% 
  unique()


V_TOURNAMENT_F=V_TOURNAMENT_F %>% 
  mutate(Ranking_points=as.numeric(Ranking_points))

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2025.RData"))
