library(data.table)

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Extraction/ATP_2009_Extraction.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT3_2009.RData")

load("C:/Users/Thiti/Desktop/Tennis-Data/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2012_2016.RData")

V_MATCH=table_stock %>% 
  mutate(Season=2009)

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

#V_TOURNAMENT3=V_TOURNAMENT3 %>% rename(Ranking_points="Ranking points")

#NO=NO %>% filter(is.na(N)) # Tournoi à supprimer de la base de reférence

NO=NO %>% filter(!is.na(N))

NO=NO %>% select(tournament,Date,Categorie) %>% unique()

# for (i in 1:nrow(NO)){
#   
#   n=V_TOURNAMENT_F %>% 
#     filter(tournament==NO$tournament[i] & !Round %like% "Q-" & Year==2012) %>% 
#     select(tournament,Round,Ranking_points) %>% 
#     count()
#   
#   print(paste0(NO$tournament[i],": ",n))
#   
#   }
# 
# 
# V_TOURNAMENT_F %>% filter(Categorie=="Challenger 80")
# 
# 
# V_TOURNAMENT3 %>% filter(tournament!=tournament)


V_TOURNAMENT3_RED=V_TOURNAMENT3

i=1

for (i in 1:nrow(NO)){
  
  tournament=NO$tournament[i]
  
  date=NO$Date[i]
  
  tournament_red=V_TOURNAMENT_F %>% 
    filter(tournament==NO$tournament[i] & !Round %like% "Q-" & Year==2010) %>% 
    select(tournament,Round,Ranking_points,Categorie) %>% 
    left_join(V_TOURNAMENT3 %>% select(-c(Round,Categorie,Ranking_points)),by=c("tournament")) %>% 
    unique()
  
  if(nrow(tournament_red)!=0){
    
    V_TOURNAMENT3_RED=V_TOURNAMENT3_RED %>% 
      filter(!(tournament==NO$tournament[i] & Date==NO$Date[i])) %>% 
      rbind(tournament_red)
      
  }else{
    
    print("no red")
    
  }
  
  print(tournament)
}

V_TOURNAMENT3=V_TOURNAMENT3_RED

NO=V_TOURNAMENT3 %>% 
  filter(Categorie %in% c("ATP ","ATP 0","Challenger ","Challenger 0")) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Olympics - Paris","Masters Cup Atp"))

NO=NO %>% filter(!is.na(N))

NO=NO %>% select(tournament,Date,Categorie) %>% unique()

V_TOURNAMENT3_RED=V_TOURNAMENT3

replace_one_tournament=function(Categorie_i,tournament_i,Date_i){
  
  tournament_red=V_TOURNAMENT_F %>% 
    filter((Categorie==Categorie_i & !(Round %like% "Q-") & Year==2012)) %>% 
    head(6) %>% 
    select(tournament,Round,Ranking_points,Categorie) %>% 
    mutate(tournament=tournament_i) %>% 
    left_join(V_TOURNAMENT3 %>% select(-c(Round,Categorie,Ranking_points)),by=c("tournament")) %>% 
    unique()
  
  
  V_TOURNAMENT3_RED=V_TOURNAMENT3_RED %>% 
    filter(!(tournament==tournament_i & Date==Date_i)) %>% 
    rbind(tournament_red)
  
  return(V_TOURNAMENT3_RED)
  
}

tournament_i="Indianapolis"

Categorie_i='ATP 250'

Date_i="2009-07-19"


V_TOURNAMENT3=replace_one_tournament(Categorie = Categorie_i,
                                     tournament=tournament_i,
                                     Date=Date_i)

V_TOURNAMENT3_RED=V_TOURNAMENT3


for (i in 1:nrow(NO)){
  
  tournament_i=NO$tournament[i]
  
  Categorie_i='Challenger 80'
  
  Date_i=NO$Date[i]
  
  V_TOURNAMENT3_RED=replace_one_tournament(Categorie = Categorie_i,
                                       tournament=tournament_i,
                                       Date=Date_i)
  
  print(tournament_i)
}

V_TOURNAMENT3=V_TOURNAMENT3_RED

# Ajout de la master cup 

tournament_red=V_TOURNAMENT3 %>% 
  filter(tournament=="Masters Cup Atp") %>% 
  select(-c(Round,Ranking_points))

name=tournament_red$tournament %>% unique()

MainDrawMC=data.frame("tournament"=name,
                      "Round"=c("RR","RRW","SF","SFW","F","W"),
                      "Ranking_points"=c(0,200,0,400,0,500))

tournament_red=tournament_red %>% 
  left_join(MainDrawMC,by=c("tournament")) %>% 
  select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N)

V_TOURNAMENT3=V_TOURNAMENT3 %>% filter(tournament!=name)

V_TOURNAMENT3=rbind(V_TOURNAMENT3,tournament_red)

V_TOURNAMENT3=V_TOURNAMENT3 %>% filter(!is.na(N))

table(V_TOURNAMENT3$Categorie)

V_TOURNAMENT3=V_TOURNAMENT3 %>% 
  mutate(Categorie = case_when(
    tournament %in% c('Australian Open', 'French Open', 'Wimbledon', 'Us Open') ~ 'Grand Slam',
    grepl('Olympics', tournament) ~ 'Olympics',
    tournament == 'Davis Cup' ~ 'Davis Cup',
    tournament %in% c('United Cup', 'Atp Cup') ~ 'Team Cup',
    tournament == 'Masters Cup Atp' ~ 'Masters Cup',
    TRUE ~ Categorie            # Cas par défaut : utilise la valeur de f.Categorie
  ))

V_TOURNAMENT4=V_TOURNAMENT3

save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2009.RData"))

# Ajout des points de qualifs

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
             vect_b=c(0,8,16,25),
             m=128,
             "Grand Slam",
             rule="N<m")

V_1000_P1=V_ATP(V_TOURNAMENT4,
                vect_a=c("Q-1R","Q-R16","Q-R16W"),
                vect_b=c(0,16,25),
                m=56,
                "ATP 1000",
                rule="N<m")

V_1000_P2=V_ATP(V_TOURNAMENT4,
                vect_a=c("Q-1R","Q-2R","Q-2RW"),
                vect_b=c(0,8,16),
                m=95,
                "ATP 1000",
                rule="N>=m")

V_1000=rbind(V_1000_P1,V_1000_P2)

V_500_P1=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,10,20),
               m=32,
               "ATP 500",
               rule="N<m")

V_500_P2=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,4,10),
               m=47,
               "ATP 500",
               rule="N>=m")

V_500=rbind(V_500_P1,V_500_P2)

V_250_P1=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,6,12),
               m=32,
               "ATP 250",
               rule="N<m")

V_250_P2=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,3,5),
               m=47,
               "ATP 250",
               rule="N>=m")

V_250=rbind(V_250_P1,V_250_P2)


V_75_125=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,2,4),
               m=32,
               c("Challenger 75","Challenger 80","Challenger 90","Challenger 100","Challenger 110","Challenger 125","Challenger 48"),
               rule="N<m")



V_TOURNAMENT_F=rbind(V_75_125,V_250,V_500,V_1000,V_2000) %>% arrange(Date) %>% 
  unique()

V_TOURNAMENT_F=V_TOURNAMENT_F %>% 
  group_by(tournament,Date,Categorie,Round,Week_tournament,Year,Country_tournament,Surface_tournament) %>% 
  mutate(Ranking_points=max(Ranking_points,na.rm = T)) %>% 
  ungroup() %>% 
  unique()


V_TOURNAMENT_F=V_TOURNAMENT_F %>% 
  mutate(Ranking_points=as.numeric(Ranking_points))

setdiff(V_TOURNAMENT4$tournament,V_TOURNAMENT_F$tournament)

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2009.RData"))

