
j=55

for (j in 1:nrow(NO)){
  
i=NO$tournament[j]

Date_i=NO$Date[j]

Tournament_red=V_TOURNAMENT3 %>% filter((tournament==i & Date==Date_i)) %>% 
  select(tournament,Date)

Annee_filtre=ifelse(year(Date_i)==2012,year(Date_i)+1,year(Date_i)-1)

# Ajouter des conditions pour l'année de filtre
#Si 2017 alors on prend +1 si 2023 on prend -1 sinon on prend -1
Red_values=V_TOURNAMENT3 %>% 
  select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N) %>% 
  filter(tournament==i & Year==Annee_filtre) %>% 
  select(-Date)

test=Red_values %>% filter(Round=="Winner") %>% 
  pull(Ranking_points) %>% 
  as.numeric()

test=ifelse(is_empty(test)==T,0,test)

if(nrow(Red_values)!=0 & test!=0){
  
  Tournament_red=Tournament_red %>% 
    left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
    select("tournament","Date","Categorie","Round",           
           "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
           "Year","N") %>% 
    unique()
  
  V_TOURNAMENT3=V_TOURNAMENT3 %>% 
    filter(!(tournament==i & Date==Date_i)) %>% 
    rbind(Tournament_red)
  
  print(paste0(j,"-",i,"-",Date_i))
  
}else{
  
  Annee_filtre=ifelse(Annee_filtre+1!=year(Date_i),Annee_filtre+1,Annee_filtre-1)
  
  Red_values=V_TOURNAMENT3 %>% 
    select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N) %>% 
    filter(tournament==i & year(Date)==Annee_filtre) %>% 
    select(-Date)
  
  if(nrow(Red_values)==0){
    
    Red_values=V_TOURNAMENT3 %>% 
                filter(year(Tournament_red$Date) %>% unique() & 
                         tournament==Tournament_red$tournament %>% unique()) %>% 
      select(-Date)
    }else{
    Red_values=Red_values
}
                
                
  Tournament_red=Tournament_red %>% 
    left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
    select("tournament","Date","Categorie","Round",           
           "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
           "Year","N") %>% 
    unique()
  
  V_TOURNAMENT3=V_TOURNAMENT3 %>% 
    filter(!(tournament==i & Date==Date_i)) %>% 
    rbind(Tournament_red)
  
  print(paste0(j,"-",i,"-",Date_i))
  }

}


l_2012=list_tournament(2012)

l_2012_info=info_tournament(l_2012)

l_2012_info=l_2012_info %>% left_join(l_2012,by=c("tournament"))

l_2013=list_tournament(2013)

l_2013_info=info_tournament(l_2013)

l_2013_info=l_2013_info %>% left_join(l_2013,by=c("tournament"))

list=rbind(l_2012_info,l_2013_info)

list$tournament2=toupper(list$tournament) 

list=list %>% select(-tournament)

  t2=t %>%
  mutate(tournament2=toupper(tournament)) %>% 
  left_join(list,by=c("tournament2","Date")) %>% 
  select(-tournament2)

list %>% 
  filter(Round=="Winner" & `Prize money`=="10,800 $") %>% 
    select(tournament2,`Ranking points`) %>% 
  pull(`Ranking points`) %>% 
  table()

7,200$ ==> Challenger 80
6.150$ ==> C 80
5000$ ==> C80
4300 ==> c80
14,400 $ ==> C100
10,800 $ ==> C100

Istanbul 3 Chall.
Johannesburg Chall.

100 60 35 18 8 0
80 48 29 15 7 0 

V_TOURNAMENT4 %>% 
  mutate(Ranking_points=case_when())
t2 %>% filter(Round=="Winner") %>% pull(`Prize money`) %>% table()
  

red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                      Ranking_points=c(0,7,15,29,48,80))

red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                      Ranking_points=c(0,8,18,35,60,100))


j=1

t$tournament

for (j in 1:nrow(t)){
  
  i=t$tournament[j]
  
  Date_i=t$Date[j]
  
  Tournament_red=V_TOURNAMENT3 %>% filter((tournament==i & Date==Date_i)) %>% 
    select(tournament,Date,Categorie,Country_tournament,Surface_tournament,Week_tournament,Year,N)
  
  if(!i %in% c("Istanbul 3 Chall.","Johannesburg Chall.")){
    
    Red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                          Ranking_points=c(0,7,15,29,48,80),
                          tournament=rep(i,6))
    
    Tournament_red=Tournament_red %>% 
      left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
      select("tournament","Date","Categorie","Round",           
             "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
             "Year","N") %>% 
      unique()
    
    V_TOURNAMENT4=V_TOURNAMENT4 %>% 
      filter(!(tournament==i & Date==Date_i)) %>% 
      rbind(Tournament_red)
    
    print(paste0(j,"-",i,"-",Date_i))
    
  }else{
    
    Red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                          Ranking_points=c(0,8,18,35,60,100),
                          tournament=rep(i,6))
    
    Tournament_red=Tournament_red %>% 
      left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
      select("tournament","Date","Categorie","Round",           
             "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
             "Year","N") %>% 
      unique()
    
    V_TOURNAMENT4=V_TOURNAMENT4 %>% 
      filter(!(tournament==i & Date==Date_i)) %>% 
      rbind(Tournament_red)
    
    print(paste0(j,"-",i,"-",Date_i))
  }
    
    
    
}
  



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


V_ATP_P2=function(dt,vect_a,vect_b,m,categorie){
  
  V_i=V_TOURNAMENT4 %>% filter(Categorie %in% categorie)
  
  # step 1 pour les petits tableaux
  V_TOURNAMENT_RED = V_i %>% 
    filter(N >= m) 
  
  V_TOURNAMENT_RED2 = V_i %>% 
    filter(N >= m) %>%
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


V_90_125=V_ATP(V_TOURNAMENT4,
                  vect_a=c("Q-R16","Q-QF","Q-QW"),
                  vect_b=c(0,2,5),
                  m=32,
                  c("Challenger 125","Challenger 110","Challenger 100","Challenger 90"),
                  rule="N<m")

V_80=V_ATP(V_TOURNAMENT4,
               vect_a=c("Q-R16","Q-QF","Q-QW"),
               vect_b=c(0,2,4),
               m=32,
               "Challenger 80",
              rule="N<m")

V_TOURNAMENT_F=rbind(V_80,V_90_125,V_250,V_500,V_1000,V_2000) %>% arrange(Date)

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2012_2016.RData"))



V_TOURNAMENT4 %>% 
  group_by(Categorie) %>% 
  summarise(Match=max(N),Match2=min(N)) %>% 
  filter(Categorie %like% "Chall")

V_TOURNAMENT4 %>% filter(Categorie %in% c("Challenger 125","Challenger 110","Challenger 100","Challenger 90")) %>% 
  pull(N) %>% 
  table()

verif=V_MATCH %>% 
  filter(Phase=="Qualification") %>% 
  filter(tournament %in% V_90_125$tournament) %>% 
  group_by(tournament,Round) %>% 
  summarise(N=n())


