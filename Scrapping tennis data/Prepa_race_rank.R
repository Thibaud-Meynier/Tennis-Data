
library(data.table)

V_TOURNAMENT2=data.frame()

for (i in 2025:2025){
  
  list=list_tournament(i)
  
  calendar_info=info_tournament(list)
  
  list=list %>% 
    left_join(calendar_info,by=c("tournament"="tournament")) %>% 
    group_by(tournament) %>% 
    mutate(Max_Pts=max(as.numeric(`Ranking points`))) %>% 
    ungroup() %>% 
    mutate(Categorie=paste(Categorie,ifelse(is.na(Max_Pts)==T,"",Max_Pts))) %>% 
    select(tournament,Date,Categorie,Round,`Ranking points`)
  
  V_TOURNAMENT2=rbind(V_TOURNAMENT2,list)
  
  print(i)
  
}


V_TOURNAMENT3=V_TOURNAMENT2 %>% mutate(tournament2=toupper(tournament)) %>% 
  left_join(V_TOURNAMENT %>% mutate(tournament2=toupper(tournament)) %>% 
              select(tournament,tournament2,Country_tournament,Date,Surface_tournament,Week_tournament,Year),by=c("tournament2","Date"))

#tournament_list=V_TOURNAMENT

#calendar_info=info_tournament(tournament_list)

V_TOURNAMENT3=V_TOURNAMENT3 %>% rename(tournament=tournament.x) 
V_TOURNAMENT3=V_TOURNAMENT3%>% select(1:5,8:11)

V_TOURNAMENT3=V_TOURNAMENT3 %>% filter(Date<Sys.Date())

#### DEBUT REDRESSEMENT #####
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

##### Stats tournois ATP #####

ATP_250=V_TOURNAMENT4 %>% filter(Categorie=="ATP 250") %>% 
  select(tournament,Year,N,Surface_tournament)%>% 
  unique()

table(ATP_250$N)

ATP_500=V_TOURNAMENT4 %>% filter(Categorie=="ATP 500") %>% 
  select(tournament,Year,N,Surface_tournament)%>% 
  unique()

table(ATP_500$N)

ATP_1000=V_TOURNAMENT4 %>% filter(Categorie=="ATP 1000") %>% 
  select(tournament,Year,N,Surface_tournament)%>% 
  unique()

table(ATP_1000$N)

##### REDRESSEMENT DONNEES ######

# Pour un tournoi de la liste NO





NO2=data.frame(tournament=c("Charleston Chall.","Playford Chall.","Calgary Challenger","Cologne","Cologne 2",
                      "Brasília Chall.","Buenos Aires 4 Chall.","Corrientes Chall.",
                      "Buenos Aires 5 Chall.","Buenos Aires 5 Chall."),
         Date=c("2022-09-26","2018-12-31","2020-02-24","2020-10-12","2020-10-19","2021-11-22","2022-04-25","2022-06-13","2022-06-20","2023-04-24")) 

# Redressement spécial pour cologne car pas d'autres tournois

V_TOURNAMENT3_bis=V_TOURNAMENT3

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT3.RData"))

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT3_2024.RData"))



for (j in 1:nrow(NO)){
  
  i=NO$tournament[j]
  
  Date_i=NO$Date[j]
  
  Tournament_red=V_TOURNAMENT3 %>% filter((tournament==i & Date==Date_i)) %>% 
    select(tournament,Date)
  
  Annee_filtre=ifelse(year(Date_i)==2024,year(Date_i)-1,year(Date_i)-2)
  
  # Ajouter des conditions pour l'année de filtre
  #Si 2017 alors on prend +1 si 2023 on prend -1 sinon on prend -1
  Red_values=V_TOURNAMENT3_bis %>% 
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

# RED 2024 

NO=NO %>% select(tournament,Date,Country_tournament,Surface_tournament,Week_tournament,Year) %>% 
  unique() %>% 
  left_join(V_TOURNAMENT3_bis %>% 
              filter(Year==2023) %>% 
              select(tournament,Categorie),by=c("tournament")) %>% 
  unique() %>% 
  mutate(Categorie=case_when(tournament %in% "Bonn Challenger"~"Challenger 75",
                             TRUE~Categorie))

test=V_TOURNAMENT3 %>% 
  filter(Categorie %in% c("ATP 250","Challenger 125","Challenger 100","Challenger 75")) %>% 
  filter(tournament!="Winston Salem") %>% 
  select(Categorie,Round,Ranking_points) %>% unique()

NO=NO %>% 
  left_join(test,by="Categorie")

NO=NO %>% select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year)

V_TOURNAMENT3=V_TOURNAMENT3 %>% 
  filter(!(tournament %in% NO$tournament & Date %in% NO$Date)) %>% 
  rbind(NO)

V_TOURNAMENT3$Year=case_when(V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==1 ~year(V_TOURNAMENT3$Date),
                            V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
                            V_TOURNAMENT3$Week_tournament==1 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
                            TRUE ~ year(V_TOURNAMENT3$Date))


t=V_TOURNAMENT3 %>% filter(!is.na(N) & is.na(Round)) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals"))

t=V_TOURNAMENT3 %>% 
  filter(Categorie %in% c("ATP ","ATP 0","Challenger ","Challenger 0")) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Masters Cup Atp",
                            "Challenger Tour Finals","Olympics - London","Olympics - Rio De Janeiro")) %>% 
  filter(!is.na(N)) %>% 
  select(tournament,Date) %>% 
  unique()


t2=V_TOURNAMENT3 %>% 
  filter(is.na(N))

V_TOURNAMENT3=V_TOURNAMENT3 %>% select(-c(10,11))

V_TOURNAMENT3=V_TOURNAMENT3 %>% 
  mutate(tournament=(case_when(tournament=="Canberra 2 Chall."~"Canberra Chall.",
                               TRUE~tournament)))

# Garder Charleston 2022 Cancel à cause de l'ouragan, matchs joués mais pb dans le scrap

V_TOURNAMENT4=V_TOURNAMENT3 %>% 
  filter(!is.na(N))

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

# Verif du redressement

t=V_TOURNAMENT4 %>% 
  filter(Round=="Winner" & Ranking_points==0) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Masters Cup Atp",
                            "Challenger Tour Finals","Olympics - London","Olympics - Rio De Janeiro")) %>% 
  filter(!is.na(N)) %>% 
  select(tournament,Date) %>% 
  unique()

# RED 2025

V_TOURNAMENT4=V_TOURNAMENT3 %>% 
  filter(!is.na(N))

Tournament_red=V_TOURNAMENT3 %>% filter((tournament=="Canberra 2 chall." & Date=="2024-12-30")) %>% 
  select(tournament,Date,Categorie,Country_tournament,Surface_tournament,Week_tournament,Year,N)

Red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                      Ranking_points=c(0,8,16,35,64,125),
                      tournament=rep("Canberra 2 chall.",6))

Tournament_red=Tournament_red %>% 
  left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
  select("tournament","Date","Categorie","Round",           
         "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
         "Year","N") %>% 
  unique()

V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  filter(!(tournament=="Canberra 2 chall." & Date=="2024-12-30")) %>% 
  rbind(Tournament_red)



Tournament_red=V_TOURNAMENT3 %>% filter((tournament=="New Delhi chall." & Date=="2025-02-10")) %>% 
  select(tournament,Date,Categorie,Country_tournament,Surface_tournament,Week_tournament,Year,N)

Red_values=data.frame(Round=c("1R","R16","QF","SF","F","Winner"),
                      Ranking_points=c(0,6,12,22,44,75),
                      tournament=rep("New Delhi chall.",6))

Tournament_red=Tournament_red %>% 
  left_join(Red_values,by=c("tournament"),relationship = c("many-to-many")) %>% 
  select("tournament","Date","Categorie","Round",           
         "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
         "Year","N") %>% 
  unique()

V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  filter(!(tournament=="New Delhi chall." & Date=="2025-02-10")) %>% 
  rbind(Tournament_red)


save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2025.RData"))


save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2024.RData"))

# Rajout de ces matchs à la base V_MATCH

get_tournament_red=function(tournament,year,url_tournament) {
  
  #url=paste('https://www.tennisexplorer.com/',tournament,'/',year,'/','atp-men/',sep='')
  url=url_tournament
  # extraire les donn?es ? partir de la page Web
  page <- read_html(url)
  matches <- page %>% html_nodes("table.result") %>% html_table()
  
  if (nrow(matches[[2]])<=2){
    data_set=data.frame(matrix(ncol = 37,nrow = 0))  
    colnames(data_set)=colnames_calc
  }else{
    # nettoyer et organiser les donn?es extraites
    matches <- matches[[2]]
    colnames(matches)=matches[1,]
    matches=matches[-1,]
    # rennomer toutes les colonnes
    colnames(matches)=c('Date','Round','Player','Score','Set1','Set2','Set3','Set4','Set5','Odd_W','Odd_L','Info')
    matches=matches[matches$Player!='',]
    # extraire les jeux des sets tb
    matches$Set1=as.numeric(substr(matches$Set1, 1, 1))
    matches$Set2=as.numeric(substr(matches$Set2, 1, 1))
    matches$Set3=as.numeric(substr(matches$Set3, 1, 1))
    # On transforme la date
    matches$Date=dmy(paste(gsub("['^.^']", "-", substr(matches$Date,1,5)),sep="-",year))
    matches$Player=str_replace(matches$Player,'\\s+\\((.*$)',"")
    matches$test=c(1:nrow(matches))%%2
    # un data set winner et undata set looser
    winner=matches[matches$test==1,]
    colnames(winner)=c('Date','Round','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Odd_W','Odd_L','Info')
    winner=winner[,c(1:10)]
    loser=matches[matches$test==0,]
    colnames(loser)=c('Date','Round','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Odd_W','Odd_L','Info')
    loser=loser[,c(1:9,11)]
    data_set=winner[,1:2]
    
    for (i in 1:8){
      data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
      n=ncol(data_set)
      colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
      #print(i)
    }
    data_set$Score_W=as.numeric(as.character(data_set$Score_W))
    data_set$Score_L=as.numeric(as.character(data_set$Score_L))
    data_set$Odd_W=as.numeric(as.character(data_set$Odd_W))
    data_set$Odd_L=as.numeric(as.character(data_set$Odd_L))
    data_set$info=ifelse(data_set$Score_W<=1,'Walkover or Retired','Completed')
    data_set$Outcome=ifelse(data_set$Odd_W<=data_set$Odd_L,'Fav_W','Out_W')
    data_set=cbind(tournament,data_set)
    #Passage du nom du tournoi en maj
    data_set$tournament=str_to_title(as.character(data_set$tournament))
    data_set$N_match=c(1:nrow(data_set))
    data_set=data_set[order(data_set$N_match,decreasing=T),]
    #ajout du lieu du tournoi et de la surface
    location=str_extract(page %>% html_nodes("h1") %>% html_text(), "(?<=\\().*?(?=\\))")
    data_set$Location=location
    surface=str_match(page %>% 
                        html_nodes("#center > div:nth-child(2)") %>% 
                        html_text(), "\\$\\s*,\\s*(\\w+)")[,2]
    data_set$Surface=str_to_title(surface)
    data_set$Date=as.Date(ifelse(format(data_set$Date, "%m") == "12" & 
                                   as.numeric(format(data_set$Date, "%d")) >= 25, 
                                 data_set$Date - years(1), 
                                 data_set$Date))
  }
  return(data_set)
}

tournament$Phase="Main Draw"

tournament=tournament %>% 
  #rename("Winner_id"=P1,"Loser_id"=P2) %>% 
  mutate(Week=isoweek(Date),
         Season=2022) %>% 
  left_join(V_PLAYERS %>% select(Player_name,Country),by=c("Winner_id"="Player_name")) %>% 
  left_join(V_PLAYERS %>% select(Player_name,Country),by=c("Loser_id"="Player_name")) %>% 
  rename("Country_W"=Country.x,
         "Country_L"=Country.y)

tournament=tournament %>%
  select(tournament,
         Date,
         Week,
         Season,
         N_match,
         Round,
         Phase,
         info,
         Winner_id,
         Loser_id,
         Country_W,
         Country_L,
         Score_W,
         Score_L,
         Set1_W,
         Set1_L,
         Set2_W,
         Set2_L,
         Set3_W,
         Set3_L,
         Set4_W,
         Set4_L,
         Set5_W,
         Set5_L,
         Odd_W,
         Odd_L)

# Ajout des round et points pour Masters CUP et ATP_CUP/United_CUP

V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  mutate(Week_tournament=isoweek(Date))

t=V_TOURNAMENT4 %>% filter(!is.na(N) & is.na(Round)) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals")) %>% 
  unique()


# Rajout des round et points pour ATP_CUP/United_CUP/Masters Cup

# Masters Cup Atp
tournament_red=V_TOURNAMENT4 %>% filter(tournament=="Masters Cup Atp") %>% select(-c(Round,Ranking_points))

name=tournament_red$tournament %>% unique()

MainDrawMC=data.frame("tournament"=name,
                      "Round"=c("RR","RRW","SF","SFW","F","W"),
                      "Ranking_points"=c(0,200,0,400,0,500))

tournament_red=tournament_red %>% 
  left_join(MainDrawMC,by=c("tournament")) %>% 
  select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N)

V_TOURNAMENT4=V_TOURNAMENT4 %>% filter(tournament!=name)

V_TOURNAMENT4=rbind(V_TOURNAMENT4,tournament_red)


#Challenger tour finals 

tournament_red=V_TOURNAMENT4 %>% filter(tournament=="Challenger Tour Finals") %>% select(-c(Round,Ranking_points))

name=tournament_red$tournament %>% unique()

MainDrawMC=data.frame("tournament"=name,
                      "Round"=c("RR","RRW","SF","SFW","F","W"),
                      "Ranking_points"=c(0,15,0,30,0,80))

tournament_red=tournament_red %>% 
  left_join(MainDrawMC,by=c("tournament")) %>% 
  select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N) %>% 
  unique()

V_TOURNAMENT4=V_TOURNAMENT4 %>% filter(tournament!=name)

V_TOURNAMENT4=rbind(V_TOURNAMENT4,tournament_red)


# Pour le calcul de la race avec ATP Cup/United Cup on appliquera un calcul séparé

save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2024.RData"))


##### Redressement des catégories avant ajout qualif #####

table(V_TOURNAMENT4$Categorie)

V_TOURNAMENT_RED=V_TOURNAMENT4 %>% filter(tournament=="Split Challenger")

V_TOURNAMENT4=V_TOURNAMENT4 %>% filter(!tournament %in% "Split Challenger")

V_TOURNAMENT_RED=V_TOURNAMENT_RED %>% mutate(Categorie="Challenger 75")

V_TOURNAMENT_RED=V_TOURNAMENT_RED %>% select(-c(Round,Ranking_points))
  
V_TOURNAMENT_RED=V_TOURNAMENT_RED %>% 
  left_join(test,by="Categorie") %>% 
  unique()

V_TOURNAMENT_RED=V_TOURNAMENT_RED %>% select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N)

V_TOURNAMENT4=rbind(V_TOURNAMENT4,V_TOURNAMENT_RED)

V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  group_by(tournament,Date,Week_tournament,Year) %>% 
  mutate(Categorie=case_when(Categorie %in% c("ATP ","ATP 0","ATP 900","Challenger ","Challenger 0")~paste(ifelse(Categorie %like% "ATP","ATP","Challenger"),
                                                                                                         max(as.numeric(Ranking_points),na.rm = T)),
                             TRUE~Categorie)) %>% 
  ungroup() %>% 
  mutate(Categorie=case_when(Categorie=="ATP -Inf"~"ATP",
                             Categorie=="ATP 80"~"Challenger",
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


V_TOURNAMENT4 = V_TOURNAMENT4 %>% 
  unique() %>% 
  group_by(tournament,Date,Week_tournament,Year) %>% 
  mutate(Categorie=case_when(Categorie %like% "Challenger"~paste("Challenger",max(as.numeric(Ranking_points),na.rm = T)),
                             TRUE~Categorie)) %>% 
  ungroup() %>% 
  unique()

# V_TOURNAMENT4=V_TOURNAMENT4 %>% 
#     filter(!(tournament=="Barletta Chall." & Year==2012 & Categorie=="Challenger 90")) %>%
#     filter(!(tournament=="Kazan Challenger" & Year==2013 & Categorie=="Challenger 80")) %>%
#     filter(!(tournament=="Quimper Challenger" & Year==2013 & Categorie=="Challenger 80")) %>%
#     filter(!(tournament=="Segovia Challenger" & Year==2013 & Categorie %in% c("Challenger 80","Challenger 100"))) %>%
#     filter(!(tournament=="Todi Challenger" & Year==2013 & Categorie=="Challenger 80"))




save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4_2024.RData"))

# 2025

V_TOURNAMENT4=V_TOURNAMENT4 %>% 
  mutate(Categorie=case_when(tournament=="Canberra 2 chall."~"Challenger 125",
                             tournament=="New Delhi chall."~"Challenger 75",
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


###### AJOUT des Points de Qualifs ####


# ETAPE 1
# Filter sur une catégorie donnée

# ATP 2000 
# ATP 1000
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

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2024.RData"))

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2012_2016.RData"))

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData"))

bergame=get_tournament_red("Potchefstroom Chall.",
                           2020,
                           "https://www.tennisexplorer.com/potchefstroom-challenger/2020/atp-men/")

if (nrow(bergame)>=1){
  players=get_players_name(year = 2020,
                           tournament = "Potchefstroom Chall.",
                           url_tournament="https://www.tennisexplorer.com/potchefstroom-challenger/2020/atp-men/")
  
  bergame=bergame %>% 
    left_join(players,by=c("N_match"))
}

bergame=bergame %>% 
  mutate(Week=isoweek(Date),
         Season=year(Date),
         Phase="Main Draw") %>% 
  rename(Winner_id="P1",
         Loser_id="P2") %>% 
  select(-c(Winner,Loser,Surface)) %>% 
  left_join(V_PLAYERS %>% select(Player_name,Country),by=c("Winner_id"="Player_name")) %>% 
  left_join(V_PLAYERS %>% select(Player_name,Country),by=c("Loser_id"="Player_name")) %>% 
  rename(Country_W="Country.x",
         Country_L="Country.y") %>% 
  select(tournament,
         Date,
         Week,
         Season,
         N_match,
         Round ,
         Phase,
         info,      
         Winner_id,
         Loser_id,
         Country_W,
         Country_L,
         Score_W,
         Score_L,
         Set1_W,
         Set1_L,    
         Set2_W,  
         Set2_L,    
         Set3_W,    
         Set3_L,     
         Set4_W,    
         Set4_L,    
         Set5_W,     
         Set5_L ,   
         Odd_W ,    
         Odd_L)

V_TABLE_MATCH=rbind(V_TABLE_MATCH,bergame) %>% unique()

V_TABLE_MATCH=V_TABLE_MATCH %>% unique()

save(V_TABLE_MATCH,file = paste0(getwd(),"/Scrapping tennis data/Extraction/V_TABLE_MATCH.RData"))



V_TOURNAMENT_F_RED=V_TOURNAMENT_F %>%
  filter(tournament=="Adelaide" & Year==2022)

V_TOURNAMENT_F_RED$tournament=tournament_name

V_TOURNAMENT_F=rbind(V_TOURNAMENT_F,V_TOURNAMENT_F_RED)
