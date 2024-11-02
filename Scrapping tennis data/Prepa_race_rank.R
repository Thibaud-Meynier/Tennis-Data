
V_TOURNAMENT2=data.frame()

for (i in seq(2017,2023,by=1)){
  
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


V_TOURNAMENT3=V_TOURNAMENT2 %>% 
  left_join(V_TOURNAMENT %>% select(tournament,Country_tournament,Date,Surface_tournament,Week_tournament,Year),by=c("tournament","Date"))

tournament_list=V_TOURNAMENT

calendar_info=info_tournament(tournament_list)
  
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
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Masters Cup Atp"))


V_TOURNAMENT3 %>% 
  filter(is.na(Round) & is.na(Categorie))

NO=NO %>% filter(is.na(N)) # Tournoi à supprimer de la base de reférence

NO=NO %>% filter(!is.na(N))


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



NO=NO %>% select(tournament,Date,Categorie) %>% unique()

NO2=data.frame(tournament=c("Charleston Chall.","Playford Chall.","Calgary Challenger","Cologne","Cologne 2",
                      "Brasília Chall.","Buenos Aires 4 Chall.","Corrientes Chall.",
                      "Buenos Aires 5 Chall.","Buenos Aires 5 Chall."),
         Date=c("2022-09-26","2018-12-31","2020-02-24","2020-10-12","2020-10-19","2021-11-22","2022-04-25","2022-06-13","2022-06-20","2023-04-24")) 

# Redressement spécial pour cologne car pas d'autres tournois

j=1

for (j in 1:nrow(NO)){
  
  i=NO2$tournament[j]
  
  Date_i=NO2$Date[j]
  
  Tournament_red=V_TOURNAMENT4 %>% filter((tournament==i & Date==Date_i)) %>% select(tournament,Date)
  
  Annee_filtre=ifelse(year(Date_i)==2017,year(Date_i)+1,2023)
  
  # Ajouter des conditions pour l'année de filtre
  #Si 2017 alors on prend +1 si 2023 on prend -1 sinon on prend -1
  Red_values=V_TOURNAMENT3 %>% 
    select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N) %>% 
    filter(tournament==i & Year==Annee_filtre) %>% 
    select(-Date)
  
  #Red_values$tournament=i
  
  if(nrow(Red_values)!=0){
    
    Tournament_red=Tournament_red %>% 
      left_join(Red_values,by=c("tournament")) %>% 
      select("tournament","Date","Categorie","Round",           
             "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
             "Year","N")
    
    V_TOURNAMENT3=V_TOURNAMENT3 %>% 
      filter(!(tournament==i & Date==Date_i)) %>% 
      rbind(Tournament_red)
    
    print(paste0(j,"-",i,"-",Date_i))
    
  }else{
    
    Annee_filtre=ifelse(year(Date_i)==2017,year(Date_i)+1,year(Date_i)+1)
    
    Red_values=V_TOURNAMENT3 %>% 
      select(tournament,Date,Categorie,Round,Ranking_points,Country_tournament,Surface_tournament,Week_tournament,Year,N) %>% 
      filter(tournament==i & year(Date)==Annee_filtre) %>% 
      select(-Date)
    
    Tournament_red=Tournament_red %>% 
      left_join(Red_values,by=c("tournament")) %>% 
      select("tournament","Date","Categorie","Round",           
             "Ranking_points","Country_tournament","Surface_tournament","Week_tournament",   
             "Year","N")
    
    V_TOURNAMENT3=V_TOURNAMENT3 %>% 
      filter(!(tournament==i & Date==Date_i)) %>% 
      rbind(Tournament_red)
    
    print(paste0(j,"-",i,"-",Date_i))
  }
  
  
}

V_TOURNAMENT3$Year=case_when(V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==1 ~year(V_TOURNAMENT3$Date),
                            V_TOURNAMENT3$Week_tournament>=52 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
                            V_TOURNAMENT3$Week_tournament==1 & month(V_TOURNAMENT3$Date)==12 ~ year(V_TOURNAMENT3$Date)+1,
                            TRUE ~ year(V_TOURNAMENT3$Date))


t=V_TOURNAMENT3 %>% filter(!is.na(N) & is.na(Round)) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals"))

V_TOURNAMENT3 %>% 
  filter(Categorie %in% c("ATP ","ATP 0","Challenger ","Challenger 0")) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals","Atp Cup","United Cup","Olympics - Tokyo","Masters Cup Atp")) %>% 
  filter(!is.na(N))


t2=V_TOURNAMENT3 %>% 
  filter(is.na(N))

# Garder Charleston 2022 Cancel à cause de l'ouragan, matchs joués mais pb dans le scrap

V_TOURNAMENT4=V_TOURNAMENT3 %>% 
  filter(!is.na(N))

save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4.RData"))

# Rajout de ces matchs à la base V_MATCH

get_tournament_charleston=function(tournament,year,url_tournament) {
  
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

t=t=V_TOURNAMENT4 %>% filter(!is.na(N) & is.na(Round)) %>% 
  filter(!tournament %in% c("Davis Cup","Next Gen ATP Finals"))


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

# Pour le calcul de la race avec ATP Cup/United Cup on appliquera un calcul séparé

save(V_TOURNAMENT4,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT4.RData"))

###### AJOUT des Points de Qualifs ####


# ETAPE 1
# Filter sur une catégorie donnée

V_50=V_TOURNAMENT4 %>% filter(Categorie %in% c("Challenger 50"))

V_TOURNAMENT_RED = V_50 
#%>% filter(N < 95)
    
V_TOURNAMENT_RED2 = V_50 %>% 
#%>% filter(N < 95) %>%
  select(tournament, Round, Ranking_points) %>%
  unique()
    
# Créer les nouvelles lignes avec 3 lignes par tournoi
new_rows <- V_TOURNAMENT_RED2 %>%
    distinct(tournament) %>%              # Obtenir les tournois uniques
    slice(rep(1:n(), each = 3)) %>%        # Répéter chaque tournoi 3 fois
    mutate(Round = rep(c("Q-R16","Q-QF","Q-QW"), times = n_distinct(tournament)),
             Ranking_points = rep(c(0,1,3), times = n_distinct(tournament))) # Ajouter les rounds et points
    
# Combiner avec les lignes existantes
V_TOURNAMENT_RED2 = rbind(new_rows, V_TOURNAMENT_RED2)
    
# Effectuer la jointure gauche avec les autres colonnes du dataset initial
V_TOURNAMENT_RED_P1 = V_TOURNAMENT_RED %>%
    select(tournament, Date, Categorie, Country_tournament, Surface_tournament, Week_tournament, Year) %>%
    left_join(V_TOURNAMENT_RED2, by = "tournament") %>%
    unique()
    
V_1000=rbind(V_TOURNAMENT_RED_P1,V_TOURNAMENT_RED_P2) %>% arrange(Date)
  
V_50=V_TOURNAMENT_RED_P1

V_TOURNAMENT_F=rbind(V_50,V_75_80,V_90_125,V_175,V_250,V_500,V_1000,V_2000) %>% arrange(Date)

save(V_TOURNAMENT_F,file = paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F.RData"))










