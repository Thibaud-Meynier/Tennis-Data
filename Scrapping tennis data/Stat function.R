##### RED V TOURNAMENT F

load(paste0(here(),"/Scrapping tennis data/Tournament/INFO_TOURNAMENT_2002_2025.RData"))

V_TOURNAMENT=V_TOURNAMENT %>% 
  mutate(Surface_tournament=case_when(tournament=="Madrid" & Year<=2008~"indoors",
                                      tournament=="Masters Cup ATP" & Year %in% c(2002,2005:2008)~"indoors",
                                      tournament=="Bangkok" & Year %in% c(2003,2004)~"indoors",
                                      tournament=="Houston" & Year %in% c(2002)~"clay",
                                      tournament=="Davis Cup" & Year>=2019~"indoors",
                                      tournament=="Davis Cup" & Year<=2019~"various",
                                      TRUE~Surface_tournament)) %>% 
  mutate(tournament = gsub("Challenger", "Chall.", tournament, ignore.case = TRUE)) %>% 
  mutate(Surface_tournament=case_when(Surface_tournament=="clay"~"Clay",
                                      Surface_tournament=="hard"~"Hard",
                                      Surface_tournament=="grass"~"Grass",
                                      Surface_tournament=="indoors"~"Indoors",
                                      Surface_tournament=="various"~"Various",
                                      TRUE~Surface_tournament))


##### H2H Function ####

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

# tournoi="Masters Cup Atp"
# 
# Date_match="2016-11-20"
# 
# surface="all"
# 
# W="Djokovic"
# 
# L="Murray"
# 
# R="F"
# 
# winner_url="https://www.tennisexplorer.com/player/djokovic/"
# 
# loser_url="https://www.tennisexplorer.com/player/murray/"
# 
# h2h(winner_url,loser_url,
#     surface=surface,Date_match = Date_match,tournoi=tournoi,
#     W=W,L=L,R=R)

data_set_cn=c("Year","Tournament","Winner","Loser",             
              "Score_W","Score_L","Set1_W","Set1_L",            
              "Set2_W","Set2_L","Set3_W" ,"Set3_L" ,           
              "Set4_W","Set4_L","Set5_W","Set5_L"  ,          
              "Round","Date","Surface_tournament", "Week","row_i")  


get_h2h=function(winner_url,loser_url){
  
  # Semaine=isoweek(Date_match)
  
  # winner_url="https://www.tennisexplorer.com/player/djokovic/"
  # 
  # loser_url="https://www.tennisexplorer.com/player/zverev-6f768/"
  
  winner=sub(".*/player/([^/]+)/.*", "\\1", winner_url)
  
  loser=sub(".*/player/([^/]+)/.*", "\\1", loser_url)  
  
  url_h2h=paste0("https://www.tennisexplorer.com/mutual/",winner,"/",loser,"/")
  
  page <- read_html(url_h2h)
  
  h2h <- page %>% html_nodes("table.result") %>% html_table()
  
  h2h = h2h[[2]] %>% as.data.frame()
  
  if (ncol(h2h)>=11){
    
    h2h=h2h %>% filter(!Tournament %in% exclusion)
    
    # rennomer toutes les colonnes
    colnames(h2h)=c('Year','Tournament','Player','Score','Surface','Set1','Set2','Set3','Set4','Set5','Round')
    
    h2h=h2h %>% select(-Surface)
    
    h2h=h2h[h2h$Player!='',]
    # extraire les jeux des sets tb
    h2h$Set1=as.numeric(substr(h2h$Set1, 1, 1))
    h2h$Set2=as.numeric(substr(h2h$Set2, 1, 1))
    h2h$Set3=as.numeric(substr(h2h$Set3, 1, 1))
    h2h$Set4=as.numeric(substr(h2h$Set4, 1, 1))
    h2h$Set5=as.numeric(substr(h2h$Set5, 1, 1))
    
    h2h$Player=str_replace(h2h$Player," [A-Z]\\.$","")
    h2h$test=c(1:nrow(h2h))%%2
    
    # un data set winner et undata set looser
    winner=h2h[h2h$test==1,]
    colnames(winner)=c('Year','Tournament','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Round')
    winner=winner[,c(1:10)]
    loser=h2h[h2h$test==0,]
    colnames(loser)=c('Year','Tournament','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Round')
    loser=loser[,c(1:9,10)]
    data_set=winner[,c(1:2)]
    
    for (i in 1:8){
      data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
      n=ncol(data_set)
      colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
      #print(i)
    }
    
    data_set=data_set %>% select(-c(18))
    
    data_set$Score_W=as.numeric(as.character(data_set$Score_W))
    data_set$Score_L=as.numeric(as.character(data_set$Score_L))
    
    #Passage du nom du tournoi en maj
    data_set$Tournament=str_to_title(as.character(data_set$Tournament))
    
    data_set=data_set %>% 
      mutate(Tournament = gsub("Challenger", "Chall.", Tournament, ignore.case = TRUE))
    
    data_set$Round=ifelse(data_set$Round==FALSE,"F",data_set$Round)
    
    # On veut récuperer la catégorie du tournoi et la surface
    data_set=data_set %>% 
      mutate(Round=case_when(Tournament=="Masters Cup Atp" & !Round %in% c("F","SF")~"RR",
                             # Round=="FALSE"~"F",
                             is.na(Round) ~ "-",
                             Round == "" ~ "-",              # Chaîne vide
                             str_trim(Round) == "" ~ "-", 
                             TRUE~Round)) %>% 
      mutate(tournament2=toupper(Tournament)) %>% 
      left_join(V_TOURNAMENT %>% mutate(tournament2=toupper(tournament)) %>% 
                  select(tournament2,Year,Date,Surface_tournament),by=c("tournament2","Year")) %>% 
      select(-c(tournament2)) %>% 
      mutate(Week=isoweek(Date)) %>% 
      arrange(Year,Week,desc(Round)) %>% 
      mutate(row_i=row_number())
    
    data_set$row_i=as.numeric(data_set$row_i)
    
  }else{
    data_set=data.frame(matrix(ncol = 37,nrow = 0))  
    
    colnames(data_set)=data_set_cn
  }
  
  return(data_set)
}


get_stat_h2h=function(data_set,surface,Season,tournoi,W,L,R,YB=NULL){
  
  Annee=Season
  
  if (nrow(data_set) == 0) {
    # Retourner un data_set vide ou effectuer une autre action appropriée
    data_set <- data_set
    
    # Winner stat
    
    N_W_W=0
    
    N_S_W_W=0
    
    N_G_W_W=0
    
    winner_stat=list(Number_Win=N_W_W,
                     Number_Set_Won=N_S_W_W,
                     Number_Games_Won=N_G_W_W)
    
    # Loser stat
    
    N_W_L=0
    
    N_S_W_L=0
    
    N_G_W_L=0
    
    loser_stat=list(Number_Win=N_W_L,
                    Number_Set_Won=N_S_W_L,
                    Number_Games_Won=N_G_W_L)
    
  } else {
    
    ref=data_set %>% 
      filter(stringdist(Tournament, tournoi, method = "lv")<=5 & Year==Annee & Round==R) %>% 
      #filter(Tournament==tournoi & Year==Annee & Week<=Semaine & Round==R) %>% 
      pull(row_i) %>% 
      as.numeric()
    
    if(surface=="all"){ #nrow(data_set)>1
      
      data_set=data_set %>%
        filter(row_i<ref) %>%
        select(-row_i)
      
    }else {
      
      data_set=data_set %>%
        filter(row_i<ref) %>%
        select(-row_i) %>%
        filter(Surface_tournament==surface)
      
    }
    
    ## arguement YB
    
    if(!is.null(YB)) {
      data_set <- data_set %>%
        filter(Year >= (Annee - YB))
    }
    
    data_set$G_W=rowSums(data_set[, c("Set1_W", "Set2_W", "Set3_W", "Set4_W", "Set5_W")], na.rm = TRUE)
    
    data_set$G_L=rowSums(data_set[, c("Set1_L", "Set2_L", "Set3_L", "Set4_L", "Set5_L")], na.rm = TRUE)
    
    # Winner stat
    
    N_W_W=data_set %>% filter(Winner==W) %>% count() %>% as.numeric()
    
    N_S_W_W=data_set %>% filter(Winner==W) %>% summarise(N_S_W=sum(Score_W,na.rm=T)) %>% pull(N_S_W)  +
      data_set %>% filter(Loser==W) %>% summarise(N_S_W=sum(Score_L,na.rm=T)) %>% pull(N_S_W)
    
    N_G_W_W=data_set %>% filter(Winner==W) %>% summarise(N_G_W=sum(G_W,na.rm=T)) %>% pull(N_G_W) +
      data_set %>% filter(Loser==W) %>% summarise(N_G_W=sum(G_L,na.rm=T)) %>% pull(N_G_W)
    
    winner_stat=list(Number_Win=N_W_W,
                     Number_Set_Won=N_S_W_W,
                     Number_Games_Won=N_G_W_W)
    
    # Loser stat
    
    N_W_L=data_set %>% filter(Winner==L) %>% count() %>% as.numeric()
    
    N_S_W_L=data_set %>% filter(Winner==L) %>% summarise(N_S_W=sum(Score_W,na.rm=T)) %>% pull(N_S_W) +
      data_set %>% filter(Loser==L) %>% summarise(N_S_W=sum(Score_L,na.rm=T)) %>% pull(N_S_W)
    
    N_G_W_L=data_set %>% filter(Winner==L) %>% summarise(N_G_W=sum(G_W,na.rm=T)) %>% pull(N_G_W)+
      data_set %>% filter(Loser==L) %>% summarise(N_G_W=sum(G_L,na.rm=T))%>% pull(N_G_W)
    
    loser_stat=list(Number_Win=N_W_L,
                    Number_Set_Won=N_S_W_L,
                    Number_Games_Won=N_G_W_L)
  }
  
  return(list(W=winner_stat,L=loser_stat))
  
}

# 
# h2h=function(winner_url,loser_url,surface,Date_match,Season,tournoi,W,L,R){
#   
#   Annee=Season
#   
#   Semaine=isoweek(Date_match)
#   
#   # winner_url="https://www.tennisexplorer.com/player/djokovic/"
#   # 
#   # loser_url="https://www.tennisexplorer.com/player/zverev-6f768/"
#   
#   winner=sub(".*/player/([^/]+)/.*", "\\1", winner_url)
#   
#   loser=sub(".*/player/([^/]+)/.*", "\\1", loser_url)  
#   
#   url_h2h=paste0("https://www.tennisexplorer.com/mutual/",winner,"/",loser,"/")
#   
#   page <- read_html(url_h2h)
#   
#   h2h <- page %>% html_nodes("table.result") %>% html_table()
#   
#   h2h = h2h[[2]] %>% as.data.frame()
#   
#   if (ncol(h2h)>=11){
#     
#   h2h=h2h %>% filter(!Tournament %in% exclusion)
#   
#   # rennomer toutes les colonnes
#   colnames(h2h)=c('Year','Tournament','Player','Score','Surface','Set1','Set2','Set3','Set4','Set5','Round')
#   
#   h2h=h2h %>% select(-Surface)
#   
#   h2h=h2h[h2h$Player!='',]
#   # extraire les jeux des sets tb
#   h2h$Set1=as.numeric(substr(h2h$Set1, 1, 1))
#   h2h$Set2=as.numeric(substr(h2h$Set2, 1, 1))
#   h2h$Set3=as.numeric(substr(h2h$Set3, 1, 1))
#   h2h$Set4=as.numeric(substr(h2h$Set4, 1, 1))
#   h2h$Set5=as.numeric(substr(h2h$Set5, 1, 1))
#   
#   h2h$Player=str_replace(h2h$Player," [A-Z]\\.$","")
#   h2h$test=c(1:nrow(h2h))%%2
#   
#   # un data set winner et undata set looser
#   winner=h2h[h2h$test==1,]
#   colnames(winner)=c('Year','Tournament','Winner','Score_W','Set1_W','Set2_W','Set3_W','Set4_W','Set5_W','Round')
#   winner=winner[,c(1:10)]
#   loser=h2h[h2h$test==0,]
#   colnames(loser)=c('Year','Tournament','Loser','Score_L','Set1_L','Set2_L','Set3_L','Set4_L','Set5_L','Round')
#   loser=loser[,c(1:9,10)]
#   data_set=winner[,c(1:2)]
#   
#   for (i in 1:8){
#     data_set=cbind(data_set,winner[,(2+i)],loser[,(2+i)])
#     n=ncol(data_set)
#     colnames(data_set)[(n-1):n]=c(colnames(winner)[(2+i)],colnames(loser)[(2+i)])
#     #print(i)
#   }
#   
#   data_set=data_set %>% select(-c(18))
#   
#   data_set$Score_W=as.numeric(as.character(data_set$Score_W))
#   data_set$Score_L=as.numeric(as.character(data_set$Score_L))
#   
#   #Passage du nom du tournoi en maj
#   data_set$Tournament=str_to_title(as.character(data_set$Tournament))
#   
#   data_set=data_set %>% 
#   mutate(Tournament = gsub("Challenger", "Chall.", Tournament, ignore.case = TRUE))
#   
#   data_set$Round=ifelse(data_set$Round==FALSE,"F",data_set$Round)
#   
#   # On veut récuperer la catégorie du tournoi et la surface
#   data_set=data_set %>% 
#     mutate(Round=case_when(Tournament=="Masters Cup Atp" & !Round %in% c("F","SF")~"RR",
#                            # Round=="FALSE"~"F",
#                            is.na(Round)==T~"",
#                            TRUE~Round)) %>% 
#     mutate(tournament2=toupper(Tournament)) %>% 
#     left_join(V_TOURNAMENT %>% mutate(tournament2=toupper(Tournament)) %>% 
#                 select(tournament2,Year,Date,Surface_tournament),by=c("tournament2","Year")) %>% 
#     select(-c(tournament2)) %>% 
#     mutate(Week=isoweek(Date)) %>% 
#     arrange(Year,Week,desc(Round)) %>% 
#     mutate(row_i=row_number())
#   
#   data_set$row_i=as.numeric(data_set$row_i)
#   
#   }else{
#    data_set=data.frame(matrix(ncol = 37,nrow = 0))  
#    
#     colnames(data_set)=data_set_cn
#   }
#   
#   
#   if (nrow(data_set) == 0) {
#     # Retourner un data_set vide ou effectuer une autre action appropriée
#     data_set <- data_set
#     
#     # Winner stat
#     
#     N_W_W=0
#     
#     N_S_W_W=0
#     
#     N_G_W_W=0
#     
#     winner_stat=list(Number_Win=N_W_W,
#                      Number_Set_Won=N_S_W_W,
#                      Number_Games_Won=N_G_W_W)
#     
#     # Loser stat
#     
#     N_W_L=0
#     
#     N_S_W_L=0
#     
#     N_G_W_L=0
#     
#     loser_stat=list(Number_Win=N_W_L,
#                     Number_Set_Won=N_S_W_L,
#                     Number_Games_Won=N_G_W_L)
#     
#   } else {
#     
#     ref=data_set %>% 
#       filter(Tournament==tournoi & Year==Annee & Round==R) %>% 
#       #filter(Tournament==tournoi & Year==Annee & Week<=Semaine & Round==R) %>% 
#       pull(row_i) %>% 
#       as.numeric()
#   
# if(surface=="all"){ #nrow(data_set)>1
# 
#     data_set=data_set %>%
#       filter(row_i<ref) %>%
#       select(-row_i)
#   
# }else {
# 
#     data_set=data_set %>%
#       filter(row_i<ref) %>%
#       select(-row_i) %>%
#       filter(Surface_tournament==surface)
#   
# }
#     data_set$G_W=rowSums(data_set[, c("Set1_W", "Set2_W", "Set3_W", "Set4_W", "Set5_W")], na.rm = TRUE)
#     
#     data_set$G_L=rowSums(data_set[, c("Set1_L", "Set2_L", "Set3_L", "Set4_L", "Set5_L")], na.rm = TRUE)
#     
#     # Winner stat
#     
#     N_W_W=data_set %>% filter(Winner==W) %>% count() %>% as.numeric()
#     
#     N_S_W_W=data_set %>% filter(Winner==W) %>% summarise(N_S_W=sum(Score_W,na.rm=T)) %>% pull(N_S_W)  +
#       data_set %>% filter(Loser==W) %>% summarise(N_S_W=sum(Score_L,na.rm=T)) %>% pull(N_S_W)
#     
#     N_G_W_W=data_set %>% filter(Winner==W) %>% summarise(N_G_W=sum(G_W,na.rm=T)) %>% pull(N_G_W) +
#       data_set %>% filter(Loser==W) %>% summarise(N_G_W=sum(G_L,na.rm=T)) %>% pull(N_G_W)
#     
#     winner_stat=list(Number_Win=N_W_W,
#                      Number_Set_Won=N_S_W_W,
#                      Number_Games_Won=N_G_W_W)
#     
#     # Loser stat
#     
#     N_W_L=data_set %>% filter(Winner==L) %>% count() %>% as.numeric()
#     
#     N_S_W_L=data_set %>% filter(Winner==L) %>% summarise(N_S_W=sum(Score_W,na.rm=T)) %>% pull(N_S_W) +
#       data_set %>% filter(Loser==L) %>% summarise(N_S_W=sum(Score_L,na.rm=T)) %>% pull(N_S_W)
#     
#     N_G_W_L=data_set %>% filter(Winner==L) %>% summarise(N_G_W=sum(G_W,na.rm=T)) %>% pull(N_G_W)+
#       data_set %>% filter(Loser==L) %>% summarise(N_G_W=sum(G_L,na.rm=T))%>% pull(N_G_W)
#     
#     loser_stat=list(Number_Win=N_W_L,
#                     Number_Set_Won=N_S_W_L,
#                     Number_Games_Won=N_G_W_L)
#   }
#   
#   return(list(W=winner_stat,L=loser_stat))
# 
# }

#prendre en compte le fait que y'ait eu 0 duels avant
# 
# player_id="Djokovic Novak"
# 
# Date_match=as.Date("2013-11-11")
# 
# lag_week=12
# 
# match_count(table_stock,player_id,lag_week=14,surface = "all",Date_match=Date_match)

# 
# match_count=function(df,player_id,lag_week,surface,Date_match){
# 
#   Date_min=Date_match-lag_week*7
#   
#   if(surface=="all"){
#     
#  
#   df=df %>% 
#     filter((Winner_id==player_id|Loser_id==player_id) & (Date<Date_match & Date>=Date_min))
#   
#   # N_match
#   
#   N_match=df %>% count() %>% as.numeric()
#   
#   # N_W
#   
#   N_W=df %>% filter(Winner_id==player_id) %>% count() %>% as.numeric()
#   
#   # N_L
#   
#   N_L=df %>% filter(Loser_id==player_id) %>% count() %>% as.numeric()
#   
#   #N_W_F_B
#   
#   N_W_F_B=df %>% filter(Winner_id==player_id & Odd_W<Odd_L) %>% count() %>% as.numeric()
#   
#   #N_W_O_B
#   
#   N_W_O_B=df %>% filter(Winner_id==player_id & Odd_W>=Odd_L) %>% count() %>% as.numeric()
#   
#   #N_L_F_B
#   
#   N_L_F_B=df %>% filter(Loser_id==player_id & Odd_L<Odd_W) %>% count() %>% as.numeric()
#   
#   #N_L_O_B
#   
#   N_L_O_B=df %>% filter(Loser_id==player_id & Odd_L>=Odd_W) %>% count() %>% as.numeric()
#   
#   stat_player=list(N_match=N_match,
#                    N_Win=N_W,
#                    N_Loss=N_L,
#                    N_Win_Fav_Book=N_W_F_B,
#                    N_Win_Out_Book=N_W_O_B,
#                    N_Loss_Fav_Book=N_L_F_B,
#                    N_Loss_Out_Book=N_L_O_B)
#   
#   }else{
#     
#     df=df %>% 
#       filter((Winner_id==player_id|Loser_id==player_id) & (Date<Date_match & Date>=Date_min) & Surface_tournament==surface)
#     
#     # N_match
#     
#     N_match=df %>% count() %>% as.numeric()
#     
#     # N_W
#     
#     N_W=df %>% filter(Winner_id==player_id) %>% count() %>% as.numeric()
#     
#     # N_L
#     
#     N_L=df %>% filter(Loser_id==player_id) %>% count() %>% as.numeric()
#     
#     #N_W_F_B
#     
#     N_W_F_B=df %>% filter(Winner_id==player_id & Odd_W<Odd_L) %>% count() %>% as.numeric()
#     
#     #N_W_O_B
#     
#     N_W_O_B=df %>% filter(Winner_id==player_id & Odd_W>=Odd_L) %>% count() %>% as.numeric()
#     
#     #N_L_F_B
#     
#     N_L_F_B=df %>% filter(Loser_id==player_id & Odd_L<Odd_W) %>% count() %>% as.numeric()
#     
#     #N_L_O_B
#     
#     N_L_O_B=df %>% filter(Loser_id==player_id & Odd_L>=Odd_W) %>% count() %>% as.numeric()
#     
#     stat_player=list(N_match=N_match,
#                      N_Win=N_W,
#                      N_Loss=N_L,
#                      N_Win_Fav_Book=N_W_F_B,
#                      N_Win_Out_Book=N_W_O_B,
#                      N_Loss_Fav_Book=N_L_F_B,
#                      N_Loss_Out_Book=N_L_O_B)
#     
#   }
#   
#   return(stat_player)
# }
# 


match_count=function(df,player_id,lag_week,surface,Date_match){
  
  Date_min=Date_match-lag_week*7
  
  if(surface=="all"){    
    
    df=df %>% 
      filter((Winner_id==player_id|Loser_id==player_id) & (Date<Date_match & Date>=Date_min))
    
  }else{
    
    df=df %>% 
      filter((Winner_id==player_id|Loser_id==player_id) & (Date<Date_match & Date>=Date_min) & Surface_tournament==surface)
    
  }
  
  # N_match
  
  N_match=df %>% count() %>% as.numeric()
  
  # N_W
  
  N_W=df %>% filter(Winner_id==player_id) %>% count() %>% as.numeric()
  
  # N_L
  
  N_L=df %>% filter(Loser_id==player_id) %>% count() %>% as.numeric()
  
  #N_W_F_B
  
  N_W_F_B=df %>% filter(Winner_id==player_id & Odd_W<Odd_L) %>% count() %>% as.numeric()
  
  #N_W_O_B
  
  N_W_O_B=df %>% filter(Winner_id==player_id & Odd_W>=Odd_L) %>% count() %>% as.numeric()
  
  #N_W_F_R
  
  N_W_F_R=df %>% filter(Winner_id==player_id & Rank_W<Rank_L) %>% count() %>% as.numeric()
  
  #N_W_O_R
  
  N_W_O_R=df %>% filter(Winner_id==player_id & Rank_W>Rank_L) %>% count() %>% as.numeric()
  
  #N_L_F_B
  
  N_L_F_B=df %>% filter(Loser_id==player_id & Odd_L<Odd_W) %>% count() %>% as.numeric()
  
  #N_L_O_B
  
  N_L_O_B=df %>% filter(Loser_id==player_id & Odd_L>=Odd_W) %>% count() %>% as.numeric()
  
  #N_L_F_R
  
  N_L_F_R=df %>% filter(Loser_id==player_id & Rank_W<Rank_L) %>% count() %>% as.numeric()
  
  #N_L_O_R
  
  N_L_O_R=df %>% filter(Loser_id==player_id & Rank_W>Rank_L) %>% count() %>% as.numeric()
  
  stat_player=list(N_match=N_match,
                   N_Win=N_W,
                   N_Loss=N_L,
                   N_Win_Fav_Book=N_W_F_B,
                   N_Win_Out_Book=N_W_O_B,
                   N_Loss_Fav_Book=N_L_F_B,
                   N_Loss_Out_Book=N_L_O_B,
                   N_Win_Fav_Rank=N_W_F_R,
                   N_Win_Out_Rank=N_W_O_R,
                   N_Loss_Fav_Rank=N_L_F_R,
                   N_Loss_Out_Rank=N_L_O_R)    
  
  return(stat_player)
}
