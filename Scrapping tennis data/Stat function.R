
##### RED V TOURNAMENT F

load(paste0(here(),"/Scrapping tennis data/Tournament/INFO_TOURNAMENT_2002_2026.RData"))

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

data_set_cn=c("Year","Tournament","Winner","Loser",             
              "Score_W","Score_L","Set1_W","Set1_L",            
              "Set2_W","Set2_L","Set3_W" ,"Set3_L" ,           
              "Set4_W","Set4_L","Set5_W","Set5_L"  ,          
              "Round","Date","Surface_tournament", "Week","row_i")  


get_h2h=function(winner_url,loser_url){
  
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
      mutate(Tournament = gsub("Challenger", "Chall.", Tournament, ignore.case = TRUE)) %>% 
      mutate(Tournament = case_when(Tournament=="Paris"~"Paris - Masters",TRUE~Tournament))
    
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
  
  if(nrow(data_set)==0){
    
    ref=NA
    
  } else {
    
    ref=data_set %>% 
      filter(stringdist(Tournament, tournoi, method = "lv")<=5 & Year==Annee & Round==R) %>% 
      pull(row_i) %>% 
      as.numeric()
  }
  
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
    
  } else if (is_empty(data_set %>% 
                      filter(stringdist(Tournament, tournoi, method = "lv")<=5 & Year==Annee & Round==R) %>% 
                      pull(row_i) %>% 
                      as.numeric())==T){
    
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
    
    
  }  else {
    
    ref=data_set %>% 
      filter(stringdist(Tournament, tournoi, method = "lv")<=5 & Year==Annee & Round==R) %>% 
      #filter(Tournament==tournoi & Year==Annee & Week<=Semaine & Round==R) %>% 
      pull(row_i) %>% 
      as.numeric()
    
    if(surface=="all"){ 
      
      data_set=data_set %>%
        filter(row_i<ref) %>%
        select(-row_i)
      
    }else {
      
      surface=ifelse(surface %in% c("Indoors","Various"),"Hard",surface)
      
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

match_count <- function(df, player_id, lag_week, surface, Date_match,tournoi,Categ,Saison) {
  
  
  # Date du premier match du joueur dans ce tournoi
  first_match_tournament <- df[
    (Winner_id == player_id | Loser_id == player_id) & 
      tournament == tournoi & Season==Saison,
    min(Date)
  ]
  
  # Sécuriser si aucun match trouvé
  if(length(first_match_tournament) == 0 || is.infinite(first_match_tournament)) {
    first_match_tournament <- Date_match-1
  }
  
  cutoff_date <- first_match_tournament - 1
  
  # Grand Chelem : reculer au lundi de la semaine précédente
  if(Categ == "Grand Slam") {
    cutoff_date <- floor_date(first_match_tournament, unit = "week", week_start = 1) - 7
  }
  
  Date_min <- cutoff_date - lag_week * 7
  
  if(surface == "all") {
    df_filtered <- df[(Winner_id == player_id | Loser_id == player_id) & 
                        Date <= cutoff_date & 
                        Date >= Date_min]
  } else {
    surface <- ifelse(surface %in% c("Indoors", "Various"), "Indoors", surface)
    
    df_filtered <- df[(Winner_id == player_id | Loser_id == player_id) & 
                        Date <= cutoff_date & 
                        Date >= Date_min & 
                        Surface_tournament == surface]
  }
  
  if(nrow(df_filtered) == 0) {
    
    stat_player=list(N_match = 0, N_Win = 0, N_Loss = 0,
                     N_Win_Fav_Book = 0, N_Win_Out_Book = 0,
                     N_Loss_Fav_Book = 0, N_Loss_Out_Book = 0,
                     N_Win_Fav_Rank = 0, N_Win_Out_Rank = 0,
                     N_Loss_Fav_Rank = 0, N_Loss_Out_Rank = 0)
    
    return(stat_player)
  }
  
  # N_match
  
  N_match=df_filtered %>% nrow()
  
  # N_W
  
  N_W=df_filtered %>% filter(Winner_id==player_id) %>% nrow()
  
  # N_L
  
  N_L=df_filtered %>% filter(Loser_id==player_id) %>% nrow()
  
  #N_W_F_B
  
  N_W_F_B=df_filtered %>% filter(Winner_id==player_id & Odd_W<Odd_L) %>% nrow()
  
  #N_W_O_B
  
  N_W_O_B=df_filtered %>% filter(Winner_id==player_id & Odd_W>=Odd_L) %>% nrow()
  
  #N_W_F_R
  
  N_W_F_R=df_filtered %>% filter(Winner_id==player_id & Rank_W<Rank_L) %>% nrow()
  
  #N_W_O_R
  
  N_W_O_R=df_filtered %>% filter(Winner_id==player_id & Rank_W>Rank_L) %>% nrow()
  
  #N_L_F_B
  
  N_L_F_B=df_filtered %>% filter(Loser_id==player_id & Odd_L<Odd_W) %>% nrow()
  
  #N_L_O_B
  
  N_L_O_B=df_filtered %>% filter(Loser_id==player_id & Odd_L>=Odd_W) %>% nrow()
  
  #N_L_F_R
  
  N_L_F_R=df_filtered %>% filter(Loser_id==player_id & Rank_W<Rank_L) %>% nrow()
  
  #N_L_O_R
  
  N_L_O_R=df_filtered %>% filter(Loser_id==player_id & Rank_W>Rank_L) %>% nrow()
  
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

##### INFO ON PLAYERS #####

get_player_win_rate = function(Player_name,Date_match,Categ){
  
  Date_match=as.Date(Date_match)
  
  WR_GLOBAL=V_MATCH_HIST %>% 
    filter(Phase == "Main Draw" & 
             (Categorie %in% c("Grand Slam", "Olympics", "Masters",
                               "ATP 1000", "ATP 500", 'ATP 250') | 
                tournament %in% c("Atp Cup", "United Cup"))) %>%  
    filter((Winner_id == Player_name | Loser_id == Player_name) & Date < Date_match & Categorie==Categ) %>% 
    group_by(Categorie) %>% 
    summarise(N=n(),
              PRCT_WIN=mean(Winner_id == Player_name,na.rm=T)*100
    ) %>% 
    pull(PRCT_WIN)
  
  WR_GLOBAL = ifelse(is.null(WR_GLOBAL),0,WR_GLOBAL)
  
  return(WR_GLOBAL)
}

#get_player_win_rate("Djokovic Novak","2025-12-31","Grand Slam")


get_player_win_rate_surface = function(Player_name,Date_match,Surf){
  
  Date_match=as.Date(Date_match)
  
  Surf=ifelse(Surf=="Various","Indoors",Surf)
  
  WR_Surface=V_MATCH_HIST %>% 
    filter(Phase == "Main Draw" & 
             (Categorie %in% c("Grand Slam", "Olympics", "Masters",
                               "ATP 1000", "ATP 500", 'ATP 250') | 
                tournament %in% c("Atp Cup", "United Cup"))) %>%  
    filter((Winner_id == Player_name | Loser_id == Player_name) & Date < Date_match & Surface_tournament==Surf) %>% 
    group_by(Surface_tournament) %>% 
    summarise(N=n(),
              PRCT_WIN=mean(Winner_id == Player_name,na.rm=T)*100
    ) %>% 
    pull(PRCT_WIN)
  
  WR_Surface = ifelse(is.null(WR_Surface),0,WR_Surface)
  
  return(WR_Surface)
}

get_player_best_run = function(Player_name,Date_match,Categ){
  
  Date_match=as.Date(Date_match)
  
  palmares <- V_MATCH_HIST %>% 
    filter(Phase == "Main Draw" & 
             (Categorie %in% c("Grand Slam", "Olympics", "Masters",
                               "ATP 1000", "ATP 500", 'ATP 250') | 
                tournament %in% c("Atp Cup", "United Cup"))) %>%  
    filter((Winner_id == Player_name | Loser_id == Player_name) & Date < Date_match & Categorie==Categ) %>% 
    mutate(
      Round = ifelse(Winner_id == Player_name & Round == "F", "W", Round),
      Round_Factor = factor(Round, levels = c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F", "W"),
                            ordered = TRUE)  # ← ordonné !
    ) %>% 
    summarise(
      Best_Run = levels(Round_Factor)[max(as.integer(Round_Factor), na.rm = TRUE)]
    ) %>% 
    pull(Best_Run)
  
  return(palmares)
  
}

#get_player_best_run("Karatsev Aslan","2021-02-18","Grand Slam")

is_giant_killer = function(Player_name,Date_match){
  
  Date_match=as.Date(Date_match)
  
  last_match = V_MATCH_HIST %>% 
    filter(Phase == "Main Draw" & 
             (Categorie %in% c("Grand Slam", "Olympics", "Masters",
                               "ATP 1000", "ATP 500", 'ATP 250') | 
                tournament %in% c("Atp Cup", "United Cup"))) %>%  
    filter(
      (Winner_id == Player_name | Loser_id == Player_name) &  # ← tous les matchs
        Date < Date_match 
    ) %>% 
    arrange(desc(Date)) %>% 
    slice(1)                                                   # ← dernier match
  
  # Vérifier si ce dernier match est un giant kill
  giant_kill = last_match %>%
    filter(
      Winner_id == Player_name &   # ← a gagné
        Odd_W >= 5                   # ← en tant qu'outsider
    ) 
  
  is_giant_kill = ifelse(nrow(giant_kill)==0,0,1) %>% as.numeric()
  
  return(is_giant_kill)
  
}


#is_giant_killer("Cecchinato Marco","2018-06-05")


#V_MATCH_t %>% 
#  filter(Winner_id=="Cecchinato Marco" & tournament=="French Open" & Season==2018) %>% 
#  select(Winner_id,Loser_id,Date,MATCH_ID,Odd_W,Odd_L)

is_finalist = function(Player_name,Date_match){
  
  Date_match=as.Date(Date_match)
  
  finalist = V_MATCH_HIST %>% 
    filter(Phase == "Main Draw" 
             # (Categorie %in% c("Grand Slam", "Olympics", "Masters",
             #                   "ATP 1000", "ATP 500", 'ATP 250') | 
             #    tournament %in% c("Atp Cup", "United Cup"))
           ) %>%  
    filter(
      (Winner_id == Player_name | Loser_id == Player_name) & Round=="F" & 
        Date < Date_match & 
        Date >= (Date_match - 14) 
    ) %>% 
    mutate(Outcome=ifelse(Winner_id == Player_name,"W",Round)) %>% 
    arrange(desc(Date)) %>% 
    head(1)
  
  R=ifelse(nrow(finalist)==0,0,
           ifelse(finalist$Outcome=="F",1,2))
  
  return(R)
}


#is_finalist("Nadal Rafael","2011-04-17",ID=125959)

is_qualies = function(Player_name,Date_match,tournoi,Year){
  
  Date_match=as.Date(Date_match)
  
  qualies = V_MATCH_HIST %>%
    filter(Phase=="Qualification" &  tournament==tournoi & Season==Year & Date<=Date_match) %>% 
    filter((Winner_id == Player_name | Loser_id == Player_name)) %>% 
      summarise(
        N_wins   = sum(Winner_id == Player_name),
        N_losses = sum(Loser_id == Player_name)
      ) %>%
      mutate(
        Status = case_when(
          N_wins > 0 & N_losses == 0 ~ "Q",    # ← que des victoires = Qualifié
          N_losses > 0               ~ "LL",   # ← au moins une défaite = Lucky Loser
          TRUE                       ~ "0"     # ← pas de matchs de qualif
        )
      ) %>%
      pull(Status)
  
  return(qualies)
  
}


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

  }else if (surface=="Hard"){

    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_hard) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_hard)


  }
  else{

    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_indoors) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_indoors)

    return(elo_player)

  }
}

last_elo2=function(base,player_name,categorie="all",Date_match,tournoi){
  
  if (categorie=="all"){
    
    elo_player=base %>%
      filter(Player_name==player_name & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player)
    
    return(elo_player)
    
  }else if (categorie %in% c("Grand Slam","Olympics","Masters")){
    
    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_major) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_major)
    
    return(elo_player)
    
  }else if (categorie=="ATP 1000"){
    
    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_atp_1000) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_atp_1000)
    
    return(elo_player)
    
    return(elo_player)
    
  }else if (categorie %in% c("ATP 500","Team")){
    
    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_atp_500) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_atp_500)
    
    
  }
  else{
    
    elo_player=base %>%
      filter(Player_name==player_name & !is.na(Elo_player_atp_250) & Date<Date_match & tournament!=tournoi) %>%
      mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>%
      arrange(desc(Date),desc(Round)) %>%
      mutate(ORDRE_ELO=row_number()) %>%
      filter(ORDRE_ELO==1) %>%
      select(Player_name,tournament,Date,Elo_player_atp_250)
    
    return(elo_player)
    
  }
}


penalty=function(diff_date){
  
  penalty=ifelse(between(diff_date,60,80),0.7,
                 ifelse(between(diff_date,81,180),0.85,
                        ifelse(diff_date>180,1,0)))
  
  return(penalty)
}


get_tennis_week <- function(date) {
  year <- year(date)
  week_num <- isoweek(date)
  
  # Gérer le cas des premiers jours de janvier qui appartiennent 
  # à la dernière semaine de l'année précédente
  if (month(date) == 1 && week_num >= 52) {
    year <- year - 1
  }
  
  # Gérer le cas de fin décembre appartenant à la semaine 1 de l'année suivante
  if (month(date) == 12 && week_num == 1) {
    year <- year + 1
  }
  
  return(paste0(year, "-", sprintf("%02d", week_num)))
}

evol_rank=function(Player,Date_match,lag){
  
  date_lag=Date_match-lag
  
  week_lag=get_tennis_week(date_lag)

  Rank_player_lag=V_RANK %>% 
    filter(Player_name==Player & Week_Rank==week_lag)
  
  if(nrow(Rank_player_lag)>=1){
    Rank_player_lag=Rank_player_lag$Rank
  }else{
    Rank_player_lag=1000
  }
  
  week=get_tennis_week(Date_match)
  
  Rank_player=V_RANK %>% 
    filter(Player_name==Player & Week_Rank==week)
  
  if(nrow(Rank_player)>=1){
    Rank_player=Rank_player$Rank
  }else{
    Rank_player=1000
  }
  
  Diff_Rank=as.numeric(Rank_player_lag)-as.numeric(Rank_player)
  
  return(Diff_Rank)
  
}

# evol_rank("Tsitsipas Stefanos",as.Date("2026-03-30"),365)

best_rank = function(Player, Date_match, rank = FALSE) {
  
  week = get_tennis_week(Date_match)
  
  # Classement actuel
  Rank_player = V_RANK %>%
    filter(Player_name == Player & Week_Rank == week)
  
  if (nrow(Rank_player) >= 1) {
    Rank_player = as.numeric(Rank_player$Rank)
  } else {
    Rank_player = 1000
    
    Points_player = 10
  }
  
  # Meilleur classement historique (jusqu'à la date du match incluse)
  Best_rank_player = V_RANK %>%
    filter(Player_name == Player & Week_Rank <= week) %>%
    summarise(best_rank = min(as.numeric(Rank), na.rm = TRUE),
              max_points = max(as.numeric(Points)))
  
  if (length(Best_rank_player) == 0 || is.infinite(Best_rank_player)) {
    Best_rank_player = 1000
    
    Max_points_player = 10
  }
  
  if (rank==TRUE){
    
    return(Best_rank_player %>% pull(best_rank))
    
  }else{
    
    return(Best_rank_player %>% pull(max_points))
    
  }

}


#diff_best_rank("Tsitsipas Stefanos",as.Date("2016-03-30"))
