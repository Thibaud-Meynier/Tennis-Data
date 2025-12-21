#ranking de base

#ranking nouveau

# le facteur K

# les probas induites

# l'issue du match

#i=1
# & Categorie %in% c("ATP 1000","Grand Slam","ATP 500","ATP 250","Team","Masters","Olympics")


tournament=V_MATCH_t %>% 
  filter(Phase=="Main Draw") %>% 
  filter(if(length(surface) == 1 && surface == "all") TRUE else Surface_tournament %in% surface) %>%
  arrange(Date,tournament,desc(N_match),Season)

print(nrow(tournament))

tournament$Elo_W=NA

tournament$Elo_L=NA

tournament$Elo_W_NEW=NA

tournament$Elo_L_NEW=NA

for (i in 1:nrow(tournament)){
  
  # on récup pour 1 match donné les infos de chaque joueur et du match
  
  row=tournament[i,]
  
  p1=row$Winner_id
  
  p2=row$Loser_id
  
  date_match=row$Date
  
  n_match=row$N_match
  
  Round=row$Round
  
  info=row$info
  
  # on regarde l'historique du joueur p1
  hist_p1=tournament %>% filter((Winner_id==p1|Loser_id==p1) & (Date<=date_match))
  
  lim=which(hist_p1$tournament==row$tournament & 
              hist_p1$Date==date_match & 
              hist_p1$N_match==n_match & 
              hist_p1$Round==Round)
  
  hist_p1=hist_p1 %>% 
    slice(1:(lim-1))
  
  last_match=tail(hist_p1,1)
  
  #hist_p1=hist_p1 %>% filter(N_match==last_match$N_match & tournament==last_match$tournament)
  
  # Calcul de la durée entre le match et leur dernier match
  
  dday_p1=as.numeric(date_match-last_match$Date)
  
  # on prend le dernier elo_rank du joueur
  last_elo_p1=ifelse(is_empty(ifelse(p1==last_match$Winner_id,last_match$Elo_W_NEW,last_match$Elo_L_NEW))==TRUE,NA,
                     ifelse(p1==last_match$Winner_id,last_match$Elo_W_NEW,last_match$Elo_L_NEW))
  
  # on regarde l'historique du joueur p2
  hist_p2=tournament %>% filter((Winner_id==p2|Loser_id==p2)&(Date<=date_match))
  
  lim=which(hist_p2$tournament==row$tournament & 
              hist_p2$Date==date_match & 
              hist_p2$N_match==n_match & 
              hist_p2$Round==Round)
  
  hist_p2=hist_p2 %>% 
    slice(1:(lim-1))
  
  last_match=tail(hist_p2,1)
  
  #hist_p2=hist_p2 %>% filter(N_match==last_match$N_match & tournament==last_match$tournament)
  
  # Calcul de la durée entre le match et leur dernier match
  
  dday_p2=as.numeric(date_match-last_match$Date)
  
  # on prend le dernier elo_rank du joueur
  last_elo_p2=ifelse(is_empty(ifelse(p2==last_match$Winner_id,last_match$Elo_W_NEW,last_match$Elo_L_NEW))==TRUE,NA,
                     ifelse(p2==last_match$Winner_id,last_match$Elo_W_NEW,last_match$Elo_L_NEW))
  
  # on recherche si le joueur 1 a déjà disputé un match ou non
  
  threshold=300
  
  count_match_p1=hist_p1 %>% nrow()
  
  count_match_p1=ifelse(count_match_p1>=threshold,threshold,count_match_p1)
  
  count_match_p2=hist_p2 %>% nrow()
  
  count_match_p2=ifelse(count_match_p1>=threshold,threshold,count_match_p2)
  
  # calcul des dummy pour le k factor
  
  Round_adjust <- ifelse(Round == "F", 1,
                         ifelse(Round == "SF", 0.9,
                                ifelse(Round == "QF", 0.8, 0.7)))
  
  Walkover=ifelse(row$info=="Completed",1,0.5)
  
  penalty=function(diff_date){
    
    penalty=ifelse(between(diff_date,60,80),0.7,
                   ifelse(between(diff_date,81,180),0.85,
                          ifelse(diff_date>180,1,0)))
    
    return(penalty)
  }
  
  covid=ifelse(between(date_match,as.Date("2020-08-01"),as.Date("2021-02-01")) ,0,1)
  
  penalty_p1=penalty(dday_p1)*covid
  
  penalty_p2=penalty(dday_p2)*covid
  
  if (length(surface) != 1|length(surface) == 1 & surface!="all"){
    
    penalty_p1=0
    
    penalty_p2=0
    
  }
  # on reprend le dernier elo (1500 si pas d'historique)
  
  elo_p1=ifelse(is.na(last_elo_p1)==T,1500,last_elo_p1-(penalty_p1*100)) # Si pas de elo on met 1500
  
  elo_p2=ifelse(is.na(last_elo_p2)==T,1500,last_elo_p2-(penalty_p2*100))
  
  proba_p1 <- 1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400)) # calcul des probas données par le Elo
  proba_p2 <- 1 / (1 + 10 ^ ((elo_p1 - elo_p2)/400)) 
  
  rating=function(elo_p){
    
    rating=1+ 18/(1+(2^((elo_p-1500)/100)))
    
    return(rating)
  }
  
  # Prime de GC
  
  level=ifelse(is.na(row$Categorie) | grepl("Challenger", row$Categorie, ignore.case = TRUE), 0.65,
               ifelse(row$Categorie == "Grand Slam", 1,
                      ifelse(row$Categorie %in% c("Olympics", "Masters"), 0.95,
                             ifelse(row$Categorie == "ATP 1000", 0.9,
                                    ifelse(row$Categorie %in% c("ATP 500", "Team"), 0.8,
                                           ifelse(row$Categorie == "ATP 250", 0.7, 0.65)
                                    )
                             )
                      )
               )
  )
  
  # k_p1=250/((count_match_p1+5)^0.4)*level*Round_adjust*Walkover*rating(elo_p1) # calcul du facteur k pour chaque joueur
  # k_p2=250/((count_match_p2+5)^0.4)*level*Round_adjust*Walkover*rating(elo_p2)
  
  k_p1=32*level*Round_adjust*Walkover*rating(elo_p1)
  k_p2=32*level*Round_adjust*Walkover*rating(elo_p2)
  
  # dummy pour calculer le nouveau elo
  s_p1=1 
  s_p2=0
  
  elo_p1_new=elo_p1+(k_p1)*(s_p1-proba_p1)
  
  elo_p2_new=elo_p2+(k_p2)*(s_p2-proba_p2)
  
  # tournament$Elo_W_prec[i]=round(elo_p1,2)
  # tournament$Elo_L_prec[i]=round(elo_p2,2)
  
  
  tournament$Elo_W[i]=round(elo_p1,1)
  
  tournament$Elo_L[i]=round(elo_p2,1)
  
  tournament$Elo_W_NEW[i]=round(elo_p1_new,1)
  
  tournament$Elo_L_NEW[i]=round(elo_p2_new,1)
  
  print(i)
}
