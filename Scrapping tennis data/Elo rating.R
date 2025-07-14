#ranking de base

#ranking nouveau

# le facteur K

# les probas induites

# l'issue du match

i=6162

tournament=table_stock

for (i in 1:nrow(tournament)){
  
  # on récup pour 1 match donné les infos de chaque joueur et du match
  row=tournament[i,]
  
  p1=row$P1
  
  p2=row$P2
  
  date_match=row$Date
  
  n_match=row$N_match
  
  # on regarde l'historique du joueur p1
  hist_p1=tournament %>% filter((P1==p1|P2==p1) & (Date<=date_match & N_match!=n_match))
  
  last_match=tail(hist_p1,1)
  
  hist_p1=hist_p1 %>% filter(N_match==last_match$N_match & tournament==last_match$tournament)
  
  # Calcul de la durée entre le match et leur dernier match
  
  dday_p1=as.numeric(date_match-last_match$Date)
  
  # on prend le dernier elo_rank du joueur
  last_elo_p1=ifelse(is_empty(ifelse(p1==hist_p1$P1,hist_p1$Elo_W,hist_p1$Elo_L))==TRUE,NA,
                     ifelse(p1==hist_p1$P1,hist_p1$Elo_W,hist_p1$Elo_L))
  
  # on regarde l'historique du joueur p2
  hist_p2=tournament %>% filter((P1==p2|P2==p2)&(Date<=date_match & N_match!=n_match))
  
  last_match=tail(hist_p2,1)
  
  hist_p2=hist_p2 %>% filter(N_match==last_match$N_match & tournament==last_match$tournament)
  
  # Calcul de la durée entre le match et leur dernier match
  
  dday_p2=as.numeric(date_match-last_match$Date)
  
  # on prend le dernier elo_rank du joueur
  last_elo_p2=ifelse(is_empty(ifelse(p2==hist_p2$P1,hist_p2$Elo_W,hist_p2$Loser))==TRUE,NA,
                     ifelse(p2==hist_p2$P1,hist_p2$Elo_W,hist_p2$Loser))
  
  # on recherche si le joueur 1 a déjà disputé un match ou non
  
  count_match_p1=nrow(tournament %>% filter((P1==p1|P2==p1) & (Date<=date_match & N_match!=n_match)))
  
  count_match_p2=nrow(tournament %>% filter((P1==p2|P2==p2) & (Date<=date_match & N_match!=n_match)))
  
  # calcul dummy sur penalité absence joueur 
  
  penalty_p1=ifelse(between(dday_p1,60,80),1,
         ifelse(between(dday_p1,81,180),1.25,
                ifelse(dday_p1>180,1.5,0)))
  
  penalty_p2=ifelse(between(dday_p2,60,80),1,
                    ifelse(between(dday_p2,81,180),1.25,
                           ifelse(dday_p2>180,1.5,0)))
  
 
  
  # on reprend le dernier elo (1500 si pas d'historique)
  
  elo_p1=ifelse(is.na(last_elo_p1),1500,last_elo_p1+(penalty_p1*100)) # Si pas de elo on met 1500
  
  elo_p2=ifelse(is.na(last_elo_p2),1500,last_elo_p2+(penalty_p2*100))
  
  proba_p1 <- 1 / (1 + 10 ^ ((elo_p2 - elo_p1)/400)) # calcul des probas données par le Elo
  proba_p2 <- 1 / (1 + 10 ^ ((elo_p1 - elo_p2)/400)) 
  
  k_p1=250/((count_match_p1+5)^0.4) # calcul du facteur k pour chaque joueur
  k_p2=250/((count_match_p2+5)^0.4)
  
  # dummy pour calculer le nouveau elo
  s_p1=1 
  s_p2=0
  
  # Prime de GC
  
  level=ifelse(tournament$Categorie=="Grand Slam",1.1,1)
  
  elo_p1_new=elo_p1+(k_p1*level)*(s_p1-proba_p1)
  
  elo_p2_new=elo_p2+(k_p2*level)*(s_p2-proba_p2)
  
  # tournament$Elo_W_prec[i]=round(elo_p1,2)
  # tournament$Elo_L_prec[i]=round(elo_p2,2)
  tournament$Elo_W[i]=round(elo_p1_new,1)
  
  tournament$Elo_L[i]=round(elo_p2_new,1)
  
  print(i)
}
