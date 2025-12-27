load(paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_2024_Extraction.RData"))

load(paste0(getwd(),"/Scrapping tennis data/Info_players/V_PLAYERS_RED.RData"))

table_stock=table_stock %>% 
  mutate(tournament = gsub("Challenger", "Chall.", tournament, ignore.case = TRUE)) %>% 
  mutate(Season=2024,
         tournament2=toupper(tournament)) %>% 
  filter(tournament!="Riyadh - Exhibition") %>% 
  rename(Winner_id="P1",
         Loser_id="P2") %>% 
  left_join(V_PLAYERS %>% select(Player_name,URL_players) %>% unique() %>% 
              rename(Winner_URL=URL_players),by=c("Winner_id"="Player_name")) %>% 
  left_join(V_PLAYERS %>% select(Player_name,URL_players) %>% unique() %>% 
              rename(Loser_URL=URL_players),by=c("Loser_id"="Player_name")) %>% 
  left_join(V_TOURNAMENT %>% select(tournament,Surface_tournament,Year) %>% 
              mutate(tournament2=toupper(tournament)) %>% 
              select(-tournament),
            by=c("tournament2","Season"="Year")) %>% 
  select(-tournament2) %>% 
  mutate(Surface=case_when(is.na(Surface)~Surface_tournament,
                           TRUE~Surface)) %>% 
  mutate(Surface=case_when(Surface=="clay"~"Clay",
                           Surface=="hard"~"Hard",
                           Surface=="grass"~"Grass",
                           Surface=="indoors"~"Indoors",
                           Surface=="various"~"Various",
                           TRUE~Surface)) 


table_stock=table_stock %>% 
  mutate(Round=case_when(tournament=="Masters Cup Atp" & Round=="-"~"RR",
                         TRUE~Round))

# table_stock %>% filter(is.na(Surface)) %>% group_by(tournament) %>% count()

table_stock$Number_Win=NA

table_stock$Loser_Win=NA

start_time=Sys.time()

for (i in 14063:nrow(table_stock)){
  
  winner_url=table_stock$Winner_URL[i]
  
  loser_url=table_stock$Loser_URL[i]
  
  surface=table_stock$Surface[i]
  
  # Date_match=table_stock$Date[i]
  
  Season=2024
  
  tournoi=table_stock$tournament[i]
  
  W=table_stock$Winner[i]
  
  L=table_stock$Loser[i]
  
  R=table_stock$Round[i]
  
  ##### CALCUL #####
  
  if (i%%100==0){
    
    
    print(paste0(i,": ",table_stock$Winner_id[i]," vs. ",table_stock$Loser_id[i],
                 " ",round(as.numeric(Sys.time() - start_time, units = "secs")/60,2)))
    
  }
  
  result=get_h2h(winner_url,loser_url)
  
  stat_g=get_stat_h2h(result,"all",Season,tournoi,W,L,R)
  
  stat_s=get_stat_h2h(result,surface,Season,tournoi,W,L,R)
  
  table_stock$Number_Win[i]=stat_g$W$Number_Win
  
  table_stock$Loser_Win[i]=stat_g$L$Number_Win
  
  
}

End=Sys.time()-start_time
