# sortir la liste des tournois joués une année  donnée

start=Sys.time()

# year=2017
# 
# tournament_name="Davis Cup"
# 
# url_tournament="https://www.tennisexplorer.com/davis-cup/2017/atp-men/"

# tournoi et joueurs

tournament=get_atp_cup(year=year,
                          tournament=tournament_name,
                          url_tournament=url_tournament)

if (nrow(tournament)>=1){
  
  players=get_players_name(year = year,
                           tournament = tournament_name,
                           url_tournament=url_tournament)
  
  tournament=tournament %>% 
    left_join(players,by=c("N_match"))
  
  date=calcul_date_rank(year,tournament) # on prend le classement du lundi même pour les tournois commençants le dimanche
  
  # on récupère le classement de la semaine
  # 
  # rank=rank_scrap(date)
  # 
  # #race=race_scrap(date)
  # 
  # # On croise le rang avec les joueurs 
  # 
  # tournament=tournament %>% 
  #   left_join(rank %>% select(1,3,4,5,6),by=c("P1"="Player name")) %>% 
  #   left_join(rank %>% select(1,3,4,5,6),by=c("P2"="Player name")) %>% 
  #   # left_join(race %>% select(1,3,5),by=c("P1"="Player name")) %>% 
  #   # left_join(race %>% select(1,3,5),by=c("P2"="Player name")) %>% 
  #   rename("Rank_W"=Rank.x,
  #          "Rank_L"=Rank.y,
  #          "Pts_W"=Points.x,
  #          "Pts_L"=Points.y,
  #          # "Rank_Race_W"=Race_Rank.x,
  #          # "Rank_Race_L"=Race_Rank.y,
  #          # "Pts_Race_W"=Race_Points.x,
  #          # "Pts_Race_L"=Race_Points.y,
  #          "Country_W"=Country.x,
  #          "Country_L"=Country.y,
  #          "Winner_id"=P1,
  #          "Loser_id"=P2,
  #          "Winner_url"=URL_players.x,
  #          "Loser_url"=URL_players.y) %>% 
  #   mutate("Elo_W"=NA,"Elo_L"=NA,"Phase"="Main Draw")
  
}

print(round(Sys.time()-start,2))
