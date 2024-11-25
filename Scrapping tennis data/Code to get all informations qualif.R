# sortir la liste des tournois joués une année  donnée

start=Sys.time()

# year=2022
# 
# tournament_name="french-open"

# tournoi et joueurs

tournament_qualif=get_tournament_qualif(year=year,
                                        tournament=tournament_name,
                                        url_tournament=url_tournament)

if (nrow(tournament_qualif)>=1){
  
players_qualif=get_players_name_qualif(year = year,
                                       tournament = tournament_name,
                                       url_tournament=url_tournament)

tournament_qualif=tournament_qualif %>% 
  left_join(players_qualif,by=c("N_match")) %>% 
  mutate("Phase"="Qualification")

# date_qualif=calcul_date_rank_qualif(year,tournament_qualif)  # on prend le classement de la semaine des qualifs
# 
# # on récupère le classement de la semaine
# 
# rank_qualif=rank_scrap(date_qualif)
# 
# #race_qualif=race_scrap(date_qualif)
# 
# # On croise le rang avec les joueurs 
# 
# tournament_qualif=tournament_qualif %>% 
#   left_join(rank_qualif %>% select(1,3,4,5,6),by=c("P1"="Player name")) %>% 
#   left_join(rank_qualif %>% select(1,3,4,5,6),by=c("P2"="Player name")) %>% 
#   # left_join(race_qualif %>% select(1,3,4,5),by=c("P1"="Player name")) %>% 
#   # left_join(race_qualif %>% select(1,3,4,5),by=c("P2"="Player name")) %>% 
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
#   mutate("Elo_W"=NA,"Elo_L"=NA,"Phase"="Qualification")

}

print(round(Sys.time()-start,2))
