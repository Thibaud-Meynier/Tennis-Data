# sortir la liste des tournois joués une année  donnée

start=Sys.time()

year=2017

tournament_name="Davis Cup"

url_tournament="https://www.tennisexplorer.com/davis-cup/2017/atp-men/"

# tournoi et joueurs

tournament=get_davis_cup(year=year,
                          tournament=tournament_name,
                          url_tournament=url_tournament)

players=get_players_name(year = year,
                         tournament = tournament_name,
                         url_tournament=url_tournament)

tournament=tournament %>% 
  left_join(players,by=c("N_match"))


# Filtrer les lundis
days_year <- seq.Date(as.Date(paste(year-1, "-01-01", sep = "")), 
                      as.Date(paste(year, "-12-31", sep = "")), by = "day")

mondays <- days_year[weekdays(days_year) == "lundi"] %>% as.data.frame()

colnames(mondays)[1]="Date"

mondays$week=week(mondays$Date)

list_week=unique(week(tournament$Date))

date=mondays %>% filter(week %in% list_week & year(Date)==year) %>% 
  select(Date) %>% 
  as.matrix()

# on récupère le classement de la semaine

rank_tot=data.frame()

for (i in date){

  rank=rank_scrap(i)
  rank$Date=i
  rank$Week=week(rank$Date)
  
  rank_tot=rbind(rank_tot,rank)
  
  print(i)
}


#race=race_scrap(date)

tournament$Week=week(tournament$Date)

# On croise le rang avec les joueurs 

tournament=tournament %>% 
  left_join(rank_tot %>% select(1,3,4,5,6,8),by=c("P1"="Player name","Week"="Week")) %>% 
  left_join(rank_tot %>% select(1,3,4,5,6,8),by=c("P2"="Player name","Week"="Week")) %>% 
  # left_join(race %>% select(1,3,5),by=c("P1"="Player name")) %>% 
  # left_join(race %>% select(1,3,5),by=c("P2"="Player name")) %>% 
  rename("Rank_W"=Rank.x,
         "Rank_L"=Rank.y,
         "Pts_W"=Points.x,
         "Pts_L"=Points.y,
         # "Rank_Race_W"=Race_Rank.x,
         # "Rank_Race_L"=Race_Rank.y,
         # "Pts_Race_W"=Race_Points.x,
         # "Pts_Race_L"=Race_Points.y,
         "Country_W"=Country.x,
         "Country_L"=Country.y,
         "Winner_id"=P1,
         "Loser_id"=P2,
         "Winner_url"=URL_players.x,
         "Loser_url"=URL_players.y) %>% 
  mutate("Elo_W"=NA,"Elo_L"=NA) %>% 
  select(-Week)

print(round(Sys.time()-start,2))
