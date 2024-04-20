# sortir la liste des tournois joués une année  donnée

start=Sys.time()

year=2022

tournament_name="montreal"

# tournoi et joueurs

tournament=get_tournament(year=year,tournament=tournament_name)

players=get_players_name(year = year,tournament = tournament_name)

tournament=tournament %>% 
  left_join(players,by=c("N_match"))

# Filtrer les lundis
days_year <- seq.Date(as.Date(paste(year, "-01-01", sep = "")), 
                      as.Date(paste(year, "-12-31", sep = "")), by = "day")

mondays <- days_year[weekdays(days_year) == "lundi"] %>% as.data.frame()

colnames(mondays)[1]="Date"

mondays$week=week(mondays$Date)

# on récupère la semaine du tournoi pour ensuite prendre le classement du lundi de cette semaine

date=mondays$Date[mondays$week==week(min(tournament$Date))]

# on récupère le classement de la semaine

rank=rank_scrap(date)

race=race_scrap(date)

# On croise le rang avec les joueurs 

tournament=tournament %>% 
  left_join(rank %>% select(1,3,5),by=c("P1"="Player name")) %>% 
  left_join(rank %>% select(1,3,5),by=c("P2"="Player name")) %>% 
  left_join(race %>% select(1,3,4,5),by=c("P1"="Player name")) %>% 
  left_join(race %>% select(1,3,4,5),by=c("P2"="Player name")) %>% 
  rename("Rank_W"=Rank.x,
         "Rank_L"=Rank.y,
         "Pts_W"=Points.x,
         "Pts_L"=Points.y,
         "Rank_Race_W"=Race_Rank.x,
         "Rank_Race_L"=Race_Rank.y,
         "Pts_Race_W"=Race_Points.x,
         "Pts_Race_L"=Race_Points.y,
         "Country_W"=Country.x,
         "Country_L"=Country.y) %>% 
  mutate("Elo_W"=NA,"Elo_L"=NA)

print(Sys.time()-start)

# Get players profil url

url=paste0("https://www.tennisexplorer.com/ranking/atp-men/?date=",date,"&page=",1)
page=read_html(url)
page %>% html_nodes("td.t-name>a") %>% html_attr("href")
