# sortir la liste des tournois joués une année  donnée

start=Sys.time()

year=2022

tournament_name="french-open"

# tournoi et joueurs

tournament_qualif=get_tournament_qualif(year=year,tournament=tournament_name)

players_qualif=get_players_name_qualif(year = year,tournament = tournament_name)

tournament_qualif=tournament_qualif %>% 
  left_join(players_qualif,by=c("N_match"))

# Filtrer les lundis
days_year <- seq.Date(as.Date(paste(year, "-01-01", sep = "")), 
                      as.Date(paste(year, "-12-31", sep = "")), by = "day")

mondays <- days_year[weekdays(days_year) == "lundi"] %>% as.data.frame()

colnames(mondays)[1]="Date"

mondays$week=week(mondays$Date)

# on récupère la semaine du tournoi pour ensuite prendre le classement du lundi de cette semaine

date_qualif=mondays$Date[mondays$week==week(min(tournament_qualif$Date)-1)] # on prend le classement de la semaine des qualifs

# on récupère le classement de la semaine

rank_qualif=rank_scrap(date_qualif)

race_qualif=race_scrap(date_qualif)

# On croise le rang avec les joueurs 

tournament_qualif=tournament_qualif %>% 
  left_join(rank_qualif %>% select(1,3,5),by=c("P1"="Player name")) %>% 
  left_join(rank_qualif %>% select(1,3,5),by=c("P2"="Player name")) %>% 
  left_join(race_qualif %>% select(1,3,4,5),by=c("P1"="Player name")) %>% 
  left_join(race_qualif %>% select(1,3,4,5),by=c("P2"="Player name")) %>% 
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

print(round(Sys.time()-start,2))

# Get players profil url

url=paste0("https://www.tennisexplorer.com/ranking/atp-men/?date=",date,"&page=",1)
page=read_html(url)
page %>% html_nodes("td.t-name>a") %>% html_attr("href")
