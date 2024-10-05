source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

year_enc=year(Sys.Date())
week_enc=isoweek(Sys.Date())
day_enc=day(Sys.Date())

rank_week=rank_scrap(Sys.Date())

list=list_tournament(year_enc)

list$Week=isoweek(list$Date)

week_tournament=list %>% filter(year(Date)==year_enc & isoweek(Date)==week_enc)


url="https://www.tennisexplorer.com/shanghai/2024/atp-men/"

page_info=read_html(url)

page_info %>%
  html_nodes("#tournamentTabs-1-data > table") %>% 
  html_table() %>% 
  as.data.frame() %>% 
  select(2,3,5,6,7)

table_match=page_info %>%
html_nodes("#tournamentTabs-1-data > table") %>% 
html_table() %>% 
  as.data.frame() %>% 
  select(2,3,5,6,7)

names(table_match)=table_match[1,]

table_match=table_match[-1,]

colnames(table_match)=c("Round","Match","H2H","H", "A")

match_ref=data.frame()

for (i in 1:nrow(table_match)){
  
href=page_info %>%
  html_nodes(paste0("#tournamentTabs-1-data > table > tbody > tr:nth-child(",i,") > td.t-name > a")) %>% 
  html_attr("href")


href=paste0("https://www.tennisexplorer.com",href)

match_ref=rbind(match_ref,href)

print(href)
}

colnames(match_ref)="url"

table_match$url=match_ref$url

players_id=data.frame("P1"=NA,"P2"=NA,"N_match"=NA)

for (i in 1:nrow(table_match)){
  
match_id=table_match$url[i]

page_match <- read_html(match_id)

players=page_match %>% html_nodes("th.plName") %>% html_text() 

players_id[i,1]=players[1]
players_id[i,2]=players[2]
players_id[i,3]=i

print(i)
}

table_match$P1=players_id$P1
table_match$P2=players_id$P2


table_match= table_match %>% 
  left_join(rank_week %>% select(`Player name`,Points,Rank,Country),by=c("P1"="Player name")) %>% 
  left_join(rank_week %>% select(`Player name`,Points,Rank,Country),by=c("P2"="Player name")) %>% 
  rename("Points P1"="Points.x",
         "Points P2"="Points.y",
         "Rank P1"="Rank.x",
         "Rank P2"="Rank.y",
         "Country P1"="Country.x",
         "Country P2"="Country.y")
  

table_match=table_match %>% select(Round,P1,P2,H2H,H,A,
                                   `Points P1`,`Rank P1`,`Country P1`,
                                   `Rank P2`,`Points P2` ,`Country P2`)
                                   