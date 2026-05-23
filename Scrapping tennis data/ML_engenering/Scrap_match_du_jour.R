

# A Partir de la liste des tournois de la semaine, scrapper les matchs associés 

## import de la liste des tournois et les filtrer sur 


year = year(Sys.Date())

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_",year,".RData"))

start_week = floor_date(Sys.Date(), unit = "week", week_start = 1)

V_TOURNAMENT_F = V_TOURNAMENT_F %>% 
  filter(Date<=start_week+7 & Date>= (start_week-1) & Categorie %like% "ATP")


MATCH_TO_PLAY=data.frame()

for (i in 1:nrow(V_TOURNAMENT_F)){
  
  url=V_TOURNAMENT_F$URL[i]
  
  page_info=read_html(url)
  
  table_match=page_info %>%
    html_nodes("table.result") %>% 
    html_table()
  
  
  table_match=table_match[[1]] %>% 
    as.data.frame() %>% 
    select(1,2,3,5,6,7)
  
  names(table_match)=table_match[1,]
  
  table_match=table_match[-1,]
  
  colnames(table_match)=c("Date","Round","Match","H2H","H", "A")
  
  table_match$tournament=V_TOURNAMENT_F$tournament[i]
  
  match_ref=data.frame()
  
  for (i in 1:nrow(table_match)){
    
    href=page_info %>%
      html_node(paste0("table.result>tbody>tr:nth-child(",i,")>td.t-name>a")) %>% 
      #html_nodes(paste0("#tournamentTabs-1-data > table > tbody > tr:nth-child(",i,") > td.t-name > a")) %>% 
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
  
  
  MATCH_TO_PLAY=rbind(MATCH_TO_PLAY,table_match)
  
  print(i)
  
  
}


MATCH_TO_PLAY = MATCH_TO_PLAY %>% 
  mutate(
    Date = case_when(
      grepl("^today",    Date) ~ as.POSIXct(paste(Sys.Date(),    str_extract(Date, "\\d{2}:\\d{2}")), format = "%Y-%m-%d %H:%M"),
      grepl("^tomorrow", Date) ~ as.POSIXct(paste(Sys.Date()+1, str_extract(Date, "\\d{2}:\\d{2}")), format = "%Y-%m-%d %H:%M"),
      TRUE ~ as.POSIXct(Date, format = "%Y-%m-%d %H:%M")
    )
  ) %>% 
  filter(
   as.Date(Date)==Sys.Date() | as.Date(Date)<(as.Date(paste0(Sys.Date()+1," ","07:00:00")))
  )


# Ajouter les infos sur le joueurs (age, taille, poids, pays, main, classement)

# Calculer les stats 
