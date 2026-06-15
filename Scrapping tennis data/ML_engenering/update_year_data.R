conflicts_prefer(dplyr::filter)
conflicts_prefer(lubridate::month)
conflicts_prefer(lubridate::isoweek)
conflicts_prefer(lubridate::year)

year=2026

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",year,"_Extraction.RData"))

load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",year,".RData"))

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_",year,".RData"))

load(paste0(getwd(),"/Scrapping tennis data/Tournament/INFO_TOURNAMENT_2002_2025.RData"))

table_stock_new=data.frame()

list_new=list_tournament(year)

# on prend que les tournois joués la semaine précédente 

list_new2=list_new %>% filter(isoweek(Date)<isoweek(Sys.Date()))

list_all_upper <- toupper(list_new2$tournament)

list_old_upper <- toupper(unique(table_stock$tournament))

diff_upper <- setdiff(list_all_upper, list_old_upper)

to_scrap <- list_new2$tournament[list_all_upper %in% diff_upper]

to_scrap <- unique(to_scrap)

to_scrap <- to_scrap[!to_scrap %in% c("Fujairah 2 chall.")]

match_scrap=list_new2 %>% filter(tournament %in% to_scrap)

match_scrap=match_scrap %>% filter(Date<Sys.Date()-1) # On ne scrap pas les tournois qui commencent le dimanche

Start=Sys.time()

for (a in 1:nrow(match_scrap)){
  
  tournament_name=match_scrap[a,1]
  
  print(paste0(tournament_name," ",a))
  
  url_tournament=match_scrap[a,2]
  
  # year=i
  
  if (!tournament_name %in% c("Davis Cup","Next Gen ATP Finals","Masters Cup ATP","Challenger Tour Finals",
                              "ATP Cup","Olympics - Tokyo","United Cup","Olympics - Rio de Janeiro","Olympics - London",
                              "Olympics - Beijing","Olympics - Athens","Olympics - Paris")){
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations qualif.R"))
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations.R"))
    
  }else if (tournament_name=="Davis Cup"){
    
    tournament_qualif=data.frame(matrix(ncol = 37,nrow = 0))
    
    colnames(tournament_qualif)=colnames_calc
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations DC.R"))
    
  }else if (tournament_name=="ATP Cup"|tournament_name=="United Cup"){
    
    tournament_qualif=data.frame(matrix(ncol = 37,nrow = 0))
    
    colnames(tournament_qualif)=colnames_calc
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations ATP_Cup.R"))
    
  }else {
    
    tournament_qualif=data.frame(matrix(ncol = 37,nrow = 0))  
    
    colnames(tournament_qualif)=colnames_calc
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations.R"))
    
  }
  
  tournament_tot=rbind(tournament_qualif,tournament)
  
  table_stock_new=rbind(table_stock_new,tournament_tot)
  
}

# table_stock=table_stock %>% filter(ETAT=="OLD")
# 
table_stock$ETAT="OLD"

table_stock_new$ETAT="NEW"

table_stock=rbind(table_stock,table_stock_new)

Sys.time()-Start

save(table_stock,file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",year,"_Extraction.RData"))

##### RANK #####

year_start <- floor_date(Sys.Date(), "year")

last_monday <- floor_date(Sys.Date(), unit = "week", week_start = 1)

# Premier lundi de l'année
first_monday <- floor_date(year_start, unit = "week", week_start = 1)

# Si floor ramène à fin décembre, on avance d'une semaine

if (first_monday < year_start) first_monday <- first_monday + 7

# Tous les lundis depuis le premier lundi de l'année
all_mondays <- seq(first_monday, last_monday, by = "week")

# Lundis déjà présents dans la table
mondays_in_rank <- unique(floor_date(rank$Date, unit = "week", week_start = 1))

# Lundis manquants
date <- as.Date(setdiff(all_mondays, mondays_in_rank))

for (i in date){
  
  rank_new <- rank_scrap(as.Date(i))
  
  rank_new=rank_new %>% 
    mutate(Date=as.Date(i),
           Year=year,
           Week=week(as.Date(i)))
  
  rank <- rbind(rank, rank_new)
  
  print(as.Date(i))
  
}


save(rank, file=paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",year,".RData"))

##### TOURNAMENT #####

v_tournament_scrap=setdiff(toupper(list_new$tournament), toupper(V_TOURNAMENT_F$tournament))

list_scrap=list_new %>% filter(toupper(tournament) %in% v_tournament_scrap)

#list_scrap=list_new

list_scrap$Country_tournament=NA
list_scrap$Surface_tournament=NA
list_scrap$Points_tournament=NA

for (i in 1:nrow(list_scrap)){
  
  url=list_scrap$URL[i]
  
  page_info=read_html(url)
  
  country_tournament=page_info %>%
    html_nodes("#center > h1") %>%
    html_text()
  
  info <- sub(".*\\(([^)]+)\\).*", "\\1", country_tournament)
  
  surface_tournament=page_info %>% 
    html_nodes("#center > div:nth-child(2)") %>% 
    html_text()
  
  info2 <- extrait <- sub(".*\\, (\\w+)\\,.*", "\\1", surface_tournament)
  
  ranking_points_tournament=
    page_info %>%
    html_nodes("table.result.moneydetails") %>%
    html_table() %>%
    as.data.frame()
  
  if(ncol(ranking_points_tournament)<=4 & ncol(ranking_points_tournament)>0 & nrow(ranking_points_tournament)<=9){
    
    names(ranking_points_tournament)=ranking_points_tournament[1,]
    ranking_points_tournament=ranking_points_tournament[-1,]
    
    ranking_points_tournament = ranking_points_tournament[,c(1,3)]
    
    ranking_points_tournament$tournament=list_scrap$tournament[i]
    
    ranking_points_tournament=ranking_points_tournament %>%
      mutate(Round=case_when(Round=="1. round"~"1R",
                             Round=="2. round"~"2R",
                             Round=="3. round"~"3R",
                             Round=="round of 16"~"R16",
                             Round=="quarterfinal"~"QF",
                             Round=="semifinal"~"SF",
                             Round=="final"~"F",
                             Round=="-"~"",
                             TRUE~"Winner"))
    
    points=max(as.numeric(ranking_points_tournament$`Ranking points`))
    
    list_scrap$Country_tournament[i]=info
    list_scrap$Surface_tournament[i]=info2
    list_scrap$Points_tournament[i]=points
    
  }else{
    
    points=0
    
    list_scrap$Country_tournament[i]=info
    list_scrap$Surface_tournament[i]=info2
    list_scrap$Points_tournament[i]=points
    
  }
  
  
  print(i)
  
}


list_scrap$Week_tournament=isoweek(list_scrap$Date)

list_scrap$Year=case_when(list_scrap$Week_tournament>=52 & month(list_scrap$Date)==1 ~year(list_scrap$Date),
                          list_scrap$Week_tournament>=52 & month(list_scrap$Date)==12 ~ year(list_scrap$Date)+1,
                          list_scrap$Week_tournament==1 & month(list_scrap$Date)==12 ~ year(list_scrap$Date)+1,
                            TRUE ~ year(list_scrap$Date))

list_scrap = list_scrap %>%  
  mutate(tournament = gsub("chall", "Chall", tournament, ignore.case = TRUE))

red_categ <- function(Annee, tournoi) {
  Red_value <- V_TOURNAMENT %>%
    filter(tournament == tournoi & Year == (Annee - 1)) %>%
    pull(Points_tournament)
  
  # Fallback si aucune valeur trouvée
  if (length(Red_value) == 0) return(NA_real_)
  return(Red_value)
}

# Vectorisation obligatoire pour mutate()
red_categ_vec <- Vectorize(red_categ)

list_scrap <- list_scrap %>%
  mutate(Points_tournament = case_when(
    Points_tournament == 0 ~ red_categ_vec(Year, tournament),
    TRUE ~ Points_tournament
  ))

list_scrap=list_scrap %>% 
  mutate(Categorie=paste(Categorie,ifelse(is.na(Points_tournament)==T,"",Points_tournament)))

V_TOURNAMENT_F=rbind(V_TOURNAMENT_F,list_scrap)

V_TOURNAMENT=rbind(V_TOURNAMENT,V_TOURNAMENT_F)

save(V_TOURNAMENT_F,file=paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2026.RData"))

save(V_TOURNAMENT,file=paste0(getwd(),"/Scrapping tennis data/Tournament/INFO_TOURNAMENT_2002_2026.RData"))

##### ELO ######

# Update du ELO global 

source("./Scrapping tennis data/update_elo_rate.R")

# Update du ELO hard

s="Hard"

source("./Scrapping tennis data/update_elo_rate_surface.R")

# Update du ELO Clay

s="Clay"

source("./Scrapping tennis data/update_elo_rate_surface.R")

# Update du ELO Grass

s="Grass"

source("./Scrapping tennis data/update_elo_rate_surface.R")

# Update du ELO Indoors

s="Indoors"

source("./Scrapping tennis data/update_elo_rate_surface.R")

# Update du ELO Major

c="major"

source("./Scrapping tennis data/update_elo_rate_categorie.R")

# Update du ELO ATP 1000

c="ATP 1000"

source("./Scrapping tennis data/update_elo_rate_categorie.R")

# Update du ELO ATP 500

c="ATP 500"

source("./Scrapping tennis data/update_elo_rate_categorie.R")

# Update du ELO ATP 250

c="ATP 250"

source("./Scrapping tennis data/update_elo_rate_categorie.R")

# Update ELO Rating G 

source("./Scrapping tennis data/update_elo_rank.R")

# Update V_MATCH_HSIT 

source("./Scrapping tennis data/update_match_hist.R")

# Update MATCHS_STATS

source("./Scrapping tennis data/update_match_stats.R")

# Update TABLE_ML

source("./Scrapping tennis data/update_match_stats2.R")
