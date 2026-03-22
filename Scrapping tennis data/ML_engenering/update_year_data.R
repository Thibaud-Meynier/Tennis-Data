
year=2026

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

load(file = paste0(getwd(),"/Scrapping tennis data/Extraction/ATP_",year,"_Extraction.RData"))

load(file = paste0(getwd(),"/Scrapping tennis data/Rank/RANK_ATP_",year,".RData"))

load(paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_",year,".RData"))

table_stock_new=data.frame()

list_new=list_tournament(year)

list_old=V_TOURNAMENT %>% filter(isoweek(Date)<isoweek(Sys.Date()))

# list=list %>% filter(isoweek(Date)<isoweek(Sys.Date()))

list_all_upper <- toupper(list_new$tournament)

list_old_upper <- toupper(unique(list_old$tournament))

diff_upper <- setdiff(list_all_upper, list_old_upper)

to_scrap <- list_new$tournament[list_all_upper %in% diff_upper]

to_scrap <- unique(to_scrap)

to_scrap <- to_scrap[to_scrap != "Fujairah 2 chall."]

list_scrap=list_new %>% filter(tournament %in% to_scrap)

Start=Sys.time()

for (a in 1:nrow(list_scrap)){
  
  tournament_name=list_scrap[a,1]
  
  print(paste0(tournament_name," ",a))
  
  url_tournament=list_scrap[a,2]
  
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

table_stock=rbind(table_stock,table_stock_new)

Sys.time()-Start

##### RANK #####
year_start <- floor_date(Sys.Date(), "year")

today <- Sys.Date()

week_pass <- unique(isoweek(seq(year_start, today, by = "week")))

week_to_scrap=setdiff(week_pass,unique(isoweek(rank$Date)))

week_to_scrap=week_to_scrap[week_to_scrap != 1]

date=floor_date(as.Date(paste0(year, "-01-01")) + (week_to_scrap - 1) * 7, unit = "weeks", week_start = 1)

rank_new=rank_scrap(date)

rank=rbind(rank,rank_new)

##### TOURNAMENT #####


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

list_scrap=list_scrap %>% 
  mutate(Categorie=paste(Categorie,ifelse(is.na(Points_tournament)==T,"",Points_tournament)))

list_scrap= list_scrap %>%  
  mutate(tournament = gsub("chall", "Chall", tournament, ignore.case = TRUE))

V_TOURNAMENT=rbind(list_old,list_scrap)

save(V_TOURNAMENT,file=paste0(getwd(),"/Scrapping tennis data/Tournament/V_TOURNAMENT_F_2026.RData"))
