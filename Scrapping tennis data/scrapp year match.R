
year=2017

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

table_stock=data.frame()
# 
# for (i in seq(2017,2023,by=1)){
#   
  list=list_tournament(year)
  
  #print(i)

Start=Sys.time()

  for (a in 91:nrow(list)){
    
    tournament_name=list[a,1]
    
    print(tournament_name)
    
    url_tournament=list[a,2]
    
    # year=i
    
    if (!tournament_name %in% c("Davis Cup","Next Gen ATP Finals","Masters Cup ATP")){
      
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations qualif.R"))
    
    source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations.R"))
    
    }else if (tournament_name=="Davis Cup"){
      
      source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations DC.R"))
      
    }else {
      # Retourne 0 lignes normalement car pas de qualif pour ces tournois
      source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations qualif.R"))
      
      source(paste0(getwd(),"/Scrapping tennis data/Code to get all informations.R"))
      
  }
    
    tournament_tot=rbind(tournament_qualif,tournament)
    
    table_stock=rbind(table_stock,tournament_tot)

}
#}

Sys.time()-Start
# LExington pb avec la règle de gestion ifelse