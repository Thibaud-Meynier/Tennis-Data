
year=2023

source(paste0(getwd(),"/Scrapping tennis data/exclusion tournament.R"))

source(paste0(getwd(),"/Scrapping tennis data/scrapping tennis tournament.R"))

table_stock=data.frame()
# 
# for (i in seq(2017,2023,by=1)){
#   
  list=list_tournament(year)
  
  #print(i)

Start=Sys.time()

  for (a in 26:nrow(list)){
    
    tournament_name=list[a,1]
    
    print(paste0(tournament_name," ",a))
    
    url_tournament=list[a,2]
    
    # year=i
    
    if (!tournament_name %in% c("Davis Cup","Next Gen ATP Finals","Masters Cup ATP",
                                "ATP Cup","Olympics - Tokyo","United Cup")){
      
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
    
    table_stock=rbind(table_stock,tournament_tot)

}
 #}

Sys.time()-Start

save(table_stock,file = paste0(getwd(),"/Scrapping tennis data/","ATP_",year,"_Extraction.RData"))
