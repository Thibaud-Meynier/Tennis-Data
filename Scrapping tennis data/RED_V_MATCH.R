library(lubridate)
library(sqldf)

V_TABLE_MATCH=V_MATCH %>% 
  mutate(Week=isoweek(Date),
         Year=year(Date)) %>% 
  select(tournament,
         Date,
         Week,
         Season,
         N_match,
         Round,
         Phase,
         info,
         Winner_id,
         Loser_id,
         Country_W,
         Country_L,
         #Winner_url,
         #Loser_url,
         Score_W,
         Score_L,
         Set1_W,
         Set1_L,
         Set2_W,
         Set2_L,
         Set3_W,
         Set3_L,
         Set4_W,
         Set4_L,
         Set5_W,
         Set5_L,
         Odd_W,
         Odd_L
         )

V_TABLE_MATCH=rbind(V_TABLE_MATCH,tournament)

save(V_TABLE_MATCH,file = paste0(getwd(),"/Scrapping tennis data/Extraction/V_TABLE_MATCH.RData"))

V_TABLE_MATCH_TEST=sqldf("select f.Country_tournament
                        ,(case when f.Categorie='ATP' and f.Points_tournament=250 then 'ATP_250'
                               when f.Categorie='ATP' and f.Points_tournament=500 then 'ATP_500'
                               when f.Categorie='ATP' and f.Points_tournament=1000 then 'ATP_1000'
                               when a.tournament in ('Australian Open','French Open','Wimbledon','US Open') then 'GC'
                               when a.tournament like '%Olympics%' then 'Olympics'
                               when a.tournament='Davis Cup' then 'Davis Cup'
                               when a.tournament in ('United Cup','ATP Cup') then 'Team Cup'
                               when a.tournament='Masters Cup ATP' then 'Masters Cup'
                               when a.tournament='Hamburg' then 'ATP_500'
                               when a.tournament in ('Metz','Cologne','Cologne 2','Eastbourne') then 'ATP_250'
                               else f.Categorie||'_'||f.Points_tournament end) as Categorie_tournament
                        ,f.Surface_tournament
                        ,a.*
                        ,b.Rank as Rank_W
                        ,b.Points as Points_W
                        ,d.Size as Size_W
                        ,d.Weight as Weight_W
                        ,d.Birth_date as Birth_date_W
                        ,d.Hand as Hand_W
                        

                        ,c.Rank as Rank_L
                        ,c.Points as Points_L
                        ,e.Size as Size_L
                        ,e.Weight as Weight_L
                        ,e.Birth_date as Birth_date_L
                        ,e.Hand as Hand_L
       
                       from V_TABLE_MATCH a
                       
                       left join V_RANK b on b.Player_name=a.Winner_id 
                        and b.Week=a.Week 
                        and b.Year=a.Year
                       
                       left join V_RANK c on c.Player_name=a.Loser_id 
                        and c.Week=a.Week 
                        and c.Year=a.Year
                            
                       left join V_PLAYERS d on d.Player_name=a.Winner_id   
                       
                       left join V_PLAYERS e on e.Player_name=a.Loser_id
                         
                       left join V_TOURNAMENT_F f on f.tournament=a.tournament 
                        and f.Year=a.Season 
                         --and a.Date>=(f.Date-10) 
                         --and a.Date<=(f.Date+30)")

#Test simulation 50_50

T50_50=V_TABLE_MATCH_TEST %>% filter(abs(as.numeric(Rank_W)-as.numeric(Rank_L))<=5 
                              & substr(Categorie_tournament,1,3) %in% c("ATP",'GC') & info=="Completed"
                              & Phase=='Main Draw') %>% 
  select(tournament,Categorie_tournament,Phase,Round,Surface_tournament,info,Date,Winner_id,Loser_id,Odd_W,Odd_L,Rank_W,Rank_L) %>% 
  mutate(select_match=ifelse(Odd_W>Odd_L,1,
                              ifelse(Odd_W==Odd_L & Rank_W>Rank_L,1,0))) %>% 
  #mutate(max_odd=pmax(Odd_W,Odd_L)) %>% 
  mutate(Cash=ifelse(select_match==1,10*Odd_W-10,-10))

plot(cumsum(T50_50 %>% filter(!is.na(Cash) & pmax(Odd_W,Odd_L)<=2.3 & 
                                Categorie_tournament=="ATP_500") %>% pull(Cash)),type='l')


T50_50 %>% filter(Odd_W<=3|Odd_L<=3)

T50_50=V_TABLE_MATCH_TEST %>% filter(abs(as.numeric(Rank_W)-as.numeric(Rank_L))<=5 
                                     &info=="Completed" & substr(Categorie_tournament,1,3)=='Cha') %>% 
  select(tournament,Categorie_tournament,Phase,Round,Surface_tournament,info,Date,Winner_id,Loser_id,Odd_W,Odd_L,Rank_W,Rank_L) %>% 
  mutate(select_match=ifelse(Odd_W>Odd_L,1,
                             ifelse(Odd_W==Odd_L & Rank_W>Rank_L,1,0))) %>% 
  #mutate(max_odd=pmax(Odd_W,Odd_L)) %>% 
  mutate(Cash=ifelse(select_match==1,10*Odd_W-10,-10))

table(T50_50$select_match,T50_50$Categorie_tournament)

plot(cumsum(T50_50 %>% filter(!is.na(Cash) 
                              & pmax(Odd_W,Odd_L)<=3 & Categorie_tournament=="Challenger_100.0") 
            %>% pull(Cash)),type='l')

sum(is.na(V_PLAYERS$Size))

sum(is.na(V_PLAYERS$Birth_date))
