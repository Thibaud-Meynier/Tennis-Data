library(tidyverse)

TABLE=V_MATCH_t %>% 
  mutate(Favori=case_when(Rank_W<Rank_L~Winner_id,TRUE~Loser_id),
         Outsider=case_when(Rank_L<Rank_W~Winner_id,TRUE~Loser_id),
         Issue=case_when(Favori==Winner_id~"Fav_W",TRUE~"Out_W"),
         Rank_F=case_when(Rank_W<Rank_L~Rank_W,TRUE~Rank_L),
         Rank_O=case_when(Rank_L<Rank_W~Rank_W,TRUE~Rank_L),
         Points_F=case_when(Rank_W<Rank_L~Points_W,TRUE~Points_L),
         Points_O=case_when(Rank_L<Rank_W~Points_W,TRUE~Points_L),
         Odd_F=case_when(Rank_W<Rank_L~Odd_W,TRUE~Odd_L),
         Odd_O=case_when(Rank_L<Rank_W~Odd_W,TRUE~Odd_L)) %>% 
  select(tournament,
         Season,
         Date,
         Week_tournament,
         Categorie,
         Surface_tournament,
         Round,
         Favori,
         Outsider,
         Rank_F,
         Points_F,
         Rank_O,
         Points_O,
         Issue,
         Odd_F,
         Odd_O)
