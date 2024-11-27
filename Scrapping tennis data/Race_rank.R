# Calcul classement race atp 

# Table des matchs
matches <- data.frame(
  tournament_id = c(1, 1, 1, 1),
  round = c("1R", "QF", "SF", "F"),
  winner_id = c(101, 101, 101, 101),
  loser_id = c(102, 103, 104, 105)
)

# Transformation des données avec pivot_longer
matches_long <- matches %>%
  pivot_longer(cols = c(winner_id, loser_id),
               names_to = "result_type",
               values_to = "player_id") %>%
  mutate(result = ifelse(result_type == "winner_id", "Win", "Loss")) %>%
  select(-result_type)

# Table de correspondance des points ATP
points_atp <- data.frame(
  round = c("1R", "QF", "SF", "F", "W"),
  points = c(10, 100, 180, 300, 500)
)

# Ajouter une colonne indiquant si le joueur a gagné le tournoi
matches_long <- matches_long %>%
  mutate(round = ifelse(result == "Win" & round == "F", "W", round))

# Joindre les points ATP
matches_long <- matches_long %>%
  left_join(points_atp, by = "round")


# Data to build 

# Categorie Points Race

# GS

# Qualif

#Q1 = 0
#Q2 = 8
#Q3 = 16
#Q-F=25

# Main Draw

#R1 = 10
#R2 = 45
#R3 = 90
#R4 = 180
#QF = 360
#SF = 720
#Final = 1200
#Winner = 2000

# M1000 (IW, Miami)

# Qualif

#Q1 = 0
#Q2 = 8
#Q-F=16

# Main Draw

#R1 = 10
#R2 = 25
#R3 = 45
#R4 = 90
#QF = 180
#SF = 360
#Final = 600
#Winner = 1000

# MC, Bercy, Cinci

# Qualif

#Q1 = 0
#Q2 = 8
#Q-F=25

# Main Draw

#R2 = 10
#R3 = 45
#R4 = 90
#QF = 180
#SF = 360
#Final = 600
#Winner = 1000

# ATP 500 (48 players)

# Qualif

#Q1 = 0
#Q2 = 4
#Q-F=10

# Main Draw

#R3 = 20
#R4 = 45
#QF = 90
#SF = 180
#Final = 300
#Winner = 500

# ATP 500 (32 players)

# Qualif

#Q1 = 0
#Q2 = 10
#Q-F=20

# Main Draw

#R4 = 45
#QF = 90
#SF = 180
#Final = 300
#Winner = 500

# ATP 250 (48 players) Winston Salem

# Qualif

#Q1 = 0
#Q2 = 3
#Q-F=5

# Main Draw

#R3 = 10
#R4 = 20
#QF = 45
#SF = 90
#Final = 150
#Winner = 250


# ATP 250 (32 players)

# Qualif

#Q1 = 0
#Q2 = 6
#Q-F=12

# Main Draw

#R4 = 20
#QF = 45
#SF = 90
#Final = 150
#Winner = 250

# ATP 125

#R3 = 5
#R4 = 10
#QF = 25
#SF = 45
#Final = 75
#Winner = 125

# ATP 110

#R3 = 5
#R4 = 9
#QF = 20
#SF = 40
#Final = 65
#Winner = 110

# ATP 100

#R3 = 5
#R4 = 8
#QF = 18
#SF = 35
#Final = 60
#Winner = 100

# ATP 90

#R3 = 5
#R4 = 8
#QF = 17
#SF = 33
#Final = 55
#Winner = 90

# ATP 80

#R3 = 3
#R4 = 7
#QF = 15
#SF = 29
#Final = 48
#Winner = 80

##### Table technique calcul classement Race #####

# NB Les points entre qualifs et tournoi principale se cumule

QualifGS=data.frame("Phase"="Qualification",
                    "Round"=c("Q-1R","Q-2R","Q-3R","Q-QW"),
                    "Points"=c(0,8,16,25))

MainDrawGS=data.frame("Phase"="Main Draw",
                      "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                      "Points"=c(10,45,90,180,360,720,1200,2000))

# ATP cup

MainDrawATP_CUP_1_10=data.frame("Phase"="Main Draw",
                                "Rank_Range"="1-10",
                      "Round"=c("RR","QF","SF","F","W"),
                      "Points"=c(75,120,180,0,250))

MainDrawATP_CUP_11_25=data.frame("Phase"="Main Draw",
                                "Rank_Range"="11-25",
                                "Round"=c("RR","QF","SF","F","W"),
                                "Points"=c(65,100,140,0,200))

MainDrawATP_CUP_26_50=data.frame("Phase"="Main Draw",
                                "Rank_Range"="26-50",
                                "Round"=c("RR","QF","SF","F","W"),
                                "Points"=c(50,75,105,0,150))

MainDrawATP_CUP_51_100=data.frame("Phase"="Main Draw",
                                "Rank_Range"="51-100",
                                "Round"=c("RR","QF","SF","F","W"),
                                "Points"=c(25,35,50,0,75))

MainDrawATP_CUP_101=data.frame("Phase"="Main Draw",
                                "Rank_Range"="101+",
                                "Round"=c("RR","QF","SF","F","W"),
                                "Points"=c(20,25,35,0,50))


# United Cup 

MainDrawUnited_CUP_1_10=data.frame("Phase"="Main Draw",
                                "Rank_Range"="1-10",
                                "Round"=c("RR","QF","SF","F","W"),
                                "Points"=c(55,80,130,0,180))

MainDrawUnited_11_20=data.frame("Phase"="Main Draw",
                                 "Rank_Range"="11-20",
                                 "Round"=c("RR","QF","SF","F","W"),
                                 "Points"=c(45,65,105,0,140))

MainDrawUnited_21_30=data.frame("Phase"="Main Draw",
                                 "Rank_Range"="21_30",
                                 "Round"=c("RR","QF","SF","F","W"),
                                 "Points"=c(40,55,90,0,120))

MainDrawUnited_31_50=data.frame("Phase"="Main Draw",
                                  "Rank_Range"="31-50",
                                  "Round"=c("RR","QF","SF","F","W"),
                                  "Points"=c(35,40,60,0,90))

MainDrawUnited_51_100=data.frame("Phase"="Main Draw",
                               "Rank_Range"="101+",
                               "Round"=c("RR","QF","SF","F","W"),
                               "Points"=c(25,35,40,0,60))

MainDrawUnited_101_250=data.frame("Phase"="Main Draw",
                                 "Rank_Range"="101_250",
                                 "Round"=c("RR","QF","SF","F","W"),
                                 "Points"=c(20,25,35,0,40))

MainDrawUnited_251=data.frame("Phase"="Main Draw",
                                 "Rank_Range"="251+",
                                 "Round"=c("RR","QF","SF","F","W"),
                                 "Points"=c(15,20,25,0,35))
# Masters Cup/Challenger tour finals

MainDrawMC=data.frame("Phase"="Main Draw",
                      "Round"=c("RR","RRW","SF","SFW","F","W"),
                      "Points"=c(0,200,0,400,0,500))

MainDrawMC=data.frame("Phase"="Main Draw",
                      "Round"=c("RR","RRW","SF","SFW","F","W"),
                      "Points"=c(0,15,0,30,0,80))

# M1000

QualifM1000_96=data.frame("Phase"="Qualification",
                    "Round"=c("Q-R16","Q-QF","Q-QW"),
                    "Points"=c(0,8,16))

MainDrawM1000_96=data.frame("Phase"="Main Draw",
                      "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                      "Points"=c(10,25,45,90,180,360,600,1000))



QualifM1000=data.frame("Phase"="Qualification",
                          "Round"=c("Q-R16","Q-QF","Q-QW"),
                          "Points"=c(0,8,25))

MainDrawM1000=data.frame("Phase"="Main Draw",
                            "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                            "Points"=c(10,45,90,180,360,600,1000))

# ATP 500

Qualif500_48=data.frame("Phase"="Qualification",
                          "Round"=c("Q-R16","Q-QF","Q-QW"),
                          "Points"=c(0,4,10))

MainDraw500_48=data.frame("Phase"="Main Draw",
                            "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                            "Points"=c(20,45,90,180,300,500))


Qualif500_32=data.frame("Phase"="Qualification",
                         "Round"=c("Q-R16","Q-QF","Q-QW"),
                         "Points"=c(0,10,20))

MainDraw500_32=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(45,90,180,300,500))


# ATP 250

Qualif250_48=data.frame("Phase"="Qualification",
                         "Round"=c("Q-R16","Q-QF","Q-QW"),
                         "Points"=c(0,3,5))

MainDraw250_48=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(10,20,45,90,150,250))


Qualif250=data.frame("Phase"="Qualification",
                         "Round"=c("Q-R16","Q-QF","Q-QW"),
                         "Points"=c(0,6,12))

MainDraw250=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(20,45,90,150,250))


# Dans le cas des qualifications ou victoire finale, il y a une catégorie en plus par rapport aux nombre de tour
# par exemple W, donc si round==F et statut==Winner round=W else round
# pour els qualif même chose, si round=Q-QF et statut==winner round=Q-QW

# ATP 175

Qualif175=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                        "Points"=c(0,3,6))

# ATP 125

Qualif125=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,2,5))

# ATP 100

Qualif110=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,2,5))

Qualif100=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,2,5))

Qualif90=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,2,5))

# ATP 80

Qualif75=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,2,4))

Qualif80=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                    "Points"=c(0,2,4))

# ATP 50

Qualif50=data.frame("Round"=c("Q-R16","Q-QF","Q-QW"),
                     "Points"=c(0,1,3))



##### Calcul classement race ####

V_RACE_RANK=V_TABLE_MATCH_TEST %>% 
  select(tournament,Categorie,Week,Season,Round,Phase,Winner_id,Loser_id)

table(V_RACE_RANK$Round)
# Gerer les Q-SF en Q-QF

V_RACE_RANK=V_RACE_RANK %>% 
  pivot_longer(
    cols = c(Winner_id, Loser_id),       # Colonnes à transformer
    names_to = "Issue",                  # Nouvelle colonne pour l'origine
    values_to = "Player_ID"              # Nouvelle colonne pour les ID des joueurs
  ) %>%
  mutate(Issue = ifelse(Issue == "Winner_id", "W", "L")) %>% 
  mutate(Round = ifelse(Round =="Q-SF", "Q-QF",Round))

V_RACE_RANK=V_RACE_RANK %>% 
  mutate(Round=case_when(Phase=="Main Draw" & Round=="F" & Issue=="W"~"Winner",
                         Phase=="Qualification" & Categorie!="Grand Slam" & Round=="Q-QF" & Issue=="W"~"Q-QW",
                         Phase=="Qualification" & Categorie=="Grand Slam" & Round=="Q-3R" & Issue=="W"~"Q-QW",
                         Categorie=="Masters Cup" & Round=="-" & Issue=="W"~"RRW",
                         Categorie=="Masters Cup" & Round=="-" & Issue=="L"~"RR",
         TRUE~Round))
#V_RACE_RANK=V_RACE_RANK %>% filter(is.na(Categorie_tournament))

V_TABLE_MATCH_TEST %>% filter(tournament=="United Cup")

table(V_RACE_RANK$tournament)

V_RACE_RANK=V_RACE_RANK %>% 
  left_join(V_TOURNAMENT_F %>% select(tournament,Categorie,Year,Round,Ranking_points),by=c("tournament","Categorie_tournament"="Categorie","Season"="Year","Round"))

# Pb Tournoi de bergame 2020