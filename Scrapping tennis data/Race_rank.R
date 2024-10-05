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

QualifGS=data.frame("Phase"="Qualification",
                    "Round"=c("Q-Q1","Q-Q2","Q-Q3","Q-QF"),
                    "Points"=c(0,8,16,25))

MainDrawGS=data.frame("Phase"="Main Draw",
                      "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                      "Points"=c(10,45,90,180,360,720,1200,2000))

# M1000

QualifM1000_96=data.frame("Phase"="Qualification",
                    "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                    "Points"=c(0,8,16))

MainDrawM1000_96=data.frame("Phase"="Main Draw",
                      "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                      "Points"=c(10,25,45,90,180,360,600,1000))



QualifM1000=data.frame("Phase"="Qualification",
                          "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                          "Points"=c(0,8,25))

MainDrawM1000=data.frame("Phase"="Main Draw",
                            "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                            "Points"=c(10,45,90,180,360,600,1000))

# ATP 500

Qualif500_48=data.frame("Phase"="Qualification",
                          "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                          "Points"=c(0,4,10))

MainDraw500_48=data.frame("Phase"="Main Draw",
                            "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                            "Points"=c(20,45,90,180,300,500))


Qualif500_32=data.frame("Phase"="Qualification",
                         "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                         "Points"=c(0,10,20))

MainDraw500_32=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(45,90,180,300,500))


# ATP 250

Qualif250_48=data.frame("Phase"="Qualification",
                         "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                         "Points"=c(0,3,5))

MainDraw250_48=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(10,20,45,90,150,250))


Qualif250=data.frame("Phase"="Qualification",
                         "Round"=c("Q-Q1","Q-Q2","Q-QF"),
                         "Points"=c(0,6,12))

MainDraw250=data.frame("Phase"="Main Draw",
                           "Round"=c("R1","R2","R3","R4","QF","SF","F","W"),
                           "Points"=c(20,45,90,150,250))


