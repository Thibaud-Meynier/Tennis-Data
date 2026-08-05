
tournament=V_MATCH_t %>% 
  filter(Phase=="Main Draw") %>% 
  ungroup() %>% 
  arrange(Date,tournament,desc(N_match),Season)

tournament=tournament %>% 
  select(Categorie,Season,Week_tournament,tournament,Round,Date,N_match,Surface_tournament,info,
         Winner_id,Loser_id)

tournament$Elo_W=NA

tournament$Elo_L=NA

tournament$Elo_W_NEW=NA

tournament$Elo_L_NEW=NA

tournament=setDT(tournament)


# --- Fonctions utilitaires ---------------------------------------------

# Pénalité d'inactivité : continue, monte linéairement de 0 à 1 entre 60 et 240 jours
penalty <- function(diff_date) {
  ifelse(is.na(diff_date), 0,
         ifelse(diff_date <= 60, 0, pmin(1, (diff_date - 60) / 180))
  )
}

# Facteur lié au niveau de rating (limite l'emballement en haut/bas d'échelle)
rating_factor <- function(elo) {
  # Borne inf = 1.0, Amplitude = 1.0 (Plage de 1.0 à 2.0)
  1+ 2.5/(1+(2^((elo-1500)/100)))
}

# Facteur d'expérience : seuil "doux" à 40-50 matchs, décroissance exponentielle

experience_factor <- function(n_match, decay_rate = 100) {
  1 + 1.5 * exp(-n_match / decay_rate)
}

level_factor <- function(categorie) {
  case_when(
    categorie == "Grand Slam" ~ 1.0,
    categorie %in% c("Olympics", "Masters") ~ 0.95,
    categorie == "ATP 1000" ~ 0.9,
    categorie %in% c("ATP 500", "Team") ~ 0.8,
    categorie == "ATP 250" ~ 0.7,
    categorie == "Challenger 175" ~ 0.6,
    categorie == "Challenger 125" ~ 0.5,
    TRUE ~ 0.4  # Valeur par défaut pour tout autre cas
  )
}

round_factor <- function(round) {
  case_when(
    round == "F"  ~ 1.0,
    round == "SF" ~ 0.9,
    round == "QF" ~ 0.8,
    TRUE          ~ 0.7  # Valeur par défaut (ex: 1er/2e/3e tour, etc.)
  )
}

# --- État par joueur (hashmap O(1) au lieu de filter() O(n) répété) ----

player_state <- new.env(hash = TRUE)

get_player_state <- function(pid) {
  key <- as.character(pid)
  if (!exists(key, envir = player_state, inherits = FALSE)) {
    return(list(elo = 1500, last_date = as.Date(NA), n_match = 0))
  }
  get(key, envir = player_state, inherits = FALSE)
}

set_player_state <- function(pid, elo, date, n_match) {
  key <- as.character(pid)
  assign(key, list(elo = elo, last_date = date, n_match = n_match), envir = player_state)
}

# --- Boucle principale ---------------------------------------------------

# IMPORTANT : garantir l'ordre chronologique strict (et l'ordre exact
# des matchs joués le même jour) pour préserver l'anti-fuite temporelle

tournament <- tournament %>% arrange(Date, tournament, N_match)

n <- nrow(tournament)

Elo_W <- Elo_L <- Elo_W_NEW <- Elo_L_NEW <- numeric(n)

K_BASE <- 32

pb= progress_bar$new(
  format = "[:bar] :current/:total (:percent) ETA: :eta",
  total = n,
  clear = FALSE,
  width = 60,
  force = TRUE
)

for (i in seq_len(n)) {
  row <- tournament[i, ]
  p1 <- row$Winner_id
  p2 <- row$Loser_id
  date_match <- row$Date
  
  st1 <- get_player_state(p1)
  st2 <- get_player_state(p2)
  
  dday_p1 <- if (st1$n_match == 0) NA_real_ else as.numeric(date_match - st1$last_date)
  dday_p2 <- if (st2$n_match == 0) NA_real_ else as.numeric(date_match - st2$last_date)
  
  covid <- ifelse(between(date_match, as.Date("2020-08-01"), as.Date("2021-02-01")), 0, 1)
  
  pen_p1 <- penalty(dday_p1) * covid
  pen_p2 <- penalty(dday_p2) * covid
  
  elo_p1 <- st1$elo - pen_p1 * 100
  elo_p2 <- st2$elo - pen_p2 * 100
  
  proba_p1 <- 1 / (1 + 10^((elo_p2 - elo_p1) / 400))
  proba_p2 <- 1 - proba_p1
  
  lvl <- level_factor(row$Categorie)
  rnd <- round_factor(row$Round)
  wo  <- ifelse(row$info == "Completed", 1, 0)
  
  k_p1 <- K_BASE * lvl * rnd * wo * rating_factor(elo_p1) * experience_factor(st1$n_match)
  k_p2 <- K_BASE * lvl * rnd * wo * rating_factor(elo_p2) * experience_factor(st2$n_match)
  
  elo_p1_new <- elo_p1 + k_p1 * (1 - proba_p1)
  elo_p2_new <- elo_p2 + k_p2 * (0 - proba_p2)
  
  Elo_W[i]     <- round(elo_p1, 1)
  Elo_L[i]     <- round(elo_p2, 1)
  Elo_W_NEW[i] <- round(elo_p1_new, 1)
  Elo_L_NEW[i] <- round(elo_p2_new, 1)
  
  set_player_state(p1, elo_p1_new, date_match, st1$n_match + 1)
  set_player_state(p2, elo_p2_new, date_match, st2$n_match + 1)
  
  pb$tick()
}

tournament$Elo_W     <- Elo_W
tournament$Elo_L     <- Elo_L
tournament$Elo_W_NEW <- Elo_W_NEW
tournament$Elo_L_NEW <- Elo_L_NEW


tournament=tournament %>% 
  select(Categorie,Season,Week_tournament,tournament,Round,Date,N_match,Surface_tournament,info,
         Winner_id,Loser_id,Elo_W_NEW,Elo_L_NEW)

ELO_RATING=tournament



elo_players=function(player_name,data){
  
  ELO_PLAYER=data %>% 
    filter(Winner_id==player_name|Loser_id==player_name) %>% 
    mutate(Player_name=player_name) %>% 
    mutate(Elo_player=case_when(Winner_id==player_name~Elo_W_NEW,
                                TRUE~Elo_L_NEW)) %>% 
    
    mutate(Round = factor(Round, levels=c("-", "1R", "2R", "3R", "R16", "QF", "SF", "F"),ordered = TRUE)) %>% 
    ungroup() %>% 
    select(Player_name,Season,tournament,Date,Week_tournament,Round,
           Elo_player) %>% 
    group_by(Player_name,Season,tournament) %>% 
    arrange(desc(Date), desc(Round)) %>%
    mutate(ORDRE_DESC_ELO = row_number()) %>% 
    filter(ORDRE_DESC_ELO==1) %>% 
    select(-ORDRE_DESC_ELO) %>% 
    arrange(Date,Season,Week_tournament) %>% 
    ungroup() %>% 
    mutate(Date=as.Date(ifelse(Round=="F" & weekdays(Date)=="Monday",(Date-1),
                               adjust_to_last_sunday(Date)))) %>% 
    mutate(Week_tournament=isoweek(Date)) %>% 
    mutate(Week2=(Week_tournament+1))
  
  return(ELO_PLAYER)
}


joueurs <- c("Sinner Jannik", 
             "Alcaraz Carlos", 
             "Djokovic Novak",
             "Zverev Alexander",
             "Jodar Rafael",
             "Fils Arthur",
             "Fritz Taylor")

df_elo <- purrr::map_dfr(joueurs, function(p) {
  elo_players(p, ELO_RATING) %>% mutate(Player = p)
})

ggplotly(ggplot(df_elo, aes(x = Date, y = Elo_player, color = Player)) +
           geom_line(linewidth = 0.5) +
           theme_classic())
