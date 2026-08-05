library(dplyr)
library(tidyr)
library(plotly)

joueurs = V_MATCH_final %>% 
     select(Name = Winner_id,Size = Size_W,Weight = Weight_W) %>% 
     bind_rows(V_MATCH_final %>% select(Name = Loser_id,Size = Size_L,Weight = Weight_L)) %>% 
  unique()

# ==========================================
# 1. NETTOYAGE ET ENRICHISSEMENT DES MATCHS
# ==========================================
matchs_clean <- V_MATCH_HIST %>%
  filter(info == "Completed") %>%
  mutate(
    Rank_W = as.numeric(Rank_W),
    Rank_L = as.numeric(Rank_L),
    Score_W = as.numeric(Score_W),
    Score_L = as.numeric(Score_L),
    
    # Statut Favori / Outsider selon le classement ATP/WTA
    w_is_fav = ifelse(!is.na(Rank_W) & !is.na(Rank_L) & Rank_W < Rank_L, 1, 0),
    w_is_out = ifelse(!is.na(Rank_W) & !is.na(Rank_L) & Rank_W > Rank_L, 1, 0),
    l_is_fav = w_is_out,
    l_is_out = w_is_fav,
    
    total_sets = Score_W + Score_L,
    is_bo5 = (!is.na(Set4_W) & Set4_W != "") | (!is.na(Set4_L) & Set4_L != ""),
    
    # Set décisif (3e set en BO3, 5e set en BO5)
    is_decisive = (is_bo5 & total_sets == 5) | (!is_bo5 & total_sets == 3),
    
    # Détection si le match est un sweep (2-0 en BO3 ou 3-0 en BO5)
    is_sweep_match = (!is_bo5 & Score_W == 2 & Score_L == 0) | (is_bo5 & Score_W == 3 & Score_L == 0),
    
    # Détection des Tie-Breaks (Set à 7-6 ou 6-7) et identification du gagnant du TB
    tb1_w = ifelse(Set1_W == 7 & Set1_L == 6, 1, 0), tb1_l = ifelse(Set1_W == 6 & Set1_L == 7, 1, 0),
    tb2_w = ifelse(Set2_W == 7 & Set2_L == 6, 1, 0), tb2_l = ifelse(Set2_W == 6 & Set2_L == 7, 1, 0),
    tb3_w = ifelse(Set3_W == 7 & Set3_L == 6, 1, 0), tb3_l = ifelse(Set3_W == 6 & Set3_L == 7, 1, 0),
    tb4_w = ifelse(Set4_W == 7 & Set4_L == 6, 1, 0), tb4_l = ifelse(Set4_W == 6 & Set4_L == 7, 1, 0),
    tb5_w = ifelse(Set5_W == 7 & Set5_L == 6, 1, 0), tb5_l = ifelse(Set5_W == 6 & Set5_L == 7, 1, 0),
    
    tb_won_w = coalesce(tb1_w,0) + coalesce(tb2_w,0) + coalesce(tb3_w,0) + coalesce(tb4_w,0) + coalesce(tb5_w,0),
    tb_won_l = coalesce(tb1_l,0) + coalesce(tb2_l,0) + coalesce(tb3_l,0) + coalesce(tb4_l,0) + coalesce(tb5_l,0),
    
    nb_tb_match = tb_won_w + tb_won_l,
    has_tb = ifelse(nb_tb_match > 0, 1, 0)
  )

# ==========================================
# 2. PARTICIPATIONS INDIVIDUELLES
# ==========================================
participations <- bind_rows(
  # Victoires
  matchs_clean %>%
    select(Name = Winner_id, Surface_tournament, Round, is_decisive, is_sweep_match, 
           has_tb, nb_tb_joues = nb_tb_match, nb_tb_gagnes = tb_won_w,
           is_fav = w_is_fav, is_out = w_is_out, 
           Opponent_Rank = Rank_L) %>% # <<< Transmet le classement du perdant
    mutate(
      victoire = 1,
      is_sweep_win = ifelse(is_sweep_match == TRUE, 1, 0)
    ),
  
  # Défaites
  matchs_clean %>%
    select(Name = Loser_id, Surface_tournament, Round, is_decisive, is_sweep_match, 
           has_tb, nb_tb_joues = nb_tb_match, nb_tb_gagnes = tb_won_l,
           is_fav = l_is_fav, is_out = l_is_out, 
           Opponent_Rank = Rank_W) %>% # <<< Transmet le classement du gagnant
    mutate(
      victoire = 0,
      is_sweep_win = 0
    )
)

# ==========================================
# 3. STATISTIQUES PAR SURFACE
# ==========================================
stats_surfaces <- participations %>%
  filter(!Surface_tournament %in% c("(0 $, Various surfaces, men)","Various")) %>% 
  group_by(Name, Surface_tournament) %>%
  summarise(
    nb_matchs = n(),
    nb_vict   = sum(victoire),
    pct_win   = round((nb_vict / nb_matchs) * 100, 1),
    .groups = "drop"
  ) %>%
  # Pivote les colonnes par surface
  pivot_wider(
    names_from = Surface_tournament,
    values_from = c(nb_matchs, nb_vict, pct_win),
    names_glue = "{.value}_{tolower(Surface_tournament)}",
    values_fill = 0 # Remplace les NA par 0 si le joueur n'a jamais joué sur la surface
  )

# ==========================================
# 4. STATISTIQUES STATUT (FAVORI / OUTSIDER)
# ==========================================
stats_classement <- participations %>%
  mutate(
    # Définition de la tranche de classement de l'ADVERSAIRE
    # (Remplace 'Opponent_Rank' par le vrai nom de ta colonne de classement adverse)
    tranche_opp = case_when(
      Opponent_Rank >= 1  & Opponent_Rank <= 5   ~ "top5",
      Opponent_Rank >= 6  & Opponent_Rank <= 10  ~ "top6_10",
      Opponent_Rank >= 11 & Opponent_Rank <= 30  ~ "top11_30",
      Opponent_Rank >= 31 & Opponent_Rank <= 50  ~ "top31_50",
      Opponent_Rank >= 51 & Opponent_Rank <= 100 ~ "top51_100",
      Opponent_Rank >= 101                       ~ "top101_plus",
      TRUE ~ NA_character_
    )
  ) %>%
  group_by(Name) %>%
  summarise(
    # --- STATS FAVORIS / OUTSIDERS ---
    matchs_fav  = sum(is_fav, na.rm = TRUE),
    vict_fav    = sum(is_fav & victoire == 1, na.rm = TRUE),
    pct_win_fav = ifelse(matchs_fav > 0, round((vict_fav / matchs_fav) * 100, 1), NA),
    
    matchs_out  = sum(is_out, na.rm = TRUE),
    vict_out    = sum(is_out & victoire == 1, na.rm = TRUE),
    pct_win_out = ifelse(matchs_out > 0, round((vict_out / matchs_out) * 100, 1), NA),
    
    # --- COMPTAGE DES MATCHS ET VICTOIRES PAR TRANCHE ---
    # Top 5
    m_top5      = sum(tranche_opp == "top5", na.rm = TRUE),
    v_top5      = sum(tranche_opp == "top5" & victoire == 1, na.rm = TRUE),
    pct_top5    = ifelse(m_top5 > 0, round((v_top5 / m_top5) * 100, 1), 0),
    
    # Top 6-10
    m_top6_10   = sum(tranche_opp == "top6_10", na.rm = TRUE),
    v_top6_10   = sum(tranche_opp == "top6_10" & victoire == 1, na.rm = TRUE),
    pct_top6_10 = ifelse(m_top6_10 > 0, round((v_top6_10 / m_top6_10) * 100, 1), 0),
    
    # Top 11-30
    m_top11_30   = sum(tranche_opp == "top11_30", na.rm = TRUE),
    v_top11_30   = sum(tranche_opp == "top11_30" & victoire == 1, na.rm = TRUE),
    pct_top11_30 = ifelse(m_top11_30 > 0, round((v_top11_30 / m_top11_30) * 100, 1), 0),
    
    # Top 31-50
    m_top31_50   = sum(tranche_opp == "top31_50", na.rm = TRUE),
    v_top31_50   = sum(tranche_opp == "top31_50" & victoire == 1, na.rm = TRUE),
    pct_top31_50 = ifelse(m_top31_50 > 0, round((v_top31_50 / m_top31_50) * 100, 1), 0),
    
    # Top 51-100
    m_top51_100   = sum(tranche_opp == "top51_100", na.rm = TRUE),
    v_top51_100   = sum(tranche_opp == "top51_100" & victoire == 1, na.rm = TRUE),
    pct_top51_100 = ifelse(m_top51_100 > 0, round((v_top51_100 / m_top51_100) * 100, 1), 0),
    
    # Top 101+
    m_top101_plus   = sum(tranche_opp == "top101_plus", na.rm = TRUE),
    v_top101_plus   = sum(tranche_opp == "top101_plus" & victoire == 1, na.rm = TRUE),
    pct_top101_plus = ifelse(m_top101_plus > 0, round((v_top101_plus / m_top101_plus) * 100, 1), 0),
    
    .groups = "drop"
  )

# ==========================================
# 5. STATISTIQUES GLOBALES, CLUTCH & SWEEPS
# ==========================================
stats_globales <- participations %>%
  group_by(Name) %>%
  summarise(
    total_matchs      = n(),
    total_vict        = sum(victoire),
    pct_win_global    = round((total_vict / total_matchs) * 100, 1),
    
    # Tie-Breaks
    matchs_avec_tb    = sum(has_tb, na.rm = TRUE),
    pct_matchs_tb     = round((matchs_avec_tb / total_matchs) * 100, 1),
    tot_tb_joues      = sum(nb_tb_joues, na.rm = TRUE),
    tot_tb_gagnes     = sum(nb_tb_gagnes, na.rm = TRUE),
    pct_win_tb        = ifelse(tot_tb_joues > 0, round((tot_tb_gagnes / tot_tb_joues) * 100, 1), 0),
    
    # Set Décisif
    matchs_decisifs   = sum(is_decisive, na.rm = TRUE),
    vict_decisives    = sum(is_decisive & victoire == 1, na.rm = TRUE),
    pct_win_decisif   = ifelse(matchs_decisifs > 0, round((vict_decisives / matchs_decisifs) * 100, 1), 0),
    
    # Sweeps (Correction : Calculé STRICTEMENT sur le nombre de victoires)
    nb_sweeps         = sum(is_sweep_win, na.rm = TRUE),
    pct_sweeps_wins   = ifelse(total_vict > 0, round((nb_sweeps / total_vict) * 100, 1), 0),
    .groups = "drop"
  )

# Titres (Victoire en Finale)
titres_df <- matchs_clean %>%
  # On garde uniquement les victoires en finale
  filter(Round %in% c("F")) %>%
  mutate(
    # Définition d'un titre Majeur selon le contenu de la colonne 'Categorie'
    # (Adapte les mots-clés selon l'orthographe exacte dans tes données)
    is_majeur = ifelse(
      grepl("Grand Slam|Masters 1000|ATP 1000|Finals|Masters Cup|Olympics", Categorie, ignore.case = TRUE), 
      1, 
      0
    )
  ) %>%
  group_by(Winner_id) %>%
  summarise(
    nb_titres_total   = n(),
    nb_titres_majeurs = sum(is_majeur),
    nb_titres_autres  = nb_titres_total - nb_titres_majeurs,
    .groups = "drop"
  ) %>%
  rename(Name = Winner_id)

# ==========================================
# STATISTIQUES ELO DE LA TABLE DÉDIÉE
# ==========================================

stats_elo <- ELO_RATING_PLAYERS %>%
  # Conversion des colonnes Elo en numérique si nécessaire
  mutate(
    Elo_player         = as.numeric(Elo_player),
    Elo_player_hard    = as.numeric(Elo_player_hard),
    Elo_player_clay    = as.numeric(Elo_player_clay),
    Elo_player_grass   = as.numeric(Elo_player_grass),
    Elo_player_indoors = as.numeric(Elo_player_indoors)
  ) %>%
  group_by(Player_name) %>%
  summarise(
    # Elo Global : Peak, Moyenne, Écart-Type (Stabilité)
    elo_max    = round(max(Elo_player, na.rm = TRUE), 0),
    elo_mean   = round(mean(Elo_player, na.rm = TRUE), 0),
    elo_cv     = round(sd(Elo_player, na.rm = TRUE), 1)/round(mean(Elo_player, na.rm = TRUE), 0),
    
    # Elo Moyen par surface
    elo_hard   = round(mean(Elo_player_hard, na.rm = TRUE), 0),
    elo_clay   = round(mean(Elo_player_clay, na.rm = TRUE), 0),
    elo_grass  = round(mean(Elo_player_grass, na.rm = TRUE), 0),
    elo_indoor = round(mean(Elo_player_indoors, na.rm = TRUE), 0),
    
    .groups = "drop"
  )

# ==========================================
# 6. ASSEMBLED JOUEURS (PROFILING FINAL)
# ==========================================
joueurs_profile <- joueurs %>%
  left_join(stats_globales, by = "Name") %>%
  left_join(stats_classement, by = "Name") %>%
  left_join(stats_surfaces, by = "Name") %>%
  left_join(titres_df, by = "Name") %>%
  mutate(
    nb_titres_total = ifelse(is.na(nb_titres_total), 0, nb_titres_total),
    nb_titres_majeurs =  ifelse(is.na(nb_titres_majeurs), 0, nb_titres_majeurs),
    nb_titres_autres = ifelse(is.na(nb_titres_autres), 0, nb_titres_autres),
    pct_win_fav_txt = ifelse(is.na(pct_win_fav), "N/A", paste0(pct_win_fav, "% (", vict_fav, "/", matchs_fav, ")")),
    pct_win_out_txt = ifelse(is.na(pct_win_out), "N/A", paste0(pct_win_out, "% (", vict_out, "/", matchs_out, ")"))
  )


joueurs_profile <- joueurs_profile %>%
  left_join(stats_elo, by = c("Name" = "Player_name")) %>%
  mutate(
    elo_cv_txt = ifelse(is.na(elo_cv), "N/A", as.character(elo_cv)),
    elo_surfaces_txt = paste0(
      "Hard : ", ifelse(is.na(elo_hard), "N/A", elo_hard),
      " | Clay : ", ifelse(is.na(elo_clay), "N/A", elo_clay),
      " | Grass : ", ifelse(is.na(elo_grass), "N/A", elo_grass),
      " | Indoor : ", ifelse(is.na(elo_indoor), "N/A", elo_indoor)
    )
  )

##### CLASSIFICATION #####

library(dplyr)
library(FactoMineR)
library(factoextra)

# Préparation de la matrice d'analyse
df_clustering <- joueurs_profile %>%
  # On garde le nom en identifiant de ligne
  filter(!is.na(Size) & !is.na(Weight) & !is.na(elo_mean)) %>%
  column_to_rownames("Name") %>%
  mutate(Size=as.numeric(Size), Weight=as.numeric(Weight)) %>% 
  select(
    # Morphologie
    Size,Weight,
    # Elo & Volatilité
    elo_max, elo_mean, elo_cv,
    # Performance globale & Mental
    pct_win_global, pct_win_fav, pct_win_out, 
    pct_win_decisif, pct_win_tb, pct_sweeps_wins,
    nb_titres_majeurs,nb_titres_autres,
    pct_top5,pct_top6_10,pct_top11_30,pct_top31_50,pct_top51_100,pct_top101_plus,
    # Structure match 
    pct_matchs_tb,
    # Surfaces (Elo moyen par surface)
    pct_win_hard,pct_win_clay,pct_win_indoors,pct_win_grass
  ) %>%
  # Imputation simple par la moyenne si quelques NA subsistent sur les surfaces/fav
  mutate(across(everything(), ~ ifelse(is.na(.), mean(., na.rm = TRUE), .)))

# A. Ajout des colonnes de profil de surface
df_clustering <- df_clustering %>%
  mutate(
    IMC = Weight/(Size^2),
    # Écart max entre les taux de victoire sur les surfaces (avec indoors inclus)
    ecart_surf = pmax(pct_win_hard, pct_win_clay, pct_win_grass, pct_win_indoors, na.rm = TRUE) - 
      pmin(pct_win_hard, pct_win_clay, pct_win_grass, pct_win_indoors, na.rm = TRUE),
    
    # Identification de la surface dominante (+8% de différence)
    surf_pref = case_when(
      # 1. Pur Terrien : domine largement la terre battue par rapport à l'indoor, au dur et au gazon
      pct_win_clay >= pct_win_indoors + 6 & 
        pct_win_clay >= pct_win_grass + 6 & 
        pct_win_clay >= pct_win_hard + 6 ~ "Pur Terrien",
      
      # 2. Profil Medium : Meilleur sur Dur que sur Terre, mais meilleur sur Terre que sur Indoor/Fast
      pct_win_hard > pct_win_clay & 
        pct_win_clay > pct_win_indoors ~ "Medium (Dur/Terre)",
      
      # 3. Spécialiste Indoor / Moquette
      pct_win_indoors >= pct_win_clay + 5 & 
        pct_win_indoors >= pct_win_hard + 5 &
        pct_win_indoors >= pct_win_grass + 5 ~ "Spécialiste Indoor",
      
      # 4. Spécialiste Gazon
      pct_win_grass >= pct_win_clay + 5 & 
        pct_win_grass >= pct_win_hard + 5 ~ "Spécialiste Gazon",
      
      # 5. Spécialiste Rapide Général (Dur / Gazon / Indoor dominent la terre)
      pct_win_hard >= pct_win_clay & 
        pct_win_grass >= pct_win_clay & 
        pct_win_indoors >= pct_win_clay ~ "Spécialiste Rapide",
      
      # 6. Par défaut
      TRUE ~ "Polyvalent / Autre"
    ),
    
    # Création de la variable qualitative à 3 modalités
    profil_surface = case_when(
      ecart_surf <= 6                      ~ "Polyvalent",
      surf_pref == "Pur Terrien"          ~ "Pur Terrien",
      surf_pref == "Medium (Dur/Terre)"    ~ "Medium (Dur/Terre)",
      surf_pref == "Spécialiste Indoor"   ~ "Spécialiste Indoor",
      surf_pref == "Spécialiste Gazon"    ~ "Spécialiste Gazon",
      surf_pref == "Spécialiste Rapide"   ~ "Spécialiste Rapide",
      TRUE                                ~ "Polyvalent"
    ),
    
    profil_surface = as.factor(profil_surface)
  )

# ==========================================
# 1. PRÉPARATION DE L'ACP & CLUSTER
# ==========================================
vars_num <- c(
  "Size","Weight","IMC","elo_max", "elo_mean", "elo_cv",
  "pct_win_global", "pct_win_fav", "pct_win_out", 
  "nb_titres_majeurs","nb_titres_autres",
  "pct_top5","pct_top6_10","pct_top11_30","pct_top31_50","pct_top51_100","pct_top101_plus",
  "pct_win_decisif", "pct_win_tb", "pct_sweeps_wins",
  "pct_win_hard", "pct_win_clay", "pct_win_grass", "pct_win_indoors"
)

# Sous-ensemble nettoyé pour l'ACP
df_pca_input <- df_clustering %>%
  select(all_of(vars_num), profil_surface)

# Exécution de l'ACP avec la variable quali en illustrative
res_pca <- PCA(
  df_pca_input, 
  quali.sup = "profil_surface", 
  scale.unit = TRUE, 
  graph = TRUE
)


# Affiche les individus colorés avec le centre de chaque groupe bien visible
fviz_pca_ind(
  res_pca,
  axes = c(1, 2),
  select.ind = list(cos2 = 70),
  geom = c("point"),             # Ne dessine que les points des joueurs
  habillage = "profil_surface",  # Colore selon le profil de surface
  addEllipses = TRUE,            # Ajoute les ellipses de confiance
  ellipse.type = "confidence",
  palette = "Set1"
)

res_hcpc <- HCPC(res_pca, nb.clust = 9, graph = TRUE)

# ==========================================
# 2. CRÉATION DU GRAPHIQUE GGPLOT AVEC ELLIPSES
# ==========================================
# fviz_cluster génère un objet ggplot
p_cluster <- fviz_cluster(
  res_hcpc,
  geom = "point",              # FORCE l'affichage des POINTS uniquement (masque le texte sur le graphe)
  ellipse.type = "confidence", # Ellipses de confiance à 95% autour des centroïdes
  ellipse.level = 0.95,
  show.clust.cent = TRUE,      # Affiche la croix du centre du cluster
  palette = "Set2",
  ggtheme = theme_minimal(),
  main = "Groupes de joueurs (Plan Factoriel Dim 1 & 2)"
) +
  # Astuce : On injecte la variable 'label' dans l'esthétique 'text'
  # pour que ggplotly() puisse l'afficher au survol
  aes(text = rownames(df_clustering))

res_hcpc$desc.var$quanti

# ==========================================
# 3. CONVERSION EN PLOTLY INTERACTIF
# ==========================================

df_plot <- as.data.frame(res_pca$ind$coord[, 1:3]) # Prend Dim 1, 2 et 3

colnames(df_plot) <- c("Dim1", "Dim2", "Dim3")

df_plot <- df_plot %>%
  mutate(
    Name = rownames(.),
    Cluster = as.factor(res_hcpc$data.clust$clust)
  )

# 2. Construction directe du ggplot (Axes 1 et 2 par exemple)
p_clean <- ggplot(df_plot, aes(x = Dim1, y = Dim3, color = Cluster)) +
  geom_point(aes(text = Name), size = 2) +
  stat_ellipse(type = "confidence", level = 0.95, geom = "polygon", alpha = 0.1, aes(fill = Cluster)) +
  scale_color_brewer(palette = "Set1") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  labs(
    title = "Groupes de joueurs (ACP + Ellipses de confiance)",
    x = paste0("Dim 1 (", round(res_pca$eig[1, 2], 1), "%)"),
    y = paste0("Dim 2 (", round(res_pca$eig[2, 2], 1), "%)")
  )

# 3. Conversion Plotly : l'association Nom <-> Point est 100% exacte
ggplotly(p_clean, tooltip = "text")


df_clustering$clust = res_hcpc$data.clust$clust
