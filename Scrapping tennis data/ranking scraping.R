library(rvest)

# Définir l'URL de la page Web à scraper
url <- "https://live-tennis.eu/fr/classement-atp-officiel"

# Lire le contenu HTML de la page
page <- read_html(url)

# Extraire les données des colonnes "td.rk", "td.pn", "td.sm" et "td"
Rang = html_text(html_nodes(page, "td.rk"))
Nom = html_text(html_nodes(page, "td.pn"))
Age = html_text(html_nodes(page, "td.pn + td"))

Points = html_text(html_nodes(page, "td.sm + td"))
Points=gsub("[[:alpha:]]",NA,Points)
Points
Points=na.omit(Points)

df=data.frame(Rang=Rang,Nom=Nom,Age=Age,Points=Points)

# Afficher le résultat
print(df)






