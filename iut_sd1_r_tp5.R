setwd("L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset/nba")
getwd()
list.files(path = getwd(),pattern = ".csv$",full.names = TRUE)
#La fontion list.files()= liste les fichiers et/ou répertoires dans un dossier spécifié.

#Les commandes basname() et file_âth_sans_ext()
#TEst de ses commandes sur le premier fichier de la liste 
library(tools)
print(fichiers[1])
nom_fichier = basename(path = fichiers[1])
nom_fichier_sans_extension = file_path_sans_ext(x = nom_fichier)
print(nom_fichier_sans_extension)

#Les commandes assign()
#Test de la fonction 
# Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
assign(x = nom_fichier_sans_extension, 
       value = read.csv(fichiers[1],
                        sep = ",",
                        dec = "."))
#on observe qu'un dataframe vient d'être créé avec comme nom d'objet le nom du fichier sans extension.

#Dans une boucle for maintenant 
# Boucle pour lire chaque fichier CSV
for (fichier in fichiers) {
  # Extraire le nom du fichier sans extension
  nom_objet <- file_path_sans_ext(basename(fichier))
  
  # Lire le fichier CSV et l'affecter à une variable avec le nom du fichier
  start_time <- Sys.time()
  assign(nom_objet, read.csv(fichier, 
                                sep = ",",
                                dec = "."))
  end_time <- Sys.time()
  # Calcul du temps écoulé
  execution_time <- end_time - start_time
  cat("Importation : ",nom_objet, "=" , execution_time , "\n")
}

##Exercice 2: les jointures 
#Combien de match ont été jouer à LA
df_x = subset(team, city == "Los Angeles", select = c("id", "city"))
df_y = subset(game, select = c("game_id", "team_id_home"))
dfJoin = merge(x = df_x, y = df_y, 
               by.x = "id", 
               by.y = "team_id_home", 
               all.x = TRUE)
nrow(dfJoin)
View(dfJoin)

#Qu'elle est l'affluence moyenne des spectatuers
df_x = dfJoin
df_y = subset(game_info, select = c("game_id", "attendance"))
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.x = TRUE)
mean(dfJoin$attendance, na.rm = TRUE)
View(dfJoin)

#Combien d'arbitres diff ont officié en 2020
df_x = subset(game_summary, season == 2020,
              select = c("game_id", "season"))
dfJoin = merge(x = df_x, y = officials, 
               by = "game_id",
               all.x = TRUE)
length(unique(dfJoin$official_id))
View(dfJoin)

#Combien de match a officié dick bavetta par saison 
df_x = subset(game_summary,
              select = c("game_id", "season"))
df_y = subset(officials, first_name == "Dick" & last_name == "Bavetta")
dfJoin = merge(x = df_x, y = df_y, 
               by = "game_id",
               all.y = TRUE)
View(dfJoin)
table(dfJoin$season)

##Exercice 3- GOAT: Connexion à une database SQL
#Installation de package
library(DBI)
library(RSQLite)
mydb <- dbConnect(SQLite(), "nbaDb.sqlite")

#Lister les tables de la database SQL à l'aide la fonction dbListTables()
dbListTables(mydb)

#La fonction dbGetQuery, sélectionner 5 premières lignes de la table team 
dbGetQuery(mydb, 'SELECT * FROM team LIMIT 5')

#Refaire les jointures de l'exo 2 à l'aide de cette fonction
dfJoin = dbGetQuery(mydb, '....')

#Stockage de la table dans une new table de la database SQL 
#La fonction dbWriteTable()
dbWriteTable(mydb, "nom_table", dfJoin)
dbListTables(mydb)

#Fermer le connexion avec SQL 
dbDisconnect(mydb)





