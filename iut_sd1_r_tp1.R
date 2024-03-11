#Exercice 1- utilisation d'un dataframe existant 
#Affcicher le jeu de données puis donner sa classe 
iris 
class(iris)

#Dans une vue avec la fonction view()
view(iris)

#Affichier le nb de lignes 
nrow(iris)

#le nb de colonnes 
ncol(iris)

##Afficher le nb de colonnes avec la fonction colnames()
colnames(iris)

#Afficher un résumé du dataframe
summary(iris)

#Afficher uniquement les colonnes 
iris[ , c("Sepal.Length","Species")]

#Afficher uniquement la ligne 100,103 et 105
iris[ c(100,103,105) , ]

#Affcicher uniquement les lignes 50 à 100
iris[ 50:100 , ]

#Calcul de la moyenne d'une variable
mean(iris$Sepal.Length)

#Calcul de la médiane 
median(iris$Sepal.Length)

#Calcul de l'écart-type 
sd(iris$Petal.Length)

#CAlcul des déciles 
quantile(iris$Petal.Width, probs = seq(from = 0.1, to = 0.9, by =0.1))

#Exercice 2- import/export un dataframe 
#la fonction read;csv()

#Affichage des jeux de données dans des vues 
View(dfManga)
#puis
View(dfAnime)

#Afficher la dimension donc avec la fontion dim()
dim(dfManga)
dim(dfAnime)

#Calcul de la moyenne de la variable pour les deux dataframes
mean(dfManga$Score)
mean(dfAnime$Score)

#calcul du nombre total de vote 
sum(dfManga$Vote)
sum(dfAnime$Vote)

#Calcul de l'écart-type 
sd(dfManga$Score)
sd(dfAnime$Score)

#Calcul des déciles 
quantile(dfManga$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))
quantile(dfAnime$Score, probs = seq(from = 0.1, to = 0.9, by = 0.1))

#Les fonctions subtest(), table() et prop.table()
#Combien de manga ont une note >9/10
extraction1 <- subset(dfManga, Score > 9)
nrow(extraction1) #retourne le nb de lignes qui donnera donc combien y'en a 

#Manga ayant 200000 vote ou +
extraction2 <- subset(dfManga, Vote >= 200000)
nrow(extraction2)

#Manga >200000 votes et >8/10
extraction3 <- subset(dfManga, Vote >= 200000 & Score >= 8)
nrow(extraction3)

#Note comprise entre 7/10 et 8/10 
extraction4 <- subset(dfManga, Score >= 7 & Score <= 8)
nrow(extraction4)

#calcul des effectifs.Puis Calcul des effectifs en pourcentage
effectifRating <- table(dfAnime$Rating)
print(effectifRating)
length(effectifRating)
prop.table(effectifRating)

#Anime correspondant par le Rating: R-17+
extraction2 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)")
nrow(extraction2)

#Anime correspondant par le Rating: R-17+ et note >8/10
extraction3 <- subset(dfAnime, Rating == "R - 17+ (violence & profanity)" &
                                 Score >= 8)
nrow(extraction3)

#Anime ne correspondant pas au le Rating: R-17+
extraction4 <- subset(dfAnime, Rating != "R - 17+ (violence & profanity)")
nrow(extraction4)

#Animés correspondant au Rating: PG-Children et G-All Ages
extraction5 <- subset(dfAnime, Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction5)  
#le %in% vérifie si une valeur est présente ou non 

#Animés ne correspondant pas au Rating: PG-Children et G-All Ages
extraction6 <- subset(dfAnime, !Rating %in% c("PG - Children","G - All Ages"))
nrow(extraction6)

#Anime note >9/10 et +400000 votes
extraction7 <- subset(dfAnime, Score >= 9 | Vote > 400000)
nrow(extraction7)
# "|" opérateur logique 

#Les fonctions rbind() et write.table()
#Modification des dataframe en gardant certaines variables
dfAnime <- dfAnime[ , c("Title","Score","Vote","Ranked")]
dfManga <- dfManga[ , c("Title","Score","Vote","Ranked")]

#Création de colonne avec deux choix valeurs selon l'objet
dfAnime$Type <- "Anime"
dfManga$Type <- "Manga" 

#Compilee les deux dataframes avec la fonction rbind(). Vérifier le résultat dans une vue 
dfConcat <- rbind(dfManga,dfAnime)
View(dfConcat)

#Exportation de la dataframe dans un fichier CSV avec la fonction write.table 
write.table(x = dfConcat, file = ".../.../.../ExportTp1.csv",
            sep = ";",row.names = FALSE)
            



