##Exercice A- Importer les données
setwd("L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset")
getwd()
df = read.csv(file = "quiz.csv", sep = "\t", dec = ",")

#Combien y'a t il d'étudiants
nrow(df)

#Nb colonnes 
ncol(df)

#Calcule d'un résumé des données afin de vérifier si les variables ont le bon type 
summary(df)

#Analyser les variables qualitatives en classe factor 
df$Identifiant <- as.factor(df$Identifiant)
df$TD <- as.factor(df$TD)
df$TP <- as.factor(df$TP)
class(df$Identifiant)
class(df$TD)
class(df$TP)

##Exercice B: Statistiques descriptives 
#Calcule de la moyenne de chaque quizz 
mean(df$Quiz1, na.rm = TRUE)
mean(df$Quiz2, na.rm = TRUE)
mean(df$Quiz3, na.rm = TRUE)
#meilleur moyenne sur le quiz1

#Calcule de la note max du Quizz 3 
max(df$Quiz3, na.rm = TRUE)

#Calcule la médiane du quiz2 
max(df$Quiz3, na.rm = TRUE)

#Calcule les déciles du Quiz 3 
quantile(df$Quiz3, probs = seq(from = 0.1,
                               to = 0.9, 
                               by =0.1), 
         na.rm = TRUE)
#40% ont eu 11 ou plus.

#Calcule de l'écart-type pour chaque quiz 
sd(df$Quiz1, na.rm = TRUE)
sd(df$Quiz2, na.rm = TRUE)
sd(df$Quiz3, na.rm = TRUE)
#notes plus homogènes sur le quiz1 et le plus hétérogènes sur le quiz2

#Calcule de la répartition en effectif du nb d'étudiants par groupe de TP 
table(df$TP)
#le groupe de TP 22

#Calcule de cette répartiton en %, arrondir à 2 décimales 
round(prop.table(table(df$TP)), digits = 2)

##Exercice C- Extraction 
#Pour chaque question, Afficher le résultat de l'extraction dans un objet appelé resultat avec uniquement la colonne Identifiant 
#Extraire les lignes avec les étudiants ayant une note strictemet supérieure à 10/20 au quiz 3
resultat = subset(df, Quiz3 > 10)
resultat = resultat[ , c("Identifiant","Quiz3")]
nrow(resultat)
View(resultat)

#Extraire les lignes avec les étudiants du groupe TD1 
resultat = subset(df, TD == 1)
resultat = resultat[ , c("Identifiant","TD")]
nrow(resultat)
View(resultat)

#Extraire les lignes avec les étudiants différen du groupe TP 21 
resultat = subset(df, TP != 21)
resultat = resultat[ , c("Identifiant","TP")]
nrow(resultat)
View(resultat)

#Extraction des lignes avec les étudiants ayant une note = ou comprise entre 5 et 10 au Quiz 3
resultat = subset(df, Quiz3 >= 5 & Quiz3 <= 10)
resultat = resultat[ , c("Identifiant","Quiz3")]
nrow(resultat)
View(resultat)

#Extraction d'étudiants selon leur identifiant
resultat = subset(df, Identifiant %in% c(92655100,
                                         85947666,
                                         75752354,
                                         172596215,
                                         111505723,
                                         42690322,
                                         20972010,
                                         43177455))
nrow(resultat)

#Extraction avec tous les étudiants saud ceux mentionnées dans la question précédente
resultat = subset(df, !Identifiant %in% c(92655100,
                                         85947666,
                                         75752354,
                                         172596215,
                                         111505723,
                                         42690322,
                                         20972010,
                                         43177455))
nrow(resultat)

#Extraction avec les étudiants ayant une note supérieure ou = à 15/20 à au moi,s un des 3 quiz 
resultat = subset(df, Quiz1 >= 15 | Quiz2 >= 15 | Quiz3 >= 15)
resultat = resultat[ , c("Identifiant","Quiz1","Quiz2","Quiz3")]
nrow(resultat)
View(resultat)

#Extraction avec les étudiants absents au quiz 1 
resultat = subset(df, is.na(Quiz1))
resultat = resultat[ , c("Identifiant","Quiz1")]
nrow(resultat)
View(resultat)

#Extraction avec les étudiants qui n'ont jamais été absent 
resultat = subset(df, !is.na(Quiz1) & !is.na(Quiz2) & !is.na(Quiz3))
resultat = resultat[ , c("Identifiant","Quiz1","Quiz2","Quiz3")]
nrow(resultat)
View(resultat)

##Exercice D- Top et Flop 
#Cr&tion d'une new colonne "moyenne"
df$Moyenne = round((df$Quiz1 + df$Quiz2 + df$Quiz3) / 3, digits = 1)
head(df)

#Extraction des étudiants ayant les 3 moyennes les plus basses 
rang = order(df$Moyenne, decreasing = FALSE)
resultat = df[ rang, c("Identifiant","Moyenne")]
head(resultat, n = 3)

#Extraction avec les étudiants qui ont les 3 moyennes les plus hautes 
rang = order(df$Moyenne, decreasing = TRUE)
resultat = df[ rang, c("Identifiant","Moyenne")]
head(resultat, n = 3)

#Trier le tableau selon le groupe de TP puis par la moyenne du quiz 
rang = order(df$TP, df$Moyenne, decreasing = c(FALSE,TRUE))
resultat = df[ rang, c("Identifiant","TP", "Moyenne")]
head(resultat, n = 5)

#Agréger les données dans un new tableu avec la moyenne de chaque TP 
aggregate(Moyenne ~ TP,
          data = df, 
          FUN = function(x) c(moy = mean(x) ) )
          
#Avec le minimun maximun et les effectifs pour chaques TP/TD
resultat = aggregate(Moyenne ~ TD + TP,
          data = df, 
          FUN = function(x) c(min = min(x),
                              moy = mean(x),
                              max = max(x),
                              eff = length(x) ) )
                              
##Exercice E- Traitement de données 
#Remplacement des valeurs manquantes de chaque quiz par la moyenne du quiz concerné 
df$Quiz1 = ifelse(test = is.na(df$Quiz1), 
                  yes = mean(df$Quiz1, na.rm = TRUE),
                  no = df$Quiz1)

df$Quiz2 = ifelse(test = is.na(df$Quiz2), 
                  yes = mean(df$Quiz2, na.rm = TRUE),
                  no = df$Quiz2)

df$Quiz3 = ifelse(test = is.na(df$Quiz3), 
                  yes = mean(df$Quiz3, na.rm = TRUE),
                  no = df$Quiz3)

df$Moyenne = round((df$Quiz1 + df$Quiz2 + df$Quiz3) / 3, digits = 1)

#Créer une new variable nommée en regroupant en 5 tranches avec le minim, le max, les quartiles 
df$Moyenne_Classe = cut(df$Moyenne,
                        breaks = quantile(df$Moyenne),
                        include.lowest = TRUE)
summary(df$Moyenne_Classe)

#Exporter un nouveau dataframe dans un fichiet csv 
write.table(x = df, file = "exam_export.csv", sep = ";", dec = ",", row.names = FALSE)                              