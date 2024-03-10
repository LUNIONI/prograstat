#Exercice 1-  Importer des données 
#Importer un fichier excel sur R 
install.packages("readxl")
library(readxl)
setwd("L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique") #CTRL+SHIFT+H permet d'écrire le chemin où on peut accéder au fichier 
pokemon=read_excel(path="pokemon.xlsx",sheet="pokemon")

#Nombre de ligne  et de colonne dans le dataset 
dim(pokemon) #dimension ligne+colonne
ncol(pokemon)#colonne
nrow(pokemon)#ligne 

#affichage d'un résumé des données 
summary(pokemon)

#modification des variables en type "factor"
pokemon$is_legendary <-as.factor(pokemon$is_legendary)
pokemon$generation <-as.factor(pokemon$generation)
pokemon$type <-as.factor(pokemon$type)

#affichage d"un nouveau résumé suite au changement 
summary(pokemon)

#Exercice 2-Création de colonne 
#la fonction ifelse()
med = median(pokemon$attack)
pokemon$attack_group = ifelse(pokemon$attack >= med, "attack+","attack-")
pokemon$attack_group <-as.factor(pokemon$attack_group)
summary(pokemon$attack_group)

pokemon$water_fire = ifelse(pokemon$type %in% c("water","fire"), "yes","no")
pokemon$water_fire <-as.factor(pokemon$water_fire)
summary(pokemon$water_fire)

q3_attack = quantile(pokemon$attack, probs = 0.75)
q3_defense = quantile(pokemon$defense, probs = 0.75)
q3_speed = quantile(pokemon$speed, probs = 0.75)
pokemon$best = ifelse(pokemon$attack > q3_attack &
                        pokemon$defense > q3_defense &
                        pokemon$speed > q3_speed , "yes","no")
pokemon$best <-as.factor(pokemon$best)
summary(pokemon$best)

#la fonction is.na()
#Filterer les données avec les pokémons ayant des valeurs manquantes sur la colonne weight_kg
requete = subset(pokemon, is.na(weight_kg))
View(requete)

#Filterer les données avec les pokémons n'ayant des valeurs manquantes sur la colonne weight_kg
requete = subset(pokemon, !is.na(weight_kg))
View(requete)

#Création des new varaibles avec les mêmes valeurs déjà renseignées mais en remplaçant les valeurs manquantes NA par leur valeur médianne 
med_weight_kg = median(pokemon$weight_kg, na.rm = TRUE)
pokemon$weight_kgNa = ifelse(is.na(pokemon$weight_kg) , 
                                 med_weight_kg ,
                                 pokemon$weight_kg)

med_height_m = median(pokemon$height_m, na.rm = TRUE)
pokemon$height_mNA = ifelse(is.na(pokemon$height_m) , 
                                 med_height_m ,
                                 pokemon$height_m)

#La fonction cut()
#Création d'une new varaibles en regroupant en 3 tranches avec les labels léger/moyen/lourd 
pokemon$weight_group = cut(pokemon$weight_kg,
                           breaks = 3,
                           labels = c("léger","moyen","lourd"))

#Création d'une new varaibles en regroupant en 3 tranches telles que ]0,1]/]1,2]/]2,3]/]3,max]
pokemon$height_m_group = cut(pokemon$height_m,
                             breaks = c(0,1,2,3,
                                        max(pokemon$height_m,
                                            na.rm = TRUE)))

#Création d'une new varaibles en regroupant en 5 tranches avec les min, max et quartiles telle que [min,Q1]/(Q1,Q2]/ (Q2,Q3]/(Q3,max]
pokemon$defense_group = cut(pokemon$defense,
                             breaks = quantile(pokemon$defense,
                                               na.rm = TRUE),
                            include.lowest = TRUE)
summary(pokemon$defense_group)

#Exercice 3-agregation
#la fonction aggregate()
#Calculer la moyenne d'attack par type 
aggregate(x = attack ~ type, 
          data = pokemon,
          FUN = function(x) mean(x))
 
 #Calculer la moyenne d'attack par génération et par type 
 aggregate(x = attack ~ generation + type,
          data = pokemon, 
          FUN = function(x) median(x))
 
 
#Calculer l'effectif par type 
aggregate(x = pokedex_number ~ type,
          data = pokemon,
          FUN = function(x) length(x))
          
#Calcul de la moyenne et la médiane de la statistique speed pour chaque génération et type. Affciher aussi les effectifs de chaque paire. 
aggregate(speed ~ generation + type,
          data = pokemon, 
          FUN = function(x) c(moy = mean(x),
                              med = median(x),
                              eff = length(x) ) )





