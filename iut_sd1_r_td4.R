#Exercice 1- Importer les données
setwd("L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset")
getwd()
velov= read.csv(file = "velov.csv",header = TRUE, sep=";",dec = ",")
#Ouverture du fichier CSV sur R

#Effectuation d'un résumé de données 
summary(velov)
#Vérification du type des variables statuts et codepostal 
class(velov$status) #"character" 
class(velov$CodePostal)#"integer"
#on voit donc qu'elles sont mal typées

#Les faire passer en type "factor"
velov$status=as.factor(velov$status)
velov$CodePostal=as.factor(velov$CodePostal)

#Création d'une colonne avce une condition 
velov$bornes = ifelse(velov$capacity != (velov$bikes + velov$stands), "KO" , "OK")
table(velov$bornes)
#en réalité, c'est aussi peut-être car la station est fermée OU que des usagers ont déposé leur vélo pile au moment de l'extraction.

#Exercice 2-L'histogramme
#La fonction hist()
#qui permet de construire un histogramme

#Construction d'un histogramme de la distribution de capacity 
hist(velov$capacity,main = "Distribution de \n la capacité des stations")
#x= le nom de la variable 
#main= le titre du graphique 

#Construction du même graphique avec 6 classes
hist(velov$capacity,
     main = "Distribution de \n la capacité des stations",
     breaks = 6)
#breaks= le nb d'intervalles

#Construction du même graphique mais en rouge 
hist(velov$capacity,
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red")
#col= la couleur du graphique 

#Renommer l'axe des abscices 
hist(velov$capacity,
     main = "Distribution de \n la capacité des stations",
     breaks = 6,
     col = "red",
     xlab = "Capacity")
#xlab= nommer l'axe des abscistes 

#La fonction abline()
#Qui permet d'ajouter une ligne à un graphique déjà existant 

#Ajouter une ligne bleue 
abline(h = 100, col = "blue", lty = 2)
#h=l'emplacement de la ligne horizontale 
#col= la couleur de la ligne 
#lty= le type de la ligne

#La fonction hist(),lines() et density()

#Construction du même graphique avec la densité mais pas les effectifs et en supprimant les classes
hist(velov$capacity,
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity")
#Probability= c'est true si les hauteurs des barres doivent être représenter avec la densité*

#Ajout de la courbe de densité 
#Pour ça on va utiliser les fonctions lines() et density()
#Density permet d'estimer la densité de probabilité d'un échantillion 
#Lines permet d'ajouter des lignes à un graphqiue existant *
lines(density(velov$capacity),
      lty = 2,
      col = "blue",
      lwd = 4)
#Pour density x=le variable utilisé 
#pour lines:
#x=les coordonnées x des pts 
#lwd=l'épaisseur des lignes
#lty=le type de ligne

#Modification des bornes de l'axe pour voir la courbe en entier 
#Pour ça on utilise l'argument ylim 
hist(x = velov$capacity, 
     main = "Distribution de \n la capacité des stations",
     col = "red",
     probability = TRUE,
     xlab = "Capacity",
     ylim = c(0,0.08))
#ylim= les limites de l'axe de y

lines(density(velov$capacity),
      lty = 2,
      col = "blue",
      lwd = 2)

#Exercice 3- Le boxplot 
#La fonction boxplot()

#construction d'une boite à moustache de la distribution des capacity 
boxplot(x = velov$capacity, 
        main = "Boxplot de \n la capacité des stations")

#Construction du même graphique mais en pivotant horizontalement 
boxplot(x = velov$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = TRUE)
#Horizontal= true, pour dessiner le graphique horizontalement

#Construction du même graphique en remettant verticalement et n'affichant pas les valeurs 
boxplot(x = velov$capacity, 
        main = "Boxplot de \n la capacité des stations",
        horizontal = FALSE,
        outline = FALSE)
#outline=false, veut dire qu'il enlève les pts aberrants du graphiques 

#Ajout d'un point qui correspond à la moyenne 
#il doit être gros carré et rouge 
#avec la fonction points()
#qui permet d'ajouter un point sur un graphique existant 
moyenne=mean(velov$capacity)
points(moyenne, col = "red", pch = 15, cex = 2)
#x= le calcul 
#col= la couleur
#pch= le type de symbole pour les pts
#cex=spécifie la taille relative des textes

#La fonction par()
#permet de spécifier les paramètres graphiques globaux
par(mfrow=c(1,2)) #fenêtre sur 1 ligne et 2 colonnes
#7ème
velov7 = subset(velov, CodePostal == "69007")
boxplot(x = velov7$bikes, 
        main = "Boxplot nb vélos \n 69007",
        ylim = c(0,40))
#8ème
velov8 = subset(velov, CodePostal == "69008")
boxplot(x = velov8$bikes, 
        main = "Boxplot nb vélos \n 69008",
        ylim = c(0,40))
#C'est plus simple d'analyser les deux graphiques si la borne des ordonnées est la même.
# On remarque que la disponibilité des stations est plus homogènes sur le 8ème.


#Sur le même graphique, on souhaite analyser le nb de vélos dispo en fonctionn de la variable bonus 
par(mfrow=c(1,1)) #fenêtre sur 1 ligne et 1 colonne
# Tracer le graphique boxplot
boxplot(formula = bikes ~ bonus,
        data = velov, 
        main = "Dispo vélos vs Stations Bonus")

#Les fonctions points() et tapply()
#tapply permet d'appliquer une fonction à des sous-ensembles de valeurs

#Ajout des moyennes de chaque grp sur le graphique 
#Pour ça on utilise les fonctions points() et tapply()
# Calculer les moyennes de chaque groupe
means <- tapply(X = velov$bikes, 
                INDEX = velov$bonus, 
                FUN = function(X) mean(X))
print(means)
# Ajouter les moyennes de chaque groupe au graphique
points(means, col = "red", pch = 19)

#Exercice 4- Le diagramme 
#Les fonctions barplot() et table()

#Création d'un diagramme en barre de la répartition du nombre de station bonus
#Avec la fonction barplot() 
#Qui permet de créer le diagramme en barres 
effectif = table(velov$bonus)
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus")
#height= les hauteurs des barres 
#main= le titre 

#Construction du même graphique mais pivoter à l'horizontale 
barplot(height = effectif,
        main = "Répartition du nombre \n de station bonus",
        horiz = TRUE)
#horiz=true, pour dessiner les barres à l'horizontale


#les fonction barplot(),prop.table() et legend()

#Construction du même graphqique mais en pourcentage 
frequence = prop.table(effectif)
barplot(height = frequence,
        main = "Répartition en % du nombre \n de station bonus",
        horiz = TRUE)
#La fonction prop.table permet de calculer les proportions d'un tableau 

#Construction d'un diagramme bivarié avec la répartito du nb de stations bonus en fonction du nb de station avec un terminal de paiement.
#Les 2 variables ayant les mm modalités TRUE/FALSE, il est donc important de définir le nom de l'axe des abscisses 
#L'odre des variables dans la focntion table() influence le niveau de lecture du graphique
effectif = table(velov$banking, velov$bonus)
print(effectif)
barplot(height = effectif,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?")
#On remarque qu'on ne sait pas distinguer les deux modalités car il n'y a pas de légende.
#table() crée un tableau de contingence à partir des données 
#xlab le label de l'axe des abscisses 

#Ajout d'une légende pour pouvoir distinguer.
#vert pour TRUE, rouge pour FALSE
#Calcul des pourcentages
frequence1 = prop.table(x = effectif)
barplot(height = frequence,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence)
#Ajouter une légende avec la fonction legend()
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Vérification du graphique avec les fréquences 
#pour voir si le graphique est cohérent 
print(frequence1)

#Même question mais avec les pourcentages
#Calcul des pourcentages colonnes
frequence2 = prop.table(x = effectif, margin = 2)
barplot(height = frequence2,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"))

#Préparer les labels
legend_labels <- colnames(frequence2)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence2)

#Même question avec un diagramme bivarié non empilé
#Avec l'aide de l'argument beside
#Calcul des pourcentages colonnes
frequence3 = prop.table(x = effectif, margin = 2)
barplot(height = frequence3,
        main = "Bonus vs Banking",
        xlab = "Station Bonus ?",
        col = c("red","green"),
        beside = TRUE)

#Préparer les labels
legend_labels <- colnames(frequence3)
#Ajouter une légende
legend(x = "topright", 
       legend = legend_labels, 
       fill  = c("red","green"))

#Afficher les fréquences pour vérifier le graphique
print(frequence3)

#Les fonction pie() et paste()
#Création d'un diagramme circulaire de la répartition du nombre de station bonus
#Avec la fonction pie()
#En différenciant les deux catégories avec les couleurs jaune et vert 
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"))
#la fonction pie() sert à faire un diagramme circulaire

#Construire le même graphique à l'aide de l'argument labels et la fonction paste()
#L'argument labels()= sert à mettre les etiquettes des sections 
#La fonction paste()= sert à concatène des vecteurs de caractèrees 
etiquette = paste(rownames(effectif),"\n",effectif)
pie(x = effectif,
    main = "Répartition du nombre \n de station bonus",
    col = c("yellow","green"),
    labels = etiquette)

#Les fonctions palettes() et colors()

#Construction d'un diagramme en barre
#avec le top 10 des codes postaux ayant le plus de station de velov 
effectif = table(velov$CodePostal)
top10 = sort(effectif, decreasing = TRUE)[1:10]
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = palette(),
        las = 2)  # Rotation des étiquettes à 90 degrés
#On remarque que les deux premières couleurs se répetent.
print(palette()) # la fonction `palette()` ne dispose que de 8 couleurs

#L'argument las = permet de faire pivoter les etiquettes de données
#La fonction palette()= est un vecteur qui renvoie une palette de couleurs par défaut 
#Il y'en a 8

#Même question mais avec la fonction colors()
#Elle donne accès à beaucoup plus de couleurs que la focntion palette 
barplot(height = top10,
        main = "Top 10 sur le \n nombre de station",
        col = colors(),
        las = 2)  # Rotation des étiquettes à 90 degrés

print(colors())

#C'est pour cela qu'on peut utiliser les couleurs avec le nom. 
#R les associe ensuite aux vecteurs palette() ou colors()

#Exportation du graphique dans un format .PNG avec la fonction dev.print()
dev.print(device = png, file = "export.png", width = 600)
#La fonction dev.print()= sert à exporter un graphique dans un fichier spécifié 
#En utilisant un périphérique graphique spécifié 
#Cette fonction exporte le graphique en cours de lecture

#exercice 5-Nuage de point
#La fonction plot()
#Consstruction d'un nuage de point pour étudier la corrélation entre le nb de place dispo et la capacité 
#On n'oublie pas le titre 
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité")
     
#Construire le même graphique en zoomant sur les abscisses et ordonnées de 0 à 60 $
#Avec des points sur fond noir 
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     pch=19)
     
#Les fonctions plot() et levels() 
#Construction du same graphique en affichant deux couleurs différentes selon la colonne borne 
#LA colonne doit avoir le type factor 
df$bornes = as.factor(df$bornes)
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0,60),
     ylim = c(0,60),
     col = df$bornes,
     pch=19)

 # Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = palette(), pch = 19)
       
 #Choix des couleurs 
 myColors <- c("red", "blue", "green")  
# Ajoutez plus de couleurs si nécessaire avec le code HTML des couleurs à la place des noms

# Tracer le graphique
plot(x = df$stands, y = df$capacity,
     main = "Place disponible vs Capacité",
     xlim = c(0, 60),
     ylim = c(0, 60),
     col = myColors[df$bornes],
     pch = 19)

# Ajouter une légende
legend("topright", legend = levels(df$bornes),
       col = myColors, pch = 19)
       
 #Ajout d'un carré vert présentant la moyenne du nb de places dispos et la capacité 
 moy_stands = mean(df$stands)
moy_capacity = mean(df$capacity)
points(x = moy_stands,y = moy_capacity, 
       pch = 15,
       col = myColors[3],
       cex = 2)

#Exercice 6-Cartographie 
#Création d'une carte à partir de colonnes 
# Librairies nécessaires
library(leaflet)
library(dplyr)
library(ggplot2)

# Créer une carte Leaflet
maCarte <- leaflet(df) %>% 
  addTiles() %>% 
  addMarkers(~position_longitude, 
             ~position_latitude, 
             popup = ~address)

# Afficher la carte
maCarte






