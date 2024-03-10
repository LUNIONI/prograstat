##Exercice 1, importer des données 
#Les fonctions getwd(),setwd, et read.csv

setwd(dir= "L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset" )#montre l'emplacement du fichier csv 
getwd()
bodies_karts=read.csv(file="bodies_karts.csv",header =TRUE,sep = ";",dec = "," )#lire le fichier csv 
drivers=read.csv(file="drivers.csv",header =TRUE,sep = ";",dec = "," )
gliders=read.csv(file="gliders.csv",header =TRUE,sep = "|",dec = "." )
tires=read.csv(file="tires.csv",header =TRUE,sep = "\t",dec = "," )
dim(bodies_karts)#affiche la dimension (ligne/colonne) du dataframe 
dim(drivers)
dim(gliders)
dim(tires)

##Exercice 2- statistique 
summary(bodies_karts)#	Produit un résumé statistique des données dans un objet
summary(drivers)
summary(gliders)
summary(tires)
plot(x=drivers$Weight,
     y=drivers$Acceleration,
     main="Drivers:Weight/Acceleration")
# le $ permet d'accéder à une colonne d'un dataframe  
#la commande plot permet elle de te tracer le nuage de point *
#Réponse à la question posé: 
#Il semble que les deux variables soient corrélées négativement
#Il y a autant de points mais ils sont superposés car certains drivers ont les mêmes statistiques

cor(x=drivers$Weight,
    y=drivers$Acceleration)
#calcul le coefficient de coorélation une variable par rapport à une autre 

#calcul de vérification avec la covarience et sa formuule en décomposant les calculs sinon il voit flou R 
#formule: cov(X,Y)/ecrat-type de X* ecart-type de Y
CovXY=cov(x=drivers$Weight,
          y=drivers$Acceleration)
sX=sd(drivers$Weight) #sd fonction qui calcule l'ecart-type 
sY=sd(drivers$Acceleration)
print(CovXY/(sX*sY))#résultat de la formule 

#calcul du coefficient de détermination 
coefcorr=cor(x=drivers$Weight,
             y=drivers$Acceleration)#déclaration de la variable qui calcule la coorrélation 
coefdeter=coefcorr^2 #calcul du coefficient de détermination; la coorrélation à la puissance 2
print(coefdeter)

matriceCor = cor(drivers[ , - 1])
matriceCor = round(matriceCor , 2)
#la fonction round arrondie les décimal et après la virgule on spécifie le nombre de chiffres qu'on veut après la virgule 
View(matriceCor)#la fonction view nous permet de voir la matrice qu'on a crée
#Toutes les variables semblent fortement corrélées entre elles.

#commande à executer qu'une seule fois
install.packages("corrplot")

#Construction d'une Corrélogramme
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")

#Construction pour les 3 autres 
#Pour tires 
matriceCor = round(cor(tires[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )
#Pour bodies_karts
matriceCor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )
#Pour gliders 
matriceCor = round(cor(gliders[ , - 1]),1)
corrplot(matriceCor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "black", # Ajout du coefficient de corrélation
         tl.col="black", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE 
         )
         
 #Exercice 3- manipulation de data frame 
 #Création d'un objet avec uniquement le nom du Driver et son Weight
 resultat = drivers[ , c("Driver" , "Weight")]
View(resultat)

 #Création d'un objet avec uniquement le nom du Driver et son accélération sur les 1à premières lignes 
 resultat = drivers[ 1:10 , c("Driver" , "Acceleration")]
View(resultat)

#Création d'un objet resultat sans les colonnes 5,7 et 9 
resultat = drivers[ , -c(5,7,9)]
View(resultat)

#Création d'un objet resultat sans les colonnes Weight et acccélération 
resultat = drivers[ , -c("Weight","Acceleration")] #cela fonctionne uniquement sur des index numériques.
resultat = drivers[ , -c(2,3)]

#Création d'un objet resultat avec les colonnes Driver, acceleration et Weight dans cet ordre là 
xresultat = drivers[ , c("Driver", "Acceleration", "Weight")]
View(resultat)
#Que remarquez-vous? 
#Les colonnes sont dans l'ordre défini par le vecteur.

#Creation d'un objet avec uniquement les drivers 3,12 et 32 dans cet ordre là 
resultat = drivers[ c(3,12,32) , ]
View(resultat)

 #Creation d'un objet avec uniquement les drivers 32,3 et 12 dans cet ordre là
 resultat = drivers[ c(32,3,12) , ]
View(resultat)
#Que remarquez-vous ?
#Les lignes sont dans l'ordre défini par le vecteur.

#Création d'u objet avec uniquement la colonne drivers et weight en triant les conducteurs du plus léger au plus lourd avec la fonction order()
rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)

#Création d'u objet avec uniquement la colonne drivers et accélération en triant les conducteurs du plus rapide au moins rapide 
rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)

#Création d'u objet avec les colonne drivers, weight et accélération en triant les conducteurs du plus rapide au moins rapide puis du plus léger au plus lourd 
rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat = drivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)

#Exercice 4- GOAT 
#On veut obtenir la combinaison de bodies_karts, tires, gliders et drivers qui permet d'obtenir le plus d'accélération 

#Créer un objet avec les colonnes drivers et accélération avec le ou les conducteurs avec la plus grande accélération 
help(subset)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))

#Création d'objets avec. la même logique de conserver uniquement les mllrs statistiques d'accelerations
topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))

topTires = subset(x = tires,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Tire","Acceleration"))
                   
                   
topBody = subset(x = bodies_karts,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Body","Acceleration")) 