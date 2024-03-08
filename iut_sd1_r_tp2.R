#Exercice 1- importer les données 
#importation d'un fichier CSV 
setwd(dir = "L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique")
getwd()
#lecture du fichier CSV 
fao=read.csv(file="fao.csv",header =TRUE,sep = ";",dec = "," )
#pour voir le type appuyer sur la flèche dans Environment 
View(fao)# il y'a donc 186 pays

#résumé des données avec la fonction 
summary(fao)

#Exercire 2-Statistique descriptive 

#calcul de la moyenne d'une colonne de la dataframe 
mean(fao$Dispo_alim)

#Calcul du nombre de la population 
#na.rm=TRUE, permet de supprimer les valeurs NA pour permettre de réliser des calculs
sum(fao$Population, na.rm = TRUE)

#Calcul de l'écart-type de l'importation 
sd(fao$Import_viande)
#Calcul de l'écart-type de l'exportation avec des NA 
sd(fao$Export_viande, na.rm = TRUE)

#Médiane du volume de productio 
median(fao$Prod_viande, na.rm = TRUE)

#Calcul des quartiles 
quantile(fao$Dispo_alim,probs=c(0.25,0.5,0.75))
#"c" permet de faire la combinaison des 3
#pour les quartiles 25% 5% 75%

#Calcul des centiles 
quantile(fao$Import_viande, probs = seq(0,1,0.01))

#Exercice 3- Tris et filtres 
#5 pays les moins peuplés 
flop5=head(fao[order(fao$Population),],5)
print(flop5)

#5 pays les plus peuplé
top5= head(fao[order(fao$Population,decreasing=TRUE),],5)
print(top5)
 
#5 pays qui produisent le plus de viandes 
topviande= head(fao[order(fao$Prod_viande),],5)
print(topviande)

#5 pays qui importent le plus de viandes 
topimp=head(fao[order(fao$Import_viande),],5)
print(topimp)

#disponibilité alimentaire >=2300  
dispo=subset(fao$Dispo_alim,fao$Dispo_alim >= 2300)
View(dispo)

#disponibilité alimentaire >3500
resultat = subset(df, Dispo_alim > 3500  & Import_viande > 1000)
View(resultat)

#extraire les lignes du dataset avec la France et la Belgique
resultat = subset(df, Nom %in% c("France","Belgique"))
View(resultat)

#Exercice 4- Modifier le dataframe
#Ajout d'une colonne 
df$Part_export<-df$Export_viande/df$Prod_viande

df$Dispo_alim_pays<-df$Dispo_alim*df$Population

#Exporter le nouveau dataframe dans un fichier CSV
write.table(x = df, file = "ExportTp2.csv")

#Calcul de la somme 
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale

dispo_alim_mondiale/2300

#Exercice 5-Corrélation 
#représentation graphique nuage de point pour montrer le lien entre deux variables 
plot(x = df$Prod_viande,
     y = df$Export_viande, 
     main = "Pays : Prod_viande / Export_viande")
#Calcul du coefficient 
cor(x = df$Prod_viande,
    y = df$Export_viande)

#construction de la matrice des corrélations
matriceCor = cor(df[ , - 1] , use = complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)

#instalation d'un package 
#commande à executer qu'une seule fois
install.packages("corrplot")

#Construction d'un corrélogramme
library(corrplot) #je charge mon package pour pouvoir utiliser ses fonctionalités
corrplot(matriceCor, method="circle")
