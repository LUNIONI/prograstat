#Exercice 1
a <- 10
b <- 5
resultat<-a*b
print(resultat)
A<-7.2
B<-10.1
print("Le langage R est sensible à la casse (majuscule/minuscule) car nous avons 4 objets a,b,A et B")
resultat<-A+B
print(resultat)
print("La précédente valeur de l'object resultat a été supprimée et remplacée par la somme de A et B.")
rm(a,A,b,B,resultat)#celui là supprime juste les objets demandés 
rm(list = ls())#lui il supprime tout directement

#Exercice 2
vecteur<-c(1,2,3,4,5)
class(vecteur)
vecteur[3]
v1<-1:5
v2<-v1+3
print(v2)
v3<-1:6
v4<-v3**2
print(v4)
v5<-v4/2
print(v5)

vecteur <- c("Lundi", "Mardi", "Mercredi", "Jeudi", "Vendredi", "Samedi", "Dimanche")
class(vecteur)
vecteur[c(2,7)]

vecteur<-c(TRUE, FALSE, TRUE,FALSE,TRUE)
class(vecteur)

vecteur<-c(0.2,0.5,5.8,9.1)
class(vecteur)
vecteur[-3]

vecteur<-c("janvier", "février", "mars", "avril", "mai", "juin", "juillet", "août","septembre","octobre","novembre","décembre")
class(vecteur)
vecteur[c(1,2,3)]

vecteur<-c(-6,-52,-78,-89,-115,-23)
class(vecteur)
vecteur[c(6,1)]

vecteur<-c("banana","orange","pomme","kiwi","fraise","framboise")
class(vecteur)
vecteur[c(-1,-2)]

vecteur <- c(1, 2, NA, 4, 5)
class(vecteur)

#fonctions c(),seq(),length()
sequence<-seq(1, 10)
length(sequence)
#correction du prof très utile
ma_sequence <- seq(from = 1, to = 10)
length(ma_sequence)

masequence<-seq(from=2, to=20, by=2)
length(masequence)

masequence<-seq(from=0, to=-5)
length(masequence)

masequence<-seq(from=5, to=50, by=5)
length(masequence)

masequence<-seq(from=10, to=1, by=-1)
length(masequence)

masequence<-seq(from=0, to=1, by=0.1)
length(masequence)

masequence<-seq(from=5, to=-5, by=-1)
length(masequence)

masequence<-seq(from=1, to=10, by=2)
length(masequence)

#les fonctions c(),rep()
vecteur <- rep(3, times = 5)
print(vecteur)

vecteur <- rep(c('A', 'B', 'C'), times = 3)
print(vecteur)

vecteur <- rep(1:3, times = 3)
print(vecteur)

vecteur <- rep(c(TRUE, FALSE), times = 4)
print(vecteur)

rm(vecteur)

#Exercice 3
#Les fonctions runif(), mean(), median(), min(), max()
vecteur <- runif(n = 5, min = 0, max = 1)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n = 10, min = -5, max = 5)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n = 100, min = 10, max = 20)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

vecteur <- runif(n= 15, min = 50, max = 100)
vecteur
mean(vecteur)
median(vecteur)
min(vecteur)
max(vecteur)

#Les fonctions rnorm(), mean(), sd(), hist(), quantile()
echantillon <- rnorm(n = 20, mean = -2, sd = 3)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

echantillon <- rnorm(n = 2000, mean = -2, sd = 3)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

echantillon <- rnorm(n = 2000, mean = 0, sd = 1)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))
hist(echantillon)

quantile(echantillon, probs = c(0.25))
quantile(echantillon, probs = c(0.50))
quantile(echantillon, probs = c(0.75))

quantile(echantillon, probs = seq(0,1,0.1))

quantile(echantillon, probs = seq(0,1,0.01))

#Les fonctions sum(), round()
echantillon <- rnorm(n = 3000, mean = 2400, sd = 300)
moyenne <- mean(echantillon)
ecart_type <- sd(echantillon)
print(paste("Moyenne : ", moyenne))
print(paste("Écart-type : ", ecart_type))

echantillon <- round(echantillon, 2)

masse_salariale <- sum(echantillon)
print(paste("Masse salariale : ", masse_salariale))

salaire_median <- median(echantillon)
print(paste("Salaire médian : ", salaire_median))

quantile(echantillon, probs = 0.99)

quantile(echantillon, probs = 0.2)

#Les fonctions sample(), table(), prop.table(), uique(),sort()
sample(x = c(1,2,3,4,5,6), size = 1)

simulation <- sample(x = c(1,2,3,4,5,6), size = 12, replace = TRUE)
print(simulation)

unique(simulation)

table(simulation)

prop.table( table(simulation) )
print("Non, ce n'est pas le cas")

simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)
print(simulation)

simulation <- sample(x = c(1,2,3,4,5,6), size = 100000, replace = TRUE)
frequence <- prop.table( table(simulation) )
sort(frequence, decreasing = TRUE)
print("Oui, d'après la loi des grands nombres, les probabilités d'obtenir chacune des faces se rapprochent des probabilités théoriques.")