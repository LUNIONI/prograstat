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

vecteur<-