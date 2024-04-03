##Exercice 1-Comprendre la loi normale 
#La commande rnorm()
#Création d'un graphique vide avec comme borne d'abscisse [-5,5] et d'ordonnées [0,1]
# Créer une toile de fond vide pour le graphique
par(mfrow = c(1,1))
plot(NA, xlim=c(-5, 5), ylim=c(0, 1), xlab="X", 
     ylab="Densité de probabilité", 
     main="Densités de probabilité \n de lois normales")
     
#Programmation d'une boucle for qui à chaque itération rajoute une courbe de densité 
# Tracer la densité de probabilité pour chaque simulation
moyennes <- c(0, 0, 0, -2)
sigmas <- c(0.45, 1, 2.25, 0.7)
colors <- c("red", "blue", "green", "orange")
legend_labels <- c()
for (i in 1:length(moyennes)) {
  serie = rnorm(n = 1000, 
                mean = moyennes[i], 
                sd = sigmas[i])
  lines(density(serie), col = colors[i])
  legend_labels <- c(legend_labels, paste("m =", moyennes[i], ",", "s =", sigmas[i]))
}

# Ajouter une légende
legend("topright", legend=legend_labels, col=colors, lwd=2, cex=0.8)

#Simulation d'une loi normale 
serie = rnorm(n = 1e4, mean = 0, sd = 1)

#Constructon de l'histogamme de la distribution
hist(serie, main = "loi normal centrée-réduite",
     probability = TRUE)
lines(density(serie))

#Calcule de la médiane
median(serie)

#Calcul des quartiles 
quantile(serie)

#Calule des centiles 
quantile(serie, 
         probs = seq(from = 0, 
                     to = 1, by = 0.01))

quantile(serie, 
         probs = 0.95)
# environ 1,64                  

#Les commandes pnorm() et qnorm()
#Calcule de la valeur théorique 
qnorm(p = 0.95, mean = 0, sd = 1)
pnorm(q = 1.644854, mean = 0, sd = 1)

#Quelle est la valeur théorique pour P(<x)=0.975
qnorm(p = 0.975, mean = 0, sd = 1)

#Quelle est la valeur théorique pour P(X>=1.96)=p
1 - pnorm(q = 1.96, mean = 0, sd = 1)

##Exercice 2- Construire la table de loi normale 


