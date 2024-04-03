#Exercice 1
setwd("L:/BUT/SD/Promo 2023/ilunion/Programmation de la statistique/dataset")
df <- read.csv(file = "nba2014_2015.csv", sep = ",",
               header = TRUE, dec = ".")
nrow(df)
ncol(df)
colnames(df)
Period <- as.factor(df$Period)
PTS_TYPE <- as.factor(df$PTS_TYPE)
SHOOTER<- as.factor(df$SHOOTER)

#Exercice 2
length(df$PERIOD)
length(df$PTS_TYPE)
length(df$SHOOTER)
summary(df)
sd(df$SHOT_DIST)
sd(df$SHOT_CLOCK)

#combien de tirs manqués/réussis
table(df$SHOT_RESULT)
#Il y'a 57 905 qui ont été réussi 
#Et 70 164 qui ont été manqué 

#les quartiles
quantile(df$SHOT_CLOCK, probs = c(0.25, 0.5, 0.75), na.rm = TRUE)

#les déciles
quantile(df$CLOSE_DEF_DIST, probs = seq(0.1,0.9, by=0.1),na.rm=TRUE)

#nombre de matches différents
liste_game <- unique(df$GAME_ID)
length(liste_game)
#il y'a 904 valeurs uniques 

#nombre de joueurs différents
SHOOTER <- as.factor(df$SHOOTER)
levels(df$SHOOTER)

#conversion de la variable SHOT_DIST en mètre pour que les européens comprennent nos chiffres
df$SHOT_DIST_METRE<- df$SHOT_DIST * 0.30

#nombre de points qu'a rapporté la tentative (0,2 ou 3)  
df$PTS_MARQUES <- ifelse(df$SHOT_RESULT == "made", yes = df$PTS_TYPE, 0)

#On supprime la variable GAME_RESULT car elle n'est pas utile
df$GAME_RESULT <- NULL
       
#création d'un objet sans la première colonne GAME_ID
df2 <- df[, -1 ]

#Exercice 3

#Les 100 tirs réussis ou manqués les plus loin
rang <- order(df$SHOT_DIST, decreasing = TRUE)
df3 <- df[rang,]
df3 <- df3[ 1 : 100 , ]

#Les 100 tirs réussis les plus loin
df4 = subset(df, SHOT_RESULT == "made")
df4 <- df4[ 1 : 100 , ]

#Combien de tirs à 3 points a réussi Kobe Bryant ?
df_kobe = subset(df,SHOT_RESULT == "made" &
                   PTS_TYPE == "3" & 
                   SHOOTER == "Kobe BRYANT")

dim(df_kobe)

#Le TOP5 des joueurs qui ont marqués le plus de points dans la saison
df_total <- aggregate(PTS_MARQUES ~ SHOOTER, data = df, FUN = sum)
df_total_tri <- df_total[order(df_total$PTS_MARQUES,decreasing = TRUE),]
df_top5 <-  df_total_tri[1:5  ,  ]

#Exercice 5
#Des graphiques adaptés selon le type de variable

#construction de la fonction
build_graph <- function(une_colonne, nom_colonne) {
if(is.numeric(une_colonne)) {
    print(boxplot(une_colonne, main = nom_colonne))
  }
else if (as.factor(une_colonne)) {
    tri <- table(une_colonne)
    print(barplot(tri, main = nom_colonne))
  }
  
#on déroule la fonction sur chaque colonne du data frame.
  
for (colonne in colnames(df) {
    build_graph(une_colonne = df[colonne , ] , nom_colonne = colone)
  }
}