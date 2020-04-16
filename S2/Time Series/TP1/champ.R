library(ggplot2) 
library(ggExtra)
library(ggthemes)
library(forecast)
###############################################################################
### La simulation est mon amie
###############################################################################

# La simulation : une approche pour illustrer la théorie 
# grâce à la connaissance des lois des variables aléatoires en jeu
# on peut ainsi
## -> rester ou sortir du cadre théorique (résidus normaux ou pas)
## -> savoir si theta est dans H0 ou H1 (évaluation risques 1e et 2e espèce)
## -> comparer la vérité à l'estimation pour juger du bienfondé de la méthode
# Une méthode renvoie toujours un résultat, l'illustration par simulation peut
# et doît fournir un degré de compréhension supplémentaire

###
# Nuages de points suivant les lois
###

#simulation de la serie temporelles de loi normale
# on remarque la structure circulaire du nuage de points
# cela est relié à ce qu'on pourrait observer avec des échantillons IID
# ici il y a de la corrélation car une même valeur sert d'abscisse 
# pour un point et d'ordonnée pour un autre
n <- 301 #nombre de temps

# série temporelles gaussienne
gauss_series <- rnorm(n)
x <- gauss_series[1:(n-1)]
y <- gauss_series[2:n]
# échantillons complètement IIDs
# x <- rnorm(n)  
# y <- rnorm(n)  
# modèle linéaire x(t) = a t + u(t)  
# u <- rnorm(n)
# x <- .75*(1:n) + u
z <- sqrt(x^2+y^2) #calcul de la distance à l'origine (rien de vital)
df <- data.frame(x = x, y = y, rayon = z) 
p <- ggplot(data = df, aes(x = x, y = y)) +
  geom_point(aes(x = x, y = y,col = rayon)) +
  coord_fixed(ratio = 1)
ggExtra::ggMarginal(p, type="histogram", col = "dodgerblue3", fill = "dodgerblue4", xparams = list(  bins=20), yparams = list( bins = 20))

#à partir de la structure précédente, on pourra varier les plaisirs :
# -> en changeant les paramètres de la loi normale
# -> en changeant de loi (rbinom, rt, ...)
# -> en rajoutant des fonctions du temps

###############################################################################
### Premiers pas vers la modélisation des séries temporelles
###############################################################################

# on charge la série 
#setwd(dir = "/home/marchand/Bureau/Cours/Rennes2/MASS4_SeriesChro/Docs_Ronan/")
champ <- read.table("TP1/champagne.txt")
n <- nrow(champ)
# serie temporelle
champ.ts <- ts(champ,start=c(1882,1),frequency = 12)
# data frame
date <- seq(as.Date("2001/1/1"), length.out = n, by = "month")
year <- substr(as.character(date),1, 4)
champ <- data.frame(volume = champ$V1, time = date, year = year) 
# on regarde le chronogramme
Champ_chrono <- ggplot(data = champ, aes(x = time, y = volume)) +
  geom_line(data = champ, aes(x = time, y = volume), color = "darkblue") +
  theme_economist() +
  scale_color_economist()
Champ_chrono
# traçons le mothplot pour voir la tendance annuelle en séparant les données
ggmonthplot(champ.ts) +
  scale_color_economist() +
  theme_economist()
# la croissance du pic sur le chronogramme en novembre/décembre est clairement confirmée
# mais cette croissance n'étant pas globale sur l'ensemble des mois
# on confirme l'augmentation du schéma périodique au fil du temps
# on s'intéresse alors au log pour ajuster un modèle linéaire sur le log
# LnChamp = m_t+s_t+epsilon_t => Champ = e^(m_t)e^(s_t)e^(epsilon_t)
# ce dernier modèle expliquerait l'augmentation de l'amplitude lorsque le niveau moyen augmente
champ <- data.frame(champ , logChamp = log(champ$volume))
LnChamp_chrono <- ggplot(data = champ, aes(x = time, y = logChamp)) +
  geom_line(data = champ, aes(x = time, y = logChamp), color = "darkblue") +
  theme_economist() +
  scale_color_economist()
LnChamp_chrono
ggmonthplot(ts(champ$logChamp,start=c(1882,1),frequency = 12)) +
  scale_color_economist() +
  theme_economist()
#la croissance semble plus partagé sur l'ensemble des mois
# pas strictement mais ce n'est pas grave 

lchamp <- champ$logChamp
plot(lchamp, col = "blue", type ="o")

plot(diff(lchamp, differences = 1))
mean(diff(lchamp, differences = 1))
acf(diff(lchamp, differences = 1))

plot(diff(lchamp, lag = 12))
mean(diff(lchamp, lag = 12))
acf(diff(lchamp, lag = 12))

t <- 1:105
sinusoides <- t%o%c(rep(1:5,2))*pi/6
# pour comprendre le principe du calcul
a <- 1:6
b <- (-2):2
a%o%b # la ieme ligne correspond à b multiplié par le ieme coefficient de a
b%o%a # la ieme ligne correspond à a multiplié par le ieme coefficient de b
###
sinusoides[,1:5] <- sin(sinusoides[,1:5])
sinusoides[,6:10] <- cos(sinusoides[,6:10])
sinusoides <- as.data.frame(sinusoides)
names(sinusoides) <- c(paste("sin",1:5),paste("cos",1:5))   

df <- data.frame(lchamp, t, t^2, t^3)
df <- cbind(df,sinusoides)
ModAddifitf <- lm(data = df, lchamp ~ .)
plot(ModAddifitf$residuals)
acf(ModAddifitf$residuals)
lag.plot(ModAddifitf$residuals, do.lines = F, lags=12)
summary(ModAddifitf)

selection <- step(ModAddifitf) #algo de sélection de variables sur
#la base du critère AIC
summary(selection)
plot(lchamp, col = "blue", type = "l")
lines(selection$fitted.values, col = "red")  
