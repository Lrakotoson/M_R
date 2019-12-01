
###############################
### 1.Prise en main  ##
###############################

bernoulli <- function(p){
  x <- runif(1) #par default min=0 et max=1
  if (x< p) {
    return(1)
  }else{
    return(0)
  }
}

bernoulli(1/3)

###############################
### 1.Lois de proba usuelles ##
###############################

### 1.1 Lois discretes

#1.
#fonction nombre mystère

find.number = function(myst){
  nb.alea <-  sample(1:10,1)
  nb.tirages <-  1
  while(nb.alea!=myst){
    nb.tirages <-  nb.tirages+1
    nb.alea   <-  sample(1:10,1)
  }
  return(nb.tirages)
}

#appliquer la fonction N=1000 et garder le résultat dans un vecteur:
N <- 1000
myst <- 9
res.nbmyst <-  sapply(rep(myst,N),find.number)  

#2.
table(res.nbmyst)  # le nombre d'apparition de chaque entier
frec <- table(res.nbmyst)/sum(table(res.nbmyst)) #frequence relative. 
#Remarque: Pour obtenir  la frequence relative, on peut aussi  diviser par N 
plot(frec)


# find.number() compte le nombre des essais indépendants avant d'obtenir un succès pour une expérience 
# ayant une probabilité de succès de 1/10. La loi de probabilité correspondante est donc une géométrique
# de paramètre 1/10. 

# !!! Attention, le suffixe geom dans R correspond à une loi de probabilités comptant 
# le nombre d'échecs avant le premier succès, il faut donc décaler les valeurs de 1 dans l'évaluation 
# du dgeom() !

Intervalle <- min(res.nbmyst):max(res.nbmyst) 
probTheorique <- dgeom(x=Intervalle-1, prob = 1/10) #dans R la loi geom compte le nb d'échecs avant le premier succes 
lines(probTheorique,type = 'h', col='red')
lines(probTheorique,col=3) 


###  1.2 Lois continues

#1.
x = seq(-4,4,length=100) #discretisation de l'intervalle d'interet
plot(x,dnorm(x),type="l",ylim=c(0,1),ylab = "",lwd=2)  #par défault dans dnorm mean=0 et sd=1
# on fixe ylim pour pouvoir répresenter la fonction de répartition sur le même graphique.
lines(x,pnorm(x),col=2)
legend("topleft",legend = c("Fonc. répartition", "Fonc. dénsité"), lty=c(1,1), col = c("red","black"))

#2.
q95 = qnorm(0.95)

#3.
abline(h=0)
segments(x0=q95,y0=0,x1=q95,y1=dnorm(q95),lwd=2)

#4.
plot(x,dnorm(x),type="l",ylab = "",col=1) #pour mieux visualiser
lines(x,dt(x,5),col=2) #loi student à 5 deg de liberté
lines(x,dt(x,30),col=4) #densite du loi student à 30 deg de liberté
legend("topleft", legend = c("N(0,1)","Student(5)","Student(30"),lty=c(1,1,1), col = c(1,2,4))
# Illustration du fait qu'une loi de Student converge vers une loi normale(0,1) quand le nb de degrés de liberté tends vers l'infini

###############################
### 2.Simulations      ########
###############################

### 2.1 - Loi exponentielle
#1.
set.seed(1) #pas necessaire, juste pour pouvoir reproduire le vecteur ech
ech=rexp(n=100, rate = 2) 

#2.

thetas=seq(0.1,4,by=0.1)
# calcul de valeurs la fonction de log-vraisemblance 
logvrais = sapply(thetas,FUN=function(x) sum(dexp(ech,rate=x,log=TRUE)))

#une autre façon de faire:
LogVrais <- function(ech,rate){
  n <- length(ech)
  return(n*log(rate)-rate*sum(ech))
}
valLogV=sapply(thetas,FUN=function(x){LogVrais(ech=ech,rate=x)})
# graphique
plot(thetas,valLogV,type = 'l')
#  ou plot(thetas,logvrais,type="l",col=2)

#3. l'estimatuer du max de vraisaimblance est donnée par 1/\bar{X_n} (\bar{X_n} est la moyenne empirique)
MaxVrais <- 1/mean(ech) 
abline(v=MaxVrais,col='red')
thetas[which.max(valLogV)] #pour quel theta on a obtenu une valeur maximale dans notre calcul de log-vraisemblance

### 2.2 - Loi normale "simple"
#1.
set.seed(42)
ech.norm = rnorm(n=10,mean=2,sd=sqrt(3)) ## Attention à l'ecart-type

#3.
normMult = rnorm(3,c(0,100,1000),sqrt(c(0.1,10,100)))


### pour un echantillon de taille 20 de même loi:
normMult.mat = sapply(1:20,FUN=function(i) rnorm(3,c(0,100,1000),sqrt(c(0.1,10,100))))

### 2.2 - Loi normale multi-variée

#1. 
#création de la matrice sigma
sigma <- matrix(0.2,ncol = 3, nrow = 3)
diag(sigma)=c(0.1,10,100)
sigma
#décomposition
L=chol(sigma)
L #triangulaire superieure
L <- t(L) #triangulaire inferieure
L%*%t(L) #vérification

#2.
set.seed(1)
mu = rep(0,3)
Y <- as.vector(mu+L%*%rnorm(3))
Y

library(MASS) #pour le faire directement avec la fonction mvrnorm
set.seed(1)
mvrnorm(n=1,mu=c(0,0,0),sigma)


###############################
### 3.Illustrations  des théorèmes limites  ########
###############################

### 3. 1 Loi des grandes nombres
#1. 
#2.
N=1000
library(purrr)
X <- rbernoulli(n=N,p=0.4)
# Sinon on peut faire
X <- rbinom(n=N,size = 1,p=0.4)
#3.
m_l <- sapply(1:N, FUN = function(x){mean(X[1:x])})
length(m_l)
plot(m_l,type = 'l',ylab = "Moyenne",xlab = "Taille echantion")
abline(h=0.4,col='red')
#Pour calculer les moyennes succesives on peut également faire:
m_l <- cumsum(X)/1:N


### 3. 2 Théorème centrale limite

#1.
# Soit X1,...,Xn,... v.a iid de moyenne mu et de variance sigma2 (sigma2 != 0). 
# Sn = X1+...Xn
# On a (Sn - n*mu)\sqrt(n*sigma2) ---> N(0,1) (cvg en loi)
# Avec Xi bernoulli, Sn binomiale 
# mu = p
# sigma2 = p*(1-p)

#2.Bin(n,p)=sum(de n bernoulli(p))

#3.
p=0.5; n=10; N=1000
s=rbinom(n=N, size=n,p)
Z10 <- (s-n*p)/sqrt(n*p*(1-p))
Z10
#4.
n=50
s=rbinom(n=N, size=n,p)
Z50 <- (s-n*p)/sqrt(n*p*(1-p))
n=500
s=rbinom(n=N, size=n,p)
Z500 <- (s-n*p)/sqrt(n*p*(1-p))
#5.
x <- seq(-4,4,by=0.1)
densite <- dnorm(x=x,mean = 0,sd=1) 

par(mfrow=c(2,2))
hist(Z10,probability = TRUE)
lines(x,densite,col='red',lwd=2,xlab="",ylab="")

hist(Z50,probability = TRUE)
lines(x,densite,col='red',lwd=2,xlab="",ylab="")


hist(Z500,probability = TRUE)
lines(x,densite,col='red',lwd=2,xlab="",ylab="")

# On peut également créer une fonction pour tracer les histogrammes

BerTCL <- function(N,n,p){
  s <- rbinom(n=N, size=n,p)
  Z <- (s-n*p)/sqrt(n*p*(1-p))
  hist(Z, probability = TRUE,main = paste("Histogramme n=",n))
  Xval <- seq(min(Z),max(Z),by=0.1)
  lines(Xval,dnorm(Xval),col='red')
}
BerTCL(1000,10,0.5)  
par(mfrow=c(1,3))
sapply(c(10,50,500),FUN =  function(i){BerTCL(N,i,p)})

