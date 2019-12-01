############### 1. La roulette

##### 1.1. Roulette américaine

Bet <- function(p){
  Res <- runif(1)
  if (Res<p) {
    return(1)
  }else{
    return(-1)
  }
  
}


##### 1.2. Roulette anglaise (Bonus)
EngBet <- function(pr,pv){ #pr proba de tirer rouge, pv proba de tirer 0(vert)
  Res <- runif(1)
  if(Res<pr){
    return(1)
  }else{
    if(Res<pr+pv){
      return(-1/2)
    }else{
      return(-1)
    }
  }

}

######## 2. Simulation du gain cumulé 

#### 2.1 Le joueur persévérant

#1.
JoueurPerseverant <- function(p, Nmax = 10000){
  S <- 0 #gain cumulé
  S_n <- c() #évolution du gain cumulé
  n <- 0 #nombre de paris effectués
  while (1 != S && n<Nmax) {
    S <- S + Bet(p)
    n=n+1
    S_n <- c(S_n,S)
  }
  return(S_n)
}

#2.
#En regardant les questions on voit qu'on n'a pas besoin de connaître toute la trajectoire de S_n
# On peut ne garder que la valeur minimale, le nombre de paris et la valeur finale


Strategie1 <- function(p,Nmax = 10000){
  S_n <- 0
  n <- 0
  min <- 0
  while (1 != S_n && n<Nmax) {
    S_n <- S_n+Bet(p)
    n <- n+1
    if (min > S_n){
      min <- S_n
    }
  }
  return(list(NumberOfBets=n, MaximumLoss=min,FinalGain=S_n)) 
}


n <- 10 #nombre de réalisations qu'on veut simuler
set.seed(4)
#p=18/38
#set.seed(4)
p <- 1/2
Nmax=10^3
maxn <- 0
mins <- rep(0,n)
gain <- rep(0,n)
for(i in 1:n){
  Simulation <- Strategie1(p,Nmax)
  if(maxn < Simulation$NumberOfBets){
    maxn <- Simulation$NumberOfBets
  }
  mins[i] <- Simulation$MaximumLoss
  gain[i] <- Simulation$FinalGain
}

cat(paste("Le nombre maximal de paris est",maxn))
cat(paste("Les valeurs minimales prises par le gain cumulé ",paste(mins, collapse = ", "), 
          ".\n Leur valeur moyenne est :",mean(mins)))
cat(paste("La moyenne empirique du gain final est : ",mean(gain)))

#3. Application de méthodes Monte Carlo
#a)
# calculer la moyenne empirique des profits cumulés à la fin pour MC réalisations de la strategie
ExpectedGainS1 <- function(p,MC=10^3, Nmax=100){ 
  mean(unlist(replicate(MC, Strategie1(p,Nmax))[3,]))
  }

x <- seq(0,1,by=0.05) #probabilité de gagner un pari (discrétisation de l'intervalle [0,1])
MC <- 10^3 #nombre de simulations Monte Carlo 
Nmax <- 100 #nombre maximal de paris pour une strategie fixée

#On estime l'espérance des gains cumulés à la fin de la stratégie pour différentes probabilités p. 
ExpectedGains <- sapply(x,FUN = function(x){ExpectedGainS1(x,MC,Nmax)})
plot(x,ExpectedGains,xlab = 'Probabilité de gagner un pari',type = 'b')

set.seed(20)
ExpectedGainS1(1/2,MC=10^4)
ExpectedGainS1(18/38,MC=10^4)

# Dans le cas de la roulette américaine, la moyenne empirique du profit cumulé final est négative, 
# ce qui suggère que la stratégie n'est pas avantageuse pour le joueur

#b)
#Pour estimer la probabilité de succès :
        # -on simule un grand nombre de réalisations de la stratégie
        # -on compte combien de fois le gain cumulé est égal à 1 
        # -on divise le résultat par le nombre de réalisations
ProbaSucces <- function(p, MC=10^3, Nmax=5000){
  return(sum(replicate(MC,Strategie1(p,Nmax)$FinalGain==1))/MC)
}

x <- seq(0.3,1,by=0.05)
t <- Sys.time()
probas=sapply(x, ProbaSucces)
plot(x,probas,type = 'b',xlab = "Probabilité de gagner un pari",ylab = "Probabilité de succes de la strategie ")
abline(v=18/38,col="red")

############## Stratégie de Martingale

#1.
# Simulation de la stratégie
martingale <- function(p,Mmax=Inf){ #le paramètre Mmax représente la mise maximale imposée par le casino (voir question 4)
  S <- 0
  S_n <- c(S)
  bet <- 1
  while(S!=1 && Mmax>=bet){ 
    S <- S+bet*Bet(p)
    S_n <- c(S_n,S)
    bet <- bet*2
  }
  return(S_n)
}

# Représentation graphique :

set.seed(52)
p=18/38
Simulation <- list() #on va sauvegarder les réalisations pour pouvoir ajouster la taille du graphique (xlim).
MaxLen <- 0
for(i in 1:5){
  Simulation[[i]] <- martingale(p)
  if(MaxLen < length(Simulation[[i]])){
    MaxLen <- length(Simulation[[i]])
  }
}
# Si on a parié n fois avant de gagner, la valeur minimale atteinte par le profit cumulé est -(2^(n-1)-1)
n <- MaxLen-1 #le nombre de paris dans le pire des 5 cas
MinValue <- -(2^(n-1)-1) #la valeur minimale  du profit cumulé
plot((1:length(Simulation[[1]]))-1, Simulation[[1]], ylim = c(MinValue,1), xlim=c(0, MaxLen),col=1,ylab='Gain',xlab = 'Step',type = 'b')
for (i in 2:4) {
  lines((1:length(Simulation[[i]]))-1,Simulation[[i]], ylim = c(MinValue,1),col=i, type = 'b')
}



#### Remarque : Si on connaît le nombre de paris effectués par le joueur (ou la dernière somme pariée), 
# on peut reconstituer la trajectoire du profit cumulé. On peut donc aussi travailler avec la fonction suivante :

martingale2 <- function(p){
  S <- 0
  bet <- 1
  while(S!=1){ 
    S <- S+bet*Bet(p)
    bet <- bet*2
  }
  return(log(bet,2))  # la fonction retourne le nombre de paris effectués.
}

#2. Min value
pr <- 18/38
pe <- 1/2
minR <- 0
minE <- 0
n <- 10

for(i in 1:n){
  Simulation=martingale(pe)
  if(minE > min(Simulation)){
    minE <- min(Simulation)
  }
  Simulation=martingale(pr)
  if(minR > min(Simulation)){
    minR <- min(Simulation)
  }
}
minR; minE

# Le for peut être remplacé par :
minR <- min(replicate(n,-2^(martingale2(pr)-1)+1))
minE <- min(replicate(n, -2^(martingale2(pe)-1)+1))
minR; minE

#3. #Espérance du nombre de paris perdus avant le premier gain :
ExpectedNbBets <- function(p, MC=10^5){
  set.seed(1) #Pour pouvoir utiliser le même échantillon
  NbBets <- replicate(MC,martingale2(p)-1) #martigale2(p) = le nombre de paris effectués, donc nb. de paris perdus=martingale2(p) -1 
  return(mean(NbBets))
}
ExpectedNbBets(18/38)
ExpectedNbBets(1/2)



#Espérance de la valeur minimale
ExpectedMinVal <- function(p, MC=10^6){
  set.seed(1) #not necessary
  mins <- replicate(MC, -2^(martingale2(p)-1)+1)
  return(mean(mins))
}
ExpectedMinVal(18/38)
ExpectedMinVal(1/2)

#On voit bien que 2^{E[X]} != E[2^X] !

#4. Mmax est déjà pris en compte (avec +infini comme valeur par défaut) pour la fonction martingale.

# Pour la deuxième fonction on peut par exemple renvoyer une valeur aberrante à la place du nombre de paris effectués
# si le joueur s'arrête avant de gagner
martingale2 <- function(p,Mmax=Inf){
  S <- 0
  bet <- 1
  while(S!=1 && Mmax>=bet){ 
    S <- S+bet*Bet(p)
    bet <- bet*2
  }
  if(S == 1){
  return(log(bet,2))
  }else{
    return(S) #Si S est différent de 1, alors S<0 !  
  }
}
#Attention : si res=martingale2(p)>0 alors res est le nombre de paris; 
            # si res<0,  alors res est le profit cumulé quand la somme à parier dépasse Mmax, c.a.d la somme perdue par le joueur. 

#5.
SuccessProbability <- function(p,MC=10^5,Mmax=Inf){
  success=0
  for( i in 1:MC){
    success=success+(max(martingale(p,Mmax))==1) 
  }
  return(success/MC)
}

#ou avec apply (plus rapide) :
SuccessProbability <- function(p,MC=10^5,Mmax=Inf){
  return(sum(replicate(MC, martingale2(p,Mmax)> 0))/MC)
}
SuccessProbability(1/2,Mmax = 100)
SuccessProbability(18/38,Mmax = 100)

# Évolution de la probabilité de succès :
x <- seq(0,1,by=0.05)
Mmax = 100
probas <- sapply(x, FUN = function(x){SuccessProbability(x,Mmax)})
plot(x,probas,xlab = "Probabilité de gagner un pari",ylab = "Probabilité de succès de la stratégie",
     main = paste("Stratégie de Martingale, Mmax=",Mmax))

#6.
SuccessProbability(18/38,Mmax = 100)
#On peut remarquer que la probabilité de gagner reste très élevée, même avec Mmax=100. Le joueur a beaucoup de chances de gagner 
# une petite somme (une unité) et peu de chances de perdre une grande somme. Pour voir si la stratégie est avantageuse
# on peut essayer d'estimer l'espérance de son gain. 

ExpectedGain <- function(p,MC=10^5,Mmax=Inf){
  mean(sapply(1:MC, FUN = function(x){
    Res <- martingale(p,Mmax)
    return(Res[length(Res)])
    }))
}
ExpectedGain(1/2,Mmax = 100)
ExpectedGain(18/38,Mmax = 100)
# Le fait que la moyenne empirique du gain sur un grand nombre de réalisations indépendantes est négative, suggère que
# la stratégie n'est pas favorable au joueur. 
