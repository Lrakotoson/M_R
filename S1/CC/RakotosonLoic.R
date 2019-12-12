# 1 - Importation
## 1. et 2.
data <- read.table("GroupeA.csv",
                   sep = ",",
                   skip = 7,
                   header = T,
                   encoding = "UTF-8",
                   row.names = 1,
                   na.strings = "DM"
)

summary(data)

## 3.
colnames(data)[colSums(is.na(data)) > 0]

## 4.
rownames(data)[which.min(data$Réalisateur)]

## 5.
data$Moyenne <- rowMeans(data, na.rm = T)

colnames(data)[which(data["James Cameron",] < data["James Cameron",]$Moyenne)]

## 6.
data <- rbind(data, colMeans(data, na.rm = T))
row.names(data)[9] <- "MOYENNE"

## 7.
wallen <- data["Woody Allen",]
wallen$acteurs <- rowSums(wallen[,1:4])
wallen[,1:4] <- rep(NULL,4)
wallen[,"Moyenne"] <- NULL

barplot(t(wallen)[,1],
        main = "Nominations obtenus par Woody Allen",
        ylab = "Nombre de nominations",
        xlab = "Catégories",
        col = rainbow(7)
)

legend("topleft",
       legend = colnames(wallen),
       fill= rainbow(7)
)

## 8.
na <- which(is.na(data), arr.ind=TRUE)
data[na] <- rowMeans(data, na.rm=TRUE)[na[,1]]

# 2 - Simulation
## 2.1 Lois discrètes
### 1.
X <- function(p,x){
  N <- length(x)
  U <- runif(1)
  if (U <= p[1]){
    X <- x[1]
  } else {
    for (k in 2:N) {
      if(sum(p[1:(k-1)]) < U & U <= sum(p[1:k])) {
        X <- x[k]
      }
    }
  }
  return(X)
}

### 2.
SampleX <- function(p, x, n = 1){
  X <- rep(0,n)
  for (i in 1:n){
    X[i] <- X(p,x)
  }
  return(X)
}

## 2.2 Lois continues
### 2.2.1 Simulation à partir d'une autre
#### 1.
MyChi2 <- function(k){
  if (k<1) {
    ch <- "Erreur: k inférieur à 1"
  } else {
    ch <- sum(rnorm(k, mean = 0, sd = 1)^2)
  }
  return(ch)
}

#### 2.
MyUnif <- function(a,b){
  U <- runif(1)
  if (a>0 & b>1){
    X <- U+a
  } else if (a>0 & b<1) {
    X <- U+a-b
  } else if (a<0 & b <1) {
    X <- U-a-b
  } else if (a<0 & b<1) {
    X <- U-b
  } else {
    X <- U
  }
  return(X)
}

### 2.2.2 Simulation à partir d'une fonction de répartition
Exp <- function(l = 1,n = 1){
  if (l < 0) {
    X <- "Erreur: lambda négative"
  } else {
    X <- rep(0,n)
    for (i in 1:n){
      U <- runif(1)
      X[i] <- -(1/l)*log(1-U)
    }
  }
  return(X) 
}