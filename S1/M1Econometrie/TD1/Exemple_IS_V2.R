setwd("S:/MES DOCUMENTS/document/LICENCE.MASTER/annee2017-2018/M1_MAS/Econometrie_S1/CorrelationS_I")
### setwd("H:/M1_MAS/Econometrie_S1")
getwd()
library(AER)
library(car)
library(sandwich)
library(lmtest)
library(gmm)
library(systemfit)
library(ggplot2)

corr_IS <-read.table("Exemple_I_S.csv",head=TRUE,sep=";" )
head(corr_IS)
summary(corr_IS)


boxplot(corr_IS$I_GDP,corr_IS$S_GDP, corr_IS$I_GDP1,corr_IS$S_GDP1,corr_IS$I_GDP2,corr_IS$S_GDP2,las=1, names = c("I_GDP 1980-2013", "S_GDP 1980-2013" , "I_GDP Per 1", "S_GDP Per 1 ", "I_GDP Per 2", "S_GDP Per 2 "), col = c( "red", "royalblue2", "red", "royalblue2", "red", "royalblue2")  )
## las = 1 l�gende horizontale, las = 2 l�gende verticale
mtext("Per 1 : 1980-1996, Per 2 : 1997-2013", side=1, line=3)

plot(I_GDP~S_GDP, data=corr_IS, main="Correlation entre le taux d'investissement et le taux d'�pargne 1980-2013" )
abline((lm(corr_IS$I_GDP~corr_IS$S_GDP)$coefficients), col="blue")




#####estimation MCO 
cor(corr_IS$I_GDP,corr_IS$S_GDP)
cor(corr_IS$I_GDP,corr_IS$S_GDP)^2

Periode_G <- lm( I_GDP ~ S_GDP, data=corr_IS)
summary(Periode_G)
coef(Periode_G)
fitted(Periode_G)
residuals(Periode_G)
scrc = sum(Periode_G$residuals^2)
scrc

###test mobilite des capitaux
linearHypothesis(Periode_G, "S_GDP = 1")


####estimation avec une sp�cificit� OCDE
boxplot(corr_IS$I_GDP ~ corr_IS$OECD)
boxplot(corr_IS$S_GDP ~ corr_IS$OECD) 

S_GDP_OECD = corr_IS[,"S_GDP"]*corr_IS[,"OECD"]
Periode_GOECD <- lm( I_GDP ~ S_GDP+S_GDP_OECD+ OECD, data=corr_IS)
summary(Periode_GOECD)
anova(Periode_G,Periode_GOECD)


##test de chow
fichier1 = corr_IS[corr_IS$OECD == 1,]  ## selectionne les pays OCDE
head(fichier1)
EQ1_OECD = lm(I_GDP ~ S_GDP, data=fichier1)
summary(EQ1_OECD)
scr1 = sum(EQ1_OECD$residuals^2)
scr1
fichier2 = corr_IS[corr_IS$OECD == 0,]  ## selectionne les pays Non OCDE
head(fichier2)
EQ1_NOECD = lm(I_GDP ~ S_GDP, data=fichier2)
summary(EQ1_NOECD)
scr2 = sum(EQ1_NOECD$residuals^2)
scr2


#degr�s de libert�
ddl_n = (Periode_G$df.residual - (EQ1_OECD$df.residual + EQ1_NOECD$df.residual))
ddl_n
ddl_d = EQ1_OECD$df.residual + EQ1_NOECD$df.residual
ddl_d
#F du test de Chow et p-value
FChow = ((scrc-(scr1+scr2))/ddl_n)/((scr1+scr2)/ddl_d)
FChow
pvalue = pf(FChow,ddl_n,ddl_d,lower.tail=FALSE)
pvalue


###analyse des r�sidus
##analyse graphique 
residG = residuals(Periode_G )
residG2 = residG^2
yp = fitted(Periode_G)

plot(residG2~yp, data=corr_IS, main="heterosc�dasticit� - la variance est-elle fonction du I_GDP pr�vu ?" )
plot(residG2~GDP, data=corr_IS, main="heterosc�dasticit� - la variance est-elle fonction du PIB ?" )

#heteroscedasticite##
S_GDP_SQ = corr_IS$S_GDP^2
bptest(Periode_G, ~ S_GDP + S_GDP_SQ, data=corr_IS) #test de white
Test_White<- lm( residG2 ~ S_GDP + S_GDP_SQ, data=corr_IS)
summary(Test_White)

#bptest(Periode_G)
#Test_White2<- lm( residG2 ~ S_GDP , data=corr_IS)
#summary(Test_White2)

gqtest(Periode_G, order.by = ~ GDP, fraction = 6, data=corr_IS) # GoldfeldQuant

## test de specification - ramsey rest test
resettest(Periode_G) ## test de misspecification
## equivalent 
yp2 <- yp^2
yp3 <- yp^3
Periode_GT <- lm( I_GDP ~ S_GDP + yp2 + yp3, data=corr_IS)
summary(Periode_GT)
anova(Periode_G,Periode_GT)

##heteroscedasticite application des MCG - pas n�cessaire ici ##
Periode_G_C <- lm( I_GDP ~ S_GDP, data=corr_IS, weights=(1/GDP))
summary(Periode_G_C)
## weights=(1/GDP) signifie qu'on suppose que var de l'al�a = sigma2*GDP 


##heteroscedasticite correction de White - pas n�cessaire ici ##
vcov(Periode_G) ## matrice de variance-cov MCO
vcovHC(Periode_G) ## matrice de variance-cov avec la correction de white
coeftest(Periode_G, vcov = vcovHC)


## outlier
influence.measures(Periode_G)

### analyse de la corr�lation par sous-p�riode
EQ1 <- I_GDP~S_GDP
EQ2 <- I_GDP1~S_GDP1
EQ3 <- I_GDP2~S_GDP2


System <- list( EQ1, EQ2, EQ3)
model_MCO <- systemfit( System, "OLS", data=corr_IS, maxit=100)
summary(model_MCO, residCov=FALSE, equations = FALSE)
## summary(model_MCO)
## coef(model_MCO)
## fitted(model_MCO)
## residuals(model_MCO)



#####estimation MCO sans contraintes - test de contraintes de Fisher 
System1 <- list( EQ2, EQ3)
model_scontraint <- systemfit( System1, "OLS", data=corr_IS, maxit=100)
summary(model_scontraint, residCov=FALSE, equations = FALSE)
R1 <- matrix( 0, nrow = 2, ncol = 4 )
R1[ 1, 1 ] <- 1
R1[ 1, 3 ] <- -1
R1[ 2, 2 ] <- 1
R1[ 2, 4 ] <- -1
linearHypothesis( model_scontraint, R1, test = "F" )

model_sure <- systemfit( System1, "SUR", data=corr_IS, maxit=100)
summary(model_sure, residCov=FALSE, equations = FALSE)
R1 <- matrix( 0, nrow = 2, ncol = 4 )
R1[ 1, 1 ] <- 1
R1[ 1, 3 ] <- -1
R1[ 2, 2 ] <- 1
R1[ 2, 4 ] <- -1
linearHypothesis( model_sure, R1, test = "F" )

####test du ratio de vraissemblance

model_contraint <- systemfit( System1, "OLS", data=corr_IS, restrict.matrix = R1)
summary(model_contraint, residCov=FALSE, equations = FALSE)
lrTest1 <- lrtest( model_contraint, model_scontraint )
print( lrTest1 ) 

model_sure_c <- systemfit( System1, "SUR", data=corr_IS, restrict.matrix = R1, maxit=100)
summary(model_sure_c, residCov=FALSE, equations = FALSE)
lrTest2 <- lrtest( model_sure_c, model_sure )
print( lrTest2 )


