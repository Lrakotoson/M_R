# -*- coding: utf-8 -*-
rm(list = ls()) #enlever les variables gardées en memoire

# ######################
# 1. Jeu de donnees R
# ######################

#1.
data(mtcars)

is.matrix(mtcars)
is.data.frame(mtcars)
is.list(mtcars)
class(mtcars)

#2.
dim(mtcars)

#3.
names(mtcars)
length(names(mtcars))

#4.
help(mtcars)
#5.
summary(mtcars)
str(mtcars) 

#6.
x  <-  mtcars[,1]
x <-  mtcars$mpg

#7.
x[c(15,3,4)]
#8.
x[1:10]
rev(x[1:10]) ; x[10:1]
#9.
tail(x,10)
#10.
mtcars$am = as.factor(mtcars$am)
summary(mtcars)
is.factor(mtcars$am)
#11.
y = mtcars$am
#12.
newData = cbind(x,y)
#13.
is.matrix(newData) ; is.data.frame(newData)
dim(newData)
mode(newData)
summary(newData)

#14.
newData[12,]
#15.
newData[1:8,]
#16.
newData[,2]
#17.
newData[newData[,2]==1,1] #attention à la difference entre y et newData[,2]
mtcars$mpg[mtcars$am==0]

#18.
ConsoMoy = c(mean(mtcars$mpg[mtcars$am==0]),mean(mtcars$mpg[mtcars$am==1]))

#19.
ConsoMoy2 = c(mean(subset(mtcars,am==0)$mpg),mean(subset(mtcars,am==1)$mpg))
is.vector(ConsoMoy2)
ConsoMoy3 = by(mtcars$mpg,mtcars$am,mean)
is.array(ConsoMoy3)
ConsoMoy4 = aggregate(mpg~am,data=mtcars,mean) 
#ou 
aggregate(mtcars$mpg,list(Transmission=mtcars$am),mean)
is.data.frame(ConsoMoy4)

#20
ConsoSd=c(sd(subset(mtcars,am==0)$mpg),sd(subset(mtcars,am==1)$mpg))
is.vector(ConsoSd)
#21.
ConsoRes <- list(Automatique = cbind(ConsoMoy,ConsoSd)[1,], Manuelle=cbind(ConsoMoy,ConsoSd)[2,])



# ##########################
# 2.1. Importation simple
# ##########################

#1.
Cancer = read.table("cancerprostate.txt",header=TRUE) 
View(Cancer) # les données sont mis sur une seule colonne
Cancer = read.table("cancerprostate.txt",header=TRUE,sep=";")
View(Cancer)
class(Cancer) ; dim(Cancer)
summary(Cancer)
str(Cancer)

Cancer = read.table("cancerprostate.txt",header=TRUE,sep=";",colClasses = c(rep("numeric",2),
                     rep("factor",4),"numeric"))
summary(Cancer)  
Cancer = read.table("cancerprostate.txt",header=TRUE,sep=";",colClasses = c( "integer","numeric",
                                                                            rep("factor",4),"numeric"))
str(Cancer)

#2.
Test1 = read.table("test1.csv",header=TRUE,sep=",") # Ajouter sep="," 
View(Test1)
#ou
Test1 = read.csv("test1.csv",header=TRUE)
View(Test1)

#3.
Test2       = read.csv("test2.csv",header=TRUE)
dim(Test2)
summary(Test2)
str(Test2)
anyNA(Test2)
sum(is.na(Test2))


#enlever les NA
Test2clear  = na.omit(Test2) #le plus simple
# ou 
Test2clear=Test2[!apply(is.na(Test2),1,any),]
dim(Test2clear)
#ou
select      = which(is.na(Test2),arr.ind=TRUE)
class(select); dim(select) #select est une matrice contenant les coord des NA (ligne x colonne)
length(unique(select[,1])) # nombre des lignes de Test2 contenant au moins une NA
Test2.clear = Test2[-select[,1],]
dim(Test2.clear)


####################################
## 2.2 Fusion de jeux de donnees
####################################
#1.
Etat1 = read.csv("etat1.csv",header=TRUE,sep=";")
dim(Etat1)
summary(Etat1)
Etat2 = read.csv("etat2.csv",header = TRUE)
dim(Etat2)
summary(Etat2)
etatc=cbind(Etat1,Etat2)
dim(etatc)
summary(etatc)

Etat           = merge(Etat1,Etat2,by="etat")
summary(Etat)
dim(Etat)

# Que se passe-t-il si les individus diffèrent entre les 2 jeux de données ?
Etat1b          = Etat1[-c(4,6),] # on enleve deux lignes du Etat1
Etat.bis       = merge(Etat1b,Etat2,by="etat") # les lignes respectives sont enlevees. 
Etat.ter       = merge(Etat1b,Etat2,by="etat",all=TRUE) #les données manquantes sont remplacees par NA 

#2 Création et manipulation de facteurs
typeof(Etat$Income)
Etat$richesse  = cut(Etat$Income,breaks =c(min(Etat$Income),4000,5000,max(Etat$Income)),labels=c("Pauvre","Aise","Riche"))
any(is.na(Etat$richesse))

Etat$richesse  = cut(Etat$Income,breaks =c(min(Etat$Income)-1,4000,5000,max(Etat$Income)),labels=c("Pauvre","Aise","Riche"))
any(is.na(Etat$richesse))
summary(Etat$richesse)
#ou
Etat$richesse  = cut(Etat$Income,breaks =c(min(Etat$Income),4000,5000,max(Etat$Income)),labels=c("Pauvre","Aise","Riche"), include.lowest = TRUE)
any(is.na(Etat$richesse))


decoupe        = quantile(Etat$Income,probs=seq(0,1,length=6))
decoupe
Etat$richesse2 = cut(Etat$Income,breaks=decoupe,include.lowest = TRUE) 
any(is.na(Etat$richesse2))
summary(Etat$richesse2)

split(Etat$Income,Etat$richesse2)
levels(Etat$richesse2) = c("R1","R2","R3","R4","R5")
summary(Etat$richesse2)
nameslev       = paste("R",1:5,sep="")

levels(Etat$richesse2) = c("R1","R1","R2","R2","R3") #fusionner deux niveaux
summary(Etat$richesse2)

# ##############################
# 2.3 Variables au format Date
# ##############################

#1.
readLines('ski.csv',5)
ski  = read.table("ski.csv",header=TRUE,sep="|",skip=2)
str(ski)

#2.
ski2 = read.table("ski.csv",header=TRUE,sep="|",skip=2,colClasses = c("character","numeric","factor","Date"))
class(ski2$first.time.skiing)
summary(ski2); summary(ski)


t=Sys.time(); ski  = read.table("ski.csv",header=TRUE,sep="|",skip=2); Sys.time()-t

# ###############################
# 2.4  Donnees manquantes
# ###############################

Bladder = read.table("bladder.txt")
names(Bladder)
dim(Bladder)
str(Bladder)

any(is.na(Bladder))
# Repeter plusieurs fois les instructions suivantes
(v=sample(1:ncol(Bladder),1))
summary(Bladder[,v])
boxplot(Bladder[,v])

# ou simplement:
summary(Bladder[,52])
boxplot(Bladder[,52])


summary(Bladder[,1973])
boxplot(Bladder[,1973])

summary(Bladder[,956])
boxplot(Bladder[,956])

#4.
Bladder = read.table("bladder.txt",na.strings = -500)
any(is.na(Bladder))
sum(is.na(Bladder))


#5.
# avec fonction apply
Bladder = read.table("bladder.txt",na.strings = -500)
Bladder = apply(Bladder,2,FUN=function(x){replace(x,is.na(x),mean(x,na.rm=TRUE))})
sum(is.na(Bladder))
class(Bladder) 
Bladder=as.data.frame.matrix(Bladder) #pour le re-transformer en data.frame

# avec boucle
Bladder = read.table("bladder.txt",na.strings = -500)
for (j in 1:ncol(Bladder)){
  Bladder[,j] = replace(Bladder[,j],is.na(Bladder[,j]),mean(Bladder[,j],na.rm=TRUE))
}
sum(is.na(Bladder))

# sans boucle et sans definir une fonction dans apply
colmoy    = apply(Bladder,2,mean,na.rm=TRUE)
replaceNA = t(t(is.na(Bladder))*colmoy) #une matrice tq r_ij= 0 si Bladder_ij != NA et r_ij= moyenne de la colonne j sinon.
Bladder[is.na(Bladder)]=0
Bladder   = Bladder+replaceNA
sum(is.na(Bladder))


##################################
## 2.5 Importation depuis une url
##################################
Database_webmaster <- read.csv("http://tecfa.unige.ch/guides/R/data/edutechwiki-fr-gw-oct-6-2014.csv", header = TRUE, sep= ",")
summary(Database_webmaster)
View(Database_webmaster)



# ############################
# # 2.6 Importation robuste
# ############################

Data        = read.table("donnees.csv",header=TRUE,sep=";",dec=",") #renvoie un avertisment
DataChar    = scan("donnees.csv",what="char",sep=";")
nomcol      = DataChar[2:5] #ou simplement nomcol <- DataChar[1:5] si on considere le nom des individu comme une variable
DataChar    = gsub(",",".",DataChar) 
matbrut     = matrix(DataChar,nrow=4,ncol=5,byrow=TRUE)
nomlign     = matbrut[-1,1]
donnees     = matbrut[-1,-1]
donnees     = data.frame(donnees)
rownames(donnees) = nomlign
colnames(donnees) = nomcol
#les 5 derniers lignes peuvent etre remplacées par
donnees     = data.frame(matbrut[-1,],row.names = 1)
colnames(donnees) = nomcol


summary(donnees)
donnees[,1] = as.numeric(as.character(donnees[,1])) #transformation facteur en valeurs numeriques
donnees[,2] = as.numeric(as.character(donnees[,2]))
donnees[,3] = as.numeric(as.character(donnees[,3]))

###################################
## 3. Exportation de jeux de donnees
###################################
#1.
etat.supp = Etat[Etat$etat=="Hawaii",]
Etat      = Etat[-(Etat$etat=="Hawaii"),]
Etat      = Etat[Etat$etat!="Hawaii",]
Etat
#2.
write.table(Etat,file="EtatAmericains.txt")  #  ,row.names=FALSE
#3.
write.table(etat.supp,file="EtatAmericains.txt",append=TRUE,col.names=FALSE)

#5.
save(Bladder,file="Bladder.Rdata") #attention au nom de la variable!
#7.
rm(Bladder)
# pour supprimer toutes les variable:  rm(list=ls())
# pour sauvegrader toutes les objets : save(list=ls(),file= 'SaveAll.Rdata')
#8.
load("Bladder.Rdata")
dim(Bladder) 
A = load("Bladder.Rdata")
dim(A)
A

# ############################
# # 4. Donnees Accidents
# ############################

#1.
caract = read.csv("https://www.data.gouv.fr/s/resources/base-de-donnees-accidents-corporels-de-la-circulation/20170915-153739/caracteristiques_2016.csv",
                  header=TRUE)
dim(caract)
summary(caract)
#2.
caract$dateheure = strptime(paste("20",caract$an,":",caract$mois,":",caract$jour,":",floor(caract$hrmn/100),":",caract$hrmn-(floor(caract$hrmn/100)*100),sep=""),format="%Y:%m:%d:%H:%M")
caract$dateheure[1]
#ou plus simple:
caract$dateheure <- strptime(paste(caract$an,caract$mois,caract$jour,floor(caract$hrmn/100),caract$hrmn%%100,sep= "-"),format = "%y-%m-%d-%H-%M")

#3. Pour verifier si on a des NA's
sum(is.na(caract$dateheure))
sum(is.na(caract$int))
sum(is.na(caract$atm))
sum(is.na(caract$col))
#ou
apply(caract,2,FUN=function(x) sum(is.na(x)))
apply(caract,2,anyNA)

caract = caract[,c(1,17,8:10)]
# ou 
caract=caract[,c("Num_Acc","dateheure","int","atm","col")]

#4.
lieux = read.table("https://www.data.gouv.fr/s/resources/base-de-donnees-accidents-corporels-de-la-circulation/20170915-153921/lieux_2016.csv",header=TRUE,sep=",")
lieux = lieux[,c(1,2,15)]
#ou
lieux=lieux[,c("Num_Acc","catr","surf")]
dim(lieux)

#5.
usagers  = read.table("https://www.data.gouv.fr/s/resources/base-de-donnees-accidents-corporels-de-la-circulation/20170915-153940/usagers_2016.csv",header=TRUE,sep=",")
dim(usagers)

#6. 
usagers$grav=as.factor(usagers$grav)
levels(usagers$grav)=c("indemne","tues","hospitalise","leger")
summary(usagers$grav)

#7
tab.grav = as.data.frame.matrix(table(usagers$Num_Acc,usagers$grav))
tab.grav[1:5,]

colnames(tab.grav) = c("indemne","tues","hospitalise","leger") # pas necessaire si usagers est transformé en amont en factor
tab.grav$Num_Acc   = as.numeric(row.names(tab.grav)) #si on veut sauvegarder NumAcc

# 8.
Accidents=merge(merge(caract,lieux), tab.grav) 
Accidents[1:5,]

#ou
Accidents3 = merge(caract,merge(lieux,tab.grav,by.x="Num_Acc"),by="Num_Acc")
Accidents3[1:5,]

#si on ne veut pas sauvegarder Num_Acc (pas recomandé)
caract2=caract[,c("dateheure","int","atm","col")] 
lieux2=lieux[,c("catr","surf")]
Accidents2=cbind(caract2, lieux2,tab.grav) #creates a table data regrouped (if Num_Acc is saved in the intermediery tables, then its column might appear multiple times)
Accidents2[1:5,]


# 9.
table_catr_tues=aggregate(tues~catr,Accidents,sum) #nombre de tués par categorie

table_catr_tues$catr[table_catr_tues$tues==max(table_catr_tues$tues)] #la categorie de route avec le plus de tués 


#10.
AccEte   = Accidents[dateheure>strptime("2016-06-21",format="%Y-%m-%d") & dateheure<strptime("2016-09-22",format="%Y-%m-%d"),]
AccHiver = Accidents[dateheure<strptime("2016-03-20",format="%Y-%m-%d") | dateheure>strptime("2016-12-21",format="%Y-%m-%d"),]

dim(AccEte)[1]
dim(AccHiver)[1] 
dim(AccEte)[1] > dim(AccHiver)[1] #donc plus d'accidents l'ete 

#nombre des victimes tues par rapport au nombre d'accidents dans la categorie
sum(AccEte$tues)/dim(AccEte)[1]
sum(AccHiver$tues)/dim(AccHiver)[1]
