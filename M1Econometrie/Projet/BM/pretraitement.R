#########################CHOMAGE#################################
CHOM <- read.csv("CHOM.csv", encoding="UTF-8", na.strings="..")[,c(4:7)]
names(CHOM) <- c("Code", "Y2014", "Y2015", "Y2016")
CHOM$CHOM <- (CHOM$Y2015 - CHOM$Y2014)*100/CHOM$Y2014
na <- which(is.na(CHOM$CHOM), arr.ind=TRUE)
CHOM$CHOM[na] <- ((CHOM$Y2016 - CHOM$Y2014)*100/CHOM$Y2014)[na]
CHOM <- CHOM[,c("Code", "CHOM")]

########################CONSO###################################
CONSO <- read.csv("CONSO.csv", encoding="UTF-8", na.strings="..")[,c(4:7)]
names(CONSO) <- c("Code", "Y2014", "Y2015", "Y2016")
CONSO$CONSO <- CONSO$Y2015
na <- which(is.na(CONSO$CONSO), arr.ind=TRUE)
CONSO$CONSO[na] <- rowMeans(CONSO[,c("Y2014", "Y2015", "Y2016")], na.rm=TRUE)[na]
CONSO <- CONSO[,c("Code", "CONSO")]

########################EPARGNE#################################
EPARGNE <- read.csv("EPARGNE.csv", encoding="UTF-8", na.strings="..")[,c(4:7)]
names(EPARGNE) <- c("Code", "Y2014", "Y2015", "Y2016")
EPARGNE$EPARGNE <- (EPARGNE$Y2015 - EPARGNE$Y2014)*100/EPARGNE$Y2014
na <- which(is.na(EPARGNE$EPARGNE), arr.ind=TRUE)
EPARGNE$EPARGNE[na] <- ((EPARGNE$Y2016 - EPARGNE$Y2014)*100/EPARGNE$Y2014)[na]
EPARGNE <- EPARGNE[,c("Code", "EPARGNE")]

########################INFL###################################
INFL <- read.csv("INFL.csv", encoding="UTF-8", na.strings="..")[,c(4:7)]
names(INFL) <- c("Code", "Y2014", "Y2015", "Y2016")
INFL$INFL <- INFL$Y2015
na <- which(is.na(INFL$INFL), arr.ind=TRUE)
INFL$INFL[na] <- rowMeans(INFL[,c("Y2014", "Y2015", "Y2016")], na.rm=TRUE)[na]
INFL <- INFL[,c("Code", "INFL")]

########################PIB###################################
PIB <- read.csv("PIB.csv", encoding="UTF-8", na.strings="..")[,c(4:7)]
names(PIB) <- c("Code", "Y2014", "Y2015", "Y2016")
PIB$PIB <- PIB$Y2015
na <- which(is.na(PIB$PIB), arr.ind=TRUE)
PIB$PIB[na] <- rowMeans(PIB[,c("Y2014", "Y2015", "Y2016")], na.rm=TRUE)[na]
PIB <- PIB[,c("Code", "PIB")]

########################REVENU###################################
REVENU <- read.csv("REVENU.csv", encoding="UTF-8", na.strings="..")[,c(4,5)]
names(REVENU) <- c("Code", "REVENU")

#######################FINAL##################################
data <- Reduce(function(x, y) merge(x, y), list(CONSO, CHOM, EPARGNE, INFL, PIB, REVENU))
data <- data[complete.cases(data),]
rownames(data) <- data$Code
data$Code <- NULL

# #############################################################
# #############################################################
