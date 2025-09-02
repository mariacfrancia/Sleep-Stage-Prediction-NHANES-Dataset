library(NHANES)
rm(list=ls())

set.seed(345)
################################################################################
##### PUESTA EN ORDEN###########################################################
################################################################################
# Eliminar ID repetidos
N = NHANES[!duplicated(NHANES$ID), ]

# Individuos adultos
N_adultos = N[N$Age>=21 & N$Age<=80 & N$SurveyYr == '2011_12',]

# Variables a eliminar
var_eliminar = c(2,3,5,6,7,8,11,18,19,21,22,23,27,28,29,30,31,32,38,39,41,43,47,48,49,53,56,57,59,60,61,63,64,65,66,67,68,69,70,71,72,73,74,75,76)

# Mujeres
N_mujeres = N_adultos[N_adultos$Gender == 'female',]
N_mujeres = N_mujeres[,-var_eliminar]

# variables cualitativas categóricas
N_mujeres$MaritalStatus = as.factor(N_mujeres$MaritalStatus)
N_mujeres$HomeOwn = as.factor(N_mujeres$HomeOwn)
N_mujeres$Work = as.factor(N_mujeres$Work)
N_mujeres$Diabetes = as.factor(N_mujeres$Diabetes)
N_mujeres$SleepTrouble = as.factor(N_mujeres$SleepTrouble)
N_mujeres$PhysActive = as.factor(N_mujeres$PhysActive)
N_mujeres$Alcohol12PlusYr = as.factor(N_mujeres$Alcohol12PlusYr)
N_mujeres$Smoke100 = as.factor(N_mujeres$Smoke100)

# Variables cualitativas ordinales:
l = levels(as.factor(N_mujeres$HHIncomeMid))
N_mujeres$HHIncomeMid = factor(N_mujeres$HHIncomeMid, levels = l,ordered = TRUE)
l = levels(N_mujeres$Education)
N_mujeres$Education = factor(N_mujeres$Education,levels = l,ordered = TRUE)
l = levels(N_mujeres$HealthGen)
l = rev(l)
N_mujeres$HealthGen = factor(N_mujeres$HealthGen,levels = l,ordered = TRUE)
l = levels(N_mujeres$LittleInterest)
N_mujeres$LittleInterest = factor(N_mujeres$LittleInterest,levels = l,ordered = TRUE)
l = levels(N_mujeres$Depressed)
N_mujeres$Depressed = factor(N_mujeres$Depressed,levels = l,ordered = TRUE)
l = levels(N_mujeres$TVHrsDay)
N_mujeres$TVHrsDay = factor(N_mujeres$TVHrsDay,levels = l,ordered = TRUE)
l = levels(N_mujeres$CompHrsDay)
N_mujeres$CompHrsDay = factor(N_mujeres$CompHrsDay,levels = l,ordered = TRUE)

HomeRoomsag = ifelse(N_mujeres$HomeRooms<=4,"0_4_Rooms",ifelse(N_mujeres$HomeRooms<=8,"5_8_Rooms","8_plus_Rooms"))
l = levels(as.factor(HomeRoomsag))
N_mujeres$HomeRooms = factor(HomeRoomsag,levels=l,ordered = TRUE)

# Binarizamos algunas variables
N_mujeres$DaysMentHlthBad = ifelse(N_mujeres$DaysMentHlthBad>0,1,0)
N_mujeres$DaysMentHlthBad = as.factor(N_mujeres$DaysMentHlthBad)
names(N_mujeres)[names(N_mujeres) == "DaysMentHlthBad"] <- "HasMentHlthBad"

# Borramos Other en HomeOwn
N_mujeres = N_mujeres[N_mujeres$HomeOwn!="Other",]
N_mujeres$HomeOwn = droplevels(N_mujeres$HomeOwn)
summary(N_mujeres)

# colocamos la variable respuesta al final
SleepTrouble = N_mujeres[,26]
N_mujeres=data.frame(N_mujeres[,-26],SleepTrouble) 

summary(N_mujeres)

################################################################################
##### REGLAS EDICION ###########################################################
################################################################################

library(editrules)

E = editfile(file="reglas_edicion_NHANES.txt")

# Aqui cuenta como errores los que son NA
ve = violatedEdits(E, N_mujeres)
summary(ve)
plot(ve)

# Quitamos los NA y comprobamos las reglas de edicion
filas_sin_na =  complete.cases(N_mujeres)
N_sin_NA = N_mujeres[filas_sin_na,]
dim(N_sin_NA) # Si quitamos todas las filas con algun NA

vep = violatedEdits(E, N_sin_NA)
summary(vep)
plot(vep)

# Las unicas que incumplen son las de BPDiaAve = 0 -> lo ponemos como NA:
ind = which(N_mujeres$BPDiaAve==0)
N_mujeres$BPDiaAve[ind] = NA

# Comprobamos que no hay inconsistencias
filas_sin_na =  complete.cases(N_mujeres)
N_sin_NA = N_mujeres[filas_sin_na,]
dim(N_sin_NA) # Si quitamos todas las filas con algun NA

vep = violatedEdits(E, N_sin_NA)
summary(vep)

N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

################################################################################
##### OUTLIERS #################################################################
################################################################################

# Función detección de atípicos en variables asimétricas
hboutlier <- function(x,r){
  x <- x[is.finite(x)]
  stopifnot(length(x) > 0, all(x > 0))
  xref <- median(x)
  if (xref <= sqrt(.Machine$double.eps)){
    warning("Valor de referencia cercano a cero: los resultados pueden ser inexactos")
  }
  isOutlier <- pmax(x/xref, xref/x) > r
  return(isOutlier)
}


# Variable Age: Cuantitativa continua. Simetrica y no hay atipicos
library(moments)
skewness(N_mujeres_yes$Age,na.rm=TRUE)
skewness(N_mujeres_no$Age,na.rm=TRUE)
boxplot(N_mujeres$Age~N_mujeres$SleepTrouble) # sin atipicos
par(mfrow=c(1,2))
hist(N_mujeres_yes$Age,prob=TRUE,main=paste("Histograma de Age (Yes)"), xlab=paste("Valores de Age"))
hist(N_mujeres_no$Age,prob=TRUE,main=paste("Histograma de barras de Age (No)"), xlab=paste("Valores de Age"))


# Variable Poverty: Cuantitativa continua. Simetrica y no hay atipicos
skewness(N_mujeres_yes$Poverty,na.rm=TRUE)
skewness(N_mujeres_no$Poverty,na.rm=TRUE)
par(mfrow=c(1,1))
boxplot(N_mujeres$Poverty~N_mujeres$SleepTrouble) # sin atipicos
par(mfrow=c(1,2))
hist(N_mujeres_yes$Poverty,prob=TRUE, main=paste("Histograma de Poverty (Yes)"), xlab=paste("Valores de Poverty"))
hist(N_mujeres_no$Poverty,prob=TRUE, main=paste("Histograma de Poverty (No)"), xlab=paste("Valores de Poverty"))

# Variable Weight: Cuantitativa continua. Asimétrica
skewness(N_mujeres_yes$Weight,na.rm=TRUE)
skewness(N_mujeres_no$Weight,na.rm=TRUE) 
par(mfrow=c(1,2))
hist(N_mujeres_yes$Weight,prob=TRUE, main=paste("Histograma de Weight (Yes)"), xlab=paste("Valores de Weight"))
hist(N_mujeres_no$Weight,prob=TRUE, main=paste("Histograma de Weight (No)"), xlab=paste("Valores de Weight"))

# hboulier por grupo
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$Weight),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$Weight),]
r = 1:10
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in r){
  h_yes= hboutlier(x_yes$Weight,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$Weight,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(r,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta r
plot(r,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta r
# cogemos r=2
id_yes=x_yes$ID[which(hboutlier(x_yes$Weight,2))]
id_no=x_no$ID[which(hboutlier(x_no$Weight,2))]
N_mujeres$Weight[N_mujeres$ID %in% id_yes]
N_mujeres$Weight[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$Weight > 160),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable Height: Cuantitativa continua. Simétrica y sin atípicos influyentes
skewness(N_mujeres_yes$Height,na.rm=TRUE)
skewness(N_mujeres_no$Height,na.rm=TRUE)
par(mfrow=c(1,1))
boxplot(N_mujeres$Height~N_mujeres$SleepTrouble) # sin atípicos influyentes
par(mfrow=c(1,2))
hist(N_mujeres_yes$Height,prob=TRUE, main=paste("Histograma de Height (Yes)"), xlab=paste("Valores de Height"))
hist(N_mujeres_no$Height,prob=TRUE, main=paste("Histograma de Height (No)"), xlab=paste("Valores de Height"))
boxplot.stats(N_mujeres_yes$Height,coef=3)$out
boxplot.stats(N_mujeres_no$Height,coef=3)$out
sort(N_mujeres$Height,decreasing=TRUE)


# Variable Pulse: Cuantitativa continua. Simétrica y sin atípicos influyentes
skewness(N_mujeres_yes$Pulse,na.rm=TRUE)
skewness(N_mujeres_no$Pulse,na.rm=TRUE)
par(mfrow=c(1,1)) # Si se hace coef de Bowley es bastante simétrica
boxplot(N_mujeres$Pulse~N_mujeres$SleepTrouble) 
par(mfrow=c(1,2))
hist(N_mujeres_yes$Pulse,prob=TRUE,main=paste("Histograma de Pulse (Yes)"), xlab=paste("Valores de Pulse"))
hist(N_mujeres_no$Pulse,prob=TRUE,main=paste("Histograma de Pulse (No)"), xlab=paste("Valores de Pulse"))
sort(N_mujeres_no$Pulse,decreasing=TRUE)
boxplot.stats(N_mujeres_yes$Pulse,coef=3)$out #Sin atípicos influyentes
boxplot.stats(N_mujeres_no$Pulse,coef=3)$out
sort(N_mujeres_yes$Pulse,decreasing=TRUE)


# Variable BPDiaAve: Cuantitativa continua. Asimetrica en la clase "No"
skewness(N_mujeres_yes$BPDiaAve,na.rm=TRUE)
skewness(N_mujeres_no$BPDiaAve,na.rm=TRUE)
par(mfrow=c(1,1))
boxplot(N_mujeres$BPDiaAve~N_mujeres$SleepTrouble)
boxplot.stats(N_mujeres_yes$BPDiaAve,coef=3)$out # Outliers no influyentes en la clase simétrica
par(mfrow=c(1,2))
hist(N_mujeres_yes$BPDiaAve,prob=TRUE,main=paste("Histograma de BPDiaAve (Yes)"), xlab=paste("Valores de BPDiaAve"))
hist(N_mujeres_no$BPDiaAve,prob=TRUE,main=paste("Histograma de BPDiaAve (No)"), xlab=paste("Valores de BPDiaAve"))

# hboutlier en grupo "No"
x_no = N_mujeres_no[!is.na(N_mujeres_no$BPDiaAve),]
r = 1:10
n_no = numeric() #cantidad de outliers grupo No
for (i in r){
  h_no= hboutlier(x_no$BPDiaAve,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,1))
plot(r,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta r
# cogemos r=2
id_no=x_no$ID[which(hboutlier(x_no$BPDiaAve,2))]
N_mujeres$BPDiaAve[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$ID %in% c(id_no)),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable BPSysAve: Cuantitativa continua. Asimetrica 
skewness(N_mujeres_yes$BPSysAve,na.rm=TRUE)
skewness(N_mujeres_no$BPSysAve,na.rm=TRUE)
par(mfrow=c(1,2))
hist(N_mujeres_yes$BPSysAve,prob=TRUE,main=paste("Histograma de BPSysAve (Yes)"), xlab=paste("Valores de BPSysAve"))
hist(N_mujeres_no$BPSysAve,prob=TRUE,main=paste("Histograma de BPSysAve (No)"), xlab=paste("Valores de BPSysAve"))
plot(sort(N_mujeres_no$BPSysAve))

# hboutlier por grupo
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$BPSysAve),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$BPSysAve),]
r = 1:10
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in r){
  h_yes= hboutlier(x_yes$BPSysAve,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$BPSysAve,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(r,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta r
plot(r,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta r
#cogemos r=2
id_yes=x_yes$ID[which(hboutlier(x_yes$BPSysAve,2))]
id_no=x_no$ID[which(hboutlier(x_no$BPSysAve,2))]
N_mujeres$BPSysAve[N_mujeres$ID %in% id_yes]
N_mujeres$BPSysAve[N_mujeres$ID %in% id_no]


# Variable Testosterone: Cuantitativa continua. Asimétrica
skewness(N_mujeres_yes$Testosterone,na.rm=TRUE)
skewness(N_mujeres_no$Testosterone,na.rm=TRUE)
par(mfrow=c(1,1))
boxplot(N_mujeres$Testosterone~N_mujeres$SleepTrouble)
par(mfrow=c(1,2))
hist(N_mujeres_yes$Testosterone,prob=TRUE,main=paste("Histograma de Testosterone (Yes)"), xlab=paste("Valores de Testosterone"))
hist(N_mujeres_no$Testosterone,prob=TRUE,main=paste("Histograma de Testosterone (Yes)"), xlab=paste("Valores de Testosterone"))

# hboutlier por grupo
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$Testosterone),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$Testosterone),]
r = 1:10
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in r){
  h_yes= hboutlier(x_yes$Testosterone,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$Testosterone,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(r,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta r
plot(r,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta r
# cogemos r=6 en SI y r=9 en NO
id_yes=x_yes$ID[which(hboutlier(x_yes$Testosterone,6))]
id_no=x_no$ID[which(hboutlier(x_no$Testosterone,9))]
N_mujeres$Testosterone[N_mujeres$ID %in% id_yes]
N_mujeres$Testosterone[N_mujeres$ID %in% id_no]

N_mujeres2= N_mujeres
N_mujeres = N_mujeres[!(N_mujeres$Testosterone<1),]
N_mujeres = N_mujeres[!(N_mujeres$Testosterone>200),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable DirectChol: Cuantitativa continua. Asimetrica 
skewness(N_mujeres_yes$DirectChol,na.rm=TRUE)
skewness(N_mujeres_no$DirectChol,na.rm=TRUE)
par(mfrow=c(1,2))
hist(N_mujeres_yes$DirectChol,prob=TRUE, main=paste("Histograma de DirectChol (Yes)"), xlab=paste("Valores de DirectChol"))
hist(N_mujeres_no$DirectChol,prob=TRUE, main=paste("Histograma de DirectChol (No)"), xlab=paste("Valores de DirectChol"))

# hboutlier por grupo
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$DirectChol),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$DirectChol),]
r = 1:10
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in r){
  h_yes= hboutlier(x_yes$DirectChol,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$DirectChol,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(r,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta r
plot(r,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta r
# cogemos r=2
id_yes=x_yes$ID[which(hboutlier(x_yes$DirectChol,2))]
id_no=x_no$ID[which(hboutlier(x_no$DirectChol,2))]
N_mujeres$DirectChol[N_mujeres$ID %in% id_yes]
N_mujeres$DirectChol[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$DirectChol > 3 | N_mujeres$DirectChol < 0.5),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable TotChol: Cuantitativa continua. Asimétrica en el grupo "No"
skewness(N_mujeres_yes$TotChol,na.rm=TRUE) # simetrica
skewness(N_mujeres_no$TotChol,na.rm=TRUE) # asimetrica
par(mfrow=c(1,1))
boxplot(N_mujeres$TotChol~N_mujeres$SleepTrouble)
boxplot.stats(N_mujeres_yes$TotChol,coef=3)$out # Sin datos atípicos influyentes en la clase simétrica
par(mfrow=c(1,2))
hist(N_mujeres_yes$TotChol,prob=TRUE, main=paste("Histograma de TotChol (Yes)"), xlab=paste("Valores de TotChol"))
hist(N_mujeres_no$TotChol,prob=TRUE, main=paste("Histograma de TotChol (No)"), xlab=paste("Valores de TotChol"))

# hboutlier en el grupo "No"
x_no = N_mujeres_no[!is.na(N_mujeres_no$TotChol),]
g = seq(1,10,1)
n_no = numeric() #cantidad de outliers grupo No
for (i in g){
  h_no= hboutlier(x_no$TotChol,i)
  n_no = c(n_no,sum(h_no))
}
par(mfrow=c(1,1))
plot(g,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta i

# punto de inflexion en r = 2

id_no=x_no$ID[which(hboutlier(x_no$TotChol,2))]+
N_mujeres$TotChol[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$ID %in% c(id_no)),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable UrineVol1: Cuantitativa continua. Asimetrica TOMA VALORES ENTEROS!!!
skewness(N_mujeres_yes$UrineVol1,na.rm=TRUE)
skewness(N_mujeres_no$UrineVol1,na.rm=TRUE)
par(mfrow=c(1,2))
hist(N_mujeres_yes$UrineVol1,prob=TRUE, main=paste("Histograma de UrineVol1 (Yes)"), xlab=paste("Valores de UrineVol1"))
hist(N_mujeres_no$UrineVol1,prob=TRUE, main=paste("Histograma de UrineVol1 (No)"), xlab=paste("Valores de UrineVol1"))

# Estudiamos asimetria con hboutlier
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$UrineVol1),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$UrineVol1),]
g = seq(1,20,1)
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in g){
  h_yes= hboutlier(x_yes$UrineVol1,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$UrineVol1,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(g,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta i
plot(g,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta i

id_yes=x_yes$ID[which(hboutlier(x_yes$UrineVol1,10))]
id_no=x_no$ID[which(hboutlier(x_no$UrineVol1,13))]
N_mujeres$UrineVol1[N_mujeres$ID %in% id_yes]
N_mujeres$UrineVol1[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$ID %in% c(id_yes)),]
N_mujeres = N_mujeres[!(N_mujeres$UrineVol1 < 2),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable UrineFlow1: Cuantitativa continua. Asimetrica
skewness(N_mujeres_yes$UrineFlow1,na.rm=TRUE)
skewness(N_mujeres_no$UrineFlow1,na.rm=TRUE)
par(mfrow=c(1,2))
hist(N_mujeres_yes$UrineFlow1,prob=TRUE, main=paste("Histograma de UrineFlow1 (Yes)"), xlab=paste("Valores de UrineFlow1"))
hist(N_mujeres_no$UrineFlow1,prob=TRUE, main=paste("Histograma de UrineFlow1 (No)"), xlab=paste("Valores de UrineFlow1"))

# Estudiamos asimetria con hboutlier
x_yes = N_mujeres_yes[!is.na(N_mujeres_yes$UrineFlow1),]
x_no = N_mujeres_no[!is.na(N_mujeres_no$UrineFlow1),]
g = seq(1,20,1)
n_yes = numeric() #cantidad de outliers grupo Yes
n_no = numeric() #cantidad de outliers grupo No
for (i in g){
  h_yes= hboutlier(x_yes$UrineFlow1,i)
  n_yes = c(n_yes,sum(h_yes))
  h_no= hboutlier(x_no$UrineFlow1,i)
  n_no = c(n_no,sum(h_no))
}

par(mfrow=c(1,2))
plot(g,n_yes,type='l') #cómo cambia la cantidad de outliers Yes detectados a medida que aumenta i
plot(g,n_no,type='l') #cómo cambia la cantidad de outliers No detectados a medida que aumenta i
id_yes=x_yes$ID[which(hboutlier(x_yes$UrineFlow1,13))]
id_no=x_no$ID[which(hboutlier(x_no$UrineFlow1,13))]
N_mujeres$UrineFlow1[N_mujeres$ID %in% id_yes]
N_mujeres$UrineFlow1[N_mujeres$ID %in% id_no]

N_mujeres = N_mujeres[!(N_mujeres$ID %in% c(id_no)),]
N_mujeres = N_mujeres[!(N_mujeres$UrineFlow1 > 9),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]


# Variable SleepHrsNight: Cuantitativa continua. Simétrica 
skewness(N_mujeres_yes$SleepHrsNight,na.rm=TRUE)
skewness(N_mujeres_no$SleepHrsNight,na.rm=TRUE)
par(mfrow=c(1,1))
boxplot(N_mujeres$SleepHrsNight~N_mujeres$SleepTrouble)
par(mfrow=c(1,2))
hist(N_mujeres_yes$SleepHrsNight,prob=TRUE, main=paste("Histograma de SleepHrsNight (Yes)"), xlab=paste("Valores de SleepHrsNight"))
hist(N_mujeres_no$SleepHrsNight,prob=TRUE, main=paste("Histograma de SleepHrsNight (No)"), xlab=paste("Valores de SleepHrsNight"))
boxplot.stats(N_mujeres_yes$SleepHrsNight,coef=3)$out
boxplot.stats(N_mujeres_no$SleepHrsNight,coef=3)$out

N_mujeres=N_mujeres[complete.cases(N_mujeres$ID),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

################################################################################
##### ESTUDIO DE NA'S ##########################################################
################################################################################

summary(N_mujeres)

# Quitamos los que coincidan
N_mujeres = N_mujeres[!((is.na(N_mujeres$Pulse) & is.na(N_mujeres$BPSysAve)) & is.na(N_mujeres$BPDiaAve)),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

# BPSysAve 1 NA en ID= 68998. Registro con más NA, quitamos
N_mujeres = N_mujeres[!(is.na(N_mujeres$BPSysAve)),]
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]
summary(N_mujeres)

# BPDiaAve: Agregamos la media en la clase yes (simetrica) y la mediana en la no (asimetrica)
# identificamos los ID de cada clase
id_na = N_mujeres$ID[which(is.na(N_mujeres$BPDiaAve))]
id_yes = N_mujeres[N_mujeres$ID %in% id_na & N_mujeres$SleepTrouble=='Yes',]$ID
id_no = N_mujeres[N_mujeres$ID %in% id_na & N_mujeres$SleepTrouble=='No',]$ID

media_yes = mean(N_mujeres_yes$BPDiaAve,na.rm=TRUE)
mediana_no = median(N_mujeres_no$BPDiaAve,na.rm=TRUE)

N_mujeres[N_mujeres$ID %in% id_yes,c('BPDiaAve')] = media_yes
N_mujeres[N_mujeres$ID %in% id_no,c('BPDiaAve')] = mediana_no
summary(N_mujeres)
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

# Quitamos los que coinciden
N_mujeres = N_mujeres[!((is.na(N_mujeres$HealthGen)) & (is.na(N_mujeres$HasMentHlthBad) & is.na(N_mujeres$LittleInterest)) & is.na(N_mujeres$Depressed)),]
summary(N_mujeres)
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

# SleepHours: media por clases
id_na = N_mujeres$ID[which(is.na(N_mujeres$SleepHrsNight))]
id_yes = N_mujeres[N_mujeres$ID %in% id_na & N_mujeres$SleepTrouble=='Yes',]$ID
id_no = N_mujeres[N_mujeres$ID %in% id_na & N_mujeres$SleepTrouble=='No',]$ID

media_yes = mean(N_mujeres_yes$SleepHrsNight,na.rm=TRUE)
media_no = mean(N_mujeres_no$SleepHrsNight,na.rm=TRUE)

N_mujeres[N_mujeres$ID %in% id_yes,c('SleepHrsNight')] = media_yes
N_mujeres[N_mujeres$ID %in% id_no,c('SleepHrsNight')] = media_no
summary(N_mujeres)
N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]

# HHIncome tiene muchos NA que coinciden con Poverty: los quitamos
N_mujeres = N_mujeres[!((is.na(N_mujeres$HHIncomeMid)) & (is.na(N_mujeres$Poverty))),]
summary(N_mujeres)

##### IMPUTACIÓN #####
original=N_mujeres
library(mice)
set.seed(345)
imputacion = mice(N_mujeres, method="cart", m=1)
N_mujeres = complete(imputacion)
summary(N_mujeres)

N_mujeres = N_mujeres[,-1]
columnas_factor = c('Education','MaritalStatus','HHIncomeMid','HomeOwn','Work','Diabetes','HealthGen','HasMentHlthBad','LittleInterest','Depressed','PhysActive','TVHrsDay','CompHrsDay','Alcohol12PlusYr','Smoke100','HomeRooms')
vars_cont = c(1,9,5,10,11,12,13,14,15,16,17,18,24)

N_mujeres_yes = N_mujeres[N_mujeres$SleepTrouble == 'Yes',]
N_mujeres_no = N_mujeres[N_mujeres$SleepTrouble == 'No',]
 
################################################################################
###### NORMALIDAD ##############################################################
################################################################################
library(nortest)
normales = c()
for (i in vars_cont){
  test = lillie.test(N_mujeres[,i])
  if (test$p.value>0.05){
    normales = c(normales,i)
  }
}
names(N_mujeres)[normales]

# Solo nos sale que Height es normal, por lo que tendremos que comparar la correlacion con spearman

################################################################################
###### CORRELACION #############################################################
################################################################################

# Correlacion entre continuas y ordinales con Spearman
vars_correlacion = c(vars_cont,2,4,6,20,22,23,26,27)
N_mujeres_ord = sapply(N_mujeres[, vars_correlacion],as.numeric)
matriz_cor = cor(N_mujeres_ord, method='spearman', use = "everything") 
# correlacion > 0.7 ---> correladas
correlaciones_altas <- abs(matriz_cor) > 0.7 #matriz booleana
#correlaciones_altas <- abs(matriz_cor) > 0.6 #matriz booleana
# Vemos que las correlaciones mayores de 0.6 son tambien mayores de 0.7-> no hay casos que estudiar
diag(correlaciones_altas) <- FALSE #la diagonal esta correlada con sí misma
(alta_correlacion <- which(correlaciones_altas, arr.ind = TRUE))
(ind_correlacion <- unique(rownames(matriz_cor)[alta_correlacion[, 1]]))
# correlacion: "UrineFlow1", "UrineVol1", "HHIncomeMid" y "Poverty"

# Tenemos que quitar UrineFlow1 (12) o UrineVol1 (11): vemos la que está más correlada con el resto
sort(abs(matriz_cor[11,-c(11,12)]),decreasing=TRUE)
sort(abs(matriz_cor[12,-c(11,12)]),decreasing=TRUE) #sale esta

# Tenemos que quitar HHIncomeMid (15) o Poverty (3): vemos la que está más correlada con el resto
sort(abs(matriz_cor[3,-c(3,15)]),decreasing=TRUE) #sale esta
sort(abs(matriz_cor[15,-c(3,15)]),decreasing=TRUE)

# Quitaria UrineFlow1 (18) y Poverty (5) por ser las menos interpretables, ya que el resto de correlaciones son parecidas
names(N_mujeres)
N_mujeres = N_mujeres[,-c(5,18)]
summary(N_mujeres)

# DEPENDENCIA ####
# Entre predictores cualitativas, tanto binarios como no binarios.
vars_cont = c(1,8,9,10,11,12,13,14,15,16,22) # actualizamos las variables continuas
vars_binarias = c(6,17,19,23,26,27)
pred_cont = N_mujeres[,vars_cont]

pred_cat_todas = N_mujeres[,-c(vars_cont,28)]
pred_cat_names = names(pred_cat_todas)
comb <- combn(pred_cat_names, 2, simplify = FALSE)

relacionadas = c("Diabetes","HasMentHlthBad","Alcohol12PlusYr","Smoke100","MaritalStatus","HHIncomeMid","Work","HealthGen","LittleInterest","Depressed","TVHrsDay")
comb_prueba = combn(relacionadas, 2, simplify = FALSE)
for (par in comb) {
  var1 <- par[[1]]
  var2 <- par[[2]]
  
  tabla <- table(N_mujeres[[var1]], N_mujeres[[var2]])
  
  if (all(dim(tabla) == 2)) {
    test <- fisher.test(tabla)
    if (test$p.value < 0.05) {
      cat(paste("Las variables", var1, "y", var2, "son dependientes (Fisher)\n"))
    }
  } else {
    test <- chisq.test(tabla)
    if (test$p.value < 0.05) {
      cat(paste("Las variables", var1, "y", var2, "son dependientes (Chi-cuadrado)\n"))
    }
  }
}

# DEPENDENCIA entre variable respuesta (binaria) y predictores continuos: Wilcoxon
# Estudiamos Height que salia normal con un t.test
t.test(N_mujeres_yes$Height,N_mujeres_no$Height,alternative = 'two.sided')
# Sale que depende de la variable respuesta

# El resto de variables con test de Wilcoxon
dependientes_cont = c()
for (predictor in names(pred_cont)){
  test = wilcox.test(as.numeric(pred_cont[[predictor]]) ~ N_mujeres$SleepTrouble)
  if(test$p.value<0.05){
    dependientes_cont = c(dependientes_cont, predictor)
  }
}
print(dependientes_cont)


# DEPENDENCIA entre variable respuesta (binaria) y predictores categóricos: Chi cuadrado
pred_cat = N_mujeres[,-c(vars_cont,vars_binarias,28)]
summary(pred_cat)
# Vemos que las frecuencias son >= 5 -> bien
dependientes_cat = c()
for (predictor in names(pred_cat)){
  print(table(N_mujeres$SleepTrouble, pred_cat[[predictor]]))
  test = chisq.test(table(N_mujeres$SleepTrouble, pred_cat[[predictor]]))
  if(test$p.value<0.05){
    dependientes_cat = c(dependientes_cat, predictor)
  }
}
print(dependientes_cat)

# DEPENDENCIA entre variable respuesta (binaria) y predictores categóricos binario: Fisher
pred_bin = N_mujeres[,vars_binarias]
summary(pred_bin)
dependientes_bin = c()
for (predictor in names(pred_bin)){
  print(table(N_mujeres$SleepTrouble, pred_bin[[predictor]]))
  test = fisher.test(table(N_mujeres$SleepTrouble, pred_bin[[predictor]]))
  if(test$p.value<0.05){
    dependientes_bin = c(dependientes_bin, predictor)
  }
}
print(dependientes_bin)


################################################################################
###### VARIABLES DUMMYS ########################################################
################################################################################
library(fastDummies)

# Hacemos dummies del conjunto sin escalar
N_mujeres_dummy = dummy_cols(N_mujeres,
                             select_columns = columnas_factor)

summary(N_mujeres_dummy) # Datos con dummies

summary(N_mujeres) # datos sin dummies

# Cambiamos algunos nombres de variables para que no den problemas
colnames(N_mujeres_dummy)[colnames(N_mujeres_dummy) == "Education_8th Grade"] = "Education_8th_Grade"
colnames(N_mujeres_dummy)[colnames(N_mujeres_dummy) == "Education_9 - 11th Grade"] = "Education_9_11th_Grade"
colnames(N_mujeres_dummy)[colnames(N_mujeres_dummy) == "Education_High School"] = "Education_High_School"
colnames(N_mujeres_dummy)[colnames(N_mujeres_dummy) == "Education_Some College"] = "Education_Some_College"
colnames(N_mujeres_dummy)[colnames(N_mujeres_dummy) == "Education_College Grad"] = "Education_College_Grad"
names(N_mujeres_dummy)

################################################################################
##### BALANCEO DE LA CLASE YES EN EL TRAIN #####################################
################################################################################

# Cambiamos el orden de los niveles en la categoría respuesta porque se necesita en twoClassSummary
N_mujeres$SleepTrouble <- factor(N_mujeres$SleepTrouble,
                                 levels=rev(levels(N_mujeres$SleepTrouble)))
N_mujeres_dummy$SleepTrouble <- factor(N_mujeres_dummy$SleepTrouble,
                                       levels=rev(levels(N_mujeres_dummy$SleepTrouble)))

# Creamos el conjunto de entrenamiento y test
n = nrow(N_mujeres)
set.seed(345)
entreno = sample(1:n,size=floor(0.75*n))

# El conjunto de entreno lo creamos solo para las dummies, pues son necesarias para
# balancear con SMOTE

# División para los datos sin modificar
datos.entreno.N_mujeres = N_mujeres[entreno,]
datos.test.N_mujeres = N_mujeres[-entreno,]

# División para los datos con dummies
datos.entreno.N_mujeres_dummy = N_mujeres_dummy[entreno,]
datos.test.N_mujeres_dummy = N_mujeres_dummy[-entreno,]

# SMOTE para balancear las clases en los conjuntos de entreno
library(smotefamily)
set.seed(345)

### Balanceamos primero los datos sin escalar CON dummies
vars_fact = c(2,3,4,5,6,7,17,18,19,20,21,23,24,25,26,27,28)
datos.entreno.N_mujeres_dummy$SleepTrouble = as.factor(ifelse(datos.entreno.N_mujeres_dummy$SleepTrouble == 'Yes',1,0))

smote_result = SMOTE(datos.entreno.N_mujeres_dummy[,-vars_fact], datos.entreno.N_mujeres_dummy[,28], K = 5, dup_size = 1)
datos_balanceo = smote_result$data
summary(datos_balanceo)

colnames(datos_balanceo)[colnames(datos_balanceo) == "class"] = "SleepTrouble"
datos_balanceo$SleepTrouble = as.factor(ifelse(datos_balanceo$SleepTrouble == 1,'Yes','No'))
datos_balanceo$SleepTrouble <- factor(datos_balanceo$SleepTrouble,
                                               levels=rev(levels(datos_balanceo$SleepTrouble)))
summary(datos_balanceo)

# Nos han salido valores entre 0 y 1 en las dummies al interpolar -> recuperamos los niveles 0 y 1
for(i in 12:77){
  datos_balanceo[,i] = ifelse(datos_balanceo[,i]>0.5,1,0)
}
summary(datos_balanceo)

# Una vez hecho el SMOTE, para conseguir los datos de entreno sin dummy tenemos que mirar todas 
# las columnas de cada variable y ver cual a qué nivel corresponde

# Creamos las columnas factor
 
for (i in 1:length(columnas_factor)){
  datos_balanceo[[columnas_factor[i]]] = NA
}

# Identificamos las columnas de las dummy de cada variable
n = nrow(datos_balanceo)
edu = 12:16
marst = 17:22
hhinc = 23:34
home = 35:36
work = 37:39
diab = 40:41
health = 42:46
mhealth = 47:48
litint = 49:51
depres = 52:54
fisact = 55:56
tv = 57:63
pc = 64:70
alcohol = 71:72
smoke = 73:74
hr = 75:77

# Identificamos para cada registro el nivel de la variable
for (i in 1:n){
   
   level = which(datos_balanceo[i,edu]==TRUE)
   datos_balanceo[i,'Education'] = levels(datos.entreno.N_mujeres$Education)[level]
  
   level = which(datos_balanceo[i,marst]==TRUE)
   datos_balanceo[i,'MaritalStatus'] = levels(datos.entreno.N_mujeres$MaritalStatus)[level]

   level = which(datos_balanceo[i,hhinc]==TRUE)
   datos_balanceo[i,'HHIncomeMid'] = levels(datos.entreno.N_mujeres$HHIncomeMid)[level]

   level = which(datos_balanceo[i,hr]==TRUE)
   datos_balanceo[i,'HomeRooms'] = levels(datos.entreno.N_mujeres$HomeRooms)[level]
   
   level = which(datos_balanceo[i,home]==TRUE)
   datos_balanceo[i,'HomeOwn'] = levels(datos.entreno.N_mujeres$HomeOwn)[level]

   level = which(datos_balanceo[i,work]==TRUE)
   datos_balanceo[i,'Work'] = levels(datos.entreno.N_mujeres$Work)[level]

   level = which(datos_balanceo[i,diab]==TRUE)
   datos_balanceo[i,'Diabetes'] = levels(datos.entreno.N_mujeres$Diabetes)[level]

   level = which(datos_balanceo[i,health]==TRUE)
   datos_balanceo[i,'HealthGen'] = levels(datos.entreno.N_mujeres$HealthGen)[level]

   level = which(datos_balanceo[i,mhealth]==TRUE)
   datos_balanceo[i,'HasMentHlthBad'] = levels(datos.entreno.N_mujeres$HasMentHlthBad)[level]

   level = which(datos_balanceo[i,litint]==TRUE)
   datos_balanceo[i,'LittleInterest'] = levels(datos.entreno.N_mujeres$LittleInterest)[level]

   level = which(datos_balanceo[i,depres]==TRUE)
   datos_balanceo[i,'Depressed'] = levels(datos.entreno.N_mujeres$Depressed)[level]

   level = which(datos_balanceo[i,fisact]==TRUE)
   datos_balanceo[i,'PhysActive'] = levels(datos.entreno.N_mujeres$PhysActive)[level]

   level = which(datos_balanceo[i,tv]==TRUE)
   datos_balanceo[i,'TVHrsDay'] = levels(datos.entreno.N_mujeres$TVHrsDay)[level]

   level = which(datos_balanceo[i,pc]==TRUE)
   datos_balanceo[i,'CompHrsDay'] = levels(datos.entreno.N_mujeres$CompHrsDay)[level]

   level = which(datos_balanceo[i,alcohol]==TRUE)
   datos_balanceo[i,'Alcohol12PlusYr'] = levels(datos.entreno.N_mujeres$Alcohol12PlusYr)[level]

   level = which(datos_balanceo[i,smoke]==TRUE)
   datos_balanceo[i,'Smoke100'] = levels(datos.entreno.N_mujeres$Smoke100)[level]
   
}

# Pasamos a factor
datos_balanceo[columnas_factor] = lapply(datos_balanceo[columnas_factor],as.factor)
# Ordenamos las categóricas
l = levels(as.factor(datos_balanceo$HHIncomeMid))
datos_balanceo$HHIncomeMid = factor(datos_balanceo$HHIncomeMid, levels = l,ordered = TRUE)
l = levels(datos_balanceo$Education)
datos_balanceo$Education = factor(datos_balanceo$Education,levels = l,ordered = TRUE)
l = levels(datos_balanceo$HealthGen)
l = rev(l)
datos_balanceo$HealthGen = factor(datos_balanceo$HealthGen,levels = l,ordered = TRUE)
l = levels(datos_balanceo$LittleInterest)
datos_balanceo$LittleInterest = factor(datos_balanceo$LittleInterest,levels = l,ordered = TRUE)
l = levels(datos_balanceo$Depressed)
datos_balanceo$Depressed = factor(datos_balanceo$Depressed,levels = l,ordered = TRUE)
l = levels(datos_balanceo$TVHrsDay)
datos_balanceo$TVHrsDay = factor(datos_balanceo$TVHrsDay,levels = l,ordered = TRUE)
l = levels(datos_balanceo$CompHrsDay)
datos_balanceo$CompHrsDay = factor(datos_balanceo$CompHrsDay,levels = l,ordered = TRUE)
l = levels(datos_balanceo$HomeRooms)
datos_balanceo$HomeRooms = factor(datos_balanceo$HomeRooms,levels = l,ordered = TRUE)
summary(datos_balanceo)

# Eliminamos las variables dummy -> datos.entreno.N_mujeres
datos.entreno.N_mujeres = datos_balanceo[,-c(12:77)]
SleepTrouble = datos.entreno.N_mujeres[,12]
datos.entreno.N_mujeres = data.frame(datos.entreno.N_mujeres[,-12],SleepTrouble) # colocamos la variable respuesta al final
summary(datos.entreno.N_mujeres)

# Ordenamos las variables
vars_fact = c(2,3,4,5,6,7,17,18,19,20,21,23,24,25,26,27,28)
aux = datos.test.N_mujeres[,vars_fact]
datos.test.N_mujeres = datos.test.N_mujeres[,-vars_fact]
datos.test.N_mujeres = data.frame(datos.test.N_mujeres,aux)
HomeRooms = datos.test.N_mujeres[,15]
datos.test.N_mujeres = data.frame(datos.test.N_mujeres[,-15],HomeRooms) 
SleepTrouble = datos.test.N_mujeres[,27]
datos.test.N_mujeres = data.frame(datos.test.N_mujeres[,-27],SleepTrouble) 
names(datos.entreno.N_mujeres) == names(datos.test.N_mujeres)

# Eliminamos las variables dummy repetidas y las factor del conjunto de datos con dummies -> datos.entreno.N_mujeres_dummy
eliminar_dummy = c(12,17,23,35,37,40,42,47,49,52,55,57,64,71,73,75)
eliminar = c(eliminar_dummy,79:94)
datos.entreno.N_mujeres_dummy = datos_balanceo[,-eliminar]

vars_fact = vars_fact[-length(vars_fact)]
datos.test.N_mujeres_dummy = datos.test.N_mujeres_dummy[,-vars_fact]
SleepTrouble = datos.test.N_mujeres_dummy[,12]
datos.test.N_mujeres_dummy = data.frame(datos.test.N_mujeres_dummy[,-12],SleepTrouble)
datos.test.N_mujeres_dummy = datos.test.N_mujeres_dummy[,-eliminar_dummy]
summary(datos.entreno.N_mujeres_dummy)
names(datos.entreno.N_mujeres_dummy) == names(datos.test.N_mujeres_dummy)


################################################################################
##### ESCALADO ################################################################
################################################################################

library(VIM)
library(scales)

vars_cont = 1:11

#Escalado de los datos SIN dummy, tanto entreno como el test
datos.entreno.N_escalado = datos.entreno.N_mujeres
for (i in vars_cont){
  datos.entreno.N_escalado[,i] = rescale(datos.entreno.N_escalado[,i],to=c(-1,1))
}
summary(datos.entreno.N_escalado)

datos.test.N_escalado = datos.test.N_mujeres
for (i in vars_cont){
  datos.test.N_escalado[,i] = rescale(datos.test.N_escalado[,i],to=c(-1,1))
}
summary(datos.test.N_escalado)

#Escalado de los datos CON dummy, tanto entreno como el test
datos.entreno.N_escalado_dummy = datos.entreno.N_mujeres_dummy
for (i in vars_cont){
  datos.entreno.N_escalado_dummy[,i] = rescale(datos.entreno.N_escalado_dummy[,i],to=c(-1,1))
}
summary(datos.entreno.N_escalado_dummy)

datos.test.N_escalado_dummy = datos.test.N_mujeres_dummy
for (i in vars_cont){
  datos.test.N_escalado_dummy[,i] = rescale(datos.test.N_escalado_dummy[,i],to=c(-1,1))
}
summary(datos.test.N_escalado_dummy)


################################################################################
##### AJUSTE DE MODELOS DE CLASIFICACION #######################################
################################################################################


# Ahora ya tenemos los datos de entrenamiento y test con dummies:
# datos.entreno.N_mujeres, datos.entreno.N_mujeres_dummy, datos.entreno.N_escalado,datos.entreno.N_escalado_dummy
# datos.test.N_mujeres, datos.test.N_mujeres_dummy, datos.test.N_escalado,datos.test.N_escalado_dummy


# Creamos los folds
library(caret)
set.seed(345)
folds = createFolds(datos.entreno.N_mujeres$SleepTrouble, k = 5)
set.seed(345)
folds.dummy = createFolds(datos.entreno.N_mujeres_dummy$SleepTrouble, k = 5)
set.seed(345)
folds.escalado = createFolds(datos.entreno.N_escalado$SleepTrouble, k = 5)
set.seed(345)
folds.escalado.dummy = createFolds(datos.entreno.N_escalado_dummy$SleepTrouble, k = 5)


# Comprobamos que se hayan creado los mismos folds
for (i in 1:5){
  print(which(folds[[i]] != folds.dummy[[i]]))
  print(which(folds.dummy[[i]] != folds.escalado[[i]]))
  print(which(folds.escalado[[i]] != folds.escalado.dummy[[i]]))
}

# Metricas 
modelo = c()
acc = c()
errorv = c()
sens = c()
esp = c()
vppv = c()
vpnv = c()
f1v = c()
roc = c()
dummy = c()
escalado = c()
tablas = list()


################################################################################
###### REGRESIÓN LOGÍSTICA #####################################################
################################################################################


contrasts(datos.entreno.N_mujeres_dummy$SleepTrouble)
# Modelo para datos sin escalar
e = c() #variables que vamos a ir quitando de la regresión porque su p-valor>0.05
parada= TRUE
i=0
set.seed(345)
while (parada) {
  model.rglog.dum<-train(SleepTrouble~.,
                     data=datos.entreno.N_mujeres_dummy[, setdiff(names(datos.entreno.N_mujeres_dummy), e),drop = FALSE],
                     method="glm",
                     family=binomial,
                     trControl=trainControl(method="repeatedcv",number=5, index = folds.dummy, classProbs=TRUE,
                                            summaryFunction=twoClassSummary),
                     metric = 'Sens')
  model.rglog.dum
  p_valores = summary(model.rglog.dum$finalModel)$coefficients[,4]
  
  if (all(p_valores[-1]<0.05)){
    parada = FALSE
  }else{
    indice_mayor_p = which.max(p_valores[-1])
    mayor_p = colnames(datos.entreno.N_mujeres_dummy[, setdiff(names(datos.entreno.N_mujeres_dummy), e)])[indice_mayor_p]
    e = c(e,mayor_p)
    
    cat("Eliminando variable:", mayor_p, "con p =", max(p_valores[-1]), "\n")
  }
  i=i+1
}
model.rglog.dum
summary(model.rglog.dum$finalModel)

#CAMBIAMOS EL UMBRAL
probs<-predict(model.rglog.dum,datos.test.N_mujeres_dummy, type="prob")
umbral<- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres_dummy$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['REG-LOG1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l",main="Regresión logística")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'REG-LOG1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')


### SELECCIÓN DE MODELOS

customSummary_sens <- function(data, lev = NULL, model = NULL) {
  # data$obs: clases verdaderas
  # data$pred: clases predichas (factor)
  
  cm <- confusionMatrix(data$pred, data$obs, positive = "Yes")
  out <- c(Sensitivity = cm$byClass["Sensitivity"])
  return(out)
}

my_lrFuncs_sens <- lrFuncs
my_lrFuncs_sens$summary <- customSummary_sens

set.seed(345)

sensibilidad.rfe = list()
for (i in 1:61){
  print(i)
  lrRFE_sens <- rfe(SleepTrouble ~ .,
                    data = datos.entreno.N_mujeres_dummy,
                    sizes = i,
                    rfeControl = rfeControl(functions = my_lrFuncs_sens,
                                            method = "repeatedcv",
                                            number = 5,
                                            index = folds.dummy,
                                            returnResamp = "final"),
                    metric = "Sensitivity")
  set.size = i
  lm.vars = lrRFE_sens$variables # dataframe que tiene las variables con i predictores y su importancia
  lm.set = lm.vars[lm.vars$Variables == set.size, ] # Elige las variables de tamaño i en todos los folds
  
  # En cada fold, vemos de los i predictores la importancia de cada uno y hacemos 
  # la media de importancia de cada predictor
  lm.set <- aggregate(lm.set[, c("Overall")], list(lm.set$var), mean)
  
  # Ordenamos por importancia de las variables de mayor a menor y cogemos las i primeras
  lm.order <- order(lm.set[, c("x")], decreasing = TRUE)[1:set.size]
  lm.modeli = lm.set[lm.order, ] # matriz ix2 con las variables y su importancia
  
  # Ajustamos el modelo con estos i predictores y vemos si son significativos
  formula <- as.formula(paste("SleepTrouble", "~",
                                    paste(lm.modeli[,1], collapse = " + ")))
  
  model.best.subset<-train(formula,
                          data=datos.entreno.N_mujeres_dummy,
                          method="glm",
                          family=binomial,
                          trControl=trainControl(method="repeatedcv",number=5, index = folds.dummy, classProbs=TRUE,
                                                summaryFunction=twoClassSummary),
                          metric = 'Sens')
  p_valores = summary(model.best.subset$finalModel)$coefficients[,4]
  
  if (all(p_valores[-1] < 0.05)) {
    # la sensibilidad del modelo y los predictores
    sensibilidad.rfe[[paste0("modelo", i)]] = list(lrRFE_sens$results[1,2],lm.modeli[,1]) # la sensibilidad del modelo y los predictores  
  }
  
}

sensibilidad.rfe # Mejor con el modelo de 2 predictores

# Entrenamos el modelo con esos dos predictores
model.best.subset<-train(SleepTrouble ~ Height + CompHrsDay_1_hr,
                         data=datos.entreno.N_mujeres_dummy,
                         method="glm",
                         family=binomial,
                         trControl=trainControl(method="repeatedcv",number=5, index = folds.dummy, classProbs=TRUE,
                                                summaryFunction=twoClassSummary),
                         metric = 'Sens')

probs<-predict(model.best.subset,datos.test.N_mujeres_dummy, type="prob")
umbral<- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres_dummy$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['REG-LOG-BEST-FIT']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l",main="Selección de modelos")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'REG-LOG-BEST-FIT')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
##### REGULARIZACION ###########################################################
################################################################################

# Con el modelo de 20 predictores significativos
ind = numeric(length = nrow(datos.entreno.N_mujeres))
for (i in 1:5){
  fold = folds[[i]]
  ind[fold] = i
}


contrasts(datos.entreno.N_mujeres_dummy$SleepTrouble)
predictores = setdiff(names(datos.entreno.N_mujeres_dummy), e)
predictores = setdiff(predictores,'SleepTrouble')
x.train = model.matrix(SleepTrouble~.,datos.entreno.N_mujeres_dummy)[,predictores]
x.test = model.matrix(SleepTrouble~.,datos.test.N_mujeres_dummy)[,predictores]
y.train = datos.entreno.N_mujeres_dummy$SleepTrouble
y.test = datos.test.N_mujeres_dummy$SleepTrouble
gridlambda = 10^seq(5,-5,length=100)
library(glmnet)

##### LASSO #####
set.seed(345)
sensibilidad_landa = numeric(length(gridlambda))
for (l in 1:length(gridlambda)){
  landa = gridlambda[l]
  fold_sensibilidad = numeric(5)
  for(k in 1:5){
    modelo.lasso = glmnet(x.train[which(ind!=k),],y.train[which(ind!=k)],family=binomial,alpha=1,lambda = landa)
    pred_prob = predict(modelo.lasso, newx = x.train[which(ind==k),],s = landa, type = "response")
    # Umbral 0.4 -> 0.6 para el 'No'
    pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
    pred_class <- as.vector(pred_class)
    pred_class <- as.factor(pred_class)
    pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
    cm = confusionMatrix(pred_class, y.train[which(ind==k)])
    fold_sensibilidad[k] = cm$byClass[1]
  }
  sensibilidad_landa[l] = mean(fold_sensibilidad)
}

bestlam = gridlambda[which.max(sensibilidad_landa)]
cv.lasso = glmnet(x.train,y.train,family=binomial,alpha=1,lambda = gridlambda[65])
# Ahora predict me saca las probabilidades que considera como positiva (No)
pred_prob = predict(cv.lasso, newx = x.test,s = bestlam, type = "response")
# Umbral 0.4 -> 0.6 para el 'No'
pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
pred_class <- as.vector(pred_class)
pred_class <- as.factor(pred_class)
pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
# Cuando todo lo manda a un nivel, añadimos el otro
# if(levels(pred_class)=='Yes'){
#   levels(pred_class) = c(levels(pred_class),'No')
# }else if(levels(pred_class)=='No'){
#   levels(pred_class) = c(levels(pred_class),'Yes')
# }
confusionMatrix(pred_class, y.test)
(cm<-table(pred_class,y.test))
tablas[['LASSO']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
pred <- prediction(as.numeric(pred_class),as.numeric(y.test))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l",main="Lasso")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'LASSO')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

coef.lasso = predict(cv.lasso, newx = x.test,s = bestlam, type = "coefficients")
length(coef.lasso[coef.lasso!=0])
length(coef.lasso)
# Vemos que hace 0 cuatro coeficientes

##### RIDGE #####
set.seed(345)
sensibilidad_landa = numeric(length(gridlambda))
for (l in 1:length(gridlambda)){
  landa = gridlambda[l]
  fold_sensibilidad = numeric(5)
  for(k in 1:5){
    modelo.ridge = glmnet(x.train[which(ind!=k),],y.train[which(ind!=k)],family=binomial,alpha=0,lambda = landa)
    pred_prob = predict(modelo.ridge, newx = x.train[which(ind==k),],s = landa, type = "response")
    # Umbral 0.4 -> 0.6 para el 'No'
    pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
    pred_class <- as.vector(pred_class)
    pred_class <- as.factor(pred_class)
    pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
    cm = confusionMatrix(pred_class, y.train[which(ind==k)])
    fold_sensibilidad[k] = cm$byClass[1]
  }
  sensibilidad_landa[l] = mean(fold_sensibilidad)
}

bestlam = gridlambda[which.max(sensibilidad_landa)]
cv.ridge = glmnet(x.train,y.train,family=binomial,alpha=0,lambda = gridlambda[55])
# Ahora predict me saca las probabilidades que considera como positiva (No)
pred_prob = predict(cv.ridge, newx = x.test,s = bestlam, type = "response")
# Umbral 0.4 -> 0.6 para el 'No'
pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
pred_class <- as.vector(pred_class)
pred_class <- as.factor(pred_class)
pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
# Cuando todo lo manda al a un nivel, añadimos el otro
# if(levels(pred_class)=='Yes'){
#   levels(pred_class) = c(levels(pred_class),'No')
# }else if(levels(pred_class)=='No'){
#   levels(pred_class) = c(levels(pred_class),'Yes')
# }
confusionMatrix(pred_class, y.test)
(cm<-table(pred_class,y.test))
tablas[['RIDGE']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
pred <- prediction(as.numeric(pred_class),as.numeric(y.test))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l",main="Ridge")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'RIDGE')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

coef.ridge = predict(cv.ridge, newx = x.test,s = bestlam, type = "coefficients")
length(coef.ridge[coef.ridge!=0])
length(coef.ridge)
# Vemos que ningun coeficiente es 0, por lo que no elimina predictores

##### ELASTIC NET #####
sens_alfa_landa = matrix(nrow=length(gridlambda),ncol=21)
for (i in 0:20) {
  name <- paste0("alpha", i/20)
  sensibilidad_landa = numeric(length(gridlambda))
  for (l in 1:length(gridlambda)){
    landa = gridlambda[l]
    fold_sensibilidad = numeric(5)
    for(k in 1:5){
      modelo.en = glmnet(x.train[which(ind!=k),],y.train[which(ind!=k)],family=binomial,alpha=i/20,lambda = landa)
      pred_prob = predict(modelo.en, newx = x.train[which(ind==k),],s = landa, type = "response")
      # Umbral 0.4 -> 0.6 para el 'No'
      pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
      pred_class <- as.vector(pred_class)
      pred_class <- as.factor(pred_class)
      pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
      cm = confusionMatrix(pred_class, y.train[which(ind==k)])
      fold_sensibilidad[k] = cm$byClass[1]
    }
    sensibilidad_landa[l] = mean(fold_sensibilidad)
  }
  sens_alfa_landa[,i+1] = sensibilidad_landa
  aux = sensibilidad_landa[sensibilidad_landa<0.8]
  maximo = max(aux)
  bestlam = gridlambda[which(sensibilidad_landa==maximo)[1]]
}



aux = sens_alfa_landa[sens_alfa_landa<0.8]
maximo = max(aux)
indices = which(sens_alfa_landa==maximo,arr.ind = TRUE)[1,]

bestlam = indices[1]
bestalfa = (indices[2]-1)/20

cv.en = glmnet(x.train,y.train,family=binomial,alpha=bestalfa,lambda = bestlam)
# Ahora predict saca las probabilidades que considera como positiva (No)
pred_prob = predict(cv.en, newx = x.test,s = bestlam, type = "response")
# Umbral 0.4 -> 0.6 para el 'No'
pred_class <- ifelse(pred_prob < 0.6, 'Yes', 'No')
pred_class <- as.vector(pred_class)
pred_class <- as.factor(pred_class)
pred_class <- factor(pred_class,levels=rev(levels(pred_class)))
# # Cuando todo lo manda al a un nivel, añadimos el otro
# if(levels(pred_class)=='Yes'){
#   levels(pred_class) = c(levels(pred_class),'No')
# }else if(levels(pred_class)=='No'){
#   levels(pred_class) = c(levels(pred_class),'Yes')
# }
confusionMatrix(pred_class, y.test)


################################################################################
###### LDA #####################################################################
################################################################################

# Comprobación de las condiciones del LDA
# NORMALIDAD
library(reshape2)
library(knitr)
library(dplyr)
library(nortest)

# variables continuas en los datos:
vars_cont=c("Age","Weight","Height","Pulse",
                           "BPSysAve","BPDiaAve","Testosterone","DirectChol",
                           "TotChol","UrineVol1","SleepHrsNight",
                           "SleepTrouble")

# ESTUDIO DE NORMALIDAD
datos_tidy <- melt(datos.entreno.N_mujeres_dummy[,vars_cont], value.name = "valor")
kable(datos_tidy %>%
        group_by(SleepTrouble, variable) %>%
        summarise(p_value_KSL.test = lillie.test(valor)$p.value))

# En la clase no salen normal: Height, BPDiaAve
# En la clase yes salen normal: BPDiaAve, Height, TotChol
# Normalidad multivariada
library(MVN)
predictores=datos.entreno.N_mujeres_dummy[,-62]
predictores
result <- mvn(data = predictores, mvnTest = "royston")
result$multivariateNormality
# No sigue una normalidad multivariada

# HOMOGENEIDAD DE VARIANZAS
library(biotools)
boxM(data = datos.entreno.N_mujeres_dummy[,-62], grouping = datos.entreno.N_mujeres_dummy$SleepTrouble)
# No se puede asumir igualdad de covarianzas

library(caret)
set.seed(345)
model.lda5<-train(SleepTrouble~.,
                  data=datos.entreno.N_mujeres_dummy,
                  method="lda",
                  trControl=trainControl(method="repeatedcv",number=5, index = folds.dummy)) 
model.lda5

lda.pred5.dum<-predict(model.lda5,datos.test.N_mujeres_dummy,type='prob')
umbral<- 0.4
pred_clase <- ifelse(lda.pred5.dum[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres_dummy$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['LDA1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l",main="LDA")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'LDA1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')




##### LDA MEJORA #####
# ESTANDARIZACION
library(dlookr)
datos.lda.mejora.entreno = datos.entreno.N_mujeres_dummy
datos.lda.mejora.test = datos.test.N_mujeres_dummy

l=numeric(11)

for (i in 1:11){
  library(MASS)
  x=datos.lda.mejora.entreno[,i]
  x=as.numeric(x)
  b <- boxcox(lm(x ~ 1))
  # Lambda exacto
  lambda <- b$x[which.max(b$y)]
  l[i] = lambda # Vector con los lambda utilizados
  datos.lda.mejora.entreno[,i] <- (x^lambda-1)/lambda
  
  x=datos.lda.mejora.test[,i]
  x=as.numeric(x)
  datos.lda.mejora.test[,i] <- (x^lambda-1)/lambda
  
  #datos.lda.mejora.entreno[,i] = transform(datos.lda.mejora.entreno[,i],method = 'zscore')
  #datos.lda.mejora.test[,i] = transform(datos.lda.mejora.test[,i],method = 'zscore')
}

# ESTUDIO DE NORMALIDAD
datos_tidy <- melt(datos.lda.mejora.entreno[,vars_cont], value.name = "valor")
kable(datos_tidy %>%
        group_by(SleepTrouble, variable) %>%
        summarise(p_value_KSL.test = lillie.test(valor)$p.value))

# Mejora algo la normalidad
# 
# Normalidad multivariada
library(MVN)
predictores=datos.lda.mejora.entreno[,-62]
predictores
result <- mvn(data = predictores, mvnTest = "royston")
result$multivariateNormality
# No sigue una normalidad multivariada

# HOMOGENEIDAD DE VARIANZAS
library(biotools)
boxM(data = datos.lda.mejora.entreno[,-62], grouping = datos.entreno.N_mujeres_dummy$SleepTrouble)
# No se puede asumir igualdad de covarianzas

# NO CONSEGUIMOS MEJORAR LAS HIPÓTESIS

set.seed(345)
model.lda5<-train(SleepTrouble~.,
                  data=datos.lda.mejora.entreno,
                  method="lda",
                  trControl=trainControl(method="repeatedcv",number=5, index = folds.dummy)) 
model.lda5

lda.pred5.dum<-predict(model.lda5,datos.lda.mejora.test,type='prob')
umbral<- 0.4
pred_clase <- ifelse(lda.pred5.dum[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.lda.mejora.test$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.lda.mejora.test$SleepTrouble))
tablas[['LDAMEJORA']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l", main="LDA con normalización")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'LDAMEJORA')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
###### KNN #####################################################################
################################################################################
library(class)


# Maximizando la sensibilidad, datos sin dummies escalados
set.seed(345)
modelo.knn.esc = train(SleepTrouble~.,
                   data=datos.entreno.N_escalado,
                   method="knn",
                   tuneGrid=data.frame(k = seq(1,21,2)),
                   trControl=trainControl(method="repeatedcv", 
                                          number=5, 
                                          index=folds,
                                          classProbs=TRUE,
                                          summaryFunction=twoClassSummary),
                   metric="Sens")         # Optimiza segun la sensibilidad


plot(modelo.knn.esc) 
modelo.knn.esc 

# Ajustamos el KNN para k = 3 y para k = 5, y vemos cual obtiene mejor rendimiento en el test 

# Ajustamos el KNN para k = 3
set.seed(345)
modelo.knn.esc = train(SleepTrouble~.,
                       data=datos.entreno.N_escalado,
                       method="knn",
                       tuneGrid=data.frame(k = 3),
                       trControl=trainControl(method="repeatedcv", 
                                              number=5, 
                                              index=folds.escalado,
                                              classProbs=TRUE,
                                              summaryFunction=twoClassSummary),
                       metric="Sens") 

knn.pred.esc = predict(modelo.knn.esc,datos.test.N_escalado,type='prob')
umbral<- 0.4
pred_clase <- ifelse(knn.pred.esc[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_escalado$SleepTrouble,positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres$SleepTrouble))
tablas[['KNN-K3']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
par(mfrow=c(1,1))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="KNN con k=3")
abline(a=0,b=1)
#Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'KNN-K3')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'Yes')

# Ajustamos el KNN para k = 5
set.seed(345)
modelo.knn.esc = train(SleepTrouble~.,
                       data=datos.entreno.N_escalado,
                       method="knn",
                       tuneGrid=data.frame(k = 5),
                       trControl=trainControl(method="repeatedcv", 
                                              number=5, 
                                              index=folds.escalado,
                                              classProbs=TRUE,
                                              summaryFunction=twoClassSummary),
                       metric="Sens") 

knn.pred.esc = predict(modelo.knn.esc,datos.test.N_escalado,type='prob')
umbral<- 0.4
pred_clase <- ifelse(knn.pred.esc[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_escalado$SleepTrouble,positive = "Yes")
(cm<-table(pred,datos.test.N_escalado$SleepTrouble))
tablas[['KNN-K5']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_escalado$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l", main=" KNN con k=5")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'KNN-K5')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'Yes')



################################################################################
###### NAIVE BAYES #############################################################
################################################################################
library(caret)
library(e1071)
library(klaR)


# Datos sin escalar
set.seed(345)
model.nb<-train(SleepTrouble~.,
                data=datos.entreno.N_mujeres_dummy,
                method="nb",
                trControl=trainControl(method="repeatedcv", 
                                       number=5, 
                                       index=folds.dummy,
                                       classProbs=TRUE,
                                       summaryFunction=twoClassSummary),
                metric="Sens")
model.nb
nb.pred<-predict(model.nb,datos.test.N_mujeres_dummy,type='prob')
umbral<- 0.4
pred_clase <- ifelse(nb.pred[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres_dummy$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['NAIVE1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l", main="Naive Bayes")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'NAIVE1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')


##### NAIVE BAYES CON MEJORAS #####
library(dlookr)
datos.naive.mejora.entreno = datos.entreno.N_mujeres_dummy
datos.naive.mejora.test = datos.test.N_mujeres_dummy
# 
# for (i in 1:11){
#   datos.naive.mejora.entreno[,i] = transform(datos.naive.mejora.entreno[,i],method = 'minmax')
#   datos.naive.mejora.test[,i] = transform(datos.naive.mejora.test[,i],method = 'minmax')
# }

# NOS QUEDAMOS CON VARIABLES CUALITATIVAS DEPENDIENTES 
# DE LA VARIABLE RESPUESTA PERO INDEPENDIENTES ENTRE SI: 
# 'Alcohol12PlusYr', 'Diabetes', 'HHIncomeMid', 'HasMentHlthBad' EN DUMMIES
datos.naive.mejora.entreno = datos.naive.mejora.entreno[,c(1:11,21:31,35,40,58,62)]
datos.naive.mejora.test = datos.naive.mejora.test[,c(1:11,21:31,35,40,58,62)]

set.seed(345)
model.nb<-train(SleepTrouble~.,
                data=datos.naive.mejora.entreno,
                method="nb",
                trControl=trainControl(method="repeatedcv", 
                                       number=5, 
                                       index=folds.dummy,
                                       classProbs=TRUE,
                                       summaryFunction=twoClassSummary),
                metric="Sens")
model.nb
nb.pred<-predict(model.nb,datos.naive.mejora.test,type='prob')
umbral<- 0.4
pred_clase <- ifelse(nb.pred[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.naive.mejora.test$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.naive.mejora.test$SleepTrouble))
tablas[['NAIVE mejora']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.naive.mejora.test$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l", main = "Naive bayes mejora")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'NAIVE mejora')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')


##### NAIVE BAYES CON MEJORAS 2 #####
# TRANSFORMACIÓN
library(MASS)

datos.naive.mejora.entreno = datos.entreno.N_mujeres_dummy
datos.naive.mejora.test = datos.test.N_mujeres_dummy

for (i in 1:11){
  x = datos.naive.mejora.entreno[,i]
  x = as.numeric(x)
  b = boxcox(lm(x ~ 1))
  # Lambda exacto
  lambda = b$x[which.max(b$y)]
  
  datos.naive.mejora.entreno[,i] = (x^lambda-1)/lambda
  
  x = datos.naive.mejora.test[,i]
  x = as.numeric(x)
  b = boxcox(lm(x ~ 1))
  # Lambda exacto
  lambda = b$x[which.max(b$y)]
  
  datos.naive.mejora.test[,i] = (x^lambda-1)/lambda
}

# NOS QUEDAMOS CON VARIABLES CUALITATIVAS DEPENDIENTES 
# DE LA VARIABLE RESPUESTA PERO INDEPENDIENTES ENTRE SI: 
# 'Alcohol12PlusYr', 'Diabetes', 'HHIncomeMid', 'HasMentHlthBad' EN DUMMIES
datos.naive.mejora.entreno = datos.naive.mejora.entreno[,c(1:11,21:31,35,40,58,62)]
datos.naive.mejora.test = datos.naive.mejora.test[,c(1:11,21:31,35,40,58,62)]

set.seed(345)
model.nb<-train(SleepTrouble~.,
                data=datos.naive.mejora.entreno,
                method="nb",
                trControl=trainControl(method="repeatedcv", 
                                       number=5, 
                                       index=folds.dummy,
                                       classProbs=TRUE,
                                       summaryFunction=twoClassSummary),
                metric="Sens")
model.nb
nb.pred<-predict(model.nb,datos.naive.mejora.test,type='prob')
umbral<- 0.4
pred_clase <- ifelse(nb.pred[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.naive.mejora.test$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.naive.mejora.test$SleepTrouble))
tablas[['NAIVE mejora 2']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creación de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.naive.mejora.test$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
par(mfrow=c(1,1))
plot(perf,colorize=TRUE,type="l", main= "Naive bayes mejora 2")
abline(a=0,b=1)
# Área bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'NAIVE mejora 2')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')



################################################################################
###### SVM #####################################################################
################################################################################
# Las variables cuantitativas ya están escaladas y las cualitativas ya son dummy

library(kernlab)
# Usamos los folds de antes. Creamos grids para hiperparámetros
C.grid = 10^(0:10)
deg.grid = 1:13
sigma.grid = 10^(-10:10)

# Cambiamos a 1 la clase Yes y a -1 la clase No
datos.entreno.svm = datos.entreno.N_escalado_dummy
datos.entreno.svm$SleepTrouble = factor(ifelse(datos.entreno.svm$SleepTrouble == 'Yes',1,-1))
datos.test.svm = datos.test.N_escalado_dummy
datos.test.svm$SleepTrouble = factor(ifelse(datos.test.svm$SleepTrouble == 'Yes',1,-1))


# Cambiamos el orden de los niveles de la categoría respuesta
datos.entreno.svm$SleepTrouble <- factor(datos.entreno.svm$SleepTrouble,
                                         levels=rev(levels(datos.entreno.svm$SleepTrouble)))
datos.test.svm$SleepTrouble <- factor(datos.test.svm$SleepTrouble,
                                      levels=rev(levels(datos.test.svm$SleepTrouble)))
summary(datos.entreno.svm)
summary(datos.test.svm)

##### LINEAL #####
set.seed(345)
modelo.svm.lin <- train(SleepTrouble ~ ., data = datos.entreno.svm,
                        method = "svmLinear",
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 5, 
                                                 index = folds, # No calcula curva ROC
                                                 summaryFunction = twoClassSummary),
                        metric = "Sens",
                        tuneGrid = expand.grid(C = C.grid),
                        prob.model = TRUE)
modelo.svm.lin

# Predicciones SVM lineal
probs <- predict(modelo.svm.lin, newdata = datos.test.svm,type='prob') 
umbral <- 0.4
pred_clase <- ifelse(probs[,"1"] >= umbral, "1", "-1")
pred.lin <- factor(pred_clase, levels = c("1", "-1"))
(cm<-table(pred.lin, datos.test.svm$SleepTrouble))
tablas[['SVM-LINEAL']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
# Creacion de curva ROC
pred <- prediction(as.numeric(pred.lin),as.numeric(datos.test.svm$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="SVM lineal")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'SVM-LINEAL')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'Yes')

##### POLINOMICO #####
set.seed(345)
modelo.svm.pol <- train(SleepTrouble ~ ., data = datos.entreno.svm,
                        method = "svmPoly",
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 5, 
                                                 index=folds,
                                                 summaryFunction=twoClassSummary),
                        metric="Sens",
                        tuneGrid = expand.grid(C = C.grid, degree = deg.grid,scale=1),
                        prob.model = TRUE)
modelo.svm.pol

#Predicciones SVM con kernel polinomico
probs <- predict(modelo.svm.pol, newdata = datos.test.svm,type='prob')
umbral <- 0.4
pred_clase <- ifelse(probs[,"1"] >= umbral, "1", "-1")
pred.pol <- factor(pred_clase, levels = c("1", "-1"))
(cm<-table(pred.pol, datos.test.svm$SleepTrouble))
tablas[['SVM-POL']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
# Creacion de curva ROC
pred <- prediction(as.numeric(pred.pol),as.numeric(datos.test.svm$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="SVM polinómico")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'SVM-POL')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'Yes')


##### RADIAL #####
set.seed(345)
modelo.svm.rad <- train(SleepTrouble ~ ., data = datos.entreno.svm,
                        method = "svmRadial",
                        trControl = trainControl(method = "repeatedcv", 
                                                 number = 5, 
                                                 index=folds,
                                                 summaryFunction=twoClassSummary),
                        metric="Sens",
                        tuneGrid = expand.grid(C = C.grid,sigma = sigma.grid),
                        prob.model = TRUE)
modelo.svm.rad

#Predicciones SVM con kernel radial
probs <- predict(modelo.svm.rad, newdata = datos.test.svm,type='prob')
umbral <- 0.4
pred_clase <- ifelse(probs[,"1"] >= umbral, "1", "-1")
pred.rad <- factor(pred_clase, levels = c("1", "-1"))
(cm<-table(pred.rad, datos.test.svm$SleepTrouble))
tablas[['SVM-RADIAL']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
# Creacion de curva ROC
pred <- prediction(as.numeric(pred.rad),as.numeric(datos.test.svm$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr", main= "SVM radial")
plot(perf,colorize=TRUE,type="l", main= "SVM radial")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'SVM-RADIAL')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'Yes')


# Vemos la importancia de cada variable
(imp.modelo.svm.lin=varImp(modelo.svm.lin))
(imp.modelo.svm.pol=varImp(modelo.svm.pol))
(imp.modelo.svm.rad=varImp(modelo.svm.rad))
par(mfrow=c(1,3))
plot(imp.modelo.svm.lin)
plot(imp.modelo.svm.pol)
plot(imp.modelo.svm.rad)


################################################################################
###### ARBOLES DE DECISION #####################################################
################################################################################
#Entreno el modelo de árbol de clasificacion con validación cruzada (k=5):

#Modelo sin dummies
#arbol de decision completo
set.seed(345)
modelo0 <- train(SleepTrouble ~ ., 
                 data = datos.entreno.N_mujeres, 
                 method = "rpart", 
                 trControl = trainControl(method = "repeatedcv",
                                          number = 5,
                                          index=folds),
                 tuneGrid  = data.frame(cp=0),
                 parms = list(split = "gini"))
modelo0
arbol0 <- modelo0$finalModel 
cp0 <- modelo0$bestTune$cp
(imp.modelo0=varImp(modelo0))
library(rpart.plot)
par(mfrow=c(1,1))
rpart.plot(arbol0,
           box.palette = "GnBu", 
           branch.lty = 3,
           shadow.col = "gray",
           cex = 0.6 ) 
rpart.rules(arbol0, style = "tall")

printcp(arbol0)
head(arbol0$cptable, 10) #valores de las podas

#Métricas del arbol completo:
#cambiamos el umbral a 0.4:
probs0 <- predict(modelo0, newdata = datos.test.N_mujeres, type= "prob")
umbral = 0.4
pred_clase <- ifelse(probs0[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres$SleepTrouble))
tablas[['ARBOL ENTERO']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) # 0.6337209
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN)) #0.3220339
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main= "Árbol entero")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc") 
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'ARBOL ENTERO')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')

# Importancia de las variables del arbol completo
summary(arbol0)

#Con el cp optimo calculo el árbol podado:
set.seed(345)
modelo_podado <- train(SleepTrouble ~ ., 
                       data = datos.entreno.N_mujeres, 
                       method = "rpart", 
                       trControl = trainControl(method = "cv", number = 5, index = folds,
                                                classProbs = TRUE,
                                                summaryFunction=twoClassSummary),
                       metric="Sens",
                       tuneGrid = data.frame(cp = seq(0, 1, by = 0.001)),
                       parms = list(split = "gini"))
modelo_podado
arbol_podado <- modelo_podado$finalModel 
cp_podado <- modelo_podado$bestTune$cp # 0.08
# gráfica: 
library(rpart.plot)
rpart.plot(arbol_podado,
           box.palette = "GnBu", 
           branch.lty = 3,
           shadow.col = "gray") 
rpart.rules(arbol_podado, style = "tall") 

printcp(arbol_podado)
head(arbol_podado$cptable, 10) #valores de las podas
(imp.modelo.podado=varImp(modelo_podado))


#Métricas del arbol podado:
probspodado <- predict(modelo_podado, newdata = datos.test.N_mujeres, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probspodado[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres$SleepTrouble))
tablas[['ARBOL PODADO']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) 
(sensibilidad=VP/(VP+FN)) 
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres[["SleepTrouble"]]))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main= "Árbol podado")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]]) 

# Guardamos las metricas
modelo = c(modelo,'ARBOL PODADO')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')

#importancia de las variables del arbol podado
summary(arbol_podado)

#### el arbol completo tiene menor sensibilidad que el podado


#Modelo con dummies
#arbol de decision completo
set.seed(345)
modelo0.dum <- train(SleepTrouble ~ ., 
                     data = datos.entreno.N_mujeres_dummy, 
                     method = "rpart", 
                     trControl = trainControl(method = "repeatedcv",
                                              number = 5,
                                              index=folds.dummy,
                                              classProbs=TRUE,
                                              summaryFunction=twoClassSummary),
                     metric="Sens",
                     tuneGrid  = data.frame(cp=0),
                     parms = list(split = "gini"))
modelo0.dum
arbol0.dum <- modelo0.dum$finalModel 
cp0.dum <- modelo0.dum$bestTune$cp
par(mfrow=c(1,1))
rpart.plot(arbol0.dum,
           box.palette = "GnBu", 
           branch.lty = 3,
           shadow.col = "gray",
           cex = 0.6 ) 
rpart.rules(arbol0.dum, style = "tall")# reglas

printcp(arbol0.dum)
head(arbol0.dum$cptable, 10) #valores de las podas

# Importancia de las variables del arbol completo
summary(arbol0.dum)

#Métricas del arbol completo:
probs0 <- predict(arbol0.dum, newdata = datos.test.N_mujeres_dummy, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs0[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) 
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc") 
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])



#Con el cp optimo calculo el árbol podado:
set.seed(345)
modelo_podado.dum <- train(SleepTrouble ~ ., 
                           data = datos.entreno.N_mujeres_dummy, 
                           method = "rpart", 
                           trControl = trainControl(method = "cv", number = 5, index = folds.dummy),
                           tuneGrid = data.frame(cp = seq(0, 1, by = 0.001)),
                           parms = list(split = "gini"))
modelo_podado.dum
arbol_podado.dum <- modelo_podado.dum$finalModel 
cp_podado.dum <- modelo_podado.dum$bestTune$cp 
# gráfica: 
library(rpart.plot)
rpart.plot(arbol_podado.dum,
           box.palette = "GnBu", 
           branch.lty = 3,
           shadow.col = "gray") 
rpart.rules(arbol_podado.dum, style = "tall") 

printcp(arbol_podado.dum)
head(arbol_podado.dum$cptable, 10) #valores de las podas



#importancia de las variables del arbol podado
summary(arbol_podado.dum)

#Métricas del arbol podado:
probs0 <- predict(modelo_podado.dum, newdata = datos.test.N_mujeres_dummy, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs0[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) 
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN)) 
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy[["SleepTrouble"]]))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l")
abline(a=0,b=1)
# Area bajo la curva
AUC <- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]]) 

################################################################################
###### BAGGING #################################################################
################################################################################

# Modelo sin dummies
set.seed(345)
model.bagging = train(SleepTrouble~., method = "treebag", data = datos.entreno.N_mujeres,
                      trControl = trainControl(method = "repeatedcv", 
                                               number = 5, 
                                               index = folds,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary),
                      metric = "Sens")
model.bagging$results[2:5]
(imp.bagging=varImp(model.bagging))

probs<-predict(model.bagging,datos.test.N_mujeres, type="prob")
umbral<- 0.4 
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres$SleepTrouble))
tablas[['BAGGING1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="Bagging")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'BAGGING1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')


# Modelo con dummies
set.seed(345)
model.bagging.dum = train(SleepTrouble~., method = "treebag", data = datos.entreno.N_mujeres_dummy,
                      trControl = trainControl(method = "repeatedcv", 
                                               number = 5, 
                                               index = folds.dummy,
                                               classProbs = TRUE,
                                               summaryFunction = twoClassSummary),
                      metric = "Sens")
model.bagging.dum$results[2:5]
(imp.bagging=varImp(model.bagging.dum))

probs<-predict(model.bagging.dum,datos.test.N_mujeres_dummy, type="prob")
umbral<- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres_dummy$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['BAGGING2']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="Bagging dummy")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'BAGGING2')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
###### RANDOM FOREST  ##########################################################
################################################################################


# Modelo sin dummies
set.seed(345)
model.rf <- train(SleepTrouble~., method = "rf", data = datos.entreno.N_mujeres,
                  trControl = trainControl(method = "repeatedcv", 
                                           number = 5, 
                                           index = folds,
                                           classProbs = TRUE,
                                           summaryFunction = twoClassSummary),
                  metric = "Sens")
model.rf$results
i=which.max(model.rf$results[,3]) # Maxima sens
model.rf$results[i,2:4]
(imp.rf=varImp(model.rf))


probs<-predict(model.rf,datos.test.N_mujeres, type="prob")
umbral<- 0.4 
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred<-factor(pred_clase,levels = c("Yes", "No"))
confusionMatrix(pred,datos.test.N_mujeres$SleepTrouble, positive = "Yes")
(cm<-table(pred,datos.test.N_mujeres$SleepTrouble))
tablas[['RF1']] = cm

VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="Random Forest")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'RF1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')


# Modelo con dummies y umbral=0.4
set.seed(345)
model.rf.dum<-train(SleepTrouble~.,
                    data=datos.entreno.N_mujeres_dummy,
                    method="rf",
                    trControl=trainControl(method="repeatedcv", 
                                           number=5, 
                                           index=folds.dummy,
                                           classProbs=TRUE,
                                           summaryFunction=twoClassSummary),
                    metric = "Sens")

model.rf.dum$results

(imp.rf=varImp(model.rf.dum))

probs <- predict(model.rf.dum, newdata = datos.test.N_mujeres_dummy, type = "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive = 'Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['RF2']] = cm

VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="Random Forest dummy")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'RF2')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')


################################################################################
######  ADABOOSTING ############################################################
################################################################################
# Minimizando con la sensibilidad

# Modelo con dummies
set.seed(345)
model.ada.dum = train(SleepTrouble~., method = "ada", data = datos.entreno.N_mujeres_dummy,
                  trControl = trainControl(method = "repeatedcv",
                                           number = 5,
                                           index=folds,
                                           classProbs=TRUE,
                                           summaryFunction=twoClassSummary),
                  metric="Sens")
model.ada.dum
(imp.bagging=varImp(model.ada.dum))

probs <- predict(model.ada.dum, newdata = datos.test.N_mujeres_dummy, type = "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive = 'Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['ADA']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="Adaboosting")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])

# Guardamos las metricas
modelo = c(modelo,'ADA')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
######  STOCHASTIC GRADIENT BOOSTING (SGB)  ####################################
################################################################################

library(gbm)

# Modelo sin dummies
set.seed(345)
model.sgb <- train(SleepTrouble ~ ., method = "gbm", data = datos.entreno.N_mujeres,
                   trControl = trainControl(method = "repeatedcv", 
                                            number = 5,        
                                            index = folds,
                                            classProbs=TRUE,
                                            summaryFunction=twoClassSummary),
                   tuneGrid = expand.grid(n.trees = c(5000, 10000),             # Número de árboles
                                          interaction.depth = c(4),             # Profundidad de interacción
                                          shrinkage = c(0.001,0.01,0.05,0.1,0.2,0.3,0.4,0.5), # Valores de shrinkage
                                          n.minobsinnode = 5 ),                 # Número mínimo de observaciones por nodo
                   verbose = FALSE,
                   metric = 'Sens')  

model.sgb
model.sgb$results
(imp.sgb=varImp(model.sgb))
#cambiamos el umbral a 0.4:
probs <- predict(model.sgb, newdata = datos.test.N_mujeres, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres$SleepTrouble))
tablas[['SGB1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) 
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN)) 
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="SGB")
abline(a=0,b=1)
# Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]]) 

# Guardamos las metricas
modelo = c(modelo,'SGB1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')

# Modelo con dummies
set.seed(345)
model.sgb.dum <- train(SleepTrouble ~ ., method = "gbm", data = datos.entreno.N_mujeres_dummy,
                       trControl = trainControl(method = "repeatedcv", 
                                                number = 5,        
                                                index = folds.dummy,
                                                classProbs=TRUE,
                                                summaryFunction=twoClassSummary),
                       tuneGrid = expand.grid(n.trees = c(5000, 10000),             # Número de árboles
                                              interaction.depth = c(4),             # Profundidad de interacción
                                              shrinkage = c(0.001,0.01,0.05,0.1,0.2,0.3,0.4,0.5), # Valores de shrinkage
                                              n.minobsinnode = 5 ),                 # Número mínimo de observaciones por nodo
                       verbose = FALSE,
                       metric = 'Sens')  

model.sgb.dum
model.sgb.dum$results
(imp.sgb.dum=varImp(model.sgb.dum))

#cambiamos el umbral a 0.4:
probs <- predict(model.sgb.dum, newdata = datos.test.N_mujeres_dummy, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['SGB2']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP)) 
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN)) 
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main="SGB dummy")
abline(a=0,b=1)
# Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])


# Guardamos las metricas
modelo = c(modelo,'SGB2')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
######  EXTREME GRADIENT BOOSTING (XGB)  #######################################
################################################################################

set.seed(345)
model.xgb <- train(SleepTrouble ~ ., method = "xgbTree", data = datos.entreno.N_mujeres,
                   trControl = trainControl(method = "repeatedcv",number = 5,index = folds))

model.xgb
(imp.rf=varImp(model.xgb))

probs <- predict(model.xgb, newdata = datos.test.N_mujeres, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres$SleepTrouble))
tablas[['XGB1']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l",main="XGB")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])


# Guardamos las metricas
modelo = c(modelo,'XGB1')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'No')
escalado = c(escalado,'No')

# Modelo con dummies
set.seed(345)
model.xgb.dum <- train(SleepTrouble ~ ., method = "xgbTree", data = datos.entreno.N_mujeres_dummy,
                       trControl = trainControl(method = "repeatedcv",number = 5,index = folds))

model.xgb.dum
i=which.max(model.xgb.dum$results[,9]) 
(imp.rf=varImp(model.xgb.dum))

probs <- predict(model.xgb.dum, newdata = datos.test.N_mujeres_dummy, type= "prob")
umbral <- 0.4
pred_clase <- ifelse(probs[,"Yes"] >= umbral, "Yes", "No")
pred <- factor(pred_clase, levels = c("Yes", "No"))
confusionMatrix(pred, datos.test.N_mujeres_dummy$SleepTrouble,positive='Yes')
(cm<-table(pred, datos.test.N_mujeres_dummy$SleepTrouble))
tablas[['XGB2']] = cm
VP=cm[1,1]
VN=cm[2,2]
FP=cm[1,2]
FN=cm[2,1]
(accuracy=(VP+VN)/(VP+FN+VN+FP))
(error=(FN+FP)/(VP+FN+VN+FP))
(sensibilidad=VP/(VP+FN))
(especificidad=VN/(VN+FP))
(vpp=VP/(VP+FP))
(vpn=VN/(VN+FN))
(F1=2*(vpp*sensibilidad)/(vpp+sensibilidad))
##creacion de curva ROC
library(ROCR)
pred <- prediction(as.numeric(pred),as.numeric(datos.test.N_mujeres_dummy$SleepTrouble))
perf <- performance(pred,measure="tpr",x.measure="fpr")
plot(perf,colorize=TRUE,type="l", main= "XGB dummy")
abline(a=0,b=1)
#Area bajo la curva
AUC<- performance(pred,measure="auc")
AUCaltura <- AUC@y.values
cat("AUC:", AUCaltura[[1]])


# Guardamos las metricas
modelo = c(modelo,'XGB2')
acc = c(acc,accuracy)
errorv = c(errorv,error)
sens = c(sens,sensibilidad)
esp = c(esp,especificidad)
vppv = c(vppv,vpp)
vpnv = c(vpnv,vpn)
f1v = c(f1v,F1)
roc = c(roc,AUCaltura[[1]])
dummy = c(dummy,'Yes')
escalado = c(escalado,'No')

################################################################################
######## Dataframe con las metricas ############################################
################################################################################

metricas = data.frame(modelo,acc,errorv,sens,esp,vppv,vpnv,f1v,roc,dummy,escalado)
colnames(metricas) = c('Clasificador','Accuracy','Error','Sensibilidad','Esspecificidad','VPP','VPN','F1-Score','Curva ROC','Dummy','Escalado')
metricas
