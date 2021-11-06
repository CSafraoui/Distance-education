library(readxl)
library(data.table)
ead<-read_excel("C:/Users/khali/Desktop/ch/langage R/serie2/exp/ead.xlsx")

#Les variables manquantes dans la colonne filiere ne sont pas des NAs peut etre des etudiants au cp
#fili�re colonne num 8
for (i in 1:66) {
  if(is.na(ead[i,8])==TRUE){
    ead[i,8] <- 0
  }
}
#Les variables manquantes dans la colonne remarques ne sont pas des NAs mais des etudiants qu'ils ont pas donner ses remarques
#remarques colonne num 51
for (i in 1:66) {
  if(is.na(ead[i,51])==TRUE){
    ead[i,51] <- 0
  }
}
attach(ead)

#eliminer les NAs, je les garde dans une aute data.frame : df 
df<-data.table()
for (j in 1:nrow(ead)){
  if(!complete.cases(ead[j,]))
  {
    ead<-ead[-j,]
    df<-rbind(df,ead[j,])
  }
}

nrow(ead)
nrow(df)
all(!is.na(ead))

#�liminer les �tudiants en dehors de ibno tofail : colonne 5
for (i in 1:nrow(ead)) {
  if(ead[i,5]==2){
    df <- rbind(df ,ead[i,])
    ead<-ead[-i,]
  }
}
nrow(ead)
nrow(df)

#eliminer les etudiants en dehors de l'ENSA : colonne 6
for (i in 1:nrow(ead)) {
  if(ead[i,6]!=5){
    df <- rbind(df ,ead[i,])
    ead<-ead[-i,]
  }
}
nrow(ead)
nrow(df)

#eliminer les etudiants du CI mais sans filiere: la colonne 7 du niveau et 8 de la filiere
for (i in 1:nrow(ead)) {
  if(ead[i,7]>2 & ead[i,8]==0){
    df <- rbind(df ,ead[i,])
    ead<-ead[-i,]
  }
}

#eliminer les etudiants qui non pas tester l'ead: colonne 9
for (i in 1:nrow(ead)) {
  if(ead[i,9]==2){
    df <- rbind(df ,ead[i,])
    ead<-ead[-i,]
  }
}


nrow(ead)
nrow(df)
#Nettoyage des donnees aberantes dans les colonnes matiere_ead et matiere_visio car la totalite des modules de depasse pas 8 modules
for (i in 1:nrow(ead)) {
  if(ead[i,24]>9 || ead[i,25]>9){
    df <- rbind(df ,ead[i,])
    ead<-ead[-i,]
  }
}
nrow(ead)
nrow(df)

#statistique descriptive univarie variables quantitatives
names(ead) <- make.names(names(ead))
Boxplot( ~ age, data=ead, id=list(method="y"))
Boxplot( ~ matiere_visio, data=ead, id=list(method="y"))
Boxplot( ~ matiere_ead, data=ead, id=list(method="y"))
names(df) <- make.names(names(df))
#suppression des donnees aberantes
df <- rbind(df ,ead[3,])
ead<-ead[-3,]
numSummary(ead[,c("age", "matiere_ead", "matiere_visio"), drop=FALSE], statistics=c("mean", "sd", "IQR", "quantiles"), quantiles=c(0,.25,.5,.75,1))
var(ead$age)


#eliminer la colonne 51 : remarques
ead<-ead[,-51]
#eliminer la colonne 9 : teste de l'enseignement
ead<-ead[,-9] 
#eliminer la colonne 5 et 6 : etablissement et ibno_tofail 
ead<-ead[,-6]
ead<-ead[,-5] 
#eliminer la colonne 1 :Horodateur
ead<-ead[,-1]

#library("xlsx")
#write.xlsx(ead, file = "datanum.xlsx",sheetName = "datanum", append = FALSE)


#####Recodification des variables qualitative
library(abind, pos=17)
library(e1071, pos=18)
ead <- within(ead, {
  sexe <- factor(sexe, labels=c('Femme','Homme'))
})
ead <- within(ead, {
  region <- factor(region, labels=c('Tanger-T�touan-Al Hoce�ma',
                                    'F�s-Mekn�s','Rabat-Sal�-K�nitra','Casablanca-Settat'))
})
ead <- within(ead, {
  �tablissement <- factor(�tablissement, labels=c('ENSAK'))
})
ead <- within(ead, {
  niveau <- factor(niveau, labels=c('1�re','2�me','3�me',
                                    '4�me'))
})
ead <- within(ead, {
  fili�re <- factor(fili�re, labels=c('Rien','G.Ind','G.Info','G.Rst','G.E',
                                      'G,Meca'))
})

ead <- within(ead, {
  deroulement <- factor(deroulement, labels=c('Pas du tout satisfaisant',
                                              'Un peu satisfaisant','Neutre','Satisfaisant'))
})

ead <- within(ead, {
  avis1 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                  'Neutre','D\'accord','Tout a fait d\'accord'))
  avis2 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                  'Neutre','D\'accord','Tout a fait d\'accord'))
  avis3 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                  'Neutre','D\'accord','Tout a fait d\'accord'))
  apprecie1 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                      'Neutre','D\'accord','Tout a fait d\'accord'))
  apprecie2 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                      'Neutre','D\'accord','Tout a fait d\'accord'))
  apprecie3 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                      'Neutre','D\'accord','Tout a fait d\'accord'))
  apprecie4 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                      'Neutre','D\'accord','Tout a fait d\'accord'))
  apprecie4 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                      'Neutre','D\'accord','Tout a fait d\'accord'))
  modifie1 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                     'Neutre','D\'accord','Tout a fait d\'accord'))
  modifie2 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                     'Neutre','D\'accord','Tout a fait d\'accord'))
  modifie3 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                     'Neutre','D\'accord','Tout a fait d\'accord'))
  modifie4 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                     'Neutre','D\'accord','Tout a fait d\'accord'))
  enseignants1 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                         'Neutre','D\'accord','Tout a fait d\'accord'))
  enseignants2 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                         'Neutre','D\'accord','Tout a fait d\'accord'))
  enseignants3 <- factor(avis1, labels=c('Pas du tout d\'accord','Pas d\'accord',
                                         'Neutre','D\'accord','Tout a fait d\'accord'))
})
ead <- within(ead, {
  programmation_des_seances <- factor(programmation_des_seances, 
                                      labels=c('Tres satisfait','Satisfait','Neutre','Un peu satisfait',
                                               'Pas du tout satisfait'))
})
ead <- within(ead, {
  atmosphere_educative1 <- factor(atmosphere_educative1, 
                                  labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  atmosphere_educative2 <- factor(atmosphere_educative2, 
                                  labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  atmosphere_educative3 <- factor(atmosphere_educative3, 
                                  labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  atmosphere_educative4 <- factor(atmosphere_educative4, 
                                  labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
})
ead <- within(ead, {
  frequence_assistance <- factor(frequence_assistance, labels=c('Toujours',
                                                                'Souvent','Parfois','Rarement','Jamais'))
})
ead <- within(ead, {
  platforms_preferees <- factor(platforms_preferees, labels=c('Google-meet',
                                                              'Google classroom','Google-meet, Zoom','Google-meet, Google classroom',
                                                              'Google-meet, Zoom, Google classroom','Zoom, Google classroom'))
})
ead <- within(ead, {
  examen1 <- factor(examen1, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  examen2 <- factor(examen2, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  examen3 <- factor(examen3, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  examen4 <- factor(examen4, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
})
ead <- within(ead, {
  probleme_physique1 <- factor(probleme_physique1, 
                               labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  probleme_physique2 <- factor(probleme_physique2, 
                               labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  probleme_physique3 <- factor(probleme_physique3, 
                               labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
  probleme_physique4 <- factor(probleme_physique4, 
                               labels=c('Pas du tout d\'accord','Pas d\'accord','Neutre','D\'accord','Tout 
  a fait d\'accord'))
})
ead <- within(ead, {
  anxiete1 <- factor(anxiete1, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  anxiete2 <- factor(anxiete2, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  anxiete3 <- factor(anxiete3, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  anxiete4 <- factor(anxiete4, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
  anxiete5 <- factor(anxiete5, labels=c('Pas du tout d\'accord','Pas 
  d\'accord','Neutre','D\'accord','Tout a fait d\'accord'))
})
ead <- within(ead, {
  stresse <- factor(stresse, labels=c('Toujours','Souvent','Parfois',
                                      'Rarement','Jamais'))
})
ead <- within(ead, {
  deprime <- factor(deprime, labels=c('Toujours','Souvent','Parfois',
                                      'Rarement','Jamais'))
})
ead <- within(ead, {
  preference <- factor(preference, labels=c('Pr�sentielle','EAD'))
})
ead <- within(ead, {
  outils.conditions <- factor(outils.conditions, labels=c('Toujours',
                                                          'Souvent','Parfois','Rarement'))
})


#pie graphe 
library(colorspace, pos=19)
with(ead, pie(table(stresse), labels=levels(stresse), xlab="", ylab="", 
              main="stresse", col=rainbow_hcl(5)))
with(ead, pie(table(deroulement), labels=levels(deroulement), xlab="", ylab="", 
              main="deroulement", col=rainbow_hcl(5)))
with(ead, pie(table(sexe), labels=levels(sexe), xlab="", ylab="", 
              main="sexe", col=rainbow_hcl(2)))
with(ead, pie(table(region), labels=levels(region), xlab="", ylab="", 
              main="region", col=rainbow_hcl(4)))
with(ead, pie(table(niveau), labels=levels(niveau), xlab="", ylab="", 
              main="niveau", col=rainbow_hcl(4)))
with(ead, pie(table(deprime), 
              labels=levels(deprime), xlab="", ylab="", 
              main="deprime", col=rainbow_hcl(5)))
with(ead, pie(table(fili�re), labels=levels(fili�re), xlab="", ylab="", 
              main="fili�re", col=rainbow_hcl(6)))
with(ead, pie(table(frequence_assistance), labels=levels(frequence_assistance), xlab="", ylab="", 
              main="frequence_assistance", col=rainbow_hcl(5)))
with(ead, pie(table(programmation_des_seances), 
              labels=levels(programmation_des_seances), xlab="", ylab="", 
              main="programmation_des_seances", col=rainbow_hcl(5)))
with(ead, pie(table(outils.conditions), labels=levels(outils.conditions),
              xlab="", ylab="", main="outils.conditions", col=rainbow_hcl(4)))
with(ead, pie(table(preference), labels=levels(preference), xlab="", 
              ylab="", main="preference", col=rainbow_hcl(2)))


with(ead, pie(table(platforms_preferees), 
              labels=levels(platforms_preferees), xlab="", ylab="", 
              main="platforms_preferees", col=rainbow_hcl(6)))


####### Pour les variables qualitatives la representation des effectifs est des pourcentages
local({
  .Table <- with(ead, table(avis1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(avis2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(apprecie1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(apprecie2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(apprecie3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(modifie1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(modifie2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(modifie3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(modifie4))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(enseignants1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(enseignants2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(enseignants3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(atmosphere_educative1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(atmosphere_educative2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(atmosphere_educative3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(atmosphere_educative4))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(examen1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(examen2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(examen3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(examen4))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(probleme_physique1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(probleme_physique2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(probleme_physique3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(probleme_physique4))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(anxiete1))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(anxiete2))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(anxiete3))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(anxiete4))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(anxiete5))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(deroulement))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(stresse))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(sexe))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(region))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(niveau))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(deprime))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(fili�re))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(frequence_assistance))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(outils.conditions))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(preference))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(platforms_preferees))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})
local({
  .Table <- with(ead, table(programmation_des_seances))
  cat("\ncounts:\n")
  print(.Table)
  cat("\npercentages:\n")
  print(round(100*.Table/sum(.Table), 2))
})

############## test de normalite pour les variables quantitatives
normalityTest(~age, test="shapiro.test", data=ead)
hist(ead$age)
normalityTest(~matiere_ead, test="shapiro.test", data=ead)
hist(ead$matiere_ead)
normalityTest(~matiere_visio, test="shapiro.test", data=ead)
hist(ead$matiere_visio)
########### corelation
cor(ead[,c("age","matiere_ead","matiere_visio")], use="complete")
########## test non parametrique : wilcox
#age
with(ead, wilcox.test(age, alternative='two.sided', mu=21))
with(ead, wilcox.test(age, alternative='two.sided', mu=20,4))
#matiere_ead
with(ead, wilcox.test(matiere_ead, alternative='two.sided', mu=8))
with(ead, wilcox.test(matiere_ead, alternative='two.sided', mu=7,12))
#matiere_visio
with(ead, wilcox.test(matiere_visio, alternative='two.sided', mu=8))
with(ead, wilcox.test(matiere_visio, alternative='two.sided', mu=4))


#test de proportionalite : par nombre total, sexe ,et par cycle
prop.test(x =47, n =1334)
binom.test(47, 1334, p = 0.5,
           alternative = "greater")
prop.test(x = c(22, 25), n = c(652, 682))
prop.test(x =c(6,41), n =c(589,685))
##### test de fiabilite
#qualitative
library(readxl)
fiabilite<-read_excel("C:/Users/khali/Desktop/ch/langage R/serie2/exp/fiability.xlsx")

reliability(cov(fiabilite[,c("anxiete1","anxiete2","anxiete3","anxiete4",
                             "anxiete5","apprecie1","apprecie2","apprecie3","apprecie4",
                             "atmosphere_educative1","atmosphere_educative2","atmosphere_educative3",
                             "atmosphere_educative4","avis1","avis2","avis3","deprime","deroulement",
                             "enseignants1","enseignants2","enseignants3","examen1","examen2","examen3",
                             "examen4","frequence_assistance","modifie1","modifie2","modifie3",
                             "modifie4","outils.conditions","probleme_physique1","probleme_physique2",
                             "probleme_physique3","probleme_physique4","programmation_des_seances",
                             "stresse")], use="complete.obs"))



##### table croisee
local({
  .Table <- xtabs(~sexe+stresse, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})

local({
  .Table <- xtabs(~sexe+deroulement, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~stresse+deroulement, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~sexe+preference, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~niveau+fili�re, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~avis1+probleme_physique2, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~avis1+anxiete1, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
local({
  .Table <- xtabs(~avis3+preference, data=ead)
  cat("\nFrequency table:\n")
  print(.Table)
  .Test <- chisq.test(.Table, correct=FALSE)
  print(.Test)
})
#test de regression sur les variables quantitatives
eadnum<-read_excel("C:/Users/khali/Desktop/ch/langage R/serie2/exp/datanum.xlsx")
RegModel.1 <- lm(deroulement~frequence_assistance, data=eadnum)
summary(RegModel.1)
RegModel.2 <- lm(deroulement~stresse, data=eadnum)
summary(RegModel.2)
RegModel.3 <- lm(deroulement~sexe, data=eadnum)
summary(RegModel.3)
RegModel.4 <- lm(deroulement~age, data=eadnum)
summary(RegModel.4)
RegModel.5 <- lm(deroulement~fili�re, data=eadnum)
summary(RegModel.5)
RegModel.6 <- lm(deroulement~niveau, data=eadnum)
summary(RegModel.6)
RegModel.7 <- lm(deroulement~outils.conditions, data=eadnum)
summary(RegModel.7)
RegModel.8 <- lm(frequence_assistance~outils.conditions, data=eadnum)
summary(RegModel.8)

##anova test
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(RegModel.2)
par(oldpar)
Anova(RegModel.2, type="II")
library(MASS, pos=25)
Confint(RegModel.2, level=0.95)







