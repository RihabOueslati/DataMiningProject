rm(list=ls())
round(,3)
library(e1071)
library(gmodels)
library(scales)
library(nnet)

# Function that returns Root Mean Squared Error
rmse <- function(error)
{
  sqrt(mean(error^2))
}

# Function that returns Mean Absolute Error
mae <- function(error)
{
  mean(abs(error))
}

train.sj
# Example data
actual <- c(4, 6, 9, 10, 4, 6, 4, 7, 8, 7)
predicted <- c(5, 6, 8, 10, 4, 8, 4, 9, 8, 9)

# Calculate error
error <- actual - predicted

# Example of invocation of functions
rmse(error)
mae(error)

--------------------ACP--------------------------
train=read.table(file=file.choose(),sep=",",dec=".",header=TRUE)
str(train.sj)
total.cases=read.table(file=file.choose(),sep=",",dec=".",header=TRUE)

summary(train)
summary(total.cases)

train$year<-NULL
train$week_start_date<-NULL
train$city<-NULL

train.sj= train[1:624,]
test.sj= train[625:936,]

train.iq= train[937:1284,]
test.iq= train[1285:1456,]

total_cases<-total.cases$total_cases[1:624]
train.sj$total_cases<-total_cases
total_cases<-total.cases$total_cases[625:936]
test.sj$total_cases<-total_cases
total_cases<-total.cases$total_cases[937:1284]
train.iq$total_cases<-total_cases
total_cases<-total.cases$total_cases[1285:1456]
test.iq$total_cases<-total_cases

str(train.sj)
summary(train.sj)
str(train.iq)
summary(train.iq)
str(test.sj)
summary(test.sj)
str(test.iq)
summary(test.iq)

library(VIM)
mice_plot <- aggr(train, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(train), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

library(matrixStats)


train.sj.matrix<-as.matrix(train.sj)
train.sj.matrix.index <- which(is.na(train.sj.matrix), arr.ind = TRUE)
train.sj.matrix[train.sj.matrix.index] <- matrixStats::colMedians(train.sj.matrix, na.rm = TRUE)[train.sj.matrix.index[, 2]]
train.sj<-data.frame(train.sj.matrix)

train.iq.matrix<-as.matrix(train.iq)
train.iq.matrix.index <- which(is.na(train.iq.matrix), arr.ind = TRUE)
train.iq.matrix[train.iq.matrix.index] <- matrixStats::colMedians(train.iq.matrix, na.rm = TRUE)[train.iq.matrix.index[, 2]]
train.iq<-data.frame(train.iq.matrix)

test.sj.matrix<-as.matrix(test.sj)
test.sj.matrix.index <- which(is.na(test.sj.matrix), arr.ind = TRUE)
test.sj.matrix[test.sj.matrix.index] <- matrixStats::colMedians(test.sj.matrix, na.rm = TRUE)[test.sj.matrix.index[, 2]]
test.sj<-data.frame(test.sj.matrix)

test.iq.matrix<-as.matrix(test.iq)
test.iq.matrix.index <- which(is.na(test.iq.matrix), arr.ind = TRUE)
test.iq.matrix[test.iq.matrix.index] <- matrixStats::colMedians(test.iq.matrix, na.rm = TRUE)[test.iq.matrix.index[, 2]]
test.iq<-data.frame(test.iq.matrix)

summary(train.sj)
summary(train.iq)
summary(test.sj)
summary(test.iq)

total_cases<-train.sj$total_cases
train.sj<-scale(train.sj)
train.sj<-data.frame(train.sj)
train.sj$total_cases<-NULL
train.sj$total_cases<-total_cases
total_cases<-train.iq$total_cases
train.iq<-scale(train.iq)
train.iq<-data.frame(train.iq)
train.iq$total_cases<-NULL
train.iq$total_cases<-total_cases
total_cases<-test.sj$total_cases
test.sj<-scale(test.sj)
test.sj<-data.frame(test.sj)
test.sj$total_cases<-NULL
test.sj$total_cases<-total_cases
total_cases<-test.iq$total_cases
test.iq<-scale(test.iq)
test.iq<-data.frame(test.iq)
test.iq$total_cases<-NULL
test.iq$total_cases<-total_cases

summary(train.sj)
summary(train.iq)
summary(test.sj)
summary(test.iq)
------------------------------------------
train.sj$total_cases<-NULL
test.sj$total_cases<-NULL
train.iq$total_cases<-NULL
test.iq$total_cases<-NULL


# Compute Pearson correlation
cor(train.sj)
cor(c1,c2)
# Correlation Matrix
cor(train.sj)[,4:10])
library(Hmisc)
rcorr(train.sj)(movies[,4:10])
library(GGally)
ggcorr(train.sj)
# Fill in "TRUE" or "FALSE" to see how the correlation matrix changes

library(GGally)
ggcorr(train.sj, 
       label = TRUE, 
       label_alpha = TRUE)

train.sj.pca=princomp(train.sj,cor=T,scores=T)
summary(train.sj.pca)
valeurPropres=train.sj.pca$sdev^2
valeurPropres
c1=train.sj.pca$loadings[,1]*train.sj.pca$sdev[1]
c2=train.sj.pca$loadings[,2]*train.sj.pca$sdev[2]
c3=train.sj.pca$loadings[,3]*train.sj.pca$sdev[3]
c4=train.sj.pca$loadings[,4]*train.sj.pca$sdev[4]
c5=train.sj.pca$loadings[,5]*train.sj.pca$sdev[5]
c6=train.sj.pca$loadings[,6]*train.sj.pca$sdev[6]
ref=cbind(c1,c2)
print(ref,digits=2)
print(t(apply(ref^2,1,cumsum)),digits=2)
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
text(c1,c2,labels=colnames(train.sj),cex=0.5)
symbols(0,0,circles=1,inches=F,add=T)


train.sj.pca$loadings
train.sj.pca$scores
train.sj.pca$sdev
plot(train.sj.pca)
biplot(train.sj.pca)
train.sj.pca$loadings
str(train.sj.pca$loadings)

angles.pour.dessiner.un.cercle <- seq (from = 0, to = 2 * pi, by = 0.01)
plot (cos (angles.pour.dessiner.un.cercle),
        sin (angles.pour.dessiner.un.cercle),
        xlab = "composante principale 1",
        ylab = "composante principale 2",
        main = "Cercle des corrélations",
        type = 'l',
        asp = 1)
mon.acp.v <- t (train.sj.pca$loadings) [1:2,]
abline(h=0,v=0)
arrows (0, 0, mon.acp.v [1,], mon.acp.v [2,], col="blue")
text (mon.acp.v [1,], mon.acp.v [2, ], colnames (mon.acp.v))

par(mfrow = c(1, 1))
pairs(train.sj[,1:10])
pairs(train.sj[c(1,22)])

valeurPropres=train.sj.pca$sdev^2
valeurPropres
c1=train.sj.pca$loadings[,1]*train.sj.pca$sdev[1]
c2=train.sj.pca$loadings[,2]*train.sj.pca$sdev[2]
ref=cbind(c1,c2)
print(ref,digits=2)
print(t(apply(ref^2,1,cumsum)),digits=3)
plot(c1,c2,xlim=c(-1,+1),ylim=c(-1,+1),type="n")
abline(h=0,v=0)
mon.acp.v <- t (train.sj.pca$loadings) [1:2,]
arrows (0, 0, mon.acp.v [1,], mon.acp.v [2,], col="blue")
text(c1,c2,labels=colnames(train.sj),cex=0.5)
symbols(0,0,circles=1,inches=F,add=T)

library(stats)
-------------------------------------------------

----------------R?gression lin?aire--------------
train.sj.quanti<-train.sj
summary(train.sj.quanti)
train.sj.quanti$week_start_date<-NULL
train.sj.quanti$year<-NULL
total_cases<-train.sj$total_cases
train.sj.quanti$total_cases<-NULL
train.sj.quanti<-scale(train.sj.quanti)
train.sj.quanti<-data.frame(train.sj.quanti)
train.sj.quanti$total_cases<-total_cases
  
  test.sj.quanti<-test.sj
  summary(test.sj.quanti)
  test.sj.quanti$week_start_date<-NULL
  test.sj.quanti$year<-NULL
  total_cases<-train.sj$total_cases
  test.sj.quanti$total_cases<-NULL
  test.sj.quanti<-scale(train.sj.quanti)
  test.sj.quanti<-data.frame(train.sj.quanti)
  test.sj.quanti$total_cases<-total_cases

  train.sj.quanti.scale<-scale(train.sj.quanti)
  train.sj.quanti.scale<-data.frame(train.sj.quanti.scale)
  
  test.sj.quanti.scale<-scale(test.sj.quanti)
  test.sj.quanti.scale<-data.frame(test.sj.quanti.scale)
  
  

modelg<-lm(train.sj.quanti.scale$total_cases~.,train.sj.quanti.scale)
summary(modelg)
library(MASS)
stepAIC(modelg)
plot(modelg)
pred.reg.global = predict(modelg, newdata= test.sj.quanti.scale)
cbind(pred.reg.global,test.sj.quanti.scale$total_cases)
qqplot(pred.reg.global,test.sj.quanti.scale$total_cases)
?lm

modelr<-lm(train.sj.quanti.scale$total_cases ~ precipitation_amt_mm + 
             reanalysis_max_air_temp_k + reanalysis_tdtr_k + station_avg_temp_c + 
             station_max_temp_c + weekofyear, data = train.sj.quanti.scale)
summary(modelr)
stepAIC(modelr)
plot(modelr)
pred.reg.reduit = predict(modelr, newdata= test.sj.quanti.scale)
cbind(pred.reg.reduit,test.sj.quanti.scale$total_cases)

error.pl.modelg<-train.sj$total_cases - pred.reg.global
MAErl.pl.modelg<-mae(error.pl.modelg)
MAErl.pl.modelg

error.pl.modelr<-train.sj$total_cases - pred.reg.reduit
MAErl.pl.modelr<-mae(error.pl.modelr)
MAErl.pl.modelr

library(stats)

stepAIC(modelg)
stepAIC(modelr)
AIC(modelg)
AIC(modelr)

AIC ---> se concentre sur le nombre de variables
BIC ---> se concentre sur le nombre de individus et se base sur AIC


-------------------------------------------------

--------------------svm-basic -----------------------
library(e1071)
svm.radial = svm(train.sj$total_cases ~ precipitation_amt_mm + 
                         reanalysis_max_air_temp_k + reanalysis_tdtr_k + station_avg_temp_c + 
                         station_max_temp_c + weekofyear ,data = train.sj)
pred.svm.radial = predict(svm.radial, newdata=test.sj)
error.svm.radial<-test.sj$total_cases- pred.svm.radial
MAEsvm.radial<-mae(error.svm.radial)
MAEsvm.radial

cbind(pred.svm.radial,test.sj$total_cases)
-----------------------------------------------------------
  --------------------svm-polynomial -----------------------

svm.polynomial = svm(train.sj$total_cases ~ precipitation_amt_mm + 
                         reanalysis_max_air_temp_k + reanalysis_tdtr_k + station_avg_temp_c + 
                         station_max_temp_c + weekofyear,kernel="polynomial" ,data = train.sj)
pred.svm.polynomial = predict(svm.polynomial, newdata=test.sj)
error.svm.polynomial<-test.sj$total_cases- pred.svm.polynomial
MAEsvm.polynomial<-mae(error.svm.polynomial)
MAEsvm.polynomial

cbind(pred.svm.polynomial,test.sj$total_cases)
-----------------------------------------------------------
  --------------------svm-basic -----------------------

  svm.sigmoid = svm(train.sj$total_cases ~ precipitation_amt_mm + 
                                reanalysis_max_air_temp_k + reanalysis_tdtr_k + station_avg_temp_c + 
                                station_max_temp_c + weekofyear,kernel="sigmoid" ,data = train.sj)
pred.svm.sigmoid = predict(svm.sigmoid, newdata=test.sj)
error.svm.sigmoid<-test.sj$total_cases- pred.svm.sigmoid
MAEsvm.sigmoid<-mae(error.svm.sigmoid)
MAEsvm.sigmoid

cbind(pred.svm.sigmoid,test.sj$total_cases)
-----------------------------------------------------------
  
  readLines("tableau.xls",n=5)
tab=read.table(file=file.choose(),sep="\t",dec=".",header=T)

#1#echantillonnage
nrow(tab)
summary(tab)
tab.train= tab[1:690,]
tab.train<-data.frame(tab.train)
total_cases<-tab.train$total_cases
tab.train<-scale(tab.train)
tab.train<-data.frame(tab.train,total_cases)
tab.train$total_cases=NULL
tab.train<-data.frame(tab.train,total_cases)
tab.train$total_cases.1=NULL
str(tab.train)
summary(tab.train)
tab.test= tab[701:936,]
tab.test<-data.frame(tab.test)
total_cases<-tab.test$total_cases
tab.test<-scale(tab.test)
tab.test<-data.frame(tab.test,total_cases)
tab.test$total_cases=NULL
tab.test<-data.frame(tab.test,total_cases)
tab.test$total_cases.1=NULL
summary(tab.test)

tab.train$total_cases<-floor(tab.train$total_cases)
tab.test$total_cases<-floor(tab.test$total_cases)

------------------1er modele----------------
  
  svm.basic = svm(tab.train$total_cases ~. , data = tab.train) #la fonction de separation est RADIAL => ca veut dire qu'il y sont tres proches ya pas une separation linéaire
summary(svm.basic)  # number of support vector: nbr d'individus qui a utiliser

pred.svm.basic = predict(svm.basic, newdata= tab.test)
errorSVMbasic <- round(tab.test$total_cases) - round(pred.svm.basic)
MAEsvmBasic<-mae(errorSVMbasic)


t1= table(pred.svm.basic,tab.test$total_cases)
pred.svm.basic.floored<-floor(pred.svm.basic)
t1= table(pred.svm.basic.floored,tab.test$total_cases)
options(max.print=999999999)
round(pred.svm.basic,3)
CT<-CrossTable(round(pred.svm.basic),round(tab.test$total_cases),prop.chisq=T)
predictionSVM<-c(round(pred.svm.basic))
total_cases<-c(tab.test$total_cases)
amineSVM<-data.frame(predictionSVM,total_cases)
prop.table(t1)
prop.table(CT)
nrow(tabtest)


pred.svm.basic.orig<-pred.svm.basic*pred.svm.basic
pred.svm.basic.orig<-ceiling(pred.svm.basic.orig)
summary(pred.svm.basic.orig)

------------------2EME MODELE--------
  
svm.polynomial.sunset= svm(total_cases ~. , data=train.sj, kernel="polynomial")
pred.svm.polynomial.sunset = predict(svm.polynomial.sunset, newdata= test.sj)

errorSVMpolynomialSunset <- round(tab.test.sunset$total_cases) - round(pred.svm.polynomial.sunset)
MAEsvmPolynomialSunset<-mae(errorSVMpolynomialSunset)


svm.polynomial= svm(total_cases ~. , data=tab.train, kernel="polynomial")
pred.svm.polynomial = predict(svm.polynomial, newdata= tab.test)

errorSVMpolynomial <- round(tab.test$total_cases) - round(pred.svm.polynomial)
MAEsvmPolynomial<-mae(errorSVMpolynomial)

-------------------3eme modele-------------------
  
  svm.sigmoid.sunset= svm(total_cases ~. , data=tab.train.sunset, kernel="sigmoid")
pred.svm.sigmoid.sunset = predict(svm.sigmoid.sunset, newdata= tab.test.sunset)

errorSVMsigmoidSunset <- round(tab.test.sunset$total_cases) - round(pred.svm.sigmoid.sunset)
MAEsvmSigmoidSunset<-mae(errorSVMsigmoidSunset)


svm.sigmoid= svm(total_cases ~. , data=tab.train, kernel="sigmoid")
pred.svm.sigmoid = predict(svm.sigmoid, newdata= tab.test)

errorSVMsigmoid <- round(tab.test$total_cases) - round(pred.svm.sigmoid)
MAEsvmSigmoid<-mae(errorSVMsigmoid)

--------------------------KNN---------------
  library(kknn)

knnSunset = kknn(total_cases ~. , tab.train.sunset , tab.test.sunset)
knnSunset
attributes(knnSunset)
knnSunset$fit
summary(knnSunset)

errorKnnSunset <- round(tab.test.sunset$total_cases) - round(knnSunset$fit)
MAEknnSunset<-mae(errorKnnSunset)

knn30Sunset = kknn(total_cases ~. , tab.train.sunset , tab.test.sunset, k=30)
errorKnn30Sunset <- round(tab.test.sunset$total_cases) - round(knn30Sunset$fit)
MAEknn30Sunset<-mae(errorKnn30Sunset)


knnSJ = kknn(total_cases ~. , tab.train , tab.test)
knnSJ
attributes(knnSJ)
knnSJ$fit
summary(knnSJ)

errorKnnSJ <- round(tab.test$total_cases) - round(knnSJ$fit)
MAEknnSJ<-mae(errorKnnSJ)

knn30SJ = kknn(total_cases ~. , tab.train , tab.test, k=30)
errorKnn30SJ <- round(tab.test$total_cases) - round(knn30SJ$fit)
MAEknn30SJ<-mae(errorKnn30SJ)


/////////////////// neurone
library(scales)
library(nnet)
res=nnet(total_cases~.,data=train.sj,rang = 0.1,decay=1, maxit=500,size =10)
plot.nnet(res,pos.col='darkgreen',neg.col='darkblue',alpha.val=0.7,rel.rsc=15, circle.cex=10,cex=1.4,circle.col='brown')
pred.nnet<- predict(res,test.sj,), type = "class")
errorNNetSJ <- round(tab.test$total_cases) - round(pred.nnet)
MAEnnet<-mae(errorNNetSJ)

//////////////////////////
  
  library(ROCR)
fr= 
  ?prediction
pred= prediction()
Tout d’abord on va charger le package ROCR, puis on va regrouper tous les courbes dans un même graphe.
predictionR=prediction(as.numeric(pred.svm.basic.sunset),as.numeric(test.sj$total_cases))
prefR =performance(predictionR,"tpr","fpr")
plot(prefR , col="green",legend="hhh")

predictionSR=prediction(as.numeric(pred.reg.global),as.numeric(test.sj$total_cases))
prefS =performance(predictionS,"tpr","fpr")
plot(prefS ,col = "red",add=T )

predictionP=prediction(as.numeric(prediction_Poly),as.numeric(acc2Test$PREMATURE))
prefP =performance(predictionP,"tpr","fpr")
plot(prefP ,col = "black" ,add=T)

pred<-prediction(as.numeric(p1), as.numeric(acc2Test$PREMATURE))
prefK =performance(pred,"tpr","fpr")
plot(prefK ,col = "blue",add=T)

predictionG=prediction(as.numeric(predict),as.numeric(acc2Test$PREMATURE))
prefG =performance(predictionG,"tpr","fpr")
plot(prefG ,col = "purple" ,add=T)
legend("bottomright", legend = c("SVM Sigmoid", "SVM Radial","SVM Polynomial","Regression Logistique","K-NN"), lty = 1, xjust = 1, yjust = 1, col = c("red","green","black","purple","blue"), title = "Line Types")


------------------discrétisation------------
library(discretization)
amine.disc<-disc.Topdown(data = amine,method = caim(tb = ))
amine.disc.chi2<-chi2(data = amine,alpha = 0.1)

head(train.sj)
str(train.sj)
train.sj$year<-NULL
train.sj$weekofyear<-NULL
train.sj$week_start_date<-NULL
train.sj$total_cases<-as.numeric(train.sj$total_cases)
test.sj$total_cases<-as.numeric(test.sj$total_cases)

Breaksage = c(0, 5, 10, 20, 30, 50, 100, max(total.cases$total_cases))
total.cases.train.sj.d = cut(train.sj$total_cases, breaks = Breaksage, include.lowest = TRUE)
total.cases.test.sj.d = cut(test.sj$total_cases, breaks = Breaksage, include.lowest = TRUE)
train.sj$total_cases <- factor(train.sj$total_cases)
test.sj$total_cases <- factor(test.sj$total_cases)
levels(total.cases.train.sj.d)
levels(total.cases.test.sj.d)
levels(total.cases.train.sj.d)<-c("faible","assez faible","moyen","assez fort","fort","très fort","exceptionnellement fort")
levels(total.cases.test.sj.d)<-c("faible","assez faible","moyen","assez fort","fort","très fort","exceptionnellement fort")
summary(total.cases.train.sj.d)
summary(total.cases.test.sj.d)
train.sj$total_cases<-total.cases.train.sj.d
test.sj$total_cases<-total.cases.test.sj.d
summary(train.sj)
summary(test.sj)

setwd("~/R/Projet DM/competition data/données traitées")

write.table(x = train.sj,file = "train.sj.disc.5.csv",row.names = FALSE,sep=",")
write.table(x = test.sj,file = "test.sj.disc.5.csv",row.names = FALSE,sep=",")
som<-read.table(file=file.choose(),dec=".",sep=",",header=TRUE)
summary(som)

R = (max(total.cases$total_cases)-min(total.cases$total_cases)) / (1 + 2 + 3 + 4 + 5)
classe1<-min(total.cases$total_cases) - min(total.cases$total_cases)+R
classe2<-min(total.cases$total_cases)+R - min(total.cases$total_cases)+2*R


-------------------rpart-----------------------------------
  library(rpart)
rm(total_cases)
arbre.sj.control<-rpart.control(minsplit=10)
test.sj.total_cases.rpart<-rpart(total_cases~.,data=test.sj,control=arbre.sj.control)
test.sj.pred.arbre<-predict(data=test.sj,test.sj.total_cases.rpart,type = "class")
length(test.sj$total_cases)
summary(ad.total_cases)
library(descr)
library(gmodels)
table(test.sj$total_cases,test.sj.pred.arbre)
length(test.sj.pred.arbre)
cbind(test.sj$total_cases,test.sj.pred.arbre)
plot(test.sj.total_cases.rpart)
text(test.sj.total_cases.rpart)

-----------------------fin rpart---------------------------
  
  ----------------------regression logistique polytomique ordinale----------------
  train.sj.log<-class(train.sj$weekofyear)
str(train.sj)
  require(MASS)
# modèle trivial réduit à la constante
str_constant <- "~ 1"
# modèle complet incluant toutes les explicatives potentielles
str_all <- "~ndvi_ne+ndvi_nw+ndvi_se+ndvi_sw+precipitation_amt_mm+reanalysis_air_temp_k+reanalysis_avg_temp_k+reanalysis_dew_point_temp_k+reanalysis_max_air_temp_k+reanalysis_min_air_temp_k+reanalysis_precip_amt_kg_per_m2+reanalysis_relative_humidity_percent+reanalysis_sat_precip_amt_mm+reanalysis_specific_humidity_g_per_kg+reanalysis_tdtr_k+station_avg_temp_c+station_diur_temp_rng_c+station_diur_temp_rng_c+station_max_temp_c+station_min_temp_c+station_precip_mm"
  rm(total_cases)
  train.sj.modele.reglog.forward <- glm(total_cases~., data = train.sj, family = binomial)
modele.forward <- stepAIC(train.sj.modele.reglog.forward, scope = list(lower = str_constant, upper = str_all), 
                          trace = TRUE, data = train.sj, direction = "forward")
summary(train.sj.modele.reglog.forward)

rain.sj.modele.reglog.stepwise <- glm(total_cases ~ 1, data = train.sj, family = binomial)
modele.stepwise <- stepAIC(rain.sj.modele.reglog.stepwise, scope = list(lower = str_constant, upper = str_all), 
                           trace = TRUE, data = appren, direction = "both")
summary(modele.stepwise)

logit = function(formula, lien = "logit", data = NULL) {
  glm(formula, family = binomial(link = lien), data)
}
m.logit<-logit(total_cases~reanalysis_precip_amt_kg_per_m2+precipitation_amt_mm+ndvi_nw ,data = train.sj)
# résultats du modèle
summary(m.logit)
exp(cbind(OR = coef(m.logit), confint(m.logit)))
summary(m.logit)
attributes(m.logit)
par(mfrow = c(1, 1))
plot(rstudent(m.logit), type = "p", cex = 0.5, ylab = "R?sidus studentis?s ", 
     col = "springgreen2", ylim = c(-3, 3))
abline(h = c(-2, 2), col = "red")
(chi2 <- with(m.logit, null.deviance - deviance))
(ddl <- with(m.logit, df.null - df.residual))
(pvalue <- pchisq(chi2, ddl, lower.tail = F))
train.sj.reglog.predict.cbind <- cbind(train.sj, predict(m.logit, newdata = train.sj, type = "link", 
                                  se = TRUE))
head(train.sj.reglog.predict.cbind)
appren.p <- within(train.sj.reglog.predict.cbind, {
  PredictedProb <- plogis(fit)
  LL <- plogis(fit - (1.96 * se.fit))
  UL <- plogis(fit + (1.96 * se.fit))
})
tail(appren.p)
appren.p <- cbind(appren.p, pred.total_cases = factor(ifelse(appren.p$PredictedProb > 
                                                       0.5, 1, 0)))
head(appren.p)
# Matrice de confusion
m.confusion <- as.matrix(table(appren.p$pred, appren.p$total_cases)))

modele <- vglm(niveau ~ ., data = bpress, family = acat())
summary(modele) 


  ---------------------fin r?gression logistique------------

-------------------------------Serie Temporelle-------------
  
  library(xts)
dltrain=read.table(file=file.choose(),sep="\t",header=T)
summary(dltrain)
serie<-ts(tab$total_cases,start=c(1990,18),end=c(2008,18),frequency=52)
plot(serie)

decomp2<-stl(serie,s.window="periodic")
tendance=(decomp2$time.series[,2])
saisonnalite=(decomp2$time.series[,1])
erreur=(decomp2$time.series[,3])

par(mfrow=c(3,1))
plot(tendance)
plot(saisonnalite)
plot(erreur)

acf(erreur)

qqplot(rnorm(937),erreur)

fitted2<-aggregate(serie,FUN=mean)

plot(fitted2)
plot(decompose(serie))
acf(serie$random)


dtrain<-read.table(file=file.choose(),sep=",")
plot(dtrain)
dltrainquan<-dtrain
dltrainquan$V1<-NULL
dltrainquan<-dltrainquan[-1,]
plot(dltrainquan$V2,dltrainquan$V4)
boxplot(dltrainquan$total_cases)
hist(dltrainquan$V4)
pie(table(dltrainquan$total_cases))
acf(ts(dltrainquan$total_cases))

library(xts)
train=read.table(file=file.choose(),sep=",",dec=".",header=TRUE)
train.sj=read.table(file=file.choose(),sep=",",dec=".",header=TRUE)
test.sj=read.table(file=file.choose(),sep=",",dec=".",header=TRUE)

week_start_date<-train$week_start_date[1:624]
train.sj$week_start_date<-week_start_date
train.sj.serie<-zoo(x = train.sj,order.by = train.sj$week_start_date,frequency = 52)

serie.sj.backup1<-serie.sj.backup$
plot.zoo(train.sj.serie)

---------------------------fin serie temporelle-------------
  
  
  -------------------------approx NA------------------------
  library(xts)
  train.na.approx<-na.approx(train)
  train.na.approx<-data.frame(train.na.approx)
  train.na.spline<-na.spline(train)
  train.na.spline<-data.frame(train.na.spline)
  train.na.median<-data.frame(train.matrix)
  
  spline<-train.na.approx$ndvi_ne
  approx<-train.na.spline$ndvi_ne
  amine<-train.sj.matrix$
  normal<-train.sj.
cbind(train$ndvi_ne,train.na.spline$ndvi_ne,train.na.approx$ndvi_ne,train.na.median$ndvi_ne)
par(mfrow=c(1,4))
plot(train$ndvi_ne)
plot(train.na.spline$ndvi_ne)
plot(train.na.approx$ndvi_ne)
plot(train.na.median$ndvi_ne)
par(mfrow=c(1,2))
plot(train.na.spline$ndvi_ne)
plot(train.na.approx$ndvi_ne)
summary(train.na.median$ndvi_ne)
par(mfrow=c(1,4))
plot(train$ndvi_ne[50:200])
abline(h = c(1,0.14050),col="red")
plot(train.na.spline$ndvi_ne[50:200])
abline(h = c(1,0.14050),col="red")
plot(train.na.approx$ndvi_ne[50:200])
abline(h = c(1,0.14050),col="red")
plot(train.na.median$ndvi_ne[50:200])
abline(h = c(1,0.14050),col="red")

par(mfrow=c(1,4))
plot(train$ndvi_ne)
abline(h = c(1,0.14050),col="red")
plot(train.na.spline$ndvi_ne)
abline(h = c(1,0.14050),col="red")
plot(train.na.approx$ndvi_ne)
abline(h = c(1,0.14050),col="red")
plot(train.na.median$ndvi_ne)
abline(h = c(1,0.14050),col="red")

par(mfrow=c(1,3))
plot(train.na.spline$ndvi_ne[50:200])
plot(train.na.approx$ndvi_ne[50:200])
plot(train.na.median$ndvi_ne[50:200])

  summary(train)
  train.matrix<-as.matrix(train)
  train.matrix.index <- which(is.na(train.matrix), arr.ind = TRUE)
  train.matrix[train.matrix.index] <- matrixStats::colMedians(train.matrix, na.rm = TRUE)[train.matrix.index[, 2]]