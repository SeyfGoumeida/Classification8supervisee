library(ElemStatLearn)
data(prostate) 

?prostate 

attach(prostate) 
prostate

g <- factor(ifelse(lpsa > median(lpsa), "high", "low")) 
g

library(lattice) 
splom(~prostate[,1:3], groups=g) 

pairs(prostate[,1:3], col=as.numeric(g))

y <- ifelse(g=="high", 1, 0) 

lm.fit <- lm(y~lcavol+lweight+age) 
lm.fit

lm.beta <- lm.fit$coef 
lm.beta

b <- -lm.beta[2]/lm.beta[4] 
a <- (0.5 - lm.beta[1] - lm.beta[3]*mean(lweight))/lm.beta[4] 
plot(lcavol, age, col=g) 
abline(a,b) 

yhat <- predict(lm.fit) 

lm.ghat <- factor(ifelse(yhat > 0.5, "high", "low")) 

sum(lm.ghat != g) 

mean(lm.ghat != g) 

table(lm.ghat, g)

set.seed(30) 
X=cbind(g, lcavol,lweight,age) 
tr <- sample(1:nrow(X),72) 
Xtrain <- X[tr,]
Xtest <- X[-tr,] 

library(class) 
set.seed(30) 
kmax=72 
err_test <- rep(NA,kmax) 
for (k in 1:kmax) 
{ 
pred <- knn(Xtrain[,-1],Xtest[,-1],Xtrain[,1],k) 
err_test[k] <- sum(pred!=Xtest[,1])/length(Xtest[,1]) 
} 
lim <- c(0,max(err_test)) 
plot(err_test,type="l",ylim=lim,col=2,xlab="nombre de voisins", ylab= "taux d'erreur") 
which.min(err_test) 

err_test1 <- rep(NA,kmax) 
set.seed(10) 
tr <- sample(1:nrow(X),72) 
train <- X[tr,] 
test <- X[-tr,] 
for (k in 1:kmax) 
{ 
pred <- knn(Xtrain[,-1],Xtest[,-1], Xtrain [,1],k) 
err_test1[k] <- sum(pred!=Xtest[,1])/length(Xtest[,1])
} 
plot(err_test,type="l",ylim=lim,col=2,xlab="nombre de voisins", ylab= "taux d'erreur") 
lines(err_test1,col=4) 
legend("bottomright", legend=c("decoupage 1", "decoupage 2"), lty=1, col=c(2,4))
which.min(err_test)
which.min(err_test1)


B<- 20
kmax <- 90
err_test <- matrix(NA,kmax,B) 
for (b in 1:B) 
{ 
tr <- sample(1:nrow(X),90) 
Xtrain <- X[tr,] 
Xtest <- X[-tr,] 
for (k in 1:kmax) 
{ 
pred <- knn(Xtrain[,-1],Xtest[,-1], Xtrain [,1],k) 
err_test[k,b] <- sum(pred!= Xtest[,1])/length(Xtest[,1]) 
}
} 
mean_err_test <- apply(err_test,1,mean) 
lim <-c(0,max(err_test)) 
matplot(err_test,type="l",lty=2,col=2,ylim=lim, xlab="nombre de voisins",ylab="taux d'erreur") 
matpoints(mean_err_test,type="l",col=2,lwd=4) 
legend("bottomright", legend=c("Erreur moyenne", "Erreurs conditionnelles"), 
lty=c(1,3),lwd=c(4,2),col=c(2,2))


?knn.cv 
err_test <- rep(NA,kmax) 
for (k in 1:kmax) 
{ 
pred <- knn.cv(X[,-1], X[,1],k) 
err_test[k] <- sum(pred!= X[,1])/length(X[,1]) 
} 
lim <-c(0,max(err_test)) 
plot(err_test,type="l",col=2,ylim=lim,xlab="nombre de voisins", ylab="taux d'erreur") 
points(mean_err_test,type="l",col=4,lwd=1) 
legend("bottomright", legend=c("Erreur loo", "Erreur moyenne"), col=c(2,4),lty=1)
which.min(mean_err_test)
which.min(err_test)



set.seed(30) 
tr <- sample(1:nrow(X),72) 
Xtrainval <- X[tr,] 
test <- X[-tr,]

B <- 25 
kmax <- 36
err_valid <- matrix(NA,kmax,B) 
for (b in 1:B) { 
    tr <- sample(1:nrow(Xtrainval),36) 
    Xtrain <- Xtrainval[tr,] 
    Xvalid <- Xtrainval[-tr,] 
    for (k in 1:kmax) { 
        pred <- knn(Xtrain[,-1],Xvalid[,-1],Xtrain[,1],k) 
        err_valid[k,b] <- sum(pred!=Xvalid[,1])/length(Xvalid[,1]) 
    } 
} 
mean_err_valid <- apply(err_valid,1,mean) 
plot(mean_err_valid,type="l")
which.min(mean_err_valid)

pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(mean_err_valid)) 
sum(pred!=Xtest[,1])/length(Xtest[,1])

err_valid <- rep(NA,kmax) 
for (k in 1:kmax) 
{ 
pred <- knn.cv(Xtrainval[,-1],Xtrainval[,1],k) 
err_valid[k] <- sum(pred!=Xtrainval[,1])/length(Xtrainval[,1]) 
} 
which.min(err_valid) 
pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(err_valid)) 
sum(pred!=Xtest[,1])/length(Xtest[,1])

B <- 50 
kmax <- 50 
err_valid <- rep(NA,kmax) 
err_test <- rep(NA,B) 
for (b in 1:B) { 
    tr <- sample(1:nrow(X),72) 
    Xtrainval <- X[tr,] 
    Xtest <- X[-tr,] 
    for (k in 1:kmax) {
        pred <- knn.cv(Xtrainval[,-1],Xtrainval[,1],k) 
        err_valid[k] <- sum(pred!=Xtrainval[,1])/length(Xtrainval[,1]) 
    } 
    pred <- knn(Xtrainval[,-1],Xtest[,-1],Xtrainval[,1],k=which.min(err_valid)) 
    err_test[b] <- sum(pred!=Xtest[,1])/length(Xtest[,1]) 
} 
boxplot(err_test,main="Erreurs test pour 50 decoupages")

prostate.d<-prostate[, -c(9,10)] 

library(e1071) 
m <- naiveBayes(g ~ ., data = prostate.d) 

m <- naiveBayes(prostate.d, g) 
m 
table(predict(m, prostate.d), g) 


n <- naiveBayes(g ~ ., data = X[,-1])


X
g
n <- naiveBayes(X[,-1], g) 
n 
table(predict(n, X[,-1]), g) 




library(kernlab)
data(spam) 

?spam

attach(spam) 

spam

cat(" les dimensions de la table spam : ",dim(spam))

spamBin = spam
spamBin[,58] <- ifelse(spam[,58]=="spam", 1, 0)
#spamBin

id=which(spamBin[,58]==0)
spamBinZero=spamBin[id,]
cat(" # nombre des messages non-spam : ",nrow(spamBinZero),"\n")
perZero=nrow(spamBinZero)/nrow(spamBin)
cat("les messages non-spam represent : ",perZero,"% de l'ensemble des données \n")

id=which(spamBin[,58]==1)
spamBinOne=spamBin[id,]
cat(" # nombre des messages Spam     : ",nrow(spamBinOne),"\n")
perOne=nrow(spamBinOne)/nrow(spamBin)
cat("les messages Spam represent     : ",perOne,"% de l'ensemble des données \n")

cat("la Moyenne des spams : " , mean(spamBin[,58]),"\n")


cat("la variance des spams avec la commande var : ",var(spamBin[,58]),"\n")
cat("la variance des spams avec la formule      : ",sum((spamBin[,58]-mean(spamBin[,58]))^2)/nrow(spamBin),"\n")

summary(spamBin)

boxplot(spamBin[,58])
boxplot(spamBin)

hist(spamBin[,58])


pairs(spamBin)
plot(spamBin)

lm.fit <- lm(spamBin[,58]~spamBin[,57]+spamBin[,56]+spamBin[,53]+spamBin[,7]+spamBin[,47]+spamBin[,49]+spamBin[,23]+spamBin[,6])

lm.beta <- lm.fit$coef 
lm.beta
sort(lm.beta)


colnames(spam)[57]
colnames(spam)[56]
colnames(spam)[53]
colnames(spam)[7]
colnames(spam)[47]
colnames(spam)[49]
colnames(spam)[23]
colnames(spam)[6]

g =spamBin[,58]
yhat = predict(lm.fit)
lm.ghat =factor(ifelse(yhat>0.5,1,0))
sum(lm.ghat != g)
mean(lm.ghat != g)


library(class) 
kmax <- 200
err_test <- rep(NA,kmax) 
set.seed(30) 
tr <- sample(1:nrow(spamBin),3000) 

X=cbind(spamBin[,58],spamBin[,57],spamBin[,56],spamBin[,53],spamBin[,7],spamBin[,47],spamBin[,49],spamBin[,23],spamBin[,6])
spamBinTrainval <- X[tr,]
cat(" La table d'apprentissage : avec dim = " ,dim(spamBinTrainval),"\n")
spamBinTrainval



spamBinTest <- X[-tr,]
cat(" La table du Test : avec dim = " ,dim(spamBinTest),"\n")
spamBinTest


for (k in 1:kmax){
    pred <- knn(spamBinTrainval[,-1],spamBinTest[,-1],spamBinTrainval[,1],k) 
   err_test[k] <- sum(pred!=spamBinTest[,1])/length(spamBinTest[,1]) 
} 

plot(err_test,type="l",col=2,xlab="nombre de voisins", ylab="taux d'erreur") 
cat(" Le err_test Min  = " ,which.min(err_test) ,"\n")
err_test[k]


library(e1071)
Z <-cbind(spam[58],spam[57],spam[56],spam[53],spam[7],spam[47],spam[49],spam[23],spam[6])
Z.d = Z[,-1]
m <- naiveBayes(Z[,1] ~ ., data = Z.d)
m

m <- naiveBayes(Z.d,Z[,1]) 
m
table(predict(m,Z.d),Z[,1])


spamNormalise = spam
dim(spam)


vecteurI <- c()
vecteurJ <- c()

for (i in c(1:4601)){
    k = 0 
    for (j in c(1:57)){
      k = k + spam[i,j]
    }
    vecteurI[i]=k
}
cat(" le vecteur I contient : ",length(vecteurI) ,"elements\n" )

for (j in c(1:57)){
    k = 0 
    for (i in c(1:4601)){
      k = k + spam[i,j]
    }
    vecteurJ[j]=k
}
cat(" le vecteur J contient : ",length(vecteurJ) ,"elements\n" )


for (i in c(1:4601)){
    for (j in c(1:57)){
      spamNormalise[i,j]=spam[i,j]/(sqrt(vecteurI[i]*vecteurJ[j]))
    }
}
spamNormalise
#pour verifier
#spam
#spamNormalise

spamBinNormalise = spamNormalise
spamBinNormalise[,58] <- ifelse(spamNormalise[,58]=="spam", 1, 0)

lm.fit <- lm(spamBinNormalise[,58]~spamBinNormalise[,57]+spamBinNormalise[,56]+spamBinNormalise[,53]+spamBinNormalise[,7]+spamBinNormalise[,47]+spamBinNormalise[,49]+spamBinNormalise[,23]+spamBinNormalise[,6]) 
lm.beta <- lm.fit$coef 
sort(lm.beta)


g =spamBinNormalise[,58]
yhat = predict(lm.fit)
lm.ghat =factor(ifelse(yhat>0.5,1,0))
sum(lm.ghat != g)
mean(lm.ghat != g)

library(class) 
kmax <- 200
err_test <- rep(NA,kmax) 
set.seed(30) 
tr <- sample(1:nrow(spamBinNormalise),3000) 

X=cbind(spamBinNormalise[,58],spamBinNormalise[,57],spamBinNormalise[,56],spamBinNormalise[,53],spamBinNormalise[,7],spamBinNormalise[,47],spamBinNormalise[,49],spamBinNormalise[,23],spamBinNormalise[,6])
spamBinNormaliseTrainval <- X[tr,]
cat(" La table d'apprentissage : avec dim = " ,dim(spamBinNormaliseTrainval),"\n")
spamBinNormaliseTrainval


spamBinNormaliseTest <- X[-tr,] 
cat(" La table du Test : avec dim = " ,dim(spamBinNormaliseTest),"\n")
spamBinNormaliseTest


for (k in 1:kmax){
    pred <- knn(spamBinNormaliseTrainval[,-1],spamBinNormaliseTest[,-1],spamBinNormaliseTrainval[,1],k) 
   err_test[k] <- sum(pred!=spamBinNormaliseTest[,1])/length(spamBinNormaliseTest[,1]) 
} 

plot(err_test,type="l",col=2,xlab="nombre de voisins", ylab="taux d'erreur") 
cat(" Le err_test Min  = " ,which.min(err_test) ,"\n")
err_test[k]

library(e1071)
Z <-cbind(spam[58],spamBinNormalise[57],spamBinNormalise[56],spamBinNormalise[53],spamBinNormalise[7],spamBinNormalise[47],spamBinNormalise[49],spamBinNormalise[23],spamBinNormalise[6])
Z.d = Z[,-1]
m <- naiveBayes(Z[,1] ~ ., data = Z.d)
m

m <- naiveBayes(Z.d,Z[,1]) 
m
table(predict(m,Z.d),Z[,1])
