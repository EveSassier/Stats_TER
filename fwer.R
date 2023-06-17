rm(list=ls())


p<-10:10000
alphi<-0.05
fwer<-1-(1-alphi/p)^p
plot(p,fwer,type="l")

#simulation du nombre de faux positifs sur plusieurs tests multiples

alpha <- 0.05
nbFP <-c()
fwer<-0
n<-1000
m<-1000
for (k in 1:n) {
  tirage <- rnorm(m)
  if (length(tirage) %% 2 != 0) {
    tirage <- c(tirage, rnorm(1))
  }
  h1 <- c(rep(0, length(tirage) / 2), rep(3, length(tirage) / 2))
  xobs <- tirage + h1
  pval <- 1 - pnorm(xobs)
  positifs <- which(pval <= alpha)
  FP <- length(which(positifs <= length(tirage) / 2))
  if (FP >= 1) {
    fwer<-fwer+1
  }
  nbFP <- c(nbFP, FP)
}
length(nbFP)
mean(nbFP)
fwer/n


#simulation du nombre de faux positifs sur plusieurs tests multiples 
#en utilisant la correction de Bonferronni

alpha <- 0.05
nbFP <-c()
fwer<-0
n<-1000
m<-1000
for (k in 1:n) {
  tirage <- rnorm(m)
  if (length(tirage) %% 2 != 0) {
    tirage <- c(tirage, rnorm(1))
  }
  h1 <- c(rep(0, length(tirage) / 2), rep(3, length(tirage) / 2))
  xobs <- tirage + h1
  pval <- 1 - pnorm(xobs)
  positifs <- which(pval <= alpha/m)
  FP <- length(which(positifs <= length(tirage) / 2))
  if (FP >= 1) {
    fwer<-fwer+1
  }
  nbFP <- c(nbFP, FP)
}
length(nbFP)
mean(nbFP)
fwer/n

#simulation du nombre de faux positifs sur plusieurs tests multiples
#en utilisant la correction de Holm-Bonferroni

alpha <- 0.05
nbFP <-c()
fwer<-0
n<-1
m<-10
for (k in 1:n) {
  tirage <- rnorm(m)
  if (length(tirage) %% 2 != 0) {
    tirage <- c(tirage, rnorm(1))
  }
  h1 <- c(rep(0, length(tirage) / 2), rep(3, length(tirage) / 2))
  xobs <- tirage + h1
  #Création de la liste de pval ordrées et nommées selon h1 ou h0
  pval <- 1 - pnorm(xobs)
  names(pval)<-h1
  ordre<-sort(pval)
  print(ordre)
  #Détection des positifs
  positifs<-c()
  i<-1
    while (ordre[i] <= alpha/(length(tirage)+1-i)) {
      positifs<-c(positifs,ordre[i])
      i<-i+1
    }
  print(positifs)
  #Détection des faux-positifs
  FP <- length(which(names(positifs)==0))
  if (FP >= 1) {
    fwer<-fwer+1
  }
  nbFP <- c(nbFP, FP)
}
length(nbFP)
mean(nbFP)
fwer/n
dfhlkdshvlakvldfbpiv
  

  
