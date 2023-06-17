rm(list=ls())
####Création des fonctions
#Génération des données
sample<-function(szsampleh0,szsampleh1,h0gap){
  drawh1<-rnorm(szsampleh1)
  drawh0<-rnorm(szsampleh0)
  xobs<-c(drawh0,drawh1+rep(h0gap,szsampleh1))
  pval<-1-pnorm(xobs)
  names(pval)<-c(rep(0,szsampleh0),rep(1,szsampleh1))
  return(pval)
}
#Correction de Bonferroni
bonf<-function(pval,alpha){
  positives<-which(pval<=alpha/length(pval))
  return(positives)
}
#Correction de Holm-Bonferroni
hbonf<-function(pval,alpha){
  ordre<-sort(pval)
  positives<-c()
  i<-1
  while (ordre[i] <= alpha/(length(pval)+1-i)) {
    positives<-c(positives,ordre[i])
    i<-i+1
  }
  return(positives)
}
#Détection des faux positifs et autres
detec<-function(pval,positives){
  FP<-which(names(positives)==0)
  TP<-which(names(positives)==1)
  results<-c(length(FP),length(TP))
  return(results)
}
#Bonferroni complet
fullbonf<-function(szsampleh0=100,szsampleh1,h0gap,alpha=0.05,nbsample=100){
  fwer<-0
  for (k in 1:nbsample){
    pval<-sample(szsampleh0,szsampleh1,h0gap)
    positives<-bonf(pval,alpha)
    results<-detec(pval,positives)
    if (results[1]>0){
      fwer<-fwer+1
    }
  }
  fwerfinal<-fwer/nbsample
  print(paste("FWER =",fwerfinal))
  return(fwerfinal)
  print(fwerfinal)
}
test<-fullbonf(50,50,3,0.05,100)

nouvellefonction<-function(x){fullbonf(50,x,3,0.05,100)}

curve(Vectorize(nouvellefonction)(x),from=10,to=100,n=20)
  
