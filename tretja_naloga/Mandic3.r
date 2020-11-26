library(combinat)
library(Rlab)

#PRVA NALOGA
izplacilo <- function(vrsta, T, type){
  #denar <- 0
  if(type == "call"){
    denar <- max((max(vrsta[(T+1):length(vrsta)]) - max(vrsta[1:T])),0)
  }
  else if(type == "put"){
    denar <- max((min(vrsta[(T+1):length(vrsta)]) - min(vrsta[1:T])),0)
  }
  return(denar)
}

#DRUGA NALOGA

binomski <- function(S0, u, d, U, R, T, type){
  
  poti <- hcube(rep(2,U), translation = -1) * u
  poti[poti < 1] <- d
  #print(poti)
  #poti <- cbind(1, poti)
  #print(poti)
  
  q = (1 + R - d)/(u - d)
  
  verjetnosti <- poti
  verjetnosti[verjetnosti == u] <- q
  verjetnosti[verjetnosti == d] <- 1-q
  
  verjetnosti <- cbind(1,verjetnosti)
  verjetnosti <- t(apply(verjetnosti,1,cumprod))
  
  cenovni.procesi <- cbind(S0, poti)
  
  cenovni.procesi <- t(apply(cenovni.procesi,1,cumprod))
  #print(cenovni.procesi)
  
  diskontni.faktor <- 1/(1+R)^U
  
  izplacila <- rep(0,length(cenovni.procesi[,1]))
  

  for(i in 1:length(cenovni.procesi[,1])){
      izplacila[i] <- izplacilo(cenovni.procesi[i,],T,type)
    }
    
  pricakovana.izplacila <- izplacila * verjetnosti[,U+1]
  
  premija  <- sum(pricakovana.izplacila) * diskontni.faktor
    
    
  #print(verjetnosti)
  return(premija)
}


#druga b

monte <- function(S0,u,d,U,R,T,type,N){

  q = (1+R-d)/(u-d)

  diskontni.faktor <- 1/(1+R)^U
  
  izplacila <- rep(0,N)
  
  for (i in 1:N){
    
    vzorec <- sample(c(u,d), (U-1), replace = TRUE, prob = c(q,1-q))
    
    #print(pot)
    
    #verjetnosti <- vzorec
    
    #verjetnosti[verjetnosti == u] <- q
    
    #verjetnosti[verjetnosti == d] <- 1 - q
    #print(verjetnosti)
    #verjetnost <- prod(verjetnosti)
    
    #print(verjetnost)
    
    cenovni.proces <- cumprod(append(S0, vzorec))
    #print(cenovni.proces)

    izplacila[i] <- izplacilo(cenovni.proces,T,type) #* verjetnost
    
  }
  
  #povp <- mean(izplacila)
  
  povp1 <- 1/N
  
  pricakovana.izplacila <- sum(izplacila)
  
  premija <- sum(izplacila) * povp1 * diskontni.faktor
  
  return(premija)
}


#TRETJA NALOGA

po.modelu <- binomski(60,1.05,0.95,15,0.01,8,"put")

desetkrat <- rep(0,100)
stokrat <- rep(0,100)
tisockrat <- rep(0,100)
for (i in 1:100) {
  desetkrat[i] <- monte(60,1.05,0.95,15,0.01,8,"put",10)
  stokrat[i] <- monte(60,1.05,0.95,15,0.01,8,"put",100)
  tisockrat[i] <- monte(60,1.05,0.95,15,0.01,8,"put",1000)
}
simulirane <- cbind(desetkrat, stokrat, tisockrat)

#abline(b = mean(desetkrat), add = TRUE)
hist(desetkrat,freq = 10, xlim = c(0,10), main = paste("Monte Carlo: N = 10"), col = "pink")
abline(v = mean(desetkrat),col = "blue")
abline(v = po.modelu, col = "green")


#lines(x = mean(desetkrat))

hist(stokrat,freq = 10, xlim = c(0,10),main = paste("Monte Carlo: N = 100"), col ="pink")
abline(v = mean(stokrat), col = "blue")
abline(v = po.modelu, col = "green")

hist(tisockrat,freq = 10, xlim = c(0,10),main = paste("Monte Carlo: N = 1000"), col = "pink")
abline(v = mean(tisockrat), col = "blue")
abline(v = po.modelu, col = "green")







