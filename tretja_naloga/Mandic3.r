library(combinat)
library(Rlab)

#PRVA NALOGA

#Pot opcije, izplačilo prodajne, izplačilo nakupne
prva.opcija <- c(50.00,52.50,49.88,52.37,49.75,52.24) #0, 0
druga.opcija <- c(50.00,52.50,55.12,57.88,60.78,63.81) #7.88,8.69
tretja.opcija <- c(50.00,47.50,49.88,47.38,45.01,42.76) #0,0
cetrta.opcija <- c(50.00,47.50,45.12,47.38,49.75,52.24) #2.26, 2.24
peta.opcija <- c(50.00,52.50,49.88,52.37,54.99,57.74) #2.49, 5.24



izplacilo <- function(vrsta, T, type){
  #če je opcija nakupna, odštevamo maksimume
  if(type == "call"){
    denar <- max((max(vrsta[(T+1):length(vrsta)]) - max(vrsta[1:T])),0)
  }
  #sicer odštevamo minimume
  else if(type == "put"){
    denar <- max((min(vrsta[(T+1):length(vrsta)]) - min(vrsta[1:T])),0)
  }
  return(denar)
}


#DRUGA NALOGA



binomski <- function(S0, u, d, U, R, T, type){
  q = (1 + R - d)/(u - d)
  
  diskontni.faktor <- 1/(1+R)^U
  
  #pripravimo si vse možne poti opcije
  
  poti <- hcube(rep(2,U), translation = -1) * u
  poti[poti < 1] <- d
  
  
  #pripravimo si posebno tabelo z verjetnostmi, dejansko nas pa zanima le vrednost zadnjega stolpca
  verjetnosti <- poti
  verjetnosti[verjetnosti == u] <- q
  verjetnosti[verjetnosti == d] <- 1-q
  
  #verjetnosti zmnožimo
  verjetnosti <- cbind(1,verjetnosti)
  verjetnosti <- t(apply(verjetnosti,1,cumprod))
  
  #sestavimo skupaj poti opcije in začetno vrednost, vse zmožimo
  cenovni.procesi <- cbind(S0, poti)
  
  cenovni.procesi <- t(apply(cenovni.procesi,1,cumprod))

  
  izplacila <- rep(0,length(cenovni.procesi[,1]))
  
  #izplačamo glede na tip opcije
  for(i in 1:length(cenovni.procesi[,1])){
      izplacila[i] <- izplacilo(cenovni.procesi[i,],T,type)
    }
    
  pricakovana.izplacila <- izplacila * verjetnosti[,U+1]
  
  premija  <- sum(pricakovana.izplacila) * diskontni.faktor
    

  return(premija)
}


#druga b

monte <- function(S0,u,d,U,R,T,type,N){
  #pripravimo si začetne podatke
  q = (1+R-d)/(u-d)

  diskontni.faktor <- 1/(1+R)^U
  
  izplacila <- rep(0,N)
  
  #ponavljamo N-krat
  for (i in 1:N){
    #vzorec simuliramo glede na do tveganja nevtralno verjetnost
    vzorec <- sample(c(u,d), (U-1), replace = TRUE, prob = c(q,1-q))
    
    #sestavimo cenovni proces za ta vzorec poti
    cenovni.proces <- cumprod(append(S0, vzorec))
    
    #sestavljamo vektor izplačil
    izplacila[i] <- izplacilo(cenovni.proces,T,type) #* verjetnost
    
  }
  #vse skupaj povprečimo in dobimo rezultat
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

var.desetkrat <- var(desetkrat)
povp.desetkrat <- mean(desetkrat)

hist(desetkrat,freq = 10, xlim = c(0,10), main = paste("Monte Carlo: N = 10"), col = "pink")
abline(v = povp.desetkrat,col = "blue",lwd=2)
abline(v = po.modelu, col = "green", lty = 2)
arrows(x0=povp.desetkrat, y0=0, x1 = povp.desetkrat + var.desetkrat, col="blue", length=0.1, lwd = 2)
arrows(x0=povp.desetkrat, y0=0, x1=povp.desetkrat - var.desetkrat, col="blue", length=0.1, lwd = 2)
legend("topright", legend=c("Monte Carlo", "analiza modela"), col=c("blue","green"), lty=c("solid","dashed"), cex=0.9)


var.stokrat <- var(stokrat)
povp.stokrat <- mean(stokrat)


hist(stokrat,freq = 10, xlim = c(0,10),main = paste("Monte Carlo: N = 100"), col ="pink")
abline(v = povp.stokrat, col = "blue",lwd=2)
abline(v = po.modelu, col = "green",lty = 2)
arrows(x0=povp.stokrat, y0=0, x1 = povp.stokrat + var.stokrat, col="blue", length=0.1, lwd = 2)
arrows(x0=povp.stokrat, y0=0, x1=povp.stokrat - var.stokrat, col="blue", length=0.1, lwd = 2)
legend("topright", legend=c("Monte Carlo", "analiza modela"), col=c("blue","green"), lty=c("solid","dashed"), cex=0.9)


var.tisockrat <- var(tisockrat)
povp.tisockrat <- mean(tisockrat)

hist(tisockrat,freq = 10, xlim = c(0,10),main = paste("Monte Carlo: N = 1000"), col = "pink")
abline(v = mean(tisockrat), col = "blue",lwd=2)
abline(v = po.modelu, col = "green",lty = 2)
arrows(x0=povp.tisockrat, y0=0, x1 = povp.tisockrat + var.tisockrat, col="blue", length=0.1, lwd = 2)
arrows(x0=povp.tisockrat, y0=0, x1 = povp.tisockrat - var.tisockrat, col="blue", length=0.1, lwd = 2)
legend("topright", legend=c("Monte Carlo", "analiza modela"), col=c("blue","green"), lty=c("solid","dashed"), cex=0.9)






