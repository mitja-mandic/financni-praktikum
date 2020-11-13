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
  
  #print(verjetnosti)
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
  #q = (1+R-d)/(u-d)
  diskontni.faktor <- 1/(1+R)^U
  izplacila <- rep(0,N)
  
  for (i in 1:N){
    vzorec = sample(c(0,1), U, replace = TRUE)
    #print(vzorec)
    pot <- vzorec * u
    pot[pot < 1] <- d
    
    #verjetnosti <- pot
    #verjetnosti[verjetnosti == u] <- q
    #verjetnosti[verjetnosti == d] <- 1 - q
    #verjetnosti <- cumprod(verjetnosti)
    #print(pot)
    #print(pot)
    #cenovni.proces <- append(S0, pot)
    
    cenovni.proces <- cumprod(append(S0, pot))
    print(cenovni.proces)

    izplacila[i] <- izplacilo(cenovni.proces,T,type) #* verjetnosti[length(verjetnosti)]
    
  }
  pricakovana.izplacila <- sum(izplacila) / N
  premija <- pricakovana.izplacila * diskontni.faktor
  return(premija)
}

