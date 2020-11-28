# GLAJENJE ČASOVNIH VRST
# Finančni praktikum 2020/21
# Mitja Mandić

#PRVA NALOGA

#uvoz in obdelava podatkov
platina <- read.csv("cetrta_naloga/platina20.csv")

platina_zakljuceni <- platina$Close


platina_zakljuceni <- gsub(",", "", platina_zakljuceni)
platina_zakljuceni <- gsub("\\$", "", platina_zakljuceni)

#podatki do sredine maja 2020

dolzina <- 129

platina_zakljuceni <- as.numeric(rev(platina_zakljuceni[1:dolzina]))

graf_platina <- ts.plot(platina_zakljuceni, ylab = "Cena")
title("Platina")


#DRUGA NALOGA


#druga a
G <- function(vrsta, k) {
  
  glajene_vr <- c()
  #seštevamo delne vsote časovne vrste in jih delimo s k
  for (i in k:length(vrsta)) {
    glajene_vr[i] <- (1/k) * sum(vrsta[(i-k+1):i])
  }
  
  return(ts(glajene_vr))
}



#glajenje reda 7, k = 7

glajena.7 <- G(platina_zakljuceni, 7)

naslednji_dan <- glajena[length(glajena)]

ts.plot(platina_zakljuceni,glajena.7, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 7")

#druga d

#standardna kvadratična napaka
MSE.7 <- sum((platina_zakljuceni[8:dolzina-1]-glajena.7[8:dolzina-1]) ^ 2) / (dolzina - 7)

#postopek ponovljen še za reda k = 14 in k = 30
#k = 14
glajena.14 <- G(platina_zakljuceni,14)

ts.plot(platina_zakljuceni, glajena.14, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 14")
glajena.14.graf <- recordPlot

MSE.14 <- sum((platina_zakljuceni[15:dolzina-1]-glajena.14[15:dolzina-1]) ^ 2) / (dolzina - 14)

#k = 30
glajena.30 <- G(platina_zakljuceni,30)

ts.plot(platina_zakljuceni, glajena.30, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 30")
glajena.30.graf <- recordPlot()

MSE.30 <- sum((platina_zakljuceni[31:dolzina-1]-glajena.14[31:dolzina-1]) ^ 2) / (dolzina - 31)


par(mfrow = c(2,2))
ts.plot(platina_zakljuceni,glajena.7, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 7")
ts.plot(platina_zakljuceni, glajena.14, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 14")
ts.plot(platina_zakljuceni, glajena.30, ylab = "Cena", col = c("black", "red"))
title("Glajenje reda 30")



#TRETJA NALOGA


#eksponentno glajenje

EG <- function(vrsta, alpha){
  l <- c()
  #začetna vrednost se ujema s prvo vrednostjo iz časovne vrste
  l[1] <- vrsta[1]
  for (i in 2:length(vrsta)){
    #rekurzivna formula za računanje naprej
    l[i] <- alpha * vrsta[i] + (1-alpha) * l[i-1]
  }
  return(ts(l))
}

#izbral sem vrednost 0.2, izračunal eksponentno glajeno vrsto.
EG.02 <- EG(platina_zakljuceni, 0.2)

ts.plot(EG.02, platina_zakljuceni, col = c("red", "black"), ylab = "Cena")
title("Eskponentno glajenje")





