# KOLEKTIVNI MODEL TVEGANJA IN PANJERJEV ALGORITEM
# Finančni praktikum 2020/21
# Mitja Mandić


library(actuar)
vzorec <- scan("vzorec4.txt")
hist(vzorec)
hist <- recordPlot()

parameter <- mde(vzorec, pweibull, measure = "CvM", start = list(shape = 10, scale = 10))
shape_ocena <- parameter$estimate[1]
scale_ocena <-parameter$estimate[2]

hist(vzorec, breaks = 20,probability = TRUE, ylab = "Gostota")
curve(dweibull(x,shape_ocena,scale_ocena), from=2,to=7, add=TRUE, col = "red")
graf_prva <- recordPlot()


#vzorčna in izračunana porazdelitvena funkcija

plot(ecdf(vzorec), col = "blue", main = "", ylab = " ")
curve(pweibull(x,shape_ocena,scale_ocena), add = TRUE)
title("Vzorčna in porazdelitvena funkcija")
vzorcna_in_porazdelitvena <- recordPlot()

#PORAZDELITEV VSOTE

pv.n <- 25 * 0.5
v.n <- 25 * 0.5 * 0.5

#paramteri weibullove

pv.w <- scale * gamma(1+1/shape) 
v.w <- scale^2 * (gamma(1+2/shape)-(gamma(1+1/shape))^2)

e.s <- pv.n * pv.w
v.s <- v.w * pv.n + pv.w^2 * v.n

#DRUGA NALOGA

#druga a

dolzina <- 5
korak <- 1/10
diskretna <- discretize(pweibull(x, shape = shape_ocena , scale=scale_ocena), from = 0, to = 5,
                        step = korak)

#diskretna

#druga b

curve(pweibull(x, shape = shape, scale = scale), from = 0, to = dolzina, xname = "x",ylab = " ")
plot(stepfun(x = seq(0, dolzina - korak, korak),  y = diffinv(diskretna)),add = TRUE)
title("Diskretizacija Weibullove porazdelitve in CDF")

#druga c

panjer <- aggregateDist(method = "recursive", model.freq = "binomial",model.sev = diskretna, size = 25, prob = 0.5, 
                      x.scale = korak, tol = 0.001, maxit = 100000)

#druga d
#disperzija in upanje kumulativne škode
skoki <- knots(panjer)

pricakovana_vrednost.panjer <- sum(knots(panjer) * diff(panjer))

pricakovana_vrednost_kvadrat.panjer <- sum(skoki ^ 2 * diff(panjer)) 

varianca.panjer <- pricakovana_vrednost_kvadrat.panjer - pricakovana_vrednost.panjer^2

#TRETJA NALOGA

#tretja a

#simulacija vsot
vzorec_bin <- rbinom(10000,25,0.5)
simulacije <- as.data.frame(vzorec_bin)
colnames(simulacije) <- "binomska"
simulacije['vsota'] <- 0

i = 1
for (i in 1:10000){
  simulacije[i,"vsota"] <- sum(rweibull(simulacije[i,1],shape_ocena,scale_ocena))
  #i = i + 1
}

#tretja b

simulirana_pv <- mean(simulacije[,"vsota"]) #simulirana vrednost: 29.651; waldov obrazec: 29.7
simulirana_var <- var(simulacije[,"vsota"]) #simulirana vrednost: 37.257; waldov obrazec: 37.3

#tretja c

"Simulirana porazdelitev in porazdelitev kumulativne škode"
plot(ecdf(simulacije[,'vsota']), main = "")
plot(panjer, sub = "", add = TRUE, col = "Blue")
title("Simulirana porazdelitev in porazdelitev kumulativne škode")
