library(actuar)
vzorec <- scan("vzorec4.txt")
hist(vzorec)
hist <- recordPlot()

parameter <- mde(vzorec, pweibull, measure = "CvM", start = list(shape = 10, scale = 10))
shape_ocena <- parameter$estimate[1]
scale_ocena <-parameter$estimate[2]

hist(vzorec, breaks = 20,probability = TRUE)
curve(dweibull(x,shape_ocena,scale_ocena), from=2,to=7, add=TRUE)
graf_prva <- recordPlot()


#vzorčna in izračunana porazdelitvena funkcija

plot(ecdf(vzorec))
curve(pweibull(x,shape_ocena,scale_ocena), add = TRUE)
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
#n = 7
#h = 1/10
#diskretizacija <- discretize(pweibull(x,shape,scale), from = 0, to = n*h, step = h)

#druga a

dolzina <- 5
korak <- 1/10
diskretna <- discretize(pweibull(x, shape = shape_ocena , scale=scale_ocena), from = 0, to = dolzina - korak ,step = korak,by = korak)


#diskretna <- discretize(
#  pweibull(x, shape = shape_ocena , scale=scale_ocena),step = korak, from = 0, to = dolzina)

diskretna
#druga b

plot(stepfun(x = seq(0, dolzina - 3 * korak, korak),  y = diskretna * 1 / korak))
curve(pweibull(x, shape = shape, scale = scale), from = 0, to = dolzina, add = TRUE)
 

#druga c


#aggregateDist(method = "recursive", data.frame(diskretna))


#TRETJA NALOGA

#tretja a
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

#tretja d




