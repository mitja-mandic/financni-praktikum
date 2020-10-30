library(actuar)
vzorec <- scan("vzorec4.txt")
hist(vzorec)
hist <- recordPlot()

parameter <- mde(vzorec, pweibull, measure = "CvM", start = list(shape = 3, scale = 3))
shape <- parameter$estimate[1]
scale <-parameter$estimate[2]

hist(vzorec, breaks = 20,probability = TRUE)
curve(dweibull(x,shape,scale), from=2,to=7, add=TRUE)
graf_prva <- recordPlot()

#vzorčna in izračunana porazdelitvena funkcija
plot(ecdf(vzorec))
curve(pweibull(x,shape,scale), add = TRUE)
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
n = 7
h = 1/10
diskretizacija <- discretize(pweibull(x,shape,scale), from = 0, to = n*h, step = h)

#druga c

plot(stepfun(x = seq(0, n*h - 2*h, h),  y = diskretizacija*1/h))

stepfun(c(0,4.8,1/10), diskretizacija)
curve(pweibull(x,shape,scale), from=0,to=5)


#kalanov
#nh <- 7
#h <- 1/10
#diskret_Y <- discretize(
#  ppareto1(x, shape = shape, min = min), step = h, from = 0, to = nh)
#diskret_Y
#
# (b)
#plot(stepfun(x = seq(h, nh-h, h),  y = diskret_Y * 10, ))
#curve(dpareto1(x, shape = shape, min = min), from = 0, to = nh, add = TRUE)