#prva naloga

euribor.15 <- read.table("podatki/hist_EURIBOR_2015.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.15 <- as.data.frame(t(euribor.15[,c(1, 22, 42, 64, 84, 104, 126, 149, 170, 192, 214, 235)]))
euribor.16 <- read.table("podatki/hist_EURIBOR_2016.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.16 <- as.data.frame(t(euribor.16[,c(1, 21, 42, 63, 84, 106, 128, 149, 172, 194, 215, 237)]))
euribor.17 <- read.table("podatki/hist_EURIBOR_2017.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.17 <- as.data.frame(t(euribor.17[,c(1, 23, 43, 66, 84, 106, 128, 149, 172, 193, 215, 237)]))

euribor <- rbind(euribor.15,euribor.16,euribor.17)

prva.c.6m <- ts(euribor[,"6m"], start = 2015, frequency = 12)
prva.c.12m <- ts(euribor[,"12m"], start = 2015, frequency = 12)

graf.ts <- ts.plot(prva.c.6m,prva.c.12m, gpars = list(xlab = "Datum", ylab = "%", col = c("red", "blue")))
legend("topright", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c("6m", "12m"))
title("EURIBOR")              

#druga naloga

#1. julij 2015, 3. oktober 2016, 1. september 2017

druga.b <- euribor[c("01/07/2015", "03/10/2016", "01/09/2017"),]
#druga.b <- as.data.frame(t(druga.b))
graf.druga.b <- plot(t(druga.b[1,]), ylim = c(-0.4, 0.4), col = c("red"),x=c(1/4,1/2,1,2,3,6,9,12), pch = 16, ylab = "%", xlab = "Časovna enota")
points(t(druga.b[2,]), col = c("blue"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
points(t(druga.b[3,]), col = c("black"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[1,]),col = c("red"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[2,]),col = c("blue"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[3,]),col = c("black"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
legend("topleft",bty="n",lty = c(1,1),col=c("red", "blue", "black"), legend=c("1. julij 2015", "3. oktober 2016", "1. september 2017"))
title("Časovna struktura Euribor")


#tretja naloga
T <- 6
U <- 12

euribor.tretja <- euribor[,c("6m","12m")]

