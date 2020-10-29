# OBRESTNE KRIVULJE
# Finančni praktikum 2020/21
# Mitja Mandić

#prva naloga

euribor.15 <- read.table("prva_naloga/podatki/hist_EURIBOR_2015.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.15 <- as.data.frame(t(euribor.15[,c(1, 22, 42, 64, 84, 104, 126, 149, 170, 192, 214, 235)]))
euribor.16 <- read.table("prva_naloga/podatki/hist_EURIBOR_2016.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.16 <- as.data.frame(t(euribor.16[,c(1, 21, 42, 63, 84, 106, 128, 149, 172, 194, 215, 237)]))
euribor.17 <- read.table("prva_naloga/podatki/hist_EURIBOR_2017.csv", sep = ",", header = TRUE, row.names = 1, check.names = FALSE)
euribor.17 <- as.data.frame(t(euribor.17[,c(1, 23, 43, 66, 84, 106, 128, 149, 172, 193, 215, 237)]))

euribor <- rbind(euribor.15,euribor.16,euribor.17)

prva.c.6m <- ts(euribor[,"6m"], start = 2015, frequency = 12)
prva.c.12m <- ts(euribor[,"12m"], start = 2015, frequency = 12)

ts.plot(prva.c.6m,prva.c.12m, gpars = list(xlab = "Datum", ylab = "%", col = c("red", "blue")))
legend("topright", bty="n", lty=c(1,1), col=c("red","blue"),
       legend=c("6m", "12m"))
title("EURIBOR")              
graf.ts <- recordPlot()
#druga naloga

#1. julij 2015, 3. oktober 2016, 1. september 2017

druga.b <- euribor[c("01/07/2015", "03/10/2016", "01/09/2017"),]
#druga.b <- as.data.frame(t(druga.b))

  plot(t(druga.b[1,]), ylim = c(-0.4, 0.4), col = c("red"),x=c(1/4,1/2,1,2,3,6,9,12), pch = 16, ylab = "%", xlab = "Časovna enota")
points(t(druga.b[2,]), col = c("blue"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
points(t(druga.b[3,]), col = c("black"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[1,]),col = c("red"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[2,]),col = c("blue"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
lines(t(druga.b[3,]),col = c("black"),pch = 16,x=c(1/4,1/2,1,2,3,6,9,12))
legend("topleft",bty="n",lty = c(1,1),col=c("red", "blue", "black"), legend=c("1. julij 2015", "3. oktober 2016", "1. september 2017"))
title("Casovna struktura Euribor")
graf.druga.b <- recordPlot()
#Vse krivulje so naraščajoče, torej imajo višjo donosnost pri kasnejših datumih zapadlosti, torej imajo normalno obliko.
#So precej položne in z izjemo leta 2015 skozi negativne.



#tretja naloga
T <- 6
U <- 12

euribor.tretja <- euribor[,c("6m","12m")]
euribor.tretja[,"terminska"] <- 2 * ((1 + euribor.tretja[,"12m"]/100)/(1 + 1/2 * euribor.tretja[,"6m"]/100) - 1)*100 #račun terminske obrestne mere

napoved <- euribor.tretja[,"terminska"]
napoved[c(31,32,33,34,35,36)] <- 0
napoved[c(seq(7,36))] <- napoved[c(seq(1,30))]

napoved[c(1,2,3,4,5,6)] <- NA
euribor.tretja["napoved"] <- napoved #Dodan stolpec z napovedanimi vrednostmi.


  plot(x=euribor.tretja[seq(6,12),"napoved"],y=euribor.tretja[seq(6,12),"6m"], xlim = c(-0.3,0.5), ylim = c(-0.3,0.5), type="p",
                xlab = "napoved", ylab = "opazovano",col = "blue")
points(x=euribor.tretja[seq(13,24),"napoved"],y=euribor.tretja[seq(13,24),"6m"], col = "red")
points(x=euribor.tretja[seq(25,36),"napoved"],y=euribor.tretja[seq(25,36),"6m"], col = "green")
lines(x = c(-0.5,0,5), y = c(-0.5,0,5), col = "grey")
legend("topleft",bty="n",lty = c(1,1),col=c("blue", "red", "green"), legend=c("2015", "2016", "2017"))
abline(lm(euribor.tretja[,"6m"]~euribor.tretja[,"napoved"]))
title("6m euribor 2015-2017")       
graf.3c <- recordPlot()

plot(x=euribor.tretja[seq(6,12),"napoved"],y=euribor.tretja[seq(6,12),"6m"], xlim = c(-0.1,0.5), ylim = c(-0.1,0.5), type="p",
                     xlab = "napoved", ylab = "opazovano",col = "blue")
abline(lm(euribor.tretja[seq(6,12),"6m"]~euribor.tretja[seq(6,12),"napoved"]))
lines(x = c(-0.5,0,5), y = c(-0.5,0,5), col = "grey")
title("6m Euribor 2015")
graf.3d.prva <- recordPlot()

plot(x=euribor.tretja[seq(13,24),"napoved"],y=euribor.tretja[seq(13,24),"6m"], xlim = c(-0.3,0.3), ylim = c(-0.3,0.3), type="p",
                                     xlab = "napoved", ylab = "opazovano",col = "red")
abline(lm(euribor.tretja[seq(13,24),"6m"]~euribor.tretja[seq(13,24),"napoved"]))
lines(x = c(-0.5,0,5), y = c(-0.5,0,5), col = "grey")
title("6m Euribor 2016")
graf.3d.druga <- recordPlot()

plot(x=euribor.tretja[seq(25,36),"napoved"],y=euribor.tretja[seq(25,36),"6m"], xlim = c(-0.3,0.1), ylim = c(-0.3,0.1), type="p",
                       xlab = "napoved", ylab = "opazovano",col = "green")
abline(lm(euribor.tretja[seq(25,36),"6m"]~euribor.tretja[seq(25,36),"napoved"]))
lines(x = c(-0.5,0,5), y = c(-0.5,0,5), col = "grey")
title("6m Euribor 2017")
graf.3d.tretja <- recordPlot()

#odgovor 3.e
#Regresijska premica bi morala biti simetrala lihih kvadrantov, na kateri bi ležale vse narisane točke. Ker je napoved večja od
#izmerjene vrednosti, so točke na grafih v desnem spodnjem kotu.

#link na github repo: https://github.com/mitja-mandic/financni-praktikum