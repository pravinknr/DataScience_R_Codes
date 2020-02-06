datasets::CO2

head(CO2)

tail(CO2)

summary(CO2)

CO2[,c(1,2,4)]

CO2[20,c(1,5)]

CO2[1:20,c(1,5)]

summary(CO2$uptake)    

summary(CO2$conc)

summary(CO2$Treatment)

plot(CO2$conc, type = "l", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants")

plot(CO2$uptake, type = "s", col="green")

barplot(CO2$conc, horiz = FALSE)

barplot(CO2$uptake, xlab = "Plants", ylab = "Uptake of Plants", col = "blue")

hist(CO2$uptake, col="orange")

boxplot(CO2)

boxplot(CO2$conc)

boxplot(CO2[,c(4,5)])

par(mfrow=c(4,4),mar=c(2,5,2,2), bty="n", las=0)
plot(CO2$conc, type = "l", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants")

plot(CO2$uptake, type = "s", col="green")

barplot(CO2$conc, horiz = FALSE)

barplot(CO2$uptake, xlab = "Plants", ylab = "Uptake of Plants", col = "blue")

hist(CO2$uptake, col="orange")

boxplot(CO2)

boxplot(CO2$conc)

boxplot(CO2[,c(4,5)])

plot(CO2$uptake, type = "p", col="green")

plot(CO2$conc, type = "c", col="red", xlab = "Plant", ylab = "Concentration of CO2", main="CO2 Concentration in plants")

plot(CO2$Treatment, col="pink")
