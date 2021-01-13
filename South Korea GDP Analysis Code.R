library(Hmisc)
library(pastecs)
library(ggplot2)
library(TSA)
library(tseries)
library(forecast)
library(lmtest)
library("readxl")
library(MLmetrics)
library(FinTS)
library(systemfit)
library(lattice)

data = read_excel("D:/Brian Stefano/Semester 4/Makroekonomi/Data Korea Selatan 2.xlsx")
#data2 = read.csv("C:/Users/lenovo/Documents/Assignment/Semester 4/Makroekonomi/Tugas/DP_LIVE_13032020035412726.csv")
#data3 = data2$Value
#multiplier = lm(data$`Belanja Rumah Tannga`~data$GDP)$coefficients
#(Intercept)     data$GDP 
#3.719357e+13 4.798302e-01 

#save(data, file = "Data Korea Selatan.rda")

YLD = 0
for (i in 1:15){
  YLD[i] = data$YL[44+i]/data$E[44+i]
}
RM = data$M/data$CPI

#2. 
xyplot(data$Y ~ data$C + data$I, col = c("blue", "purple"), main = "Plot Y terhadap C dan I", abline(-7.289e+13, 2.069))
abline(c(-7.289e+13, 2.069), col ="red")

#plot(data$Y ~ data$C)
#legend(1e+14, 1e+15, c("C", "I"), col=c("blue", "purple"), lty=1, cex=1)
plot(data$i, data$I, main = "Plot I terhadap i")
#legend(3, 1e+14, c("I"), col=c("black"), lty=1, cex=1)

coeff=coefficients(reg)
eq = paste0("y = ", round(coeff[2],1), "*x ", round(coeff[1],1))
plot(data$C, data$Y, main = "Plot Regresi Data Y terhdap C")
abline(reg, col = "red")

plot(data$I, data$Y, abline(c(8.564e+11,2.994e+00), col="red"), main = "Plot Regresi Data Y terhadap I")
abline(8.564e+11,2.994e+00)

#3.
lm(data$Y ~ data$C)
lm(data$I ~ data$Y+data$i)

#4.
summary(lm(data$Y ~ data$C))
summary(lm(data$I ~ data$Y+data$i))
summary(lm(data$Y ~ data$I))
summary(lm(data$I ~ data$i))
plot(data$i, data$I, main = "Plot Regresi Data I terhadap i")
abline(c(4.795e+14, -2.462e+13), col = "red")

#5. 
plot(data$Y, data$IM, main = "Plot IM terhadap Y")
plot(YLD, data$X[45:59], main = "Plot X terhadap YLD")


#6.
lm(data$IM ~ data$Y)
lm(data$X[45:59] ~ YLD)
plot(data$Y, data$IM, main = "Plot Regresi Data Y terhadap IM")
abline(c(-7.569e+10, 1.192e-04), col = "red")

#7.
summary(lm(data$IM ~ data$Y))
summary(lm(data$X[45:59] ~ YLD))
plot(YLD, data$X[45:59], abline(c(2.791e+14, 4.422e+00), col="red"), main = "Plot Regresi Data X terhadap YLD")

#8.


#9. 
plot(data$i, data$Y, abline(c(1.526e+15, -6.964e+13), col = "red"), main = "Plot Regresi Data Y terhadap i")
plot(data$G, data$Y, abline(c(2.074e+14, 5.846e+00), col = "red"), main = "Plot Regresi Data Y terhadap G")
xyplot(data$Y ~ data$i + data$G, main = "Plot Y terhadap i dan G")
plot(YLD, data$Y[45:59], abline(c(8.643e+14, 5.033e+00), col ="red"), main = "Plot Regresi Data Y terhadap YLD")

#10. 
lm(data$Y[45:59] ~ data$i[45:59]+data$G[45:59]+YLD)
lm(data$Y[45:59] ~ data$i[45:59])
lm(data$Y[45:59] ~ data$G[45:59])
lm(data$Y[45:59] ~ YLD)

#11. 
summary(lm(data$Y[45:59] ~ data$i[45:59]+data$G[45:59]+YLD))

#12. 

#13. 
rumahtangga <- data$C[45:59]~data$Y[45:59]
investasi <- data$I[45:59]~data$Y[45:59]+data$i[45:59]
ekspor <- data$X[45:59]~YLD
impor <- data$IM[45:59]~data$Y[45:59]
sys <- list(rumahtangga, investasi, ekspor, impor)
instr <- ~data$G[45:59]+data$i[45:59]+YLD
keynes.sys <- systemfit(sys, inst=instr, method="2SLS")
summary(keynes.sys)

#(mean = mean(data$C[45:59]))
#JKT = 0
#for (k in 1:15){
#  JKT = SST+(data$C[45+k-1]-mean)^2
#}

#(dfE   <- lm(data$C ~ data$Y)$df.residual)
#(dfReg <- nrow(data) - 1 - dfE)
#MSreg <- SSreg / dfReg
#MSE   <- 2.5481075069066e+25
#SSreg   <- 1.1339576484839e+21
#(Fstat <- SSreg/(var(keynes.sys[["eq"]][[4]][["fitted.values"]]))^0.5)
#(pval  <- pf( Fstat , 1, 13, lower.tail=FALSE ))

#14. 

#=================================================================================================================

#1. 
plot(data$Y, RM, abline(c(-2.214e+12, 1.406e-02), col = "red"), main = "Plot Regresi Data RM terhadap Y")
plot(data$i, RM, abline(c(2.209e+13, -1.960e+12), col ="red"), main = "Plot Regresi Data RM terhadap i")
#xyplot(RM ~ data$Y + data$i, main = "Plot RM terhadap Y dan i")

#2.
lm(RM ~ data$Y+data$i)
lm(RM ~ data$Y)
lm(RM ~ data$i)

#3.
summary(lm(RM ~ data$Y+data$i))

#=================================================================================================================
#1. 

#2. 

#3. 
xyplot(data$i[45:59] ~ data$G[45:59] + YLD, main = "Plot i terhadap G dan YLD")

#4. 
lm(data$Y[45:59] ~ data$G[45:59]+YLD+RM[45:59])
lm(data$i[45:59] ~ data$G[45:59]+YLD+RM[45:59])

#5. 
summary(lm(data$Y[45:59] ~ data$G[45:59]+YLD+RM[45:59]))
summary(lm(data$i[45:59] ~ data$G[45:59]+YLD+RM[45:59]))

lm(data$i[45:59] ~ data$G[45:59])
plot(data$G[45:59], data$i[45:59], abline(c(7.608e+00, -2.333e-14), col = "red"), main = "Plot Regresi Data i terhadap G")

lm(data$i[45:59] ~ YLD)
plot(YLD, data$i[45:59], abline(c(5.213e+00, -2.333e-14), col = "red"), main = "Plot Regresi i terhadap YLD")

lm(data$i[45:59] ~ RM[45:59])
plot(RM[45:59], data$i[45:59], abline(c(7.045e+00, -2.212e-13), col = "red"), main = "Plot Regresi i terhadap RM")
