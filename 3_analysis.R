#TIME SERIES BIRTHS

pacman::p_load(forecast, tidyverse, readxl, astsa, urca, wats, nls2, cairo, scales, seastests)

births$pop_total = sub("383893566.707433","195497797",births$pop_total)
births$pop_total = as.numeric(births$pop_total)

births <- mutate(births, brazil_birth_rate = births$nasc/births$pop_total*1000)
births <- mutate(births, fetal_mort_rate = births$fetal/births$nasc*1000)
preterm$preterm_rate = preterm$preterm/preterm$nasc*1000

attach(births)

#Criação de série temporal nascimentos #

brazil_br1=ts(brazil_birth_rate, start = c(2001,01), end = c(2020,10), frequency = 12)
brazil_br3=ts(brazil_birth_rate, start = c(2001,01), end = c(2022,12), frequency = 12)

sarima_brbr=auto.arima(brazil_br1,
                       trace = T, 
                       stepwise = F, 
                       approximation=F)

previsao_brbr=forecast(sarima_brbr, h=26)

plot(previsao_brbr,
     main="",
     ylim=c(0.5,2),
     ylab="",
     xlab="Period",
     xaxt="n",
     cex.axis=1.2)

title("",
      adj = 0,  # Title to the right
      line = 0.25,
      cex.main = 1)

grid(nx = 5,
     ny = NA,
     lty = 3, col = "gray", lwd = 0.2)

abline( v=2020.8, col="black", lty=2, lwd=2)
abline( v=2020.2, col="red", lty=2, lwd=2)
abline( v=2016.85, col="green4", lty=2, lwd=2)
text( 2020.8, 2, "COVID-19 effect", col="black", cex=0.9, pos=4 )
text( 2020.3, 2, "Start of pandemics", col="red", cex=0.9, pos=2 )
text( 2016.85, 1.7, "Zika effect", col="green4", cex=0.9, pos=2 )

lines(brazil_br3, col='black')

  legend(1999.7, 1,
       legend=c("Observed Cases", 
                "ARIMA Prediction", 
                "Zyka Virus epidemics",
                "COVID-19 Pandemics",
                "Annual Movel Average"),
       pch = 8,
       fill = c("black",
                "dodgerblue",
                "yellow4",
                "gray",
                "green"),
       cex=0.8,
       x.intersp = 0.25,
       bty="n")

axis(1, at=2001:2023, labels=c(2001:2023), par(las=2), cex.axis=1)


par(new=T)
plot(ma(brazil_br3, order = 12),
     xaxt="n",
     ylim=c(0.5,2),
     yaxt="n",
     ylab="",
     xlab="",
     bty='n',
     col="green",
     lty=2,
     lwd=2)

#########

#Criação de série temporal nascimentos preterm#

brazil_sb1=ts(fetal_mort_rate, start = c(2001,01), end = c(2020,02), frequency = 12)
brazil_sb3=ts(fetal_mort_rate, start = c(2001,01), end = c(2022,12), frequency = 12)

sarima_sbbr=auto.arima(brazil_sb1,
                       trace = T, 
                       stepwise = F, 
                       approximation=F)

previsao_sbbr=forecast(sarima_sbbr, h=32)

plot(previsao_sbbr,
     main="",
     ylim=c(7,12),
     ylab="Stillbirths per 1.000 births",
     xlab="Period",
     xaxt="n",
     cex.axis=1.2)

title("",
      adj = 0,  # Title to the right
      line = 0.25,
      cex.main = 1)

grid(nx = 5,
     ny = NA,
     lty = 3, col = "gray", lwd = 0.2)


abline( v=2020.2, col="red", lty=2, lwd=2)
text( 2020.1, 150, "COVID-19 effect", col="red", cex=0.9, pos=4 )
text( 2020.3, 150, "Start of pandemics", col="red", cex=0.9, pos=2 )

lines(brazil_sb3, col='black')

legend(1999.7, 1,
       legend=c("Observed Cases", 
                "ARIMA Prediction", 
                "Zyka Virus epidemics",
                "COVID-19 Pandemics",
                "Annual Movel Average"),
       pch = 8,
       fill = c("black",
                "dodgerblue",
                "yellow4",
                "gray",
                "green"),
       cex=0.8,
       x.intersp = 0.25,
       bty="n")

axis(1, at=2001:2023, labels=c(2001:2023), par(las=2), cex.axis=1)


par(new=T)
plot(ma(brazil_sb3, order = 12),
     xaxt="n",
     ylim=c(7,12),
     yaxt="n",
     ylab="",
     xlab="",
     bty='n',
     col="green",
     lty=2,
     lwd=2)



#########

attach(preterm)

#Criação de série temporal nascimentos preterm#

brazil_pt1=ts(preterm_rate, start = c(2012,01), end = c(2020,02), frequency = 12)
brazil_pt3=ts(preterm_rate, start = c(2012,01), end = c(2022,12), frequency = 12)

sarima_ptbr=auto.arima(brazil_pt1,
                       trace = T, 
                       stepwise = F, 
                       approximation=F)

previsao_ptbr=forecast(sarima_ptbr, h=32)

plot(previsao_ptbr,
     main="",
     ylim=c(90,160),
     ylab="Birth per 1.000 inhabs",
     xlab="Years",
     xaxt="n",
     cex.axis=1.2)

title("",
      adj = 0,  # Title to the right
      line = 0.25,
      cex.main = 1)

grid(nx = 5,
     ny = NA,
     lty = 3, col = "gray", lwd = 0.2)


abline( v=2020.2, col="red", lty=2, lwd=2)
text( 2020.1, 150, "COVID-19 effect", col="red", cex=0.9, pos=4 )
text( 2020.3, 150, "Start of pandemics", col="red", cex=0.9, pos=2 )

lines(brazil_pt3, col='black')

legend(1999.7, 1,
       legend=c("Observed Cases", 
                "ARIMA Prediction", 
                "Zyka Virus epidemics",
                "COVID-19 Pandemics",
                "Annual Movel Average"),
       pch = 8,
       fill = c("black",
                "dodgerblue",
                "yellow4",
                "gray",
                "green"),
       cex=0.8,
       x.intersp = 0.25,
       bty="n")

axis(1, at=2012:2023, labels=c(2012:2023), par(las=2), cex.axis=1)


par(new=T)
plot(ma(brazil_pt3, order = 12),
     xaxt="n",
     ylim=c(90,160),
     yaxt="n",
     ylab="",
     xlab="",
     bty='n',
     col="green",
     lty=2,
     lwd=2)
