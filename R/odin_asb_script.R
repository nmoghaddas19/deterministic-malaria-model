library(odin)
library(ICDMM)

out <- run_model_example()
out$plot
View(out$dat)

# ITN only
out <- run_model(model = "odin_model_asb",
                 init_EIR = 50,
                 time = 1000,
                 asb_on = 550,
                 asb_off = 551,
                 feeding_rate = 0.15,
                 u_asb = 0.0,
                 mu0 = 0.096,
                 admin2 = "Kayes")

out2 <- run_model(model = "odin_model_asb",
                 init_EIR = 50,
                 time = 1000,
                 asb_on = 550,
                 asb_off = 551,
                 feeding_rate = 0.0,
                 u_asb = 0.0,
                 mu0 = 0.096,
                 admin2 = "Kayes")
par(las=1)
plot(out$t,out$prev, main= "Prevalance", type='l', ylim = c(0, 1))
lines(out2$t, out2$prev, col = "blue")
abline(v = 365, lty = 2)

plot(out$t[5450:5750], (out$Svasb+out$Evasb+out$Ivasb)[5450:5750], main= "Stained",
     type='l', xlab = "Day", ylab = "No. stained mosquitoes", lwd=2,
     col=2, frame.plot = F)
lines(out2$t, out2$Svasb+out2$Evasb+out2$Ivasb, col = 1, lwd=2)
grid()
legend(x="topright", legend = c("15% bait feeding", "  0% bait feeding"),
       col=c(2, 1), lty=1, lwd=2, bty="n")

plot(out$t[5450:5750], (out$Svasb+out$Evasb+out$Ivasb)[5450:5750]/out$mv[5450:5750],
     type='l', col=2, lwd=2, frame.plot = F, xlab = "Day", ylab = "Proportion dye fed")
grid()
lines(out2$t[5450:5750], (out2$Svasb+out2$Evasb+out2$Ivasb)[5450:5750]/out2$mv[5450:5750],
      type='l', col=1, lwd=2)
legend(x="topright", legend = c("15% bait feeding", "  0% bait feeding"),
       col=c(2, 1), lty=1, lwd=2, bty="n")
((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[5500:5550]
max((out$Svasb+out$Evasb+out$Ivasb)/out$mv)

seasonality_data <- ICDMM::load_file("admin_units_seasonal.rds")

# vary feeding rate and measure dyed fraction
dyed_fraction <- c()
for (i in 1:50) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = i/100,
                   u_asb = 0.0)
  dyed_fraction[i] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4005):(4010)])
}
plot(1:50/100, dyed_fraction, type="l", lwd=2, frame.plot = F,
     xlab="Feeding rate", ylab="Dyed fraction", ylim=c(0,0.5))
grid()

dyed_fraction_24hr <- c()
for (i in 1:50) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = i/100,
                   u_asb = 0.0)
  dyed_fraction_24hr[i] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4010):(4015)])
}
lines(1:50/100, dyed_fraction_24hr, lwd=2,
     col = "dodgerblue")
legend(x="topleft", legend = c("12-24hr post deployment", "24-36hr post deployment"),
       col=c("dodgerblue",1), lty=1, lwd=2, bty="n")


# vary time of day traps are active and measure dyed fraction
dyed_fraction <- c()
for (i in 1:20) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = 0.15,
                   u_asb = 0.0)
  dyed_fraction[i] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4000+i):(4005+i)])
}
plot(0:19/10*24, dyed_fraction, type = "l", lwd =2, frame.plot = F, ylab="Dyed fraction",
     xlab="Hours after ASB deployment", xlim=c(0,50), ylim=c(0,max(dyed_fraction)+0.02))
grid()

dyed_fraction_10 <- c()
for (i in 1:20) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = 0.10,
                   u_asb = 0.0)
  dyed_fraction_10[i] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4000+i):(4005+i)])
}
lines(0:19/10*24, dyed_fraction_10, lwd =2, col="dodgerblue")
legend(x="topleft", legend = c("15% bait feeding", "10% bait feeding"),
       col=c(1, "dodgerblue"), lty=1, lwd=2, bty="n")

# vary background mortality/ ITN use
dyed_fraction <- c()
for (i in seq(0,100,5)) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = 0.15,
                   u_asb = 0.0,
                   ITN_IRS_on = 365,
                   itn_cov = i/100,
                   num_int = 2)
  dyed_fraction[i/5+1] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4005):(4010)])
}
plot(seq(0,100,5), dyed_fraction, type = "l", lwd =2, frame.plot = F, ylab="Dyed fraction",
     xlab="ITN coverage", ylim=c(0,1), xlim=c(0,100))
grid()

dyed_fraction_35 <- c()
for (i in seq(0,100,5)) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = 0.35,
                   u_asb = 0.0,
                   ITN_IRS_on = 365,
                   itn_cov = i/100,
                   num_int = 2)
  dyed_fraction_35[i/5 +1] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4005):(4010)])
}
lines(seq(0,100,5), dyed_fraction_35, lwd=2, col=2)
legend(x="topleft", legend = c("35% bait feeding", "15% bait feeding"),
       col=c(2, 1), lty=1, lwd=2, bty="n")
plot(out$t, out$mu, frame.plot = F, lwd=2, type="l", ylim=c(0,max(out$mu)))

# okay what about background mortality
dyed_fraction <- c()
for (i in seq(1,50,3)) {
  out <- run_model(model = "odin_model_asb",
                   init_EIR = 50,
                   time = 1000,
                   admin2 = "Kayes",
                   asb_on = 400,
                   asb_off = 401,
                   feeding_rate = 0.15,
                   u_asb = 0.0,
                   mu0 = i/100)
  dyed_fraction[(i+2)/3] <- mean(((out$Svasb+out$Evasb+out$Ivasb)/out$mv)[(4005):(4010)])
}
plot(seq(1,50,3), dyed_fraction, type = "l", lwd =2, frame.plot = F,
     ylab="Dyed fraction", xlab="ITN coverage", ylim=c(0,1))
grid()



