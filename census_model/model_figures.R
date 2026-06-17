library(data.table)
m22 <- readRDS("census_model/out2022_4.RDS")
m22_df <- data.table(
  N = m22$q50$N,
  lci = m22$q2.5$N,
  uci = m22$q97.5$N,
  date = seq.Date(as.Date("2022-08-15"), as.Date("2022-10-31"))
)

library(ggplot2)
ggplot(m22_df) +
  geom_pointrange(aes(x = date, y = N, ymin = lci, ymax = uci))


m23 <- readRDS("census_model/out2023_5.RDS")


## start of some plotting ideas (very draft and currently ugly!!)
K <- 78
days <- seq.Date(as.Date("2023-08-15"), as.Date("2023-10-31"))
#plot abundance and distribution of marked individuals
plot(
  m23$q50$N,
  ylim = c(0, 135),
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1,
  pch = 16,
  main = '2023'
) # total marked in system by day
segments(1:K, m23$q2.5$N, 1:K, m23$q97.5$N)
axis(1, at = 1:K, format(days, '%m-%d'))


points(1:K - .2, m23$q50$Nnan, col = "red", pch = 16) # total in nanticoke by day
segments(1:K - .2, m23$q2.5$Nnan, 1:K - .2, m23$q97.5$Nnan, col = "red")

points(1:K + .2, m23$q50$Nmc, col = "blue", pch = 16) # total in mc by day
segments(1:K + .2, m23$q2.5$Nmc, 1:K + .2, m23$q97.5$Nmc, col = "blue")

legend(
  "topright",
  legend = c("Nanticoke system", "Nanticoke reaches", "MC reaches"),
  text.col = c("black", "red", "blue"),
  bty = 'n',
  cex = 1.1
)


# plot daily abundance and cumulative abundance of individuals
plot(
  out2022$q50$Nsuper,
  ylim = c(0, 110),
  pch = 16,
  ,
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1
) # total marked in system by day
segments(1:K, out2022$q2.5$Nsuper, 1:K, out2022$q97.5$Nsuper)
axis(1, at = 1:K, format(days, '%m-%d'))

points(1:K - .25, out2022$q50$N, col = "red", pch = 16) # total marked in system by day
segments(1:K - .25, out2022$q2.5$N, 1:K - .25, out2022$q97.5$N, col = "red")
legend(
  "topleft",
  legend = c("Cumulative", "Daily"),
  text.col = c("black", "red"),
  bty = 'n',
  cex = 1.2
)

#Idea: plot persistence probs.
plot(
  out2022$q50$phi,
  ylim = c(0.80, 1.00),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2022$q2.5$phi, 1:K, out2022$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))

plot(
  jags.data$temp,
  out2022$q50$phi,
  ylim = c(0.80, 1.00),
  xlab = "Temp",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(jags.data$temp, out2022$q2.5$phi, jags.data$temp, out2022$q97.5$phi)

#Idea: plot recruitment probs.
plot(
  out2022$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2022$q2.5$b, 1:K, out2022$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))

#Temperature overlaying Persistence
plot(
  out2022$q50$phi,
  xaxt = "n",
  ylim = c(0.80, 1.00),
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
)
segments(1:K, out2022$q2.5$phi, 1:K, out2022$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  jags.data$temp,
  pch = 16,
  col = "red",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(12, 30)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Temperature (\u00B0C)", side = 4, line = 3)


#Flow overlaying Persistence
plot(
  out2022$q50$phi,
  xaxt = "n",
  ylim = c(0.80, 1.00),
  xlab = "Date",
  ylab = "Daily persistence probability",
  las = 1,
  pch = 16
)
segments(1:K, out2022$q2.5$phi, 1:K, out2022$q97.5$phi)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  flow$average,
  pch = 16,
  col = "blue",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(0, 400)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Flow (ft^3)/s)", side = 4, line = 3)

#Temperature overlaying Recruitment
plot(
  out2022$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2022$q2.5$b, 1:K, out2022$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  jags.data$temp,
  pch = 16,
  col = "red",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(12, 30)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Temperature (\u00B0C)", side = 4, line = 3)

#Flow overlaying Recruitment
plot(
  out2022$q50$b,
  ylim = c(0.00, 0.26),
  xaxt = "n",
  xlab = "Date",
  ylab = "Daily recruitment probability",
  las = 1,
  pch = 16
) # total marked in system by day
segments(1:K, out2022$q2.5$b, 1:K, out2022$q97.5$b)
axis(1, at = 1:K, format(days, '%m-%d'))
par(new = TRUE)
plot(
  flow$average,
  pch = 16,
  col = "blue",
  axes = FALSE,
  xlab = "",
  ylab = "",
  ylim = c(0, 400)
)
axis(side = 4, xlab = "", ylab = "")
mtext("Flow (ft^3)/s)", side = 4, line = 3)

#MC vs UNR
plot(
  out2022$q50$Nmc,
  ylim = c(0, 50),
  xaxt = "n",
  xlab = "Date",
  ylab = "Sturgeon (marked+unmarked)",
  las = 1,
  pch = 16
)
segments(1:K, out2022$q2.5$Nmc, 1:K, out2022$q97.5$Nmc)
axis(1, at = 1:K, format(days, '%m-%d'))

points(1:K - .2, UNR_2021$UNR, col = "blue", pch = 16) # total in UNR by day
segments(1:K - .2, UNR_2021$UNR_2.5, 1:K - .2, UNR_2021$UNR_97.5, col = "blue")
legend(
  "topright",
  legend = c("Marshyhope Creek Reaches", "Upper Nanticoke Reach"),
  text.col = c("black", "blue"),
  bty = 'n',
  cex = 1.1
)
