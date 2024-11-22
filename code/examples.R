#### Load all required packages ####
library(StratPal)
library(admtools)
library(paleoTS)

#### Set seed ####
# for reporducibility
set.seed(42)

#### Construct age-depth models ####
adm_list = list()
dist = paste0(seq(2, 12, by = 2), "km") # Positions in the carbonate platform, from the shore
for (pos in dist){ # Conctruction of age-depth models from tie points for each position
  adm_list[[pos]] = tp_to_adm(t = scenarioA$t_myr,
                              h = scenarioA$h_m[, pos],
                              L_unit = "m",
                              T_unit = "Myr")
}
# focus on two age-depth models here
adm2 = adm_list[["2km"]]
adm12 = adm_list[["12km"]]

#### Figure 2 ####
# age-depth models
pdf(file = "figs/fig2.pdf")
par(mfrow = c(1,2))
plot(adm2, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()
title("Platform Top")
plot(adm12, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()
title("Proximal Slope")
dev.off()

#### Figure 3 ####
# effect of stratigraphic biases on last occurrences

subdiv = 20

rolo = 200 # Rate of last occurrences
pdf(file = "figs/fig3.pdf")
par(mfrow = c(1,2))
p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = FALSE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Last Occurrences (Platform Top)")

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = FALSE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Last Occurrences (Proximal Slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))
dev.off()

#### Figure 4 ####
# effect of stratigraphic biases on fossil abundance
pdf(file = "figs/fig4.pdf")
par(mfrow = c(1,2))
p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil Abundance (Platform Top)")

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Fossil Abundance (Proximal Slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))
dev.off()

#### Figure 5 ####
# effect of water depth on fossil abundance
pdf(file = "figs/fig5.pdf")
par(mfrow = c(1,2))
gc = approxfun(scenarioA$t_myr, scenarioA$wd_m[,"12km"])
niche = snd_niche(opt = 100, tol = 30, cutoff_val = 0) # define niche with optimum at 100 m
plot(scenarioA$t_myr, gc(scenarioA$t_myr),
     type = "l",
     lwd = 3,
     xlab = "Time [Myr]",
     ylab = "Water depth [m]",
     main = "Water Depth on the Proximal Slope")

p3(rate = rolo, min_time(adm12), max_time(adm12)) |>
  apply_niche(niche, gc) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil abundance (platform top)")

dev.off()