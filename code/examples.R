library(StratPal)
library(admtools)
library(paleoTS)
adm_list = list()
dist = paste0(seq(2, 12, by = 2), "km") # Positions in the carbonate platform, from the shore
for (pos in dist){ # Conctruction of age-depth models from tie points for each position
  adm_list[[pos]] = tp_to_adm(t = scenarioA$t_myr,
                              h = scenarioA$h_m[, pos],
                              L_unit = "m",
                              T_unit = "Myr")
}
adm2 = adm_list[["2km"]]
adm12 = adm_list[["12km"]]
rolo = 200 # Rate of last occurrences
rofo = 200 # Rate of first occurrences

plot(adm2, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()
plot(adm12, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()

subdiv = 20

p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = FALSE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Last occurrences (platform top)")

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = FALSE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Last occurrences (proximal slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))


subdiv = 20

p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil abundance (platform top)")

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Fossil abundance (proximal slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))


gc = approxfun(scenarioA$t_myr, scenarioA$wd_m[,"12km"])
niche = snd_niche(opt = 100, tol = 30, cutoff_val = 0) # define niche with optimum at 100 m
plot(scenarioA$t_myr, gc(scenarioA$t_myr),
     type = "l",
     xlab = "Time [Myr]",
     ylab = "Water depth [m]",
     main = "Water depth on the proximal slope")

p3(rate = rolo, min_time(adm12), max_time(adm12)) |>
  apply_niche(niche, gc) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil abundance (platform top)")