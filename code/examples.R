#### Load all required packages ####
library(StratPal)
library(admtools)
library(paleoTS)

#### Set seed ####
# for reporducibility
set.seed(42)

#### Figure output settings ####
# sized for print in a thesis: 116 mm print width, 600 dpi PNG plus vector PDF.
# Base font size is 8 pt. Text sizes:
#   panel titles 9 pt, panel letters (A, B, ...) 10 pt bold,
#   axis titles 8 pt, tick labels 7 pt
base_pt = 8
mm_to_in = 1 / 25.4
fig_width_mm = 116
png_res = 600 # dpi
fig_state = new.env() # remembers the open figure between open_fig and close_fig
open_fig = function(file, height_mm, mfrow){
  # file: output path without extension, a .png and a .pdf version are written
  fig_state$file = file
  fig_state$height_mm = height_mm
  png(filename = paste0(file, ".png"),
      width = fig_width_mm,
      height = height_mm,
      units = "mm",
      res = png_res,
      pointsize = base_pt,
      family = "sans",
      type = "cairo")
  dev.control(displaylist = "enable") # needed to copy the figure to pdf
  fig_state$panel = 0
  par(mfrow = mfrow,
      cex = 1, # prevent automatic font shrinking in multi-panel layouts
      cex.main = 9 / base_pt, # panel titles 9 pt
      cex.lab = 8 / base_pt, # axis titles 8 pt
      cex.axis = 7 / base_pt, # tick labels 7 pt
      mar = c(3.1, 3.1, 3.3, 0.6), # top margin fits two-line titles
      mgp = c(2, 0.6, 0),
      tcl = -0.3)
}
panel_letter = function(){
  # add the next panel letter (A, B, ...) in 10 pt bold to the top left corner
  # of the current panel. Call after plotting the panel.
  fig_state$panel = fig_state$panel + 1
  mtext(LETTERS[fig_state$panel],
        side = 3,
        line = 2.1,
        adj = 0,
        at = grconvertX(0.01, from = "nfc", to = "user"),
        cex = 10 / base_pt,
        font = 2)
}
close_fig = function(){
  # copy the finished figure to pdf, so both files show identical (random) data
  # cairo_pdf embeds fonts, which print shops require
  dev.copy(cairo_pdf,
           filename = paste0(fig_state$file, ".pdf"),
           width = fig_width_mm * mm_to_in,
           height = fig_state$height_mm * mm_to_in,
           pointsize = base_pt,
           family = "sans")
  dev.off() # close pdf
  dev.off() # close png
}

#### Construct age-depth models ####
adm_list = list()
dist = paste0(seq(2, 12, by = 2), "km") # Positions in the carbonate platform, from the shore
for (pos in dist){ # Consctruction of age-depth models from tie points for each position
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
open_fig("figs/fig2", height_mm = 65, mfrow = c(1, 2))
plot(adm2, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()
title("Platform Top")
panel_letter()
plot(adm12, lty_destr = 0, lwd_acc = 3)
T_axis_lab()
L_axis_lab()
title("Proximal Slope")
panel_letter()
close_fig()

#### Figure 3 ####
# effect of stratigraphic biases on last occurrences

subdiv = 20

rolo = 200 # Rate of last occurrences
open_fig("figs/fig3", height_mm = 65, mfrow = c(1, 2))
p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = FALSE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Last Occurrences\n(Platform Top)")
panel_letter()

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = FALSE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Last Occurrences\n(Proximal Slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))
panel_letter()
close_fig()

#### Figure 4 ####
# effect of stratigraphic biases on fossil abundance
open_fig("figs/fig4", height_mm = 65, mfrow = c(1, 2))
p3(rate = rolo, min_time(adm2), max_time(adm2)) |>
  time_to_strat(adm2, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm2), max_height(adm2), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil Abundance\n(Platform Top)")
panel_letter()

p3(rate = rolo, from = min_time(adm12), to = max_time(adm12)) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(xlab = "Stratigraphic position [m]",
       main = "Fossil Abundance\n(Proximal Slope)",
       breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv))
panel_letter()
close_fig()

#### Figure 5 ####
# effect of water depth on fossil abundance
open_fig("figs/fig5", height_mm = 116, mfrow = c(2, 2))
gc = approxfun(scenarioA$t_myr, scenarioA$wd_m[,"12km"])
niche = snd_niche(opt = 100, tol = 30, cutoff_val = 0) # define niche with optimum at 100 m
plot(scenarioA$t_myr, gc(scenarioA$t_myr),
     type = "l",
     lwd = 3,
     xlab = "Time [Myr]",
     ylab = "Water depth [m]",
     main = "Water Depth\n(Proximal Slope)")
panel_letter()

plot(x = scenarioA$t_myr, 
     y = niche(gc(scenarioA$t_myr)),
     type = "l",
     lwd = 3,
     xlab = "Time [Myr]",
     ylab = "Recovery probability",
     main = "Niche in\nTime Domain")
panel_letter()

r = list(t = scenarioA$t_myr, y = niche(gc(scenarioA$t_myr)))  |> time_to_strat(adm12)

plot(x = r$h[!is.na(r$h)],
     y = r$y[!is.na(r$h)],
     type = "l",
     lwd = 3,
     xlab = "Stratigraphic position [m]",
     ylab = "Recovery probability",
     main = "Niche in\nStratigraphic Domain")
panel_letter()

p3(rate = rolo, min_time(adm12), max_time(adm12)) |>
  apply_niche(niche, gc) |>
  time_to_strat(adm12, destructive = TRUE) |>
  hist(breaks = seq(min_height(adm12), max_height(adm12), length.out = subdiv),
       xlab = "Stratigraphic position [m]", 
       main = "Fossil Abundance\n(Proximal Slope)")
panel_letter()

close_fig()