library(tools)
library(zoo)
library(Cairo)

enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# download @ https://data.egov.bg/data/view/492e8186-0d00-43fb-8f5e-f2b0b183b64f
gen_data <- enc("Обща статистика за разпространението.csv")
age_data <- enc("Разпределение по дата и по възрастови групи.csv")

##### texts
fmt_date <- "%d.%m. (%U)"
txt_title <- paste(enc("Регистрирани нови случаи (възрастово разпределение),"),
                   enc("смъртни случаи"))
txt_mvavg <- enc("-дневно средно)")
txt_noavg <- enc("дневно)")
txt_regc <- enc("регистрирани случаи")
txt_dthc <- enc("смъртни случаи")
txt_date <- enc("дата на докладване (седмица)")
txt_dcpd <- enc("смъртни случаи (")
txt_rcpd <- enc("регистрирани случаи (")
txt_src <- enc("данни: data.egov.bg, НОЩ")

##### data
gtab <- read.csv(file = gen_data)
# case counts before open data (2020-03-08 - 2020-06-05)
nc_old <- c(4, 0, 2, 1, 16, 8, 10, 10, 11, 19, 11, 15, 20, 36, 22, 16,
            17, 24, 22, 29, 38, 15, 13, 40, 23, 35, 28, 18, 28, 18, 28, 16,
            25, 17, 26, 14, 10, 28, 34, 53, 46, 32, 16, 35, 46, 49, 73, 91,
            59, 53, 63, 36, 48, 59, 49, 39, 24, 34, 52, 74, 51, 43, 49, 44,
            25, 33, 46, 31, 38, 37, 36, 24, 24, 33, 39, 41, 36, 19, 6, 10,
            17, 17, 8, 14, 14, 6, 19, 22, 25, 42)
# death counts before open data (2020-03-08 - 2020-06-05)
dc_old <- c(0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0,
            4, 1, 0, 0, 1, 1, 4, 3, 3, 2, 1, 1, 0, 1, 3, 1, 3, 3, 1, 2, 3,
            0, 1, 1, 2, 4, 3, 2, 1, 1, 2, 0, 6, 2, 2, 4, 1, 5, 2, 4, 0, 2,
            4, 1, 2, 2, 1, 3, 3, 3, 3, 2, 2, 4, 4, 5, 1, 4, 0, 0, 3, 1, 2,
            3, 1, 0, 4, 2, 1, 12)
nc <- c(nc_old, gtab[[6]])  # case  count since start
dc <- c(dc_old, gtab[[12]]) # death count since start
atab <- read.csv(file = age_data) # cases by bracket
start_date <- as.Date(atab[1, 1]) - length(nc_old)
atab[-1, -1] <- atab[-1, -1] - atab[-nrow(atab), -1] # repl. totals w/incidence
atab <- atab[-1, ]                                   # (losing one day of data)

################################################################################
# plot cases (w/age distrib), deaths; use <mvavg>-day moving averages          #
################################################################################
cd_plot <- function(mvavg = 7) {
    mvavg <- round(mvavg)
    if (mvavg > 1)
        suf <- paste0(mvavg, txt_mvavg)
    else
        suf <- txt_noavg
    d_label <- paste0(txt_dcpd, suf)
    c_label <- paste0(txt_rcpd, suf)

    # plot config
    pre_ag <- as.integer(as.Date(atab[1, 1]) - start_date) # days w/o age distr
    if (!identical(unname(rowSums(atab[2:10])),
                  unname(nc[(pre_ag + 1):length(nc)])))
        stop("failed sanity check: total case counts != summed age brackets")
    cd_factor <- 0.04       # align primary/secondary Y axis @ deaths/cases
    vis_min <- 1.05 * max(rollmean(nc, k = mvavg))
    tick_choice <- c(10, 20, 25, 50) * rep(c(1, 10, 100, 1000), each = 4)
    ytick_c <- tick_choice[tick_choice >= vis_min / 7][1] # Y ax cases tick freq
    ytick_d <- ytick_c * cd_factor                     # Y axis deaths tick freq
    ygrid <- ceiling(vis_min / ytick_c)
    ymax_c <- ygrid * ytick_c                          # max Y axis cases
    ymax_d <- ymax_c * cd_factor                       # max Y axis deaths
    xmax <- 1 + 7 * ceiling((nrow(atab) - 1 + pre_ag) / 7) # full weeks
    xgrid <- 28
    xtick <- 7
    lw_norm <- 0.7
    lw_wide <- 1.4

    # plot
    par(mar = c(6.5, 4.5, 2.12, 4.5), lwd = lw_norm)
    plot(NULL, yaxs = "i", xlim = c(1, xmax), ylim = c(0, ymax_d),
         ann = FALSE, xaxt = "n", yaxt = "n")
    grid(NA, ygrid, col = "light gray", lty = "dotted", lwd = lw_norm)
    for (i in 0:floor(xmax / xgrid))
        abline(v = 1 + i * xgrid,
               lty = "dotted", col = "light gray", lwd = lw_norm)
    mtext(txt_title, side = 3, line = 0.9, cex = 1.3, font = 2)
    mtext(txt_date, side = 1, line = 5, cex = 1.04)
    mtext(d_label, side = 2, line = 3, cex = 1.04)
    mtext(txt_src, side = 1, line = 5, adj = 1.105, cex = 0.87)
    legend("top", lty = c(1, 1), lwd = lw_wide, bg = "white",
           horiz = TRUE, bty = "n", #inset = -0.04,
           legend = c(txt_regc, txt_dthc),
           col = c("dark green", "red"))
    axis(side = 1, at = seq(1, xmax, by = xtick), las = 2, lwd = lw_norm,
         labels = format(start_date + seq(0, xmax - 1, by = xtick), fmt_date))
    zpre <- rep(0, mvavg - 1)
    # deaths
    axis(side = 2, at = seq(0, ymax_d, by = ytick_d), lwd = lw_norm)
    dc_mv <- rollmean(c(zpre, dc), k = mvavg)
    lines(dc_mv, type = "l", col = "red", lwd = lw_wide)
    # cases
    par(new = TRUE)
    plot(NULL, yaxs = "i", xlim = c(1, xmax), ylim = c(0, ymax_c),
         ann = FALSE, yaxt = "n", xaxt = "n")
    axis(side = 4, at = seq(0, ymax_c, by = ytick_c), lwd  = lw_norm)
    mtext(c_label, side = 4, line = 3, cex = 1.04)
    ncpre_mv <- rollmean(c(zpre, nc[1:(pre_ag + mvavg)]), k = mvavg)
    lines(ncpre_mv,
          type = "l",
          col = "dark green",
          lwd = lw_wide,
          lty = "dashed")
    p_agebrkt <- function(vals,  # plot age brackets
                          label,
                          col = "black",
                          lwd = lw_norm,
                          ...) {
        mv <- rollmean(c(rep(NA, pre_ag), vals), k = mvavg,
                       fill = NA, align = "right")
        lines(mv, type = "l", col = col, lwd = lwd, ...)
        text(pre_ag + length(vals) + 5, mv[length(mv)], label, col = col)
    }
    p_agebrkt(rowSums(atab[9:10]), "80+", col = "dark orange")
    p_agebrkt(rowSums(atab[8:10]), "70+", col = "dark orange")
    p_agebrkt(rowSums(atab[7:10]), "60+", col = "dark orange")
    p_agebrkt(rowSums(atab[6:10]), "50+", col = "blue")
    p_agebrkt(rowSums(atab[5:10]), "40+", col = "green")
    p_agebrkt(rowSums(atab[4:10]), "30+", col = "green")
    p_agebrkt(rowSums(atab[3:10]), "20+", col = "green")
    p_agebrkt(rowSums(atab[2:10]), "0+", col = "dark green", lwd = lw_wide)
}

################################################################################
# write to file using Cairo                                                    #
################################################################################
save_cd_plot <- function(filename,
                         mvavg = 7,
                         width = 11,
                         height = 7,
                         dpi = 100,
                         pointsize = 8.5,
                         units = "in") {
    Cairo(file = filename,
          type = file_ext(filename),
          units = units,
          width = width,
          bg = "white",
          height = height,
          pointsize = pointsize,
          dpi = dpi,
          quality = 100)
    cd_plot(mvavg)
    dev.off()
}

################################################################################
# example output                                                               #
################################################################################
save_all <- function() {
    save_cd_plot("age_7d.svg",
                 mvavg = 7,
                 width = 16,
                 height = 9,
                 pointsize = 11.5)
    save_cd_plot("age_raw.svg",
                 mvavg = 1,
                 width = 16,
                 height = 9,
                 pointsize = 11.5)
}