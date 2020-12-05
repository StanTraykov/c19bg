# R plot from estR.csv and gen data 

library(tidyverse)
library(zoo)

library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

# download @ https://data.egov.bg/data/view/492e8186-0d00-43fb-8f5e-f2b0b183b64f
gen_data <- file.path("data", enc("Обща статистика за разпространението.csv"))
gen_hist <- file.path("historical_data", "pre_opendata.csv")
est_r <- "estR.csv"

##### visuals config
skip_to <- 20 # do not include first few days in scaling calc's
tick_choice <- c(10, 15, 20, 25, 50, 75) * rep(c(1, 10, 100, 1000), each = 6)
pcr_choice <- c(1, 2, 5) * rep(c(1, 10, 100, 1000), each = 3)
line_sz <- 0.5
lty_norm <- "solid"
lty_mva <- "dotted"
clr_leg <- list(linetype = c(lty_norm, lty_norm, lty_mva, lty_norm),
                size = c(line_sz, line_sz , line_sz, line_sz))
fill_leg <- list(alpha = c(0x66, 0x77, 0x99) / (0xFF * 1.5))
clr <- list(cri = "#0082df66",
            cri_txt = "#0082df99",
            reg_c = "#77777777",
            reg_s = "#44444499",
            r_med = "#0070c0",
            pcr = "#00b050",
            pos = "red 3")
plot_theme <- theme(text = element_text(size = 14,
                                        family = windowsFont("Calibri")),
                    panel.grid.minor.x = element_blank(),
                    panel.grid.major.x = element_blank(),
                    legend.position = "top",
                    legend.title = element_blank(),
                    legend.spacing = unit(0, units = "pt"),
                    legend.margin = margin(0, 0, 0, 0),
                    plot.title = element_text(hjust = 0.5,
                                              face = "bold"),
                    plot.subtitle = element_text(hjust = 0.5,
                                                 size = 11.3),
                    axis.text.x = element_text(angle = 45, hjust = 1),
                    axis.text.y.right = element_text(size = 14))

plot_labels <- labs(
    title = paste(enc("Оценка на репродуктивно число Rₜ"),
                  enc("(time-varying instantaneous)"),
                  enc("за България")),
    subtitle = paste(enc("разпределение генерационни времена по"),
                     enc("Ferretti et al. (средно 5.5, отчетена"),
                     enc("несигурност 4-7); 7-дневни прозорци, CrI"),
                     enc("по Cori et al. (вж. github уики)")),
    caption = enc("изх. данни: data.egov.bg, НОЩ"),
    x = enc("дата на докладване (седмица)")
)
lab_y <- enc("регистрирани случаи, PCR тестове (×%d)")
clr_labels <- c(enc("PCR тестове (7 дни)"),
                enc("позитивност (7 дни)"),
                enc("рег. случаи (ср. 7 дни)"),
                enc("Rₜ медиана"))
fill_labels <- c(enc("95% CrI"),
                 enc("рег. случаи"),
                 enc("рег. сл. (нед.)"))
sec_axis_name <- paste(enc("репродуктивно число,"), enc("позитивност"))
##### clean up
gtab <- read.csv(file = gen_data, encoding = "UTF-8")
names(gtab) <- c("date", "tests", "new_tests",
                 "cases", "active_cases",
                 "new_cases", "hospitalized",
                 "in_icu", "recovered",
                 "newly_recovered", "deaths",
                 "new_deaths")
gtab[, 1] <- as.Date(gtab[, 1])
rtab <- read.csv(file = est_r)
rtab <- rtab
rtab <- rbind(NA, NA, NA, NA, NA, NA, rtab, NA)
htab <- read.csv(file = gen_hist) %>%
    mutate(date = as.Date(date))
ftab <- bind_rows(htab, gtab)
ftab <- ftab %>%
    mutate(s7_nc = rollsum(new_cases, 7, align = "right", fill = NA)) %>%
    mutate(s7_nt = rollsum(new_tests, 7, align = "right", fill = NA)) %>%
    mutate(posit7 = (s7_nc / s7_nt)) %>%
    mutate(rollmean7 = s7_nc / 7) %>%
    mutate(is_sun = ifelse(weekdays(date) == "Sunday",
                           "yes",
                           "no"))
ftab <- bind_cols(ftab, rtab)

################################################################################
# R plot                                                                       #
################################################################################
r_plot <- function() {
    sundays_only <- ftab %>% filter(is_sun == "yes")
    first_sunday <- sundays_only %>% select("date") %>% slice_head() %>% pull()
    plot_end_date <- tail(ftab$date, n = 1)
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    secondary <- sec_axis(name = sec_axis_name, trans = ~./ r_scale)
    clr_guide <- guide_legend(override.aes = clr_leg)
    fill_guide <- guide_legend(override.aes = fill_leg)
    ttab <- ftab %>% filter(date >= ftab$date[1] + skip_to)
    cmx <- max(ttab %>% select(new_cases) %>% pull(), na.rm = TRUE)
    rmx <- max(ttab %>% select(R.Quantile.0.975.R.) %>% pull(), na.rm = TRUE)
    pmx <- max(ttab %>% select(s7_nt) %>% pull(), na.rm = TRUE)
    r_scale <- tick_choice[tick_choice >= cmx/rmx][1]
    c_max <- r_scale * rmx # cases axis limit
    c_by <- r_scale        # cases axis tick
    pcr_scale <- pcr_choice[pcr_choice >= pmx/c_max][1]
    plt <- ggplot(data = ftab, mapping = aes(x = date))
    plt <- plt +
        geom_col(mapping = aes(y = new_cases, fill = is_sun),
                 width = 0.9) +
        geom_line(mapping = aes(y = rollmean7, color = "C_mva"),
                  linetype = lty_mva,
                  size = line_sz) +
        geom_line(mapping = aes(y = R.Median.R. * r_scale, color = "D_med"),
                  linetype = lty_norm,
                  size = line_sz) +
        geom_line(mapping = aes(y = s7_nt / pcr_scale, color = "A_pcr"),
                  linetype = lty_norm,
                  size = line_sz) +
        geom_line(mapping = aes(y = posit7 * r_scale, color = "B_pos"),
                  linetype = lty_norm,
                  size = line_sz) +
        geom_ribbon(mapping = aes(ymin = R.Quantile.0.025.R. * r_scale,
                                  ymax = R.Quantile.0.975.R. * r_scale,
                                  fill = "A_ribbon")) +
        # % positivity labels
        geom_text(data = sundays_only %>% filter(!is.na(posit7)),
                  mapping = aes(x = date - 3.5,
                                y = 0,
                                color = "B_pos",
                                label = paste0(round(100 * posit7), "%")),
                  vjust = 1.3,
                  size = 2.7) +
        # % positivity labels label
        geom_text(data = sundays_only %>%
                      filter(is.na(posit7)) %>%
                      slice_tail(),
                  mapping = aes(x = date,
                                y = 0,
                                color = "B_pos",
                                label = enc("седмична позитивност: ")),
                  vjust = 1.3,
                  hjust = 1,
                  size = 2.7) +
        # R median label
        geom_text(data = ftab %>% filter(date == plot_end_date - 1),
                  mapping = aes(x = date,
                                y = R.Median.R. * r_scale,
                                color = "D_med",
                                label = format(round(R.Median.R., 2),
                                               nsmall = 2)),
                  #color = clr$r_med,
                  size = 3.2,
                  nudge_x = 4) +
        # R CrI 95% lower
        geom_text(data = ftab %>% filter(date == plot_end_date - 1),
                  mapping = aes(x = date,
                                y =  r_scale * min(R.Quantile.0.025.R.,
                                                   R.Median.R. - 0.1),
                                label = format(round(R.Quantile.0.025.R., 2),
                                               nsmall = 2)),
                  color = clr$cri_txt,
                  size = 3.2,
                  nudge_x = 4) +
        # R CrI 95% upper
        geom_text(data = ftab %>% filter(date == plot_end_date - 1),
                  mapping = aes(x = date,
                                y = r_scale * max(R.Quantile.0.975.R.,
                                                  R.Median.R. + 0.1),
                                label = format(round(R.Quantile.0.975.R., 2),
                                               nsmall = 2)),
                  color = clr$cri_txt,
                  size = 3.2,
                  nudge_x = 4) +
        scale_fill_manual(name = 2,
                          values = c(clr$cri, clr$reg_c, clr$reg_s),
                          labels = fill_labels,
                          guide = fill_guide) +
        scale_color_manual(name = 1,
                           values = c(clr$pcr, clr$pos, "black", clr$r_med),
                           labels = clr_labels,
                           guide = clr_guide) +
        scale_y_continuous(breaks = seq(0, c_max, by = c_by),
                           labels = scales::label_number(),
                           limits = c(0, c_max),
                           sec.axis = secondary,
                           expand = expansion(mult = c(0.025, 0.015))) +
        scale_x_date(breaks = seq(first_sunday,
                                  last_sunday_inc,
                                  by = "7 days"),
                     limits = c(ftab$date[1], last_sunday_inc + 4),
                     date_labels = "%d.%m. (%U)",
                     expand = expansion(mult = c(0.025, 0),
                                        add = c(-1, 2))) +
        plot_labels +
        labs(y = sprintf(lab_y, pcr_scale)) +
        plot_theme
    return(plt)
}

################################################################################
# output example                                                               #
################################################################################
save_all <- function() {
    ggsave(file = paste0("r_plot.svg"), width = 11, height = 7, plot = r_plot())
}