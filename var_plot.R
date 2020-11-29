# various plots: cases/active, new cases/deaths, age plot, hospitlzd, positivity

library(tidyverse)
library(zoo)

library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

# download @ https://data.egov.bg/data/view/492e8186-0d00-43fb-8f5e-f2b0b183b64f
gen_data <- file.path("data", enc("Обща статистика за разпространението.csv"))
age_data <- file.path("data",
                      enc("Разпределение по дата и по възрастови групи.csv"))
gen_hist <- file.path("historical_data", "pre_opendata.csv")

##### visuals config
translate <- function(x) {
    t <- list(mean = enc("-дневно плаващо средно"),
              sum = enc("-дневна сума"))
    res <- t[[x]]
    if (!is.null(res)) return(res)
    return(paste(enc("-дн. прозорец: "), x))
}
line_fields <- list(casesdeaths = "new_cases|new_deaths",
                    age = "([2-8]|^)0\\+|new_deaths",
                    hospitalized = "active_cases",
                    positivity = "date|posit7|s7_nt")
area_fields <- list(cases = "^cases$|active_cases",
                 hospitalized = "hospitalized|in_icu")
tick_choice <- c(10, 20, 25, 50) * rep(c(1, 10, 100, 1000), each = 4)
no_na <- c(age = "0+", positivity = "posit7")
thin <- 0.5   # } lines
thick <- 1    # }
line_sizes <- list(casesdeaths = list(rgx = "NONE", sizes = c(thin)),
                   age = list(rgx = "^[2-8]", sizes = c(thin, thick)),
                   hospitalized = list(rgx = "NONE", sizes = c(thin)),
                   positivity = list(rgx = "NONE", sizes = c(thin)))
casesdeaths_colors <- c("blue" = enc("нови случаи"),
                        "red" = enc("смъртни случаи"))
age_colors <- c("dark green",
                rep("green", 3),
                "blue",
                rep("dark orange", 3),
                "red")
age_labels <- function(x) ifelse(x == "new_deaths", enc("смъртни случаи"), x)
age_guide <- guide_legend(nrow = 1,
                          override.aes = list(size = c(thick * 1,
                                                       rep(thin * 1, 7),
                                                       thick * 1)))
cases_fills <- c("light blue" = enc("регистрирани случаи"),
                 "dark blue" = enc("активни случаи"))
hospitalized_fills <- c("dark golden rod 1" = enc("хоспитализирани"),
                        "dark red" = enc("от тях в интензивно"))
hospitalized_colors <- c("black" = enc("активни случаи"))
positivity_colors <- c("green 3" = enc("брой проби"),
                       "red 3" = enc("позитивни, доказващи нови случаи"))
make_scale <- function(sf, x) sf(values = names(x), labels = unname(x))
plot_colors <- c(casesdeaths = make_scale(scale_color_manual,
                                          casesdeaths_colors),
                 age = scale_color_manual(values = age_colors,
                                          labels = age_labels,
                                          guide = age_guide),
                 hospitalized = make_scale(scale_color_manual,
                                           hospitalized_colors),
                 positivity = make_scale(scale_color_manual,
                                         positivity_colors))

plot_fills <- c(cases = make_scale(scale_fill_manual,
                                   cases_fills),
                hospitalized = make_scale(scale_fill_manual,
                                          hospitalized_fills))
x_label <- enc("дата на докладване (седмица)")
plot_labels <- list(
    casesdeaths = labs(title = paste(enc("Регистрирани нови случаи и"),
                                     enc("смъртни случаи от COVID-19")),
                       caption = enc("данни: НОЩ, data.egov.bg"),
                       color = enc("дневно"),
                       x = x_label,
                       y = enc("регистрирани случаи")),
    age = labs(title = paste(enc("Регистрирани нови случаи"),
                             enc("(възрастово разпределение) и "),
                             enc("смъртни случаи")),
               caption = enc("данни: data.egov.bg"),
               color = enc("дневно"),
               x = x_label,
               y = enc("регистрирани случаи")),
    cases = labs(title = enc("Регистрирани и активни случаи на COVID-19"),
                 caption = enc("данни: НОЩ, data.egov.bg"),
                 fill = NULL,
                 x = x_label,
                 y = NULL),
    hospitalized = labs(title = enc("Хоспитализирани случаи на COVID-19"),
                        caption = enc("данни: НОЩ, data.egov.bg"),
                        fill = NULL,
                        color = NULL,
                        x = x_label,
                        y = enc("хоспитализирани")),
    positivity = labs(title = enc("Дял новодоказани случаи от PCR пробите"),
                        caption = enc("данни: НОЩ, data.egov.bg"),
                        color = enc("за последните 7 дни"),
                        x = x_label,
                        y = enc("брой проби"))
)
plot_order <- list(
    cases = c("cases", "active_cases"),
    hospitalized = c("hospitalized", "in_icu", "active_cases"),
    positivity = c("s7_nt", "posit7")
)
plot_sec_y <- list(
    casesdeaths = list(label = enc("смъртни случаи"),
                       vars = "new_deaths",
                       scale = 0.04),
    age = list(label = enc("смъртни случаи"),
               vars = "new_deaths",
               scale = 0.04),
    hospitalized = list(label = enc("активни случаи"),
                        vars = "active_cases",
                        scale = 10),
    positivity = list(label = enc("новодоказани случаи"),
                      vars = "posit7",
                      scale = 0.000005,
                      type = "percent")
)
plot_theme <- theme(text = element_text(size = 14,
                                     family = windowsFont("Calibri")),
                 panel.grid.minor.x = element_blank(),
                 legend.position = "top",
                 plot.title = element_text(hjust = 0.5,
                                           face = "bold"),
                 axis.text.x = element_text(angle = 45, hjust = 1),
                 panel.grid.minor.y = element_blank())

##### clean up
# age table
atab <- read.csv(file = age_data) # cases by bracket
atab[, 1] <- as.Date(atab[, 1])
atab[-1, -1] <- atab[-1, -1] - atab[-nrow(atab), -1] # repl. totals w/incidence
atab <- atab[-1, ]                                   # (losing one day of data)
names(atab)[1] <- "date"
names(atab)[10] <- "90+"
names(atab) <- sub("X([0-9]+)\\.\\.\\.([0-9]+)", "\\1-\\2", names(atab))
for (s in c(8:1)) {
    vals <- as.integer(rowSums(atab[(s + 1):10]))
    field_name <- paste0(ifelse(s > 1, s, ""), "0+")
    atab[field_name] <- vals
}
# general table
gtab <- read.csv(file = gen_data, encoding = "UTF-8")
names(gtab) <- c("date", "tests", "new_tests",
                 "cases", "active_cases",
                 "new_cases", "hospitalized",
                 "in_icu", "recovered",
                 "newly_recovered", "deaths",
                 "new_deaths")
gtab[, 1] <- as.Date(gtab[, 1])
if (!identical(as.integer(unname(rowSums(atab[, 2:10]))),
               unname(gtab$new_cases[-1])))
    stop("failed sanity check: new case counts != summed age brackets")
# clean wide table
jtab <- left_join(gtab, atab, by = "date")
# pre-opendata table
htab <- read.csv(file = gen_hist) %>%
    mutate(date = as.Date(date))
# clean wide table with figures before opendata
ftab <- bind_rows(htab, jtab)
ftab <- ftab %>%
    mutate(s7_nc = rollsum(new_cases, 7, align = "right", fill = NA)) %>%
    mutate(s7_nt = rollsum(new_tests, 7, align = "right", fill = NA)) %>%
    mutate(posit7 = s7_nc / s7_nt)

################################################################################
# return regex matching all fields for a chart type                            #
################################################################################
all_fields <- function(chart) {
    fields <- append("date", c(line_fields[[chart]], area_fields[[chart]]))
    return(paste(fields, collapse = "|"))
}

################################################################################
# various plots                                                                #
# chart - one of: "casesdeaths" | "age" | "cases" | "hospitalized" |           #
#                 "positivity"                                                 #
# roll_* - rolling application of function (e.g. mean for moving average)      #
# line_legend - whether to label lines at last plot values                     #
################################################################################
var_plot <- function(chart,
                     roll_func = NULL,
                     roll_window = 7,
                     roll_align = "right",
                     roll_fill = NA,
                     line_legend = NULL) {
    ptab <- ftab %>%
        select(matches(all_fields(chart)))
    # scale secondary axis values
    if (chart %in% names(plot_sec_y)) {
        for (v in plot_sec_y[[chart]]$vars) {
            ptab <- ptab %>% # "{v}" := and .data[[v]] provide indirection
                mutate("{v}" := .data[[v]] / plot_sec_y[[chart]]$scale)
        }
    }
    if (!is.null(roll_func)) {
        ptab <- ptab %>%
            mutate(across(!matches("date"),
                          function(x) rollapply(x,
                                                roll_window,
                                                roll_func,
                                                align = roll_align,
                                                fill = roll_fill)))
        legend_main <- paste0(roll_window,
                              translate(deparse(substitute(roll_func))))
    } else {
        legend_main <- NULL
    }
    if (chart %in% names(no_na)) {
        field <- no_na[[chart]]
        ptab <- ptab %>% filter(!is.na(.data[[field]]))
    }
    ptab <- ptab %>%
        pivot_longer(cols = !matches("date"),
                     names_to = "metric",
                     values_to = "value")
    if (chart %in% names(plot_order))
        ptab <- ptab %>%
            mutate(metric = factor(metric, plot_order[[chart]]))
    first_sunday <- ptab %>%
        select("date") %>%
        filter(weekdays(date) == "Sunday") %>%
        slice_head() %>%
        pull()
    plot_end_date <- tail(ptab$date, n = 1)
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    visibility_min <- 1.05 * max(ptab$value, na.rm = TRUE)
    tick_by <- tick_choice[tick_choice >= visibility_min / 7][1]
    tick_max <- 10 * tick_by
    plt <- ggplot()
    if (chart %in% names(area_fields)) {
        plt <- plt +
            geom_area(data = ptab %>%
                          filter(str_detect(metric, area_fields[[chart]])),
                      mapping = aes(x = date,
                                    y = value,
                                    fill = metric),
                      position = "identity")
    }
    if (chart %in% names(line_fields)) {
        plt <- plt +
            geom_line(data = ptab %>%
                          filter(str_detect(metric, line_fields[[chart]])),
                      mapping = aes(x = date,
                                    y = value,
                                    color = metric,
                                    size = ifelse(str_detect(metric,
                                                             line_sizes[[chart]]
                                                                     [["rgx"]]),
                                                  "size1",
                                                  "size2"))) +
            scale_size_manual(values = line_sizes[[chart]][["sizes"]],
                              guide = FALSE)
    }
    if (!is.null(line_legend)) {
        plt <- plt +
            geom_text(data = ptab %>%
                          filter(date == plot_end_date,
                                 str_detect(metric, line_legend)),
                      mapping = aes(x = date,
                                    y = value,
                                    color = metric,
                                    label = metric),
                      size = 4,
                      nudge_x = 4,
                      show.legend = FALSE)
        exp_fix <- 3
    } else {
        exp_fix <- 0
    }
    if (chart %in% names(plot_sec_y)) {
        if (!is.null(plot_sec_y[[chart]]$type))
            sec_labels <- scales::label_percent()
        else
            sec_labels <- scales::label_number()
        secondary <- sec_axis(name = plot_sec_y[[chart]]$label,
                              trans = ~.* plot_sec_y[[chart]]$scale,
                              breaks = seq(0,
                                           plot_sec_y[[chart]]$scale *
                                               tick_max,
                                           by = plot_sec_y[[chart]]$scale *
                                               tick_by),
                              labels = sec_labels)
    } else {
        secondary <- waiver()
    }
    plt <- plt +
        plot_colors[chart] +
        plot_fills[chart] +
        scale_y_continuous(
            breaks = seq(0, tick_max, by = tick_by),
            expand = expansion(mult = c(0.025, 0.05)),
            labels = scales::label_number(),
            sec.axis = secondary) +
        scale_x_date(breaks = seq(first_sunday,
                                  last_sunday_inc,
                                  by = "7 days"),
                     limits = c(ptab$date[1], last_sunday_inc + 4),
                     date_labels = "%d.%m. (%U)",
                     expand = expansion(mult = c(0.025, 0),
                                        add = c(-1, 1 + exp_fix))) +
        plot_labels[[chart]]
    if (!is.null(legend_main))
        plt <- plt + labs(color = legend_main)
    plt <- plt + plot_theme
    return(plt)
}

################################################################################
# output example                                                               #
################################################################################
save_all <- function() {
    export <- function(plot, file) {
        ggsave(file = paste0(file, ".svg"), width = 11, height = 7, plot = plot)
    }
    export(file = "posit", plot = var_plot("positivity"))
    export(file = "cd", plot = var_plot("casesdeaths",
                                            roll_func = mean,
                                            roll_window = 7))
    export(file = "cases", plot = var_plot("cases"))
    export(file = "hospitalized", plot = var_plot("hospitalized"))
    export(file = "age_7", plot = var_plot("age",
                                               roll_func = mean,
                                               roll_window = 7,
                                               line_legend = "0"))
    export(file = "age_1", plot = var_plot("age", line_legend = "0"))
}