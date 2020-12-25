# various plots:
# cases/active ("cases")
# new cases + new deaths ("casedeaths")
# age plot ("age")
# disaggregated age plot ("dis")
# hospitalized / icu ("hospitalized")
# share of tests confirming new cases ("positivity")

library(magrittr)

extrafont::loadfonts(device = "win") # comment out to use default fonts
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

################################################################################
# var plot visuals                                                             #
################################################################################
var_plot_vis <- function() {
    translate <- function(x) {
        t <- list(mean = enc("-дневно плаващо средно"),
                  sum = enc("-дневна сума"))
        res <- t[[x]]
        if (!is.null(res)) return(res)
        return(paste(enc("-дн. прозорец: "), x))
    }
    line_fields <- list(casesdeaths = "new_cases|new_deaths",
                        age = "([2-8]|^)0\\+|new_deaths",
                        dis = "0-|90\\+",
                        hospitalized = "active_cases",
                        positivity = "date|posit7|s7_nt",
                        pospcr = "date|posit7_pcr|s7_nt_pcr",
                        #posag = "date|posit7_ag|s7_nt_ag"
                        posag = "date|posit_ag|new_ag_tests")
    area_fields <- list(cases = "^cases$|active_cases",
                        hospitalized = "hospitalized|in_icu")
    tick_choice <- c(10, 20, 25, 50) * rep(c(1, 10, 100, 1000), each = 4)
    no_na <- c( # don't draw rows if these fields are NA
        age = "0+",
        dis = "0-19",
        positivity = "posit7",
        pospcr = "posit7_pcr",
        posag = "posit_ag"
    )
    thin <- 0.5   # } lines
    thick <- 1    # }
    sz_norm <- list(rgx = "NONE", sizes = c(thin))
    line_sizes <- list(casesdeaths = sz_norm,
                       age = list(rgx = "^[2-8]", sizes = c(thin, thick)),
                       dis = sz_norm,
                       hospitalized = sz_norm,
                       positivity = sz_norm,
                       pospcr = sz_norm,
                       posag = sz_norm)
    casesdeaths_colors <- c("blue" = enc("нови случаи"),
                            "red" = enc("смъртни случаи"))
    age_colors <- c("dark green",
                    rep("green", 3),
                    "blue",
                    rep("dark orange", 3),
                    "red")
    dis_colors <- c("lightgreen", "green1", "green4",
                    "#5555FF", "blue4", "violet",
                    "orange", "red", "red4")
    age_labels <- function(x) ifelse(x == "new_deaths",
                                     enc("смъртни случаи"),
                                     x)
    age_guide <- ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(size = c(thick, rep(thin, 7), thick))
    )
    cases_fills <- c("light blue" = enc("регистрирани случаи"),
                     "dark blue" = enc("активни случаи"))
    hospitalized_fills <- c("dark golden rod 1" = enc("хоспитализирани"),
                            "dark red" = enc("от тях в интензивно"))
    hospitalized_colors <- c("black" = enc("активни случаи"))
    positivity_colors <- c("green 3" = enc("брой тестове"),
                           "red 3" = enc("позитивни (доказващи нови случаи)"))
    make_scale <- function(sf, x) sf(values = names(x), labels = unname(x))
    plot_colors <- c(
        casesdeaths = make_scale(ggplot2::scale_color_manual,
                                 casesdeaths_colors),
        age = ggplot2::scale_color_manual(values = age_colors,
                                          labels = age_labels,
                                          guide = age_guide),
        hospitalized = make_scale(ggplot2::scale_color_manual,
                                  hospitalized_colors),
        positivity = make_scale(ggplot2::scale_color_manual,
                                positivity_colors),
        pospcr = make_scale(ggplot2::scale_color_manual,
                            positivity_colors),
        posag = make_scale(ggplot2::scale_color_manual,
                           positivity_colors),
        dis = ggplot2::scale_color_manual(
            values = dis_colors,
            guide = ggplot2::guide_legend(nrow = 1)
        )
    )
    
    plot_fills <- c(cases = make_scale(ggplot2::scale_fill_manual,
                                       cases_fills),
                    hospitalized = make_scale(ggplot2::scale_fill_manual,
                                              hospitalized_fills))
    x_label <- enc("дата на докладване (седмица)")
    plot_labels <- list(
        casesdeaths = ggplot2::labs(
            title = paste(enc("Регистрирани нови случаи и"),
                          enc("смъртни случаи от COVID-19")),
            caption = enc("данни: НОЩ, data.egov.bg"),
            color = enc("дневно"),
            x = x_label,
            y = enc("регистрирани случаи")
        ),
        age = ggplot2::labs(
            title = paste(enc("Регистрирани нови случаи"),
                          enc("(възрастови групи) и "),
                          enc("смъртни случаи")),
            caption = enc("данни: data.egov.bg"),
            color = enc("дневно"),
            x = x_label,
            y = enc("регистрирани случаи")
        ),
        dis = ggplot2::labs(
            title = paste(enc("Регистрирани нови случаи"),
                          enc("(дезагрегирани възрастови групи)")),
            caption = enc("данни: data.egov.bg"),
            color = enc("дневно"),
            x = x_label,
            y = enc("регистрирани случаи")
        ),
        cases = ggplot2::labs(
            title = enc("Регистрирани и активни случаи на COVID-19"),
            caption = enc("данни: НОЩ, data.egov.bg"),
            fill = NULL,
            x = x_label,
            y = NULL
        ),
        hospitalized = ggplot2::labs(
            title = enc("Хоспитализирани случаи на COVID-19"),
            caption = enc("данни: НОЩ, data.egov.bg"),
            fill = NULL,
            color = NULL,
            x = x_label,
            y = enc("хоспитализирани")
        ),
        positivity = ggplot2::labs(
            title = enc("Позитивност PCR + Ag (нови случаи / брой тестове)"),
            caption = enc("данни: НОЩ, data.egov.bg"),
            color = enc("за последните 7 дни"),
            x = x_label,
            y = enc("брой тестове")
        ),
        pospcr = ggplot2::labs(
            title = enc("Позитивност PCR (нови случаи / брой тестове)"),
            caption = enc("данни: НОЩ, data.egov.bg"),
            color = enc("за последните 7 дни"),
            x = x_label,
            y = enc("брой тестове")
        ),
        posag = ggplot2::labs(
            title = enc("Позитивност Ag (нови случаи / брой тестове)"),
            caption = enc("данни: НОЩ, data.egov.bg"),
            color = enc("дневно"),
            x = x_label,
            y = enc("брой тестове")
        )
    )
    plot_order <- list(
        cases = c("cases", "active_cases"),
        hospitalized = c("hospitalized", "in_icu", "active_cases"),
        positivity = c("s7_nt", "posit7"),
        pospcr = c("s7_nt_pcr", "posit7_pcr"),
        posag = c("new_ag_tests", "posit_ag")
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
                          type = "percent"),
        pospcr = list(label = enc("новодоказани случаи"),
                      vars = "posit7_pcr",
                      scale = 0.000005,
                      type = "percent"),
        posag = list(label = enc("новодоказани случаи"),
                     vars = "posit_ag",
                     scale = 0.00005,
                     type = "percent")
    )
    plot_theme <- ggplot2::theme(
        text = ggplot2::element_text(
            size = 14,
            family = grDevices::windowsFont("Calibri")
        ),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "top",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.minor.y = ggplot2::element_blank()
    )
    vis <- list(
        translate = translate,
        fields = list(line = line_fields, area = area_fields),
        tick_choice = tick_choice,
        no_na = no_na,
        line_sizes = line_sizes,
        colors = list(line = plot_colors, area = plot_fills),
        labels = plot_labels,
        order = plot_order,
        sec_y = plot_sec_y,
        theme = plot_theme
    )
    return(vis)
}

################################################################################
# var plot tidy                                                                #
################################################################################
var_plot_tidy <- function(country_data) {
    # age table
    atab <- country_data$age
    for (s in c(8:1)) {
        vals <- as.integer(rowSums(atab[(s + 1):10]))
        field_name <- paste0(ifelse(s > 1, s, ""), "0+")
        atab[field_name] <- vals
    }
    # history + age data, clean wide table
    ftab <- dplyr::left_join(country_data$gen_inc_hist, atab, by = "date")
    roll7 <- function(x) zoo::rollsum(x, 7, align = "right", fill = NA)
    ftab <- ftab %>%
        # all tests
        dplyr::mutate(
            s7_nc = roll7(new_cases),
            s7_nt = roll7(new_tests),
            posit7 = s7_nc / s7_nt,
            s7_nc_pcr = roll7(new_pcr_cases),
            s7_nt_pcr = roll7(new_pcr_tests),
            posit7_pcr = s7_nc_pcr / s7_nt_pcr,
            s7_nc_ag = roll7(new_ag_cases),
            s7_nt_ag = roll7(new_ag_tests),
            posit7_ag = s7_nc_ag / s7_nt_ag,
            posit_ag = new_ag_cases / new_ag_tests
        )
    return(ftab)
}

################################################################################
# various plots                                                                #
# chart - one of: "casesdeaths" | "age" | "dis" | "cases" | "hospitalized" |   #
#                 "positivity" | "pospcr" | "posag"                            #
# roll_* - rolling application of function (e.g. mean for moving average)      #
# line_legend - regex specifying fields/lines that should be labelled in-plot  #
################################################################################
var_plot <- function(country_data,
                     chart,
                     roll_func = NULL,
                     roll_window = 7,
                     roll_align = "right",
                     roll_fill = NA,
                     line_legend = NULL) {
    set.seed(42)
    vis <- var_plot_vis()
    line_fields <- vis$fields$line[[chart]]
    area_fields <- vis$fields$area[[chart]]
    all_fields <- paste(c("date", line_fields, area_fields), collapse = "|")
    ptab <- var_plot_tidy(country_data) %>%
        dplyr::select(dplyr::matches(all_fields))
    # scale secondary axis values
    if (chart %in% names(vis$sec_y)) {
        for (v in vis$sec_y[[chart]]$vars) {
            ptab <- ptab %>% # "{v}" := and .data[[v]] provide indirection
                dplyr::mutate("{v}" := .data[[v]] / vis$sec_y[[chart]]$scale)
        }
    }
    if (!is.null(roll_func)) {
        ptab <- ptab %>%
            dplyr::mutate(dplyr::across(
                !matches("date"),
                function(x) zoo::rollapply(x,
                                           roll_window,
                                           roll_func,
                                           align = roll_align,
                                           fill = roll_fill)
            ))
        legend_main <- paste0(roll_window,
                              vis$translate(deparse(substitute(roll_func))))
    } else {
        legend_main <- NULL
    }
    if (chart %in% names(vis$no_na)) {
        field <- vis$no_na[[chart]]
        ptab <- ptab %>% dplyr::filter(!is.na(.data[[field]]))
    }
    ptab <- ptab %>%
        tidyr::pivot_longer(cols = !matches("date"),
                            names_to = "metric",
                            values_to = "value")
    if (chart %in% names(vis$order))
        ptab <- ptab %>%
            dplyr::mutate(metric = factor(metric, vis$order[[chart]]))
    if (chart == "posag") zzx <<- ptab
    plot_start_date <- head(ptab$date, n = 1)
    plot_end_date <- tail(ptab$date, n = 1)
    first_sunday <- ptab %>%
        dplyr::select("date") %>%
        dplyr::filter(weekdays(date, abbreviate = FALSE) == "Sunday") %>%
        dplyr::slice_head() %>%
        dplyr::pull()
    if (length(first_sunday) == 0) {
        first_sunday = plot_start_date -
            lubridate::wday(plot_start_date, week_start = 1)
    }
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    visibility_min <- 1.05 * max(ptab$value, na.rm = TRUE)
    tick_choice <- vis$tick_choice
    tick_by <- tick_choice[tick_choice >= visibility_min / 7][1]
    tick_max <- 10 * tick_by
    plt <- ggplot2::ggplot()
    if (chart %in% names(vis$fields$area)) {
        plt <- plt +
            ggplot2::geom_area(
                data = ptab %>%
                    dplyr::filter(stringr::str_detect(metric, area_fields)),
                mapping = ggplot2::aes(x = date,
                                       y = value,
                                       fill = metric),
                position = "identity"
            )
    }
    if (chart %in% names(vis$fields$line)) {
        plt <- plt +
            ggplot2::geom_line(
                data = ptab %>%
                    dplyr::filter(stringr::str_detect(metric, line_fields)),
                mapping = ggplot2::aes(
                    x = date,
                    y = value,
                    color = metric,
                    size = ifelse(
                        stringr::str_detect(metric,
                                            vis$line_sizes[[chart]]$rgx),
                        "size1",
                        "size2"
                    )
                )
            ) +
            ggplot2::scale_size_manual(values = vis$line_sizes[[chart]]$sizes,
                                       guide = FALSE)
    }
    if (!is.null(line_legend)) {
        plt <- plt +
            ggrepel::geom_text_repel(
                data = ptab %>%
                    dplyr::filter(date == plot_end_date,
                                  stringr::str_detect(metric, line_legend)),
                mapping = ggplot2::aes(x = date,
                                       y = value,
                                       color = metric,
                                       label = metric),
                direction = "y",
                size = 4,
                nudge_x = 5,
                segment.color = "#333333",
                segment.size = 0.3,
                segment.alpha = 0.3,
                show.legend = FALSE
            )
        exp_fix <- 5
    } else {
        exp_fix <- 0
    }
    if (chart %in% names(vis$sec_y)) {
        if (!is.null(vis$sec_y[[chart]]$type))
            sec_labels <- scales::label_percent()
        else
            sec_labels <- scales::label_number()
        secondary <- ggplot2::sec_axis(
            name = vis$sec_y[[chart]]$label,
            trans = ~.* vis$sec_y[[chart]]$scale,
            breaks = seq(0,
                         vis$sec_y[[chart]]$scale * tick_max,
                         by = vis$sec_y[[chart]]$scale * tick_by),
            labels = sec_labels
        )
    } else {
        secondary <- ggplot2::waiver()
    }
    plt <- plt +
        vis$colors$line[chart] +
        vis$colors$area[chart] +
        ggplot2::scale_y_continuous(
            breaks = seq(0, tick_max, by = tick_by),
            expand = ggplot2::expansion(mult = c(0.025, 0.05)),
            labels = scales::label_number(),
            sec.axis = secondary
        ) +
        ggplot2::scale_x_date(
            breaks = seq(first_sunday,
                         last_sunday_inc,
                         by = "7 days"),
            limits = c(min(plot_start_date, first_sunday),
                       last_sunday_inc + 4),
            date_labels = "%d.%m. (%U)",
            expand = ggplot2::expansion(mult = c(0.025, 0),
                                        add = c(0, 1 + exp_fix))
        ) +
        vis$labels[[chart]]
    if (!is.null(legend_main))
        plt <- plt + ggplot2::labs(color = legend_main)
    plt <- plt + vis$theme
    return(plt)
}

################################################################################
# output example                                                               #
################################################################################
var_plot_save_all <- function() {
    export <- function(plot, file) {
        ggplot2::ggsave(file = paste0(file, ".svg"),
                        width = 11,
                        height = 7,
                        plot = plot)
    }
    
    source(file.path("R", "bg_opendata.R")) # sets bg_data
    export(file = "posit", plot = var_plot(bg_data, "positivity"))
    export(file = "pospcr", plot = var_plot(bg_data, "pospcr"))
    export(file = "posag", plot = var_plot(bg_data, "posag"))
    export(file = "cd", plot = var_plot(bg_data,
                                        "casesdeaths",
                                        roll_func = mean,
                                        roll_window = 7))
    export(file = "cases", plot = var_plot(bg_data, "cases"))
    export(file = "hospitalized", plot = var_plot(bg_data, "hospitalized"))
    export(file = "age_7", plot = var_plot(bg_data,
                                           "age",
                                           roll_func = mean,
                                           roll_window = 7,
                                           line_legend = "0"))
    export(file = "age_1", plot = var_plot(bg_data, "age", line_legend = "0"))
    export(file = "age_dis_7", plot = var_plot(bg_data,
                                               "dis",
                                               roll_func = mean,
                                               roll_window = 7,
                                               line_legend = "."))
    export(file = "age_dis_1", plot = var_plot(bg_data,
                                               "dis",
                                               roll_func = mean,
                                               roll_window = 1,
                                               line_legend = "."))
}