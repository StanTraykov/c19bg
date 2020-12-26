# plot EUROSTAT and ECDC data
# - weekly deaths / age groups / comparison map / etc. (EUROSTAT)
# - 14-day COVID-19 incidence/registered deaths (ECDC)
# - excess deaths per 1M (EUROSTAT)
# - hospitalized per 1M (ECDC)
# - excess factor (EUROSTAT + ECDC)

library(magrittr)

extrafont::loadfonts(device = "win") # comment out to use default fonts
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows

make_eu_vis <- function() {
    line_sz <- list(
        # for facetted plots
        hthin = 0.2,
        thin = 0.3,
        thick = 0.7,
        # for whole page plots
        wthin = 0.6,
        mthick = 0.8,
        wthick = 1.1
    )
    line_cols = c( # facetted deaths plot
        "#AAAAAA", "#BBAA00", "#008800", "#0000BB", "#000000", "#FF0000"
    )
    col_legend = ggplot2::guide_legend( # facetted deaths plot
        nrow = 1,
        override.aes = list(
            size = c(rep(line_sz$thin, 5), line_sz$thick)
        )
    )
    ext_ls = c( # for country totals deaths plot
        "solid", "solid", "dotted", "solid", "solid", "solid",
        "solid", "solid", "solid", "solid", "solid"
    )
    ext_guide = ggplot2::guide_legend( # for country totals deaths plot
        nrow = 1,
        override.aes = list(linetype = ext_ls, shape = c(rep(NA, 10), 19))
    )
    ext_line_cols = c( # for country totals deaths plot
        "#BBBBBB", "#BBBBBB", "#BBBBBB", "#BBBBBB", "#777777",
        "#88AAAA", "#BBAA00", "#008800", "#0000BB", "#000000",
        "#FF0000"
    )
    v_labs <- list( # labels for ECDC/EUROSTAT weekly comparison charts
        r14_cases = ggplot2::labs(
            title = paste(enc("14-дневна COVID-19 заболеваемост на 100 хил.")),
            caption = enc("данни: ECDC"),
            x = enc("седмица"),
            y =  enc("14-дневна заболеваемост на 100 хил.")
        ),
        r14_deaths = ggplot2::labs(
            title = paste(enc("14-дневна COVID-19 смъртност на 1 млн.")),
            caption = enc("данни: ECDC"),
            x = enc("седмица"),
            y =  enc("14-дневна смъртност на 1 млн.")
        ),
        em_1m = ggplot2::labs(
            title = paste(
                enc("Свръхсмъртност на 1 млн. спрямо ср. 2015-2019 по EUROSTAT")
            ),
            caption = enc("данни: EUROSTAT"),
            x = enc("седмица"),
            y =  enc("свръхсмъртност на 1 млн.")
        ),
        hosp_1m = ggplot2::labs(
            title = enc("Брой хоспитализирани с COVID-19 на 1 млн."),
            caption = enc("данни: ECDC"),
            x = enc("седмица"),
            y =  enc("хоспитализирани на 1 млн.")
        ),
        tests_100k = ggplot2::labs(
            title = enc("Извършени тестове на 100 хил."),
            caption = enc("данни: ECDC"),
            x = enc("седмица"),
            y =  enc("тестове на 100 хил.")
        ),
        positivity = ggplot2::labs(
            title = enc("Позитивност (нови случаи/брой тестове)"),
            caption = enc("данни: ECDC"),
            x = enc("седмица"),
            y =  enc("позитивност")
        )
    )
    gtheme1 <- ggplot2::theme(
        text = ggplot2::element_text(
            size = 14,
            family = grDevices::windowsFont("Calibri")
        ),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "top",
        plot.title = ggplot2::element_text(hjust = 0.5,
                                           face = "bold")
    )
    gtheme2 <- gtheme1 +
        ggplot2::theme(
            axis.text = ggplot2::element_text(size = 10),
            panel.margin.y = ggplot2::unit(3, "pt"),
            panel.margin.x = ggplot2::unit(4, "pt"),
            strip.text = ggplot2::element_text(
                size = 10,
                margin = ggplot2::margin(1, 0, 1, 0)
            )
        )
    vis <- list(
        v_labs = v_labs,
        gtheme1 = gtheme1,
        gtheme2 = gtheme2,
        # excess deaths factor comparison
        comp_f = c("BG", "UK", "BE", "NL", "FR", "ES", "IT", "RO"),
        txt_title1 = enc("Умирания в"),
        txt_v = enc("ъв"),
        txt_title2 = enc("по седмици и възрастови групи"),
        txt_title3 = enc("по седмици"),
        txt_titlei = enc("Умирания по страни и седмици"),
        f_col = c(enc("средно 2015-2019 г."),
                  enc("2020 г. без доказани смъртни случаи"),
                  enc("2020 г.")),
        line_sz = line_sz,
        f_color_scale = ggplot2::scale_color_manual(
            values = c("dark gray", "red", "dark magenta")
        ),
        common_color_scale = ggplot2::scale_color_manual(
            values = line_cols,
            guide = col_legend
        ),
        common_size_scale = ggplot2::scale_size_manual(
            values = c(line_sz$thick, line_sz$thin),
            guide = FALSE
        ),
        w_size_scale = ggplot2::scale_size_manual(
            values = c(line_sz$wthick, line_sz$wthin),
            guide = FALSE
        ),
        common_xweek_scale = ggplot2::scale_x_continuous(
            breaks = seq(1, 53, by = 13)
        ),
        common_labs = ggplot2::labs(
            caption = enc("данни: EUROSTAT"),
            color = enc("година"),
            x = enc("седмица"),
            y =  enc("умирания")
        ),
        f_labs = ggplot2::labs(
            title = paste(enc("Фактори на надвишаване (свръхсмъртност /"),
                          enc("доказана смъртност)")),
            caption = enc("данни: EUROSTAT, ECDC"),
            color = enc("умирания"),
            x = enc("седмица"),
            y =  enc("умирания")
        ),
        map_vline = list( # eu map last wk w/local data (BG) indicator
            size = line_sz$hthin,
            col = "dark grey"
        ), 
        ext_color_scale = ggplot2::scale_color_manual(
            values = ext_line_cols,
            guide = ext_guide
        ),
        ext_ltypes = ggplot2::scale_linetype_manual(
            values = ext_ls,
            guide = FALSE
        )
    )
    return(function() return(vis))
}

eu_vis <- make_eu_vis()

################################################################################
# wk_plot                                                                      #
# arguments                                                                    #
#  indicator: one of "r14_cases", "r14_deaths", "em_1m, hosp_1m"               #
#  continents: one or more of "Asia", "Africa", "Europe", "Oceania", "America" #
#  top_n: number of lines to label                                             #
#  lower_x, lower_y: axis limits (default = NA = show all data)                #
################################################################################

wk_plot <- function(
    eu_data,
    indicator,
    continents = c("Asia", "Africa", "Europe", "Oceania", "America"),
    top_n = 20,
    lower_x = 10,
    lower_y = NA,
    label_fun = round,
    axis_labels = scales::label_number()
) {
    set.seed(42)
    cont_regex = paste(continents, collapse = "|")
    vy <- indicator
    vis <- eu_vis()
    pdata <- eu_data$factor_tab %>%
        dplyr::filter(stringr::str_detect(continent, cont_regex),
                      !is.na(.data[[vy]])) %>%
        dplyr::mutate(
            geo = forcats::fct_relevel(factor(geo), "BG", after = Inf)
        )
    distinct_geo <- pdata %>% dplyr::select("geo") %>% dplyr::distinct()
    distinct_cont <- pdata %>% dplyr::select("continent") %>% dplyr::distinct()
    cont_str <- paste(distinct_cont %>% dplyr::pull(), collapse = ", ")
    geo_count <- distinct_geo %>% dplyr::count() %>% dplyr::pull()
    pal <- c(scales::hue_pal()(geo_count))
    pal[length(pal)] <- "black"
    last_data_pt <- pdata %>%
        dplyr::filter(!is.na(.data[[vy]])) %>%
        dplyr::group_by(geo) %>%
        dplyr::arrange(year, week) %>%
        dplyr::slice_tail() %>%
        dplyr::ungroup()
    max_week <- last_data_pt %>%
        dplyr::pull(week) %>%
        max()
    max_pt <- pdata %>%
        dplyr::filter(!is.na(.data[[vy]])) %>%
        dplyr::group_by(geo) %>%
        dplyr::filter(.data[[vy]] == max(.data[[vy]]), week != max_week) %>%
        dplyr::ungroup()
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = .data[[vy]],
            color = geo,
            fontface = ifelse(geo == "BG", "bold", "plain"),
            label = paste0(geo_name, " (", label_fun(.data[[vy]]), ")"),
            size = ifelse(geo == "BG", "A", "B")
        )
    )
    plt <- plt +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = last_data_pt, size = 0.7) +
        ggrepel::geom_text_repel(
            data = last_data_pt %>%
                dplyr::arrange(geo == "BG", .data[[vy]]) %>%
                dplyr::slice_tail(n = top_n),
            mapping = ggplot2::aes(x = max_week),
            size = 3.6,
            nudge_x = 3,
            hjust = 0,
            direction = "y",
            point.padding = NA,
            box.padding = ggplot2::unit(0.15, units = "line"),
            max.overlaps = Inf,
            segment.color	= "#444444",
            segment.size = 0.3,
            segment.alpha = 0.4,
            show.legend = FALSE
        ) +
        shadowtext::geom_shadowtext(
            data = max_pt %>%
                dplyr::filter(geo != "BG") %>%
                dplyr::arrange(.data[[vy]]) %>%
                dplyr::slice_tail(n = 25),
            mapping = ggplot2::aes(label = geo),
            size = 3.0,
            vjust = -0.3,
            bg.color = "#ebebeb",
            show.legend = FALSE
        ) +
        ggplot2::scale_x_continuous(
            breaks = seq(1, 53, by = 2),
            limits = c(lower_x, NA),
            expand = ggplot2::expansion(mult = c(0.02, 0.26))
        ) +
        ggplot2::scale_y_continuous(
            limits = c(lower_y, NA),
            labels = axis_labels
        ) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::scale_size_manual(
            values = c(vis$line_sz$mthick, vis$line_sz$thin)
        ) +
        ggplot2::guides(color = FALSE, size = FALSE) +
        vis$v_labs[indicator] +
        ggplot2::labs(
            title = sprintf("%s (%s)", vis$v_labs[[indicator]]$title, cont_str)
        ) +
        vis$gtheme1
    return(plt)
}

################################################################################
# factor comparison                                                            #
################################################################################
fplot <- function(eu_data) {
    vis <- eu_vis()
    pdata <- eu_data$factor_tab %>%
        dplyr::filter(geo %in% vis$comp_f)
    labeled_factors <- pdata %>% dplyr::filter(ed_factor > 1.2)
    plt <- ggplot2::ggplot(data = pdata,
                           mapping = ggplot2::aes(x = week)) +
        ggplot2::geom_ribbon(
            mapping = ggplot2::aes(ymin = mean_deaths,
                                   ymax = d_2020 - covid_deaths),
            fill = "#99000044"
        ) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = mean_deaths,
                                                  color = vis$f_col[1])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_2020 - covid_deaths,
                                                  color = vis$f_col[2])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_2020,
                                                  color = "2020")) +
        shadowtext::geom_shadowtext(
            data = labeled_factors,
            mapping = ggplot2::aes(label = round(ed_covid_factor, 2),
                                   y = d_2020 - covid_deaths),
            angle = 90,
            size = 3.5,
            color = "black",
            bg.color = "white"
        ) +
        vis$f_color_scale +
        vis$common_xweek_scale +
        ggplot2::facet_wrap(~ geo_name, ncol = 2, scales = "free_y") +
        vis$f_labs +
        vis$gtheme1
    return(plt)
}

################################################################################
# contry by age groups plot (argument: country code, e.g. "BG", "UK", "EL")    #
################################################################################
cplot <- function(eu_data, country_code) {
    vis <- eu_vis()
    dtab <- eu_data$eurostat_deaths
    get_geo_names <- eu_data$get_geo_names
    if (substr(get_geo_names(country_code), 1, 1) %in% c(enc("В"), enc("Ф")))
        title_pre <- paste0(vis$txt_title1, vis$txt_v)
    else
        title_pre <- vis$txt_title1
    pdata <- dtab %>% dplyr::filter(
        geo == country_code,
        sex == "T",
        year >= 2015,
        stringr::str_detect(age, "([1234567]|80-89|90|00)")
    )
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            size = ifelse(year == 2020, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        vis$common_size_scale +
        vis$common_color_scale +
        vis$common_xweek_scale +
        ggplot2::facet_wrap(~ age, nrow = 2) +
        ggplot2::labs(
            title = paste(title_pre,
                          get_geo_names(country_code),
                          vis$txt_title2)
        ) +
        vis$common_labs +
        vis$gtheme1
    return(plt)
}

################################################################################
# contry totals plot (argument: country code, e.g. "BG", "UK", "EL")           #
################################################################################
tplot <- function(eu_data, country_code) {
    vis <- eu_vis()
    get_geo_names <- eu_data$get_geo_names
    if (substr(get_geo_names(country_code), 1, 1) %in% c(enc("В"), enc("Ф")))
        title_pre <- paste0(vis$txt_title1, vis$txt_v)
    else
        title_pre <- vis$txt_title1
    pdata <- eu_data$eurostat_deaths %>%
        dplyr::filter(geo == country_code, sex == "T", age == "TOTAL")
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            linetype = as.factor(year),
            size = ifelse(year == 2020, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            data = pdata %>% dplyr::filter(year == 2020),
            size = 2.7
        ) +
        vis$ext_ltypes +
        vis$ext_color_scale +
        vis$w_size_scale +
        vis$common_xweek_scale +
        ggplot2::labs(
            title = paste(title_pre,
                          get_geo_names(country_code),
                          vis$txt_title3)
        ) +
        vis$common_labs +
        vis$gtheme1
    return(plt)
}

################################################################################
# map plot                                                                     #
################################################################################
mplot <- function(eu_data) {
    vis <- eu_vis()
    last_bg_wk <- eu_data$last_week("BG")
    pdata <- eu_data$eurostat_deaths %>%
        dplyr::filter(geo %in% eu_data$eu_codes,
                      sex == "T",
                      age == "TOTAL",
                      year >= 2015)
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            size = ifelse(year == 2020, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(xintercept = last_bg_wk,
                            size = vis$map_vline$size,
                            color = vis$map_vline$col) +
        vis$common_size_scale +
        vis$common_color_scale +
        vis$common_xweek_scale +
        geofacet::facet_geo(~ geo_name,
                            grid = eu_data$eu_map_grid,
                            scales = "free_y") +
        ggplot2::labs(title = vis$txt_titlei) +
        vis$common_labs +
        vis$gtheme2
    return(plt)
}

################################################################################
# example output                                                               #
################################################################################
demo_save <- function() {
    eu_data <- get_eu_data()
    export <- function(plot, file, w = 11, h = 7) {
        ggplot2::ggsave(file = file, width = w, height = h, plot = plot)
    }
    export(plot = wk_plot(eu_data, indicator = "tests_100k", top_n = 100),
           file = "cmp_tst_eurp.svg")
    export(plot = wk_plot(eu_data,
                          indicator = "positivity",
                          top_n = 100,
                          label_fun = function(x) sprintf("%.1f%%", 100 * x),
                          axis_labels = scales::label_percent()),
           file = "cmp_pos_eurp.svg")
    export(plot = wk_plot(eu_data, indicator = "hosp_1m", top_n = 100),
           file = "cmp_h_eurp.svg")
    export(plot = wk_plot(eu_data, indicator = "em_1m"),
           file = "exd1m_eurp.svg")
    export(plot = wk_plot(eu_data, indicator = "r14_cases", lower_y = 0),
           file = "cmp_i_wrld.svg")
    export(plot = wk_plot(eu_data, indicator = "r14_deaths", lower_y = 0),
           file = "cmp_d_wrld.svg")
    export(plot = wk_plot(eu_data, indicator = "r14_cases",
                          continents = "Europe",
                          lower_y = 0),
           file = "cmp_i_eurp.svg")
    export(plot = wk_plot(eu_data,
                          indicator = "r14_deaths",
                          continents = "Europe",
                          lower_y = 0),
           file = "cmp_d_eurp.svg")
    export(mplot(eu_data), "00_eur_map.svg")
    export(fplot(eu_data), "00_cmp.svg", w = 14.4, h = 8)
    export(tplot(eu_data, "BG"), "00_BG_totals.svg")
    for (n in seq_along(eu_data$eu_codes)) {
        # add numbers to filenames per alphabetical order of countries
        pn <- stringr::str_pad(n, 2, pad = "0")
        export(cplot(eu_data, eu_data$eu_codes[n]),
               paste0(pn, "_", eu_data$eu_codes[n], ".svg"))
    }
}