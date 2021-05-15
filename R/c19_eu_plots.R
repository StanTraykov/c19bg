# plot EUROSTAT and ECDC data
# - weekly deaths / age groups / comparison map / etc. (EUROSTAT)
# - 14-day COVID-19 incidence/registered deaths (ECDC)
# - excess deaths per 1M (EUROSTAT)
# - hospitalized per 1M (ECDC)
# - excess factor (EUROSTAT + ECDC)

#' @importFrom magrittr %>%

make_eu_vis <- function(process_data = FALSE) {
    if (!process_data) {
        vis <- list()
        vis_f <- function() {
            if (length(vis) == 0)
                vis <<- make_eu_vis(process_data = TRUE)
            return(vis)
        }
        return(vis_f)
    }

    line_sz <- list(
        # for faceted plots
        hthin = 0.2,
        thin = 0.3,
        thick = 0.7,
        # for whole page plots
        wthin = 0.6,
        mthick = 0.8,
        wthick = 1.1
    )
    line_cols <- c( # faceted deaths plot
        "#AAAAAA", "#BBAA00", "#008800", "#0000BB", "#000000", "#D000D0",
        "#FF0000"
    )
    col_legend <- ggplot2::guide_legend( # faceted deaths plot
        nrow = 1,
        override.aes = list(
            size = c(rep(line_sz$thin, 6), line_sz$thick)
        )
    )
    ext_ls <- c( # for country totals deaths plot
        rep("solid",2), "dotted", rep("solid", 9)
    )
    ext_guide <- ggplot2::guide_legend( # for country totals deaths plot
        nrow = 1,
        override.aes = list(linetype = ext_ls, shape = c(rep(NA, 11), 19))
    )
    ext_line_cols <- c( # for country totals deaths plot
        "#BBBBBB", "#BBBBBB", "#BBBBBB", "#BBBBBB", "#777777", "#88AAAA",
        "#BBAA00", "#008800", "#0000BB", "#000000", "#D000D0", "#FF0000"
    )
    v_labs <- list( # labels for ECDC/EUROSTAT weekly comparison charts
        r14_cases = ggplot2::labs(
            title = paste(tra("14-dnevna COVID-19 zabolevaemost na 100 hil.")),
            caption = tra("danni: ECDC"),
            x = tra("sedmica"),
            y =  tra("14-dnevna zabolevaemost na 100 hil.")
        ),
        r14_deaths = ggplot2::labs(
            title = paste(tra("14-dnevna COVID-19 smartnost na 1 mln.")),
            caption = tra("danni: ECDC"),
            x = tra("sedmica"),
            y =  tra("14-dnevna smartnost na 1 mln.")
        ),
        em_1m = ggplot2::labs(
            title = paste(
                tra("Svrahsmartnost na 1 mln. spramo sr. 2015-2019 po EUROSTAT")
            ),
            caption = tra("danni: EUROSTAT"),
            x = tra("sedmica"),
            y =  tra("svrahsmartnost na 1 mln.")
        ),
        hosp_1m = ggplot2::labs(
            title = tra("Broj hospitalizirani s COVID-19 na 1 mln."),
            caption = tra("danni: ECDC"),
            x = tra("sedmica"),
            y =  tra("hospitalizirani na 1 mln.")
        ),
        tests_100k = ggplot2::labs(
            title = tra("Izvarseni testove na 100 hil."),
            caption = tra("danni: ECDC"),
            x = tra("sedmica"),
            y =  tra("testove na 100 hil.")
        ),
        positivity = ggplot2::labs(
            title = tra("Pozitivnost (novi slucai.broj testove)"),
            caption = tra("danni: ECDC"),
            x = tra("sedmica"),
            y =  tra("pozitivnost")
        )
    )
    font_size <- getOption("c19bg.font_size") * getOption("c19bg.font_scale")
    font_family <- getOption("c19bg.font_family")
    gtheme1 <- ggplot2::theme(
        text = ggplot2::element_text(
            size = font_size,
            family = font_family
        ),
        #panel.grid.minor.x = ggplot2::element_blank(),
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
        txt_title1 = tra("Umirania v"),
        txt_v = tra("av"),
        txt_title2 = tra("po sedmici i vazrastovi grupi"),
        txt_title3 = tra("po sedmici"),
        txt_titlei = tra("Umirania po strani i sedmici"),
        f_col = c(tra("sredno 2015-2019 g."),
                  tra("umirania bez dokazani smartni slucai"),
                  tra("umirania")),
        line_sz = line_sz,
        pt_sz = c(
            0.8,  # faceted plots: week points for current year (only if 1 obs)
            2.7   # whole-page plots: week points for current year
        ),
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
            caption = tra("danni: EUROSTAT"),
            color = tra("godina"),
            x = tra("sedmica"),
            y =  tra("umirania")
        ),
        f_labs = ggplot2::labs(
            title = paste(tra("Faktori na nadvisavane (svrahsmartnost ."),
                          tra("dokazana smartnost)")),
            caption = tra("danni: EUROSTAT, ECDC"),
            color = tra("umirania"),
            x = tra("sedmica"),
            y =  tra("umirania")
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
        ),
        font_family = font_family,
        font_size = font_size,
        font_small = 4 * getOption("c19bg.font_scale"),
        font_xsmall = 3.6 * getOption("c19bg.font_scale"),
        font_size_maxlab = 3.0 * getOption("c19bg.font_scale"),
        font_size_faclabs = 3.5 * getOption("c19bg.font_scale")
    )
    return(vis)
}

eu_vis <- make_eu_vis()

#' Weekly plot using ECDC/EUROSTAT data.
#'
#' @param indicator one of: "r14_cases", "r14_deaths", "em_1m", hosp_1m",
#'                  "positivity", "tests_100k"
#' @param continents continents to include (only some stats available outside
#'                   Europe/EU+)
#' @param highlight country to highlight (EU 2-ltr code, e.g. "EL" for Greece)
#' @param top_n number of lines to label
#' @param first_wk first week in 2020 to plot
#' @param lower_y indicator axis limit (default NA = show all data)
#' @param label_fun function to apply to line labels (default is to round)
#' @param axis_labels axis labels, e.g. scales::label_number(),
#'                    scales::label_percent()
#' @param eu_data eu data
#'
#' @export
#' @examples
#' \dontrun{
#' c19_eu_weekly(indicator = "r14_cases", lower_y = 0)
#' c19_eu_weekly(indicator = "positivity",
#'               top_n = 100,
#'               label_fun = function(x) sprintf("%.1f%%", 100 * x),
#'               axis_labels = scales::label_percent())
#' }
#' @family plot funcs
c19_eu_weekly <- function(
    indicator,
    continents = c("Asia", "Africa", "Europe", "Oceania", "America"),
    highlight = "BG",
    top_n = 20,
    first_wk = 10,
    lower_y = NA,
    label_fun = round,
    axis_labels = scales::label_number(),
    eu_data = c19_eu_data()
) {
    set.seed(42)
    cont_regex <- paste(continents, collapse = "|")
    vy <- indicator
    vis <- eu_vis()
    pdata <- eu_data$factor_tab %>%
        dplyr::filter(stringr::str_detect(continent, cont_regex),
                      !is.na(.data[[vy]]),
                      year > 2020 | week >= first_wk) %>%
        dplyr::mutate(
            geo = forcats::fct_relevel(factor(geo), highlight, after = Inf)
        )
    distinct_geo <- pdata %>% dplyr::distinct(geo)
    distinct_cont <- pdata %>% dplyr::distinct(continent)
    cont_str <- paste(distinct_cont %>%
                          dplyr::mutate(continent = sapply(continent, tra)) %>%
                          dplyr::pull(),
                      collapse = ", ")
    geo_count <- distinct_geo %>% dplyr::count() %>% dplyr::pull()
    pal <- c(scales::hue_pal()(geo_count))
    pal[length(pal)] <- "black"
    last_data_pt <- pdata %>%
        dplyr::filter(!is.na(.data[[vy]])) %>%
        dplyr::group_by(geo) %>%
        dplyr::arrange(year, week) %>%
        dplyr::slice_tail() %>%
        dplyr::ungroup()
    max_yr_wk <- last_data_pt %>%
        dplyr::arrange(year, week) %>%
        dplyr::slice_tail() %>%
        dplyr::pull(yr_wk)
    max_pt <- pdata %>%
        dplyr::filter(!is.na(.data[[vy]])) %>%
        dplyr::group_by(geo) %>%
        dplyr::filter(.data[[vy]] == max(.data[[vy]]), yr_wk != max_yr_wk) %>%
        dplyr::ungroup()
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = yr_wk,
            group = geo,
            y = .data[[vy]],
            color = geo,
            fontface = ifelse(geo == highlight, "bold", "plain"),
            label = paste0(geo_name, " (", label_fun(.data[[vy]]), ")"),
            size = ifelse(geo == highlight, "A", "B")
        )
    )
    plt <- plt +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = last_data_pt, size = 0.7) +
        ggrepel::geom_text_repel(
            data = last_data_pt %>%
                dplyr::arrange(geo == highlight, .data[[vy]]) %>%
                dplyr::slice_tail(n = top_n),
            mapping = ggplot2::aes(x = max_yr_wk),
            family = vis$font_family,
            size = ifelse(top_n > 20, vis$font_xsmall, vis$font_small),
            nudge_x = ifelse(top_n > 20, 4.0, 2.5),
            hjust = 0,
            direction = "y",
            point.padding = NA,
            box.padding = ggplot2::unit(0.12, units = "line"),
            max.overlaps = Inf,
            segment.color	= "dark gray",
            segment.size = 0.3,
            segment.alpha	= 0.5,
            bg.colour = "#ebebeb",
            max.time = 1,
            max.iter = 100000,
            show.legend = FALSE
        ) +
        shadowtext::geom_shadowtext(
            data = max_pt %>%
                dplyr::filter(geo != highlight) %>%
                dplyr::arrange(.data[[vy]]) %>%
                dplyr::slice_tail(n = 25),
            mapping = ggplot2::aes(label = geo),
            size = vis$font_size_maxlab,
            vjust = -0.3,
            family = vis$font_family,
            bg.color = "#ebebeb",
            show.legend = FALSE
        ) +
        ggplot2::scale_x_discrete(
            expand = ggplot2::expansion(mult = c(0.02, 0.25)),
            labels = function(x) { x[c(FALSE, TRUE)] = ""; x}
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
        vis$gtheme1 +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    return(plt)
}

#' Plot excess deaths factors for a selection of countries.
#'
#' @param countries countries to plot
#' @param eu_data eu data
#'
#' @export
#' @family plot funcs
c19_deaths_factor <- function(
    countries = c("BG", "HU", "BE", "CZ", "FR", "ES", "IT", "RO"),
    eu_data = c19_eu_data()
) {
    vis <- eu_vis()
    pdata <- eu_data$factor_tab %>%
        dplyr::filter(geo %in% countries)
    labeled_factors <- pdata %>% dplyr::filter(ed_factor > 1.2)
    plt <- ggplot2::ggplot(data = pdata,
                           mapping = ggplot2::aes(x = yr_wk, group = 1)) +
        ggplot2::geom_ribbon(
            mapping = ggplot2::aes(ymin = mean_deaths,
                                   ymax = d_tt - covid_deaths),
            fill = "#99000044"
        ) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = mean_deaths,
                                                  color = vis$f_col[1])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_tt - covid_deaths,
                                                  color = vis$f_col[2])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_tt,
                                                  color = vis$f_col[3])) +
        shadowtext::geom_shadowtext(
            data = labeled_factors,
            mapping = ggplot2::aes(label = round(ed_covid_factor, 2),
                                   y = d_tt - covid_deaths),
            angle = 90,
            size = vis$font_size_faclabs,
            family = vis$font_family,
            color = "black",
            bg.color = "white"
        ) +
        vis$f_color_scale +
        ggplot2::scale_x_discrete(
            breaks = function(x) { x[c(TRUE, FALSE)] }
        ) +
        ggplot2::facet_wrap(~ geo_name, ncol = 2, scales = "free_y") +
        vis$f_labs +
        vis$gtheme1 +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90,
                                                vjust = 0.5,
                                                hjust = 1)
        )
    return(plt)
}

#' Plot deaths in a country by age band and week.
#'
#' @param country_code two-letter EU code e.g. "BG", "UK", "EL" (=Greece)
#' @param eu_data eu data
#'
#' @export
#' @family plot funcs
c19_deaths_age <- function(country_code, eu_data = c19_eu_data()) {
    vis <- eu_vis()
    dtab <- eu_data$eurostat_deaths
    get_geo_names <- eu_data$get_geo_names
    if (substr(get_geo_names(country_code), 1, 1) %in% c(tra("V"), tra("F")))
        title_pre <- paste0(vis$txt_title1, vis$txt_v)
    else
        title_pre <- vis$txt_title1
    pdata <- dtab %>% dplyr::filter(
        geo == country_code,
        sex == "T",
        year >= 2015,
        stringr::str_detect(age, "([1234567]|80-89|90|00)")
    )
    max_yr <- pdata %>% dplyr::filter(!is.na(deaths)) %>%
        dplyr::pull(year) %>%
        max()
    num_obs_max_yr <- pdata %>%
        dplyr::filter(year == max_yr, age == "00-09", !is.na(deaths)) %>%
        dplyr::count() %>%
        dplyr::pull()
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            size = ifelse(year == max_yr, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            data = pdata %>% dplyr::filter(year == max_yr, num_obs_max_yr == 1),
            size = vis$pt_sz[1],
            show.legend = FALSE
        ) +
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

#' Plot total weekly deaths for a country.
#'
#' @param country_code two-letter EU code e.g. "BG", "UK", "EL" (=Greece)
#' @param eu_data eu data
#'
#' @export
#' @family plot funcs
c19_deaths_total <- function(country_code, eu_data = c19_eu_data()) {
    vis <- eu_vis()
    get_geo_names <- eu_data$get_geo_names
    if (substr(get_geo_names(country_code), 1, 1) %in% c(tra("V"), tra("F")))
        title_pre <- paste0(vis$txt_title1, vis$txt_v)
    else
        title_pre <- vis$txt_title1
    pdata <- eu_data$eurostat_deaths %>%
        dplyr::filter(geo == country_code, sex == "T", age == "TOTAL")
    max_yr <- pdata %>% dplyr::filter(!is.na(deaths)) %>%
        dplyr::pull(year) %>%
        max()
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            linetype = as.factor(year),
            size = ifelse(year == max_yr, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            data = pdata %>% dplyr::filter(year == max_yr),
            size = vis$pt_sz[2]
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

#' Plot comparative deaths map for EU+ countries.
#'
#' @param eu_data eu data
#' @param vline_last_wk draw vertical line on each plot at the last weekly data
#' point for this country (to facilitate easier comparison).
#'
#' @export
#' @family plot funcs
c19_deaths_map <- function(vline_last_wk = "BG", eu_data = c19_eu_data()) {
    vis <- eu_vis()
    last_bg_wk <- eu_data$last_week(vline_last_wk)
    pdata <- eu_data$eurostat_deaths %>%
        dplyr::filter(geo %in% eu_data$eu_codes,
                      sex == "T",
                      age == "TOTAL",
                      year >= 2015)
    max_yr <- pdata %>% dplyr::pull(year) %>% max()
    one_obs_max_yr <- pdata %>%
        dplyr::filter(year == max_yr, !is.na(deaths)) %>%
        dplyr::group_by(geo) %>%
        dplyr::summarise(num_obs = dplyr::n()) %>%
        dplyr::filter(num_obs == 1) %>%
        dplyr::pull(geo)
    plt <- ggplot2::ggplot(
        data = pdata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            size = ifelse(year == max_yr, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = pdata %>%
                                dplyr::filter(geo %in% one_obs_max_yr,
                                              year == max_yr),
                            show.legend = FALSE) +
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

#' Save various EU plots.
#'
#' @param ... Passed export params: w (width), h (height), file_ext (".svg",
#'            ".png", ".jpg"; others may work as well). Rest passed to ggplot2,
#'            e.g. quality for JPEG output.
#'
#' @export
#' @examples
#' \dontrun{
#' c19_eu_plots_save() # default is SVG
#' c19_eu_plots_save(file_ext = ".jpg", quality = 100)
#' }
#' @family output funcs
c19_eu_plots_save <- function(...) {
    export(
        plot = c19_eu_weekly(
            indicator = "positivity",
            top_n = 100,
            label_fun = function(x) sprintf("%.1f%%", 100 * x),
            axis_labels = scales::label_percent()
        ),
        file = "C15_cmp_pos_eurp",
        ...
    )
    export(
        plot = c19_eu_weekly(
            indicator = "r14_cases",
            continents = "Europe",
            lower_y = 0
        ),
        file = "C11_cmp_i_eurp",
        ...
    )
    export(
        plot = c19_eu_weekly(
            indicator = "r14_deaths",
            continents = "Europe",
            lower_y = 0
        ),
        file = "C11_cmp_d_eurp",
        ...
    )
    export(plot = c19_eu_weekly(indicator = "tests_100k", top_n = 100),
           file = "C14_cmp_tst_eurp",
           ...)
    export(plot = c19_eu_weekly(indicator = "hosp_1m", top_n = 100),
           file = "C13_cmp_h_eurp",
           ...)
    export(plot = c19_eu_weekly(indicator = "em_1m"),
           file = "C12_exd1m_eurp",
           ...)
    export(plot = c19_eu_weekly(indicator = "r14_cases", lower_y = 0),
           file = "C10_cmp_i_wrld")
    export(plot = c19_eu_weekly(indicator = "r14_deaths", lower_y = 0),
           file = "C10_cmp_d_wrld",
           ...)
    export(file = "D00_BG_t",
           ...,
           plot = c19_deaths_total("BG"))
    export(file = "D00_map",
           ...,
           plot = c19_deaths_map())
    export(file = "D00_cmp",
           ...,
           scale_h = 8 / 7,
           scale_w = 14.4 / 11,
           plot = c19_deaths_factor())
    eu_codes <- c19_eu_data()$eu_codes
    for (n in seq_along(eu_codes)) {
        pn <- stringr::str_pad(n, 2, pad = "0")
        cd <- eu_codes[n]
        export(file = paste0("D", pn, "_", cd),
               ...,
               plot = c19_deaths_age(cd))
    }
}
