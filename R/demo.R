# plot EUROSTAT and ECDC data
# - weekly deaths / age groups / comparison map / etc. (EUROSTAT)
# - 14-day COVID-19 incidence/registered deaths (ECDC)
# - excess deaths per 1M (EUROSTAT)
# - hospitalized per 1M (ECDC)
# - excess factor (EUROSTAT + ECDC)

library(magrittr)

extrafont::loadfonts(device = "win") # comment out to use default fonts
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

##### get data from EUROSTAT, ECDC
# download data, if missing; return local filename
dl_missing <- function(url, filename, zip_local = FALSE) {
    local_fn <- file.path("data", filename)
    if (zip_local)
        down_dest <- paste0(local_fn, ".temp")
    else
        down_dest <- local_fn
    if (!file.exists(local_fn)) {
        download.file(url, down_dest)
        if (zip_local) {
            R.utils::gzip(down_dest,
                          destname = local_fn,
                          overwrite = TRUE,
                          remove = TRUE)
        }
    }
    return(local_fn)
}

# ECDC nat'l case death
local_ncd_file <- dl_missing(
    url = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",
    filename = "ecdc_ncd.csv.gz",
    zip_local = TRUE
)

# ECDC hosp
local_hosp_file <- dl_missing(
    url = paste0("https://opendata.ecdc.europa.eu/covid19/",
                 "hospitalicuadmissionrates/csv/data.csv"),
    filename = "ecdc_hosp.csv.gz",
    zip_local = TRUE
)

# EUROSTAT
deaths_file <- "demo_r_mwk_10.tsv.gz"
local_deaths_file <- dl_missing(
    url = paste0("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
                 "BulkDownloadListing?file=data/",
                 deaths_file),
    filename = deaths_file
)

# ECDC testing
local_tstg_file <- dl_missing(
    url = "https://opendata.ecdc.europa.eu/covid19/testing/csv",
    filename = "ecdc_tstg.csv.gz",
    zip_local = TRUE
)

##### country config
# for deaths comparison / eu map (Ireland deaths data missing from EUROSTAT)
eu_codes <- c("IS", "NO", "SE", "FI", "EE", "UK", "DK", "NL", "LV", "LT", 
              "DE", "PL", "BE", "CZ", "SK", "FR", "AT", "LU", "RS", "RO", "HU", 
              "SI", "ES", "PT", "CH", "BG", "ME", "HR", "IT", "AL", "EL", "MT", 
              "CY")

# for excess deaths factor comparison
comp_f <- c("BG", "UK", "BE", "NL", "FR", "ES", "IT", "RO")

# country names
bg_names <- read.csv(file.path("prog_data", "bg_cnames.csv"), na.strings = "")
bg_names <- bg_names %>%
    dplyr::mutate(bg_name = enc(bg_name)) %>%
    dplyr::rename(geo_name = bg_name)
codes_tab <- read.csv(file.path("prog_data", "ccodes.csv"), na.strings = "")
codes_tab <- codes_tab %>%
    dplyr::left_join(bg_names, by = "tl_code") # use Bulgarian names

# return geo names in target language from EU 2-letter or ISO 3-letter codes
get_geo_names <- function(codes) {
    ret <- sapply(codes,
                  function(x) codes_tab %>%
                      dplyr::filter(geo == x | tl_code == x) %>%
                      dplyr::pull(geo_name))
    return(ret)
}

# map grid
egrid <- data.frame(row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4,
                            4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
                    col = c(1, 4, 5, 6, 7, 1, 4, 3, 7, 7, 4, 6, 3, 5, 5, 2, 4,
                            3, 6, 7, 5, 4, 2, 1, 3, 7, 6, 5, 3, 6, 7, 2, 7),
                    code = eu_codes,
                    name = get_geo_names(eu_codes))

# rearrange EU codes in target language alphabetical order
eu_codes <- codes_tab %>% 
    dplyr::filter(geo %in% eu_codes) %>%
    dplyr::arrange(geo_name) %>%
    dplyr::pull(geo)

##### visuals config
txt_title1 <- enc("Умирания в")
txt_v <- enc("ъв")
txt_title2 <- enc("по седмици и възрастови групи")
txt_title3 <- enc("по седмици")
txt_titlei <- enc("Умирания по страни и седмици")
f_col <- c(enc("средно 2015-2019 г."),
           enc("2020 г. без PCR-доказани смъртни случаи"),
           enc("2020 г."))
thin <- 0.3  # lines
thick <- 0.7
wthin <- 0.6 # for whole page plots
mthick <- 0.8
wthick <- 1.1
f_color_scale <- ggplot2::scale_color_manual(
    values = c("dark gray", "red", "dark magenta")
)
line_cols <- c("#AAAAAA", "#BBAA00", "#008800", "#0000BB", "#000000", "#FF0000")
col_legend <- ggplot2::guide_legend(
    nrow = 1,
    override.aes = list(size = c(rep(thin, 5), thick))
)
common_color_scale <- ggplot2::scale_color_manual(values = line_cols,
                                                  guide = col_legend)
common_size_scale <- ggplot2::scale_size_manual(values = c(thick, thin),
                                                guide = FALSE)
w_size_scale <- ggplot2::scale_size_manual(values = c(wthick, wthin),
                                           guide = FALSE)
common_xweek_scale <- ggplot2::scale_x_continuous(breaks = seq(1, 53, by = 13))
common_labs <- ggplot2::labs(
    caption = enc("данни: EUROSTAT"),
    color = enc("година"),
    x = enc("седмица"),
    y =  enc("умирания")
)
f_labs <- ggplot2::labs(
    title = paste(enc("Фактори на надвишаване (свръхсмъртност /"),
                  enc("PCR-доказана смъртност)")),
    caption = enc("данни: EUROSTAT, ECDC"),
    color = enc("умирания"),
    x = enc("седмица"),
    y =  enc("умирания")
)
v_labs <- list(
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
        title = enc("Позитивност (нови случаи/общ брой тестове)"),
        caption = enc("данни: ECDC"),
        x = enc("седмица"),
        y =  enc("позитивност")
    )
)
map_vline <- list(size = 0.2, col = "dark grey") # vline last week with BG data
ext_line_cols <- c("#BBBBBB", "#BBBBBB", "#BBBBBB", "#BBBBBB", "#777777",
                   "#88AAAA", "#BBAA00", "#008800", "#0000BB", "#000000",
                   "#FF0000") # for country totals plot
ext_ls <- c("solid", "solid", "dotted", "solid", "solid", "solid",
            "solid", "solid", "solid", "solid", "solid") # for country totals
ext_guide <-  ggplot2::guide_legend(
    nrow = 1,
    override.aes = list(linetype = ext_ls, shape = c(rep(NA, 10), 19))
)
ext_color_scale <- ggplot2::scale_color_manual(values = ext_line_cols,
                                               guide = ext_guide)
ext_ltypes <- ggplot2::scale_linetype_manual(values = ext_ls,
                                             guide = FALSE)
gtheme1 <- ggplot2::theme(
    text = ggplot2::element_text(size = 14,
                                 family = grDevices::windowsFont("Calibri")),
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
        strip.text = ggplot2::element_text(size = 10,
                                           margin = ggplot2::margin(1, 0, 1, 0))
    )

##### tidy data
## EUROSTAT weekly mortality
agrp_names <- function(x) { # age group labels
    x <- sub("Y_LT10", "00-09", x)
    x <- sub("Y_GE80", "80+", x)
    x <- sub("Y_GE90", "90+", x)
    x <- sub("Y", "", x)
    return(x)
}

dtab <- read.delim(gzfile(local_deaths_file))
dtab <- cbind(stringr::str_split_fixed(dtab[[1]], ",", 4), dtab[, -1])
names(dtab) <- c("age", "sex", "unit", "geo", names(dtab)[-(1:4)])
dtab <- dtab %>%
    dplyr::select(dplyr::matches("(201[0-9]|2020)W[0-5]|age|geo|sex")) %>%
    tidyr::pivot_longer(cols = tidyr::matches("20..W"),
                        names_to = c("year", "week"),
                        names_pattern = "X(....)W(..)",
                        names_transform = list(year = as.integer,
                                               week = as.integer),
                        values_to = "deaths") %>%
    dplyr::mutate(deaths = as.integer(gsub("[: pe]", "", deaths)),
                  age = agrp_names(age)) %>%
    dplyr::arrange(year, week, age)
mean_tab <- dtab %>% # mean 2015-2019 & mean 2015-2019* *with NA removed
    dplyr::filter(sex == "T",
                  year >= 2015,
                  year <= 2019,
                  age == "TOTAL") %>%
    dplyr::group_by(geo, week) %>%
    dplyr::summarize(mean_deaths = mean(deaths),
                     mean_deaths_star = mean(deaths, na.rm = TRUE))
tt_tab <- dtab %>% # 2020 deaths
    dplyr::filter(sex == "T",
                  year == 2020,
                  age == "TOTAL") %>%
    dplyr::select("geo", "week", "deaths") %>%
    dplyr::rename(d_2020 = deaths)
last_bg_wk <- tt_tab %>%
    dplyr::filter(geo == "BG", d_2020 > 0) %>%
    dplyr::summarize(max(week)) %>%
    dplyr::pull()
cmp_tab <- dplyr::left_join(mean_tab, tt_tab, by = c("geo", "week")) %>%
    dplyr::mutate(excess_deaths = d_2020 - mean_deaths) %>%
    dplyr::mutate(excess_deaths_star = d_2020 - mean_deaths_star) %>%
    dplyr::ungroup()

## ECDC nat'l cases / deaths
ncd_tab <- read.csv(gzfile(local_ncd_file), na.strings = "")
names(ncd_tab)[1] = "country" # BOM removal (fileEncoding doesn't work on Win)
ncd_tab <- ncd_tab %>%
    dplyr::rename(tl_code = country_code) %>%
    dplyr::right_join(codes_tab %>% dplyr::select("tl_code", "geo", "geo_name"),
                      by = "tl_code") %>%
    dplyr::mutate(year = as.integer(substr(year_week, 1, 4)),
                  week = as.integer(substr(year_week, 6, 7)))

# weekly COVID cases/deaths from ECDC
cd_tab <- ncd_tab %>%
    tidyr::pivot_wider(names_from = "indicator",
                       values_from = c("weekly_count",
                                       "cumulative_count",
                                       "rate_14_day")) %>%
    dplyr::rename(cases = weekly_count_cases,
                  covid_deaths = weekly_count_deaths,
                  r14_cases = rate_14_day_cases,
                  r14_deaths = rate_14_day_deaths,
                  cml_cases = cumulative_count_cases,
                  cml_deaths = cumulative_count_deaths)

# EUROSTAT & ECDC data incl. factors & excess mortality per 1M
factor_tab <- dplyr::left_join(cd_tab, cmp_tab, by = c("geo", "week")) %>%
    dplyr::mutate(ed_covid_factor = excess_deaths / covid_deaths) %>%
    dplyr::mutate(ed_factor = d_2020 / mean_deaths) %>%
    dplyr::mutate(em_1m = 1000000 * excess_deaths_star / population)

# add hospit. per ECDC
hosp_tab <- read.csv(gzfile(local_hosp_file))
names(hosp_tab)[1] = "country"
hosp_tab <- hosp_tab %>%
    dplyr::filter(indicator == "Daily hospital occupancy") %>%
    dplyr::rename(hosp_count = value) %>%
    dplyr::select("country", "date", "year_week", "hosp_count") %>%
    dplyr::mutate(
        date = as.Date(date),
        year = as.integer(substr(year_week, 1, 4)),
        week = as.integer(substr(year_week, 7, 8))
    ) %>%
    dplyr::group_by(country, year, week) %>%
    dplyr::slice_tail() %>%
    dplyr::ungroup() %>%
    dplyr::select("country", "year", "week", "hosp_count")
factor_tab <- factor_tab %>%
    dplyr::left_join(hosp_tab, by = c("country", "year", "week")) %>%
    dplyr::mutate(hosp_1m = 1000000 * hosp_count / population)

# add testing per ECDC
tstg_tab <- read.csv(gzfile(local_tstg_file))
names(tstg_tab)[1] = "country"
tstg_tab <- tstg_tab %>%
    dplyr::mutate(
        year = as.integer(substr(year_week, 1, 4)),
        week = as.integer(substr(year_week, 7, 8)),
        # note that ECDC positivity is next Monday's 7-day sum of positives,
        # divided by Sunday's 7-day sum of tests (different from BG charts)
        positivity = positivity_rate / 100
    ) %>%
    dplyr::rename(
        geo = country_code,
        tests = tests_done,
        tests_100k = testing_rate
    ) %>%
    dplyr::select("geo", "year", "week", "tests", "tests_100k", "positivity")
factor_tab <- factor_tab %>%
    dplyr::left_join(tstg_tab, by = c("geo", "year", "week"))
################################################################################
# wk_plot                                                                      #
# arguments                                                                    #
#  indicator: one of "r14_cases", "r14_deaths", "em_1m, hosp_1m"               #
#  continents: one or more of "Asia", "Africa", "Europe", "Oceania", "America" #
#  top_n: number of lines to label                                             #
#  lower_x, lower_y: axis limits (default = NA = show all data)                #
################################################################################

wk_plot <- function(
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
    pdata <- factor_tab %>%
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
            box.padding = ggplot2::unit(1.1, units = "pt"),
            label.padding = ggplot2::unit(0.12, units = "line"),
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
        ggplot2::scale_size_manual(values = c(mthick, thin)) +
        ggplot2::guides(color = FALSE, size = FALSE) +
        v_labs[indicator] +
        ggplot2::labs(
            title = sprintf("%s (%s)", v_labs[[indicator]]$title, cont_str)
        ) +
        gtheme1
    return(plt)
}

################################################################################
# factor comparison                                                            #
################################################################################
fplot <- function() {
    ptab <- factor_tab %>%
        dplyr::filter(geo %in% comp_f)
    ftab <- ptab %>%
        dplyr::filter(ed_factor > 1.2)
    plt <- ggplot2::ggplot(data = ptab,
                           mapping = ggplot2::aes(x = week)) +
        ggplot2::geom_ribbon(
            mapping = ggplot2::aes(ymin = mean_deaths,
                                   ymax = d_2020 - covid_deaths),
            fill = "#99000044"
        ) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = mean_deaths,
                                                  color = f_col[1])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_2020 - covid_deaths,
                                                  color = f_col[2])) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = d_2020,
                                                  color = "2020")) +
        shadowtext::geom_shadowtext(
            data = ftab,
            mapping = ggplot2::aes(label = round(ed_covid_factor, 2),
                                   y = d_2020 - covid_deaths),
            angle = 90,
            size = 3.5,
            color = "black",
            bg.color = "white"
        ) +
        f_color_scale +
        common_xweek_scale +
        ggplot2::facet_wrap(~ geo_name, ncol = 2, scales = "free_y") +
        f_labs +
        gtheme1
    return(plt)
}

################################################################################
# contry by age groups plot (argument: country code, e.g. "BG", "UK", "EL")    #
################################################################################
cplot <- function(country_code) {
    if (substr(get_geo_names(country_code), 1, 1) %in% c(enc("В"), enc("Ф")))
        title_pre <- paste0(txt_title1, txt_v)
    else
        title_pre <- txt_title1
    cdata <- dtab %>% dplyr::filter(
        geo == country_code,
        sex == "T",
        year >= 2015,
        stringr::str_detect(age, "([1234567]|80-89|90|00)")
    )
    plt <- ggplot2::ggplot(data = cdata,
                           mapping = ggplot2::aes(
                               x = week,
                               y = deaths,
                               color = as.factor(year),
                               size = ifelse(year == 2020, "C", "N"))
                           ) +
        ggplot2::geom_line() +
        common_size_scale +
        common_color_scale +
        common_xweek_scale +
        ggplot2::facet_wrap(~ age, nrow = 2) +
        ggplot2::labs(
            title = paste(title_pre, get_geo_names(country_code), txt_title2)
        ) +
        common_labs +
        gtheme1
    return(plt)
}

################################################################################
# contry totals plot (argument: country code, e.g. "BG", "UK", "EL")           #
################################################################################
tplot <- function(country_code) {
    if (substr(get_geo_names(country_code), 1, 1) %in% c(enc("В"), enc("Ф")))
        title_pre <- paste0(txt_title1, txt_v)
    else
        title_pre <- txt_title1
    cdata <- dtab %>% dplyr::filter(geo == country_code,
                                    sex == "T",
                                    age == "TOTAL")
    plt <- ggplot2::ggplot(data = cdata,
                           mapping = ggplot2::aes(
                               x = week,
                               y = deaths,
                               color = as.factor(year),
                               linetype = as.factor(year),
                               size = ifelse(year == 2020, "C", "N"))
                           ) +
        ggplot2::geom_line() +
        ggplot2::geom_point(
            data = cdata %>% dplyr::filter(year == 2020), size = 2.7
        ) +
        ext_ltypes +
        ext_color_scale +
        w_size_scale +
        common_xweek_scale +
        ggplot2::labs(
            title = paste(title_pre, get_geo_names(country_code), txt_title3)
        ) +
        common_labs +
        gtheme1
    return(plt)
}

################################################################################
# map plot                                                                     #
################################################################################
mplot <- function() {
    idata <- dtab %>%
        dplyr::filter(geo %in% eu_codes,
                      sex == "T",
                      age == "TOTAL",
                      year >= 2015) %>%
        dplyr::left_join(codes_tab %>% dplyr::select("geo", "geo_name"),
                         by = "geo")
    plt <- ggplot2::ggplot(
        data = idata,
        mapping = ggplot2::aes(
            x = week,
            y = deaths,
            color = as.factor(year),
            size = ifelse(year == 2020, "C", "N")
        )
    ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(xintercept = last_bg_wk,
                            size = map_vline$size,
                            color = map_vline$col) +
        common_size_scale +
        common_color_scale +
        common_xweek_scale +
        geofacet::facet_geo(~ geo_name,
                            grid = egrid,
                            scales = "free_y") +
        ggplot2::labs(title = txt_titlei) +
        common_labs +
        gtheme2
    return(plt)
}

################################################################################
# example output                                                               #
################################################################################
save_all <- function() {
    export <- function(plot, file, w = 11, h = 7) {
        ggplot2::ggsave(file = file, width = w, height = h, plot = plot)
    }
    export(plot = wk_plot(indicator = "tests_100k", top_n = 100),
           file = "cmp_tst_eurp.svg")
    export(plot = wk_plot(indicator = "positivity",
                          top_n = 100,
                          label_fun = function(x) sprintf("%.1f%%", 100 * x),
                          axis_labels = scales::label_percent()),
           file = "cmp_pos_eurp.svg")
    export(plot = wk_plot(indicator = "hosp_1m", top_n = 100),
           file = "cmp_h_eurp.svg")
    export(plot = wk_plot(indicator = "em_1m"),
           file = "exd1m_eurp.svg")
    export(plot = wk_plot(indicator = "r14_cases", lower_y = 0),
           file = "cmp_i_wrld.svg")
    export(plot = wk_plot(indicator = "r14_deaths", lower_y = 0),
           file = "cmp_d_wrld.svg")
    export(plot = wk_plot(indicator = "r14_cases",
                          continents = "Europe",
                          lower_y = 0),
           file = "cmp_i_eurp.svg")
    export(plot = wk_plot(indicator = "r14_deaths",
                          continents = "Europe",
                          lower_y = 0),
           file = "cmp_d_eurp.svg")
    export(mplot(), "00_eur_map.svg")
    export(fplot(), "00_cmp.svg", w = 14.4, h = 8)
    export(tplot("BG"), "00_BG_totals.svg")
    for (n in seq_along(eu_codes)) {
        # add numbers to filenames per alphabetical order of countries
        pn <- stringr::str_pad(n, 2, pad = "0")
        export(cplot(eu_codes[n]), paste0(pn, "_", eu_codes[n], ".svg"))
    }
}