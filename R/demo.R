# plot EUROSTAT and ECDC data
# - weekly deaths / age groups / comparison map / etc. (EUROSTAT)
# - 14-day COVID-19 incidence/registered deaths (ECDC)
# - excess deaths per 1M (EUROSTAT + ECDC)
# - excess factor (EUROSTAT + ECDC)

library(magrittr)

extrafont::loadfonts(device = "win") # comment out to use default fonts
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

##### get data from EUROSTAT, ECDC
# download data, if missing
dl_missing <- function(url, local_fn, zip_local = FALSE) {
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
}
# EUROSTAT
deaths_file <- "demo_r_mwk_10.tsv.gz"
local_deaths_file <- file.path("data", deaths_file)
df_url <- paste0("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
                 "BulkDownloadListing?file=data/",
                 deaths_file)
dl_missing(df_url, local_deaths_file)

# ECDC nat'l case death
local_ncd_file <- file.path("data", "ecdc_ncd.csv.gz")
cf_url <- paste0("https://opendata.ecdc.europa.eu/covid19/",
                 "nationalcasedeath/csv")
dl_missing(cf_url, local_ncd_file, zip_local = TRUE)

# ECDC hosp
local_hosp_file <- file.path("data", "ecdc_hosp.csv.gz")
hf_url <- paste0("https://opendata.ecdc.europa.eu/covid19/",
                 "hospitalicuadmissionrates/csv/data.csv")
dl_missing(hf_url, local_hosp_file, zip_local = TRUE)

##### country config
# country codes->names, EU+ grid (Ireland deaths data missing from EUROSTAT)
cnames <- c(IS = enc("Исландия"), NO = enc("Норвегия"), SE = enc("Швеция"),
            FI = enc("Финландия"), EE = enc("Естония"),
            UK = enc("Великобритания"), DK = enc("Дания"),
            NL = enc("Нидерландия"), LV = enc("Латвия"), LT = enc("Литва"),
            DE = enc("Германия"), PL = enc("Полша"), BE = enc("Белгия"),
            CZ = enc("Чехия"), SK = enc("Словакия"), FR = enc("Франция"),
            AT = enc("Австрия"), LU = enc("Люксембург"), RS = enc("Сърбия"),
            RO = enc("Румъния"), HU = enc("Унгария"), SI = enc("Словения"),
            ES = enc("Испания"), PT = enc("Португалия"), CH = enc("Швейцария"),
            BG = enc("България"), ME = enc("Черна гора"), HR = enc("Хърватия"),
            IT = enc("Италия"), AL = enc("Албания"), EL = enc("Гърция"),
            MT = enc("Малта"), CY = enc("Кипър"))
egrid <- data.frame(row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4,
                            4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
                    col = c(1, 4, 5, 6, 7, 1, 4, 3, 7, 7, 4, 6, 3, 5, 5, 2, 4,
                            3, 6, 7, 5, 4, 2, 1, 3, 7, 6, 5, 3, 6, 7, 2, 7),
                    code = names(cnames),
                    name = cnames)
# for country factor comparison
comp_f <- c("BG", "UK", "BE", "NL", "FR", "ES", "IT", "RO")

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
d_labs <- ggplot2::labs(
    title = paste(enc("Седмична свръхсмъртност на 1 млн."),
                  enc("(сравнение ЕС+)")),
    caption = enc("данни: EUROSTAT, ECDC"),
    x = enc("седмица"),
    y =  enc("свръхсмъртност на 1 млн.")
)
v_labs <- list(
    i14d = ggplot2::labs(title = paste(enc("14-дневна COVID-19"),
                                       enc("заболеваемост на 100 хил.")),
                         caption = enc("данни: ECDC"),
                         x = enc("дата на докладване (седмица)"),
                         y =  enc("14-дневна заболеваемост на 100 хил.")),
    d14d = ggplot2::labs(title = paste(enc("14-дневна COVID-19"),
                                       enc("смъртност на 1 млн.")),
                         caption = enc("данни: ECDC"),
                         x = enc("дата на докладване (седмица)"),
                         y =  enc("14-дневна смъртност на 1 млн.")),
    hosp1m = ggplot2::labs(title = paste(enc("Брой хоспитализирани с"),
                                         enc("COVID-19 на 1 млн.")),
                           caption = enc("данни: ECDC"),
                           x = enc("дата на докладване (седмица)"),
                           y =  enc("хоспитализирани на 1 млн."))
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
    dplyr::mutate(excess_deaths_star = d_2020 - mean_deaths_star)

## prog data
codes_tab <- read.csv(file.path("prog_data", "ccodes.csv"),
                      na.strings = "")

## ECDC nat'l cases / deaths
ncd_tab <- read.csv(gzfile(local_ncd_file), na.strings = "")
names(ncd_tab)[1] = "country"
ncd_tab <- ncd_tab %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::mutate(geo_name = substr(country, 1, 13)) %>%
    tidyr::pivot_wider(names_from = "indicator",
                       values_from = c("daily_count", "rate_14_day"))
ncd_tab <- ncd_tab %>%
    dplyr::rename(
        daily_d = daily_count_deaths,
        daily_c = `daily_count_confirmed cases`,
        i14d = `rate_14_day_confirmed cases`,
        d14d = rate_14_day_deaths,
        tl_code = country_code
    )
ncd_tab <- dplyr::right_join(ncd_tab,
                             codes_tab %>% dplyr::select("tl_code", "geo"),
                             by = "tl_code")
ncd_tab <- ncd_tab %>%
    dplyr::group_by(geo) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        d14X = 1000000 * zoo::rollsum(daily_d, 14, align = "right", fill = NA) /
            population
    ) %>%
    dplyr::mutate(
        i14X = 100000 * zoo::rollsum(daily_c, 14, align = "right", fill = NA) /
            population
    ) %>%
    dplyr::ungroup() %>%
    dplyr::arrange(geo)

diff_d <- ncd_tab %>%
    dplyr::filter(d14d - d14X > 1e-10) %>% dplyr::count() %>% dplyr::pull()
diff_i <- ncd_tab %>%
    dplyr::filter(i14d - i14X > 1e-10) %>% dplyr::count() %>% dplyr::pull()
if (diff_d + diff_i != 0)
    stop(sprintf("check fail: 14d deaths (nrow): %d, 14d incid (nrow): %d",
                 diff_d,
                 diff_i))
    

# weekly COVID deaths from ECDC
cd_tab <- ncd_tab %>%
    dplyr::select("date", "daily_d", "geo", "geo_name", "population") %>%
    dplyr::filter(date >= as.Date("2020-01-01")) %>%
    dplyr::group_by(geo) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(
        s7_d = zoo::rollsum(daily_d, 7, align = "right", fill = NA)
    ) %>%
    dplyr::filter(weekdays(date, abbreviate = FALSE) == "Monday") %>%
    dplyr::mutate(week = lubridate::isoweek(date) - 1) %>%
    dplyr::rename(covid_deaths = s7_d) %>%
    dplyr::select("geo", "week", "geo_name", "covid_deaths", "population")

## EUROSTAT & ECDC data incl. factors & excess mortality per 1M
factor_tab <- dplyr::left_join(cmp_tab, cd_tab, by = c("geo", "week")) %>%
    dplyr::mutate(ed_covid_factor = excess_deaths / covid_deaths) %>%
    dplyr::mutate(ed_factor = d_2020 / mean_deaths) %>%
    dplyr::mutate(em_1m = 1000000 * excess_deaths_star / population)

## ECDC hospitalization
hosp_tab <- read.csv(gzfile(local_hosp_file))
names(hosp_tab)[1] = "country"
hosp_tab <- hosp_tab %>%
    dplyr::filter(indicator == "Daily hospital occupancy") %>%
    dplyr::select("country", "date", "year_week", "value") %>%
    dplyr::mutate(date = as.Date(date)) %>%
    dplyr::rename(hosp = value)
geo_info_from_ecdc <- ncd_tab %>%
    dplyr::select("geo", "geo_name", "country", "population") %>%
    dplyr::distinct()
hosp_tab <- dplyr::left_join(hosp_tab, geo_info_from_ecdc, by = "country") %>%
    dplyr::mutate(hosp1m = 1000000 * hosp / population)

################################################################################
# excess deaths per 1M                                                         #
# top_n -- number of countries to label (beginning at highest count)           #
################################################################################
exd_plot <- function(top_n = 30) {
    set.seed(42)
    distinct_geo <- factor_tab %>%
        dplyr::ungroup() %>%
        dplyr::select("geo") %>%
        dplyr::distinct()
    geo_count <- distinct_geo %>% dplyr::count() %>% dplyr::pull()
    pal <- c(scales::hue_pal()(geo_count))
    geos <- distinct_geo %>% dplyr::pull()
    bg_pos <- which(geos == "BG")
    pal[bg_pos] <- "black"
    bg_tab <- factor_tab %>% dplyr::filter(geo == "BG")
    last_data_pt <- factor_tab %>%
        dplyr::filter(!is.na(em_1m)) %>%
        dplyr::group_by(geo) %>%
        dplyr::arrange(week) %>%
        dplyr::slice_tail() %>%
        dplyr::ungroup()
    last_week <- function(x) {
        ret <- last_data_pt %>%
            dplyr::filter(geo == x) %>%
            dplyr::select("week") %>%
            dplyr::pull()
        return(ret)
    }
    max_pt <- factor_tab %>%
        dplyr::filter(!is.na(em_1m)) %>%
        dplyr::group_by(geo) %>%
        dplyr::filter(em_1m == max(em_1m)) %>%
        dplyr::filter(week < last_week(geo) - 6) %>%
        dplyr::ungroup()
    plt <- ggplot2::ggplot(
        data = factor_tab,
        mapping = ggplot2::aes(
            x = week,
            y = em_1m,
            color  = geo,
            fontface = ifelse(geo == "BG", "bold", "plain"),
            label = paste0(geo_name, " (", round(em_1m), ")"),
            size = ifelse(geo == "BG", "A", "B")
        )
    )
    plt <- plt +
        ggplot2::geom_line() +
        ggplot2::geom_point(data = last_data_pt, size = 2) +
        ggplot2::geom_point(size = 1) +
        ggrepel::geom_label_repel(
            data = last_data_pt %>%
                dplyr::arrange(em_1m) %>%
                dplyr::slice_tail(n = top_n),
            size = 3.6,
            nudge_x = 3,
            hjust = 0,
            direction = "y",
            point.padding = NA,
            box.padding = ggplot2::unit(1.1, units = "pt"),
            label.padding = ggplot2::unit(0.12, units = "line"),
            fill = "#FFFFFF99",
            segment.color	= "#333333",
            segment.size = 0.4,
            segment.alpha = 0.5,
            show.legend = FALSE
        ) +
        shadowtext::geom_shadowtext(
            data = max_pt %>%
                dplyr::arrange(em_1m) %>%
                dplyr::slice_tail(n = 15),
            mapping = ggplot2::aes(label = paste0(geo, enc("ᵐᵃˣ"))),
            size = 3.0,
            nudge_y = 7,
            bg.color = "#ebebeb",
            show.legend = FALSE
        ) +
        ggplot2::geom_vline(xintercept = last_bg_wk,
                            size = map_vline$size,
                            color = map_vline$col) +
        ggplot2::scale_x_continuous(
            breaks = seq(1, 53, by = 2),
            expand = ggplot2::expansion(mult = c(0.02, 0.15))
        ) +
        ggplot2::scale_color_manual(values = pal) +
        ggplot2::scale_size_manual(values = c(mthick, thin)) +
        ggplot2::guides(color = FALSE, size = FALSE) +
        d_labs +
        gtheme1
    return(plt)
}

################################################################################
# 14d country incidence/deaths comparison; arguments:                          #
# itype: "i14d" / "d14d" -- cumulative 14d incidence/deaths per 100K/1M        #
#        "hosp1m" -- hospitalized plot (continents ignored; only for Europe)   #
# continents -- regexp default "Asia|Africa|Europe|Oceania|America"            #
# top_n -- number of countries to label (beginning at highest count)           #
################################################################################
ci14_plot <- function(itype = "i14d",
                      continents = "Asia|Africa|Europe|Oceania|America",
                      top_n = 25) {
    vy <- itype
    if (itype == "hosp1m") {
        continents <- "Europe"
        ci_tab <- hosp_tab %>% dplyr::filter(date >= as.Date("2020-03-01"))
    } else {
        ci_tab <- ncd_tab %>%
            dplyr::filter(date >= as.Date("2020-03-01"),
                          stringr::str_detect(continent, continents))
    }
    distinct_geo <- ci_tab %>% dplyr::select("geo") %>% dplyr::distinct()
    geo_count <- distinct_geo %>% dplyr::count() %>% dplyr::pull()
    bg_tab <- ci_tab %>% dplyr::filter(geo == "BG")
    set.seed(42)
    plot_end_date <- tail(ci_tab$date, n = 1)
    last_data_pt <- ci_tab %>%
        dplyr::filter(!is.na(.data[[vy]])) %>%
        dplyr::group_by(geo) %>%
        dplyr::arrange(date) %>%
        dplyr::slice_tail() %>%
        dplyr::ungroup()
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    pal <- c(scales::hue_pal()(geo_count))
    geos <- distinct_geo %>% dplyr::pull()
    bg_pos <- which(geos == "BG")
    pal[bg_pos] <- "black"
    plt <- ggplot2::ggplot(data = ci_tab,
                           mapping = ggplot2::aes(x = date,
                                                  y = .data[[vy]],
                                                  color = geo)) +
        ggplot2::geom_line(size = 0.3) +
        ggplot2::geom_line(data = bg_tab, size = 0.8, color = "black") +
        ggplot2::scale_x_date(
            breaks = seq(as.Date("2020-03-08"),
                         last_sunday_inc,
                         by = "7 days"),
            limits = c(ci_tab$date[1], last_sunday_inc + 4),
            date_labels = "%d.%m. (%U)",
            expand = ggplot2::expansion(mult = c(0.02, 0.27))
        ) +
        ggplot2::scale_y_continuous(
            expand = ggplot2::expansion(mult = c(0.02, 0.02)),
            limits = c(0, NA)
        ) +
        ggplot2::guides(color = FALSE) +
        ggrepel::geom_text_repel(
            data = last_data_pt %>%
                dplyr::arrange(.data[[vy]]) %>%
                dplyr::slice_tail(n = top_n),
            mapping = ggplot2::aes(
                x = date,
                y = .data[[vy]],
                color = geo,
                fontface = ifelse(geo == "BG", "bold", "plain"),
                label = paste0(geo_name, " (", round(.data[[vy]]), ")")
            ),
            size = 3.6,
            nudge_x = 30,
            hjust = 0,
            direction = ifelse(itype == "hosp1m", "both", "y"),
            point.padding = NA,
            box.padding = ggplot2::unit(1.1, units = "pt"),
            segment.color	= "#444444",
            segment.size = 0.3,
            segment.alpha = 0.5,
            show.legend = FALSE
        ) +
        ggplot2::scale_color_manual(values = pal) +
        v_labs[itype] +
        ggplot2::labs(title = sprintf("%s (%s)",
                                      v_labs[[itype]]$title,
                                      gsub("\\|", ", ", continents))) +
        gtheme1 +
        ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
        )
    return(plt)
}

################################################################################
# factor comparison                                                            #
################################################################################
fplot <- function() {
    ptab <- factor_tab %>%
        dplyr::filter(geo %in% comp_f) %>% 
        dplyr::mutate(cname = cnames[geo])
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
        ggplot2::facet_wrap(~ cname, ncol = 2, scales = "free_y") +
        f_labs +
        gtheme1
    return(plt)
}

################################################################################
# contry by age groups plot (argument: country code, e.g. "BG", "UK", "EL")    #
################################################################################
cplot <- function(country_code) {
    if (substr(cnames[country_code], 1, 1) %in% c(enc("В"), enc("Ф")))
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
            title = paste(title_pre, cnames[country_code], txt_title2)
        ) +
        common_labs +
        gtheme1
    return(plt)
}

################################################################################
# contry totals plot (argument: country code, e.g. "BG", "UK", "EL")           #
################################################################################
tplot <- function(country_code) {
    if (substr(cnames[country_code], 1, 1) %in% c(enc("В"), enc("Ф")))
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
            title = paste(title_pre, cnames[country_code], txt_title3)
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
        dplyr::filter(geo %in% names(cnames),
                      sex == "T",
                      age == "TOTAL",
                      year >= 2015) %>%
        dplyr::mutate(cname = cnames[geo])
    plt <- ggplot2::ggplot(data = idata,
                           mapping = ggplot2::aes(
                               x = week,
                               y = deaths,
                               color = as.factor(year),
                               size = ifelse(year == 2020, "C", "N"))
                           ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(xintercept = last_bg_wk,
                            size = map_vline$size,
                            color = map_vline$col) +
        common_size_scale +
        common_color_scale +
        common_xweek_scale +
        geofacet::facet_geo(~ cname, grid = egrid, scales = "free_y") +
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
    export(ci14_plot("hosp1m", top_n = 100), "cmp_hosp_europe.svg")
    export(exd_plot(), "cmp_excd_europe.svg")
    export(ci14_plot("i14d"), "cmp_i14d_world.svg")
    export(ci14_plot("d14d"), "cmp_d14d_world.svg")
    export(ci14_plot("i14d", continents = "Europe"), "cmp_i14d_europe.svg")
    export(ci14_plot("d14d", continents = "Europe"), "cmp_d14d_europe.svg")
    export(mplot(), "00_eur_map.svg")
    export(fplot(), "00_cmp.svg", w = 14.4, h = 8)
    export(tplot("BG"), "00_BG_totals.svg")
    for (c in names(cnames)) {
        # add numbers to filenames per BG alphabetical order of countries
        pn <- stringr::str_pad(which(sort(cnames) == cnames[c]), 2, pad = "0")
        export(cplot(c), paste0(pn, "_", c, ".svg"))
    }
}