# plot weekly deaths data from EUROSTAT: per country and overview map

library(scales)
library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geofacet)
library(ggrepel)
library(zoo)
library(shadowtext)
library(R.utils)

library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
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
            gzip(down_dest,
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
# ECDC
local_covid_file <- file.path("data", "ecdc_covid.csv.gz")
cf_url <- paste0("https://opendata.ecdc.europa.eu/covid19/casedistribution/",
                 "csv/data.csv")
dl_missing(cf_url, local_covid_file, zip_local = TRUE)

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
comp_f = c("BG", "UK", "BE", "NL", "FR", "ES", "IT", "RO")

##### visuals config
txt_title1 <- enc("Умирания в")
txt_v <- enc("ъв")
txt_title2 <- enc("по седмици и възрастови групи")
txt_title3 <- enc("по седмици")
txt_titlei <- enc("Умирания по страни и седмици")
txt_titlec <- paste(enc("Фактори на надвишаване (свръхсмъртност /"),
                    enc("PCR-доказана смъртност)"))
f_col <- c(enc("средно 2015-2019 г."),
           enc("2020 г. без PCR-доказани смъртни случаи"),
           enc("2020 г."))
thin <- 0.3  # lines
thick <- 0.7
wthin <- 0.6 # for whole page plots
wthick <- 1.1
f_color_scale <- scale_color_manual(values = c("dark gray",
                                               "red",
                                               "dark magenta"))
line_cols <- c("#AAAAAA", "#BBAA00", "#008800", "#0000BB", "#000000", "#FF0000")
col_legend <- guide_legend(nrow = 1,
                           override.aes = list(size = c(rep(thin, 5), thick)))
common_color_scale <- scale_color_manual(values = line_cols,
                                         guide = col_legend)
common_size_scale <- scale_size_manual(values = c(thick, thin), guide = FALSE)
w_size_scale <- scale_size_manual(values = c(wthick, wthin), guide = FALSE)
common_xweek_scale <- scale_x_continuous(breaks = seq(1, 53, by = 13))
common_labs <- labs(caption = enc("данни: EUROSTAT"),
                    color = enc("година"),
                    x = enc("седмица"),
                    y =  enc("умирания"))
f_labs <- labs(caption = enc("данни: EUROSTAT, ECDC"),
               color = enc("умирания"),
               x = enc("седмица"),
               y =  enc("умирания"))
v_labs <- list(i14d = labs(title = paste(enc("14-дневна COVID-19"),
                                         enc("заболеваемост на 100 хил.")),
                           caption = enc("данни: ECDC"),
                           x = enc("дата на докладване (седмица)"),
                           y =  enc("14-дневна заболеваемост на 100 хил.")),
               d14d = labs(title = paste(enc("14-дневна COVID-19"),
                                         enc("смъртност на 1 млн.")),
                           caption = enc("данни: ECDC"),
                           x = enc("дата на докладване (седмица)"),
                           y =  enc("14-дневна смъртност на 1 млн.")))
map_vline <- list(size = 0.2, col = "dark grey") # vline last week with BG data
ext_line_cols <- c("#BBBBBB", "#BBBBBB", "#BBBBBB", "#BBBBBB", "#777777",
                   "#88AAAA", "#BBAA00", "#008800", "#0000BB", "#000000",
                   "#FF0000") # for country totals plot
ext_ls <- c("solid", "solid", "dotted", "solid", "solid", "solid",
            "solid", "solid", "solid", "solid", "solid") # for country totals
ext_guide <-  guide_legend(nrow = 1,
                           override.aes = list(linetype = ext_ls,
                                               shape = c(rep(NA, 10), 19)))
ext_color_scale <- scale_color_manual(values = ext_line_cols, guide = ext_guide)
ext_ltypes <- scale_linetype_manual(values = ext_ls, guide = FALSE)
gtheme1 <- theme(text = element_text(size = 14,
                                     family = windowsFont("Calibri")),
                panel.grid.minor.x = element_blank(),
                legend.position = "top",
                plot.title = element_text(hjust = 0.5,
                                          face = "bold"))
gtheme2 <- gtheme1 +
    theme(axis.text = element_text(size = 10),
          panel.margin.y = unit(3, "pt"),
          panel.margin.x = unit(4, "pt"),
          strip.text = element_text(size = 10,
                                    margin = margin(1, 0, 1, 0)))

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
dtab <- cbind(str_split_fixed(dtab[[1]], ",", 4), dtab[, -1])
names(dtab) <- c("age", "sex", "unit", "geo", names(dtab)[-(1:4)])
dtab <- dtab %>%
    select(matches("(201[0-9]|2020)W[0-5]|age|geo|sex")) %>%
    pivot_longer(cols = matches("20..W"),
                 names_to = c("year", "week"),
                 names_pattern = "X(....)W(..)",
                 names_transform = list(year = as.integer, week = as.integer),
                 values_to = "deaths") %>%
    mutate(deaths = as.integer(gsub("[: p]", "", deaths)),
           age = agrp_names(age)) %>%
    arrange(year, week, age)

mean_tab <- dtab %>% # mean 2015-2019
    filter(sex == "T",
           geo %in% comp_f,
           year >= 2015,
           year <= 2019,
           age == "TOTAL") %>%
    group_by(geo, week) %>%
    summarize(mean_deaths = mean(deaths))
tt_tab <- dtab %>% # 2020
    filter(sex == "T",
           geo %in% comp_f,
           year == 2020,
           age == "TOTAL") %>%
    select("geo", "week", "deaths") %>%
    rename(d_2020 = deaths)
cmp_tab <- left_join(mean_tab, tt_tab, by = c("geo", "week")) %>%
    mutate(excess_deaths = d_2020 - mean_deaths)

## ECDC
ecdc_tab <- read.csv(gzfile(local_covid_file)) %>%
    rename(date = dateRep,
           geo = geoId,
           daily_d = deaths,
           geo_name = countriesAndTerritories,
           i14d =
               Cumulative_number_for_14_days_of_COVID.19_cases_per_100000) %>%
    mutate(date = as.Date(date, format = "%d/%m/%Y")) %>%
    mutate(geo_name = gsub("_", " ", substr(enc(geo_name), 1, 13))) %>%
    group_by(geo) %>%
    arrange(date) %>%
    mutate(d14d = 1000000 * rollsum(daily_d, 14, align = "right", fill = NA) /
                    popData2019) %>%
    mutate(i14X = 100000 * rollsum(cases, 14, align = "right", fill = NA) /
                    popData2019) %>%
    ungroup() %>%
    arrange(geo)
cd_tab <- ecdc_tab %>% filter(geo %in% comp_f) %>%
    select("date", "daily_d", "geo") %>%
    filter(date >= as.Date("2020-01-01")) %>%
    group_by(geo) %>%
    arrange(date) %>%
    mutate(s7_d = rollsum(daily_d, 7, align = "right", fill = NA)) %>%
    filter(weekdays(date) == "Monday") %>%
    mutate(week = lubridate::isoweek(date) - 1) %>%
    rename(covid_deaths = s7_d) %>%
    select("geo", "week", "covid_deaths")

# factor
factor_tab <- left_join(cmp_tab, cd_tab, by = c("geo", "week")) %>%
    mutate(ed_covid_factor = excess_deaths / covid_deaths) %>%
    mutate(ed_factor = d_2020 / mean_deaths)

################################################################################
# 14d country incidence/deaths comparison                                      #
# argument: "i14d" or "d14d" -- cumulative 14d incidence/deaths per 100K/1M    #
################################################################################
ci14_plot <- function(itype = "i14d",
                      continent = "Asia|Africa|Europe|Oceania|America",
                      top_n = 25) {
    vy <- itype
    ci_tab <- ecdc_tab %>%
        filter(date >= as.Date("2020-03-01"),
               str_detect(continentExp, continent))
    distinct_geo <- ci_tab %>% select("geo") %>% distinct()
    geo_count <- distinct_geo %>% count() %>% pull()
    bg_tab <- ci_tab %>% filter(geo == "BG")
    set.seed(42)
    first_sunday <- ci_tab %>%
        select("date") %>%
        filter(weekdays(date) == "Sunday") %>%
        slice_head() %>%
        pull()
    plot_end_date <- tail(ci_tab$date, n = 1)
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    pal <- c(hue_pal()(geo_count))
    geos <- distinct_geo %>% pull()
    bg_pos <- which(geos == "BG")
    pal[bg_pos] = "black"
    plt <- ggplot(data = ci_tab,
                  mapping = aes(x = date, y = .data[[vy]], color = geo)) +
        geom_line(size = 0.3) +
        geom_line(data = bg_tab, size = 1.1, color = "black") +
        scale_x_date(breaks = seq(first_sunday,
                                  last_sunday_inc,
                                  by = "7 days"),
                     limits = c(ci_tab$date[1], last_sunday_inc + 4),
                     date_labels = "%d.%m. (%U)",
                     expand = expansion(mult = c(0.02, 0.24))) +
        scale_y_continuous(expand = expansion(mult = c(0.02, 0.02)),
                           limits = c(0, NA)) +
        guides(color = FALSE) +
        geom_text_repel(data = ci_tab %>%
                            filter(date == plot_end_date) %>%
                            arrange(.data[[vy]]) %>%
                            slice_tail(n = top_n),
                        mapping = aes(x = date,
                                      y = .data[[vy]],
                                      color = geo,
                                      fontface = ifelse(geo == "BG",
                                                        "bold",
                                                        "plain"),
                                      label = paste0(geo_name,
                                                     " (",
                                                     round(.data[[vy]]),
                                                     ")")),
                        size = 3.6,
                        nudge_x = 18,
                        hjust = 0,
                        direction = "y",
                        point.padding = NA,
                        box.padding = unit(1.1, units = "pt"),
                        segment.color	= "dark gray",
                        segment.size = 0.2,
                        segment.alpha	= 0.5,
                        show.legend = FALSE) +
        scale_color_manual(values = pal) +
        v_labs[itype] +
        labs(title = paste(v_labs[[itype]]$title,
                           sprintf("(%s)", gsub("\\|", ", ", continent)))) +
        gtheme1 +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plt)
}
################################################################################
# factor comparison                                                            #
################################################################################
fplot <- function() {
    ptab <- factor_tab %>%
        mutate(cname = cnames[geo])
    ftab <- ptab %>%
        filter(ed_factor > 1.2)
    plt <- ggplot(data = ptab,
                  mapping = aes(x = week)) +
        geom_ribbon(mapping = aes(ymin = mean_deaths,
                                  ymax = d_2020 - covid_deaths),
                    fill = "#99000044") +
        geom_line(mapping = aes(y = mean_deaths,
                                color = f_col[1])) +
        geom_line(mapping = aes(y = d_2020 - covid_deaths,
                                color = f_col[2])) +
        geom_line(mapping = aes(y = d_2020, color = "2020")) +
        geom_shadowtext(data = ftab,
                        mapping = aes(label = round(ed_covid_factor, 2),
                                      y = d_2020 - covid_deaths),
                        angle = 90,
                        size = 3.5,
                        color = "black",
                        bg.color = "white") +
        f_color_scale +
        common_xweek_scale +
        facet_wrap(~ cname, ncol = 2, scales = "free_y") +
        labs(title = txt_titlec) +
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
    cdata <- dtab %>% filter(geo == country_code,
                             sex == "T",
                             year >= 2015,
                             #str_detect(age, "([34567]|80\\+)"))
                             str_detect(age, "([1234567]|80-89|90|00)"))
    plt <- ggplot(data = cdata,
                  mapping = aes(x = week,
                                y = deaths,
                                color = as.factor(year),
                                size = ifelse(year == 2020, "C", "N"))) +
        geom_line() +
        common_size_scale +
        common_color_scale +
        common_xweek_scale +
        facet_wrap(~ age, nrow = 2) +
        labs(title = paste(title_pre, cnames[country_code], txt_title2)) +
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
    cdata <- dtab %>% filter(geo == country_code,
                             sex == "T",
                             age == "TOTAL")
    plt <- ggplot(data = cdata,
                  mapping = aes(x = week,
                                y = deaths,
                                color = as.factor(year),
                                linetype = as.factor(year),
                                size = ifelse(year == 2020, "C", "N"))) +
        geom_line() +
        geom_point(data = cdata %>% filter(year == 2020), size = 2.7) +
        ext_ltypes +
        ext_color_scale +
        w_size_scale +
        common_xweek_scale +
        labs(title = paste(title_pre, cnames[country_code], txt_title3)) +
        common_labs +
        gtheme1
    return(plt)
}

################################################################################
# map plot                                                                     #
################################################################################
mplot <- function() {
    idata <- dtab %>%
        filter(geo %in% names(cnames),
               sex == "T",
               age == "TOTAL",
               year >= 2015) %>%
        mutate(cname = cnames[geo])
    last_bg_wk <- idata %>%
        filter(geo == "BG", year == 2020, deaths > 0) %>%
        summarize(max(week)) %>%
        pull()
    plt <- ggplot(data = idata,
                  mapping = aes(x = week,
                                y = deaths,
                                color = as.factor(year),
                                size = ifelse(year == 2020, "C", "N"))) +
        geom_line() +
        geom_vline(xintercept = last_bg_wk,
                   size = map_vline$size,
                   color = map_vline$col) +
        common_size_scale +
        common_color_scale +
        common_xweek_scale +
        facet_geo(~ cname, grid = egrid, scales = "free_y") +
        labs(title = txt_titlei) +
        common_labs +
        gtheme2
    return(plt)
}

################################################################################
# example output                                                               #
################################################################################
save_all <- function() {
    export <- function(plot, file, w = 11, h = 7) {
        ggsave(file = file, width = w, height = h, plot = plot)
    }
    export(ci14_plot("i14d"), "cmp_i14d_world.svg")
    export(ci14_plot("d14d"), "cmp_d14d_world.svg")
    export(ci14_plot("i14d", continent = "Europe"), "cmp_i14d_europe.svg")
    export(ci14_plot("d14d", continent = "Europe"), "cmp_d14d_europe.svg")
    export(mplot(), "00_eur_map.svg")
    export(fplot(), "00_cmp.svg", w = 14.4, h = 8)
    export(tplot("BG"), "00_BG_totals.svg")
    for (c in names(cnames))
        export(cplot(c),
               # add numbers to filenames per BG alphabetical order of countries
               paste0(str_pad(which(sort(cnames) == cnames[c]), 2, pad = "0"),
                      "_", c, ".svg"))
}