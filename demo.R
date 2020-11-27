library(stringr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(geofacet)

library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

##### get data from EUROSTAT
deaths_file <- "demo_r_mwk_10.tsv.gz"
local_deaths_file <- file.path("data", deaths_file)
df_url <- paste0("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
                 "BulkDownloadListing?file=data/",
                 deaths_file)
if (!file.exists(local_deaths_file))
    download.file(df_url, local_deaths_file)

##### country codes->names, EU+ grid (Ireland deaths data missing from EUROSTAT)
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

##### visuals config
txt_title1 <- enc("Умирания в")
txt_v <- enc("ъв")
txt_title2 <- enc("по седмици и възрастови групи")
txt_titlei <- enc("Умирания по страни и седмици")
thin <- 0.3  # lines
thick <- 0.7
line_cols <- c("#AAAAAA", "#BBAA00", "#008800", "#0000BB", "#000000", "#FF0000")
col_legend <- guide_legend(nrow = 1,
                           override.aes = list(size = c(rep(thin, 5), thick)))
common_color_scale <- scale_color_manual(values = line_cols,
                                         guide = col_legend)
common_size_scale <- scale_size_manual(values = c(thick, thin), guide = FALSE)
common_xweek_scale <- scale_x_continuous(breaks = seq(1, 53, by = 13))
common_labs <- labs(caption = enc("данни: EUROSTAT"),
                    color = enc("година"),
                    x = enc("седмица"),
                    y =  enc("умирания"))
map_vline <- list(size = 0.2, col = "dark grey") # vline last week with BG data
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
agrp_names <- function(x) {
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
    select(matches("(201[5-9]|2020)W[0-5]|age|geo|sex")) %>%
    pivot_longer(cols = matches("20..W"),
                 names_to = c("year", "week"),
                 names_pattern = "X(....)W(..)",
                 names_transform = list(year = as.integer, week = as.integer),
                 values_to = "deaths") %>%
    mutate(deaths = as.integer(gsub("[: p]", "", deaths)),
           age = agrp_names(age)) %>%
    arrange(year, week, age)

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
# map plot                                                                     #
################################################################################
mplot <- function() {
    idata <- dtab %>%
        filter(geo %in% names(cnames),
               sex == "T",
               age == "TOTAL") %>%
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
    export <- function(plot, file) {
        ggsave(file = file, width = 11, height = 7, plot = plot)
    }
    for (c in names(cnames))
        export(cplot(c),
               # add numbers to filenames per BG alphabetical order of countries
               paste0(str_pad(which(sort(cnames) == cnames[c]), 2, pad = "0"),
                      "_", c, ".svg"))
    export(mplot(), "00_eur_map.svg")
}