# plot BG oblast data in a map: incidence (new cases) & incidence per 100K

library(tidyverse)
library(ggrepel)
library(zoo)
library(geofacet)
library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

# download @ https://data.egov.bg/data/view/492e8186-0d00-43fb-8f5e-f2b0b183b64f
obl_data <- file.path("data", enc("Разпределение по дата и по области.csv"))
# NSI 2019 data
pops <- c("82835", "236305", "171809", "215477", "108018", "172262", "159470",
          "232568", "127001", "110789", "469885", "110914", "106598", "226671",
          "122546", "1328790", "409265", "313396", "119190", "184119", "117335",
          "252776", "225317", "666801", "116915", "158204", "103532", "302694")

##### visuals config
ggrid <- data.frame( # bg oblast grid from geofacet pkg; modified codes/names
    code = c("VID", "PVN", "DOB", "RSE", "SLS", "SHU",
             "VRC", "VTR", "MON", "RAZ", "VAR", "TGV", "GAB", "SFO", "LOV",
             "SOF", "BGS", "SZR", "PER", "SLV", "JAM", "PAZ", "HKV", "PDV",
             "KNL", "KRZ", "SML", "BLG"),
    name = c(enc("Видин"), enc("Плевен"), enc("Добрич"), enc("Русе"),
             enc("Силистра"), enc("Шумен"), enc("Враца"),
             enc("Велико Търново"), enc("Монтана"), enc("Разград"),
             enc("Варна"), enc("Търговище"), enc("Габрово"),
             enc("Софийска област"), enc("Ловеч"), enc("София-град"),
             enc("Бургас"), enc("Стара Загора"), enc("Перник"),
             enc("Сливен"), enc("Ямбол"), enc("Пазарджик"),
             enc("Хасково"), enc("Пловдив"), enc("Кюстендил"),
             enc("Кърджали"), enc("Смолян"), enc("Благоевград")),
    row = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
            4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L),
    col = c(1L, 3L, 6L, 4L, 5L, 5L, 2L, 3L, 1L, 4L, 5L, 4L, 3L, 1L, 2L, 2L, 5L,
            3L, 1L, 4L, 5L, 2L, 4L, 3L, 1L, 4L, 3L, 2L))
gtheme_count <- theme(text = element_text(size = 14,
                                          family = windowsFont("Calibri")),
                      panel.grid.minor.x = element_blank(),
                      legend.position = "top",
                      strip.text = element_text(margin = margin(3, 0, 3, 0)),
                      plot.title = element_text(hjust = 0.5,
                                                face = "bold"))
gtheme_i100k <- gtheme_count +
    theme(strip.background = element_rect(fill = "#a8bad2"),
          axis.title.y = element_text(color = "#587ba9"),
          plot.title = element_text(hjust = 0.5,
                                    face = "bold",
                                    color = "#587ba9"),
          panel.spacing.x = unit(17, "pt"),
          panel.grid.minor.y = element_blank())
labs_count <- labs(x = enc("месец"),
                   y = enc("7-дневно средно"),
                   title = enc("Регистрирани нови случаи по области"),
                   caption = enc("данни: data.egov.bg"))
labs_i100k <- labs(x = enc("месец"),
                   y = enc("заболеваемост на 100 хил."),
                   title = paste(enc("7-дневна заболеваемост по области"),
                                 enc("(регистрирани нови случаи на 100 хил.)")),
                   caption = enc("данни: data.egov.bg, НСИ"))
labs_no_facet <- labs(x = enc("дата на докладване (седмица)"))
##### tidy data
o_name <- function(cd) { # return human field names
    if (cd == "date") return(cd)
    cd <- sub("_ALL", "", cd)
    name <- ggrid[which(ggrid$code == cd), 2]
    return(enc(name))
}

o_short <- function(x) { # short oblast name
    x <- sub(enc("Велико"), enc("В."), x)
    x <- sub(enc("област"), enc("обл."), x)
    return(x)
}

o_pop <- function(oblast) { # return pop
    return(pops[match(oblast, ggrid$name)])
}

otab <- read.csv(file = obl_data)
otab[-1, -1] <- otab[-1, -1] - otab[-nrow(otab), -1] # repl. totals w/incidence
otab <- otab[-1, ]
names(otab)[1] <- "date"
otab <- otab %>%
    select(!ends_with("_ACT")) %>%
    rename_with(~ sapply(.x, o_name)) %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(cols = !matches("date"),
                 names_to = "oblast",
                 values_to = "cases") %>%
    mutate(oblast_short = o_short(oblast)) %>%
    group_by(oblast) %>%
    mutate(mva7 = rollapply(cases, 7, mean, align = "right", fill = NA)) %>%
    mutate(i100k = 100000 * 7 * mva7 / as.integer(o_pop(oblast)))

dummy <- data.frame(date = as.Date("2020-11-08"), # for fixing y lower limit
                    oblast = otab %>% distinct(oblast) %>% pull(),
                    mva7 = 0,
                    cases = 0,
                    i100k = 0)

################################################################################
# oblast incidence plot; arguments:                                            #
# - incid_100k: whether to plot incidence/100K instead of raw incid (def FALSE)#
# - facet: whether to geo split into small charts (default TRUE)               #
################################################################################
oblasts_plot <- function(incid_100k, facet = TRUE) {
    if (!(facet || incid_100k))
        stop("can't facet count")
    if (incid_100k) {
        plt <- ggplot(data = otab,
                      mapping = aes(x = date, y = i100k, color = oblast)) +
            labs_i100k +
            gtheme_i100k
        scales <- "fixed"
    } else {
        plt <- ggplot(data = otab,
                      mapping = aes(x = date, y = mva7, color = oblast)) +
            labs_count +
            gtheme_count
        scales <- "free_y"
    }
    plt <- plt +
        guides(color = FALSE) +
        geom_line()
    if (facet) {
        plt <- plt +
            scale_x_date(date_labels = "%m", date_breaks = "1 month") +
            geom_blank(data = dummy) +
            facet_geo(~ oblast, grid = ggrid, scales = scales)
    } else {# no facet
        set.seed(42)
        dates_with_mva <- otab %>%
            ungroup() %>%
            filter(!is.na(mva7)) %>%
            arrange(date) %>%
            select("date")
        first_sunday <- dates_with_mva %>%
            filter(weekdays(date) == "Sunday") %>% slice_head() %>% pull()
        first_mva7 <- dates_with_mva %>% slice_head() %>% pull()
        plot_end_date <- tail(otab$date, n = 1)
        days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
        last_sunday_inc <- plot_end_date + days_till_sunday
        plt <- plt +
            scale_x_date(breaks = seq(first_sunday,
                                      last_sunday_inc,
                                      by = "7 days"),
                         limits = c(first_mva7, last_sunday_inc + 4),
                         date_labels = "%d.%m. (%U)",
                         expand = expansion(mult = c(0.02, 0.26))) +
            geom_text_repel(data = otab %>%
                                filter(date == plot_end_date),
                      mapping = aes(x = date,
                                    y = i100k,
                                    color = oblast,
                                    label = paste0(oblast_short,
                                                   " (",
                                                   round(i100k),
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
            labs_no_facet +
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
            #theme(axis.text.x = element_text(angle = 90, vjust = 0.5))
    }
    return(plt)
}

################################################################################
# example output                                                               #
################################################################################
save_all <- function() {
    w <- 11; h <- 7
    ggsave(file = "oblasts_i100k.svg",
                  width = w, height = h,
                  plot = oblasts_plot(incid_100k = TRUE))
    ggsave(file = "oblasts_count.svg",
                  width = w, height = h,
                  plot = oblasts_plot(incid_100k = FALSE))
    ggsave(file = "oblasts_i_cmp.svg",
           width = w, height = h,
           plot = oblasts_plot(incid_100k = TRUE, facet = FALSE))
}