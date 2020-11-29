# plot BG oblast data in a map: incidence (new cases) & incidence per 100K

library(tidyverse)
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
##### tidy data
o_name <- function(cd) { # return human field names
    if (cd == "date") return(cd)
    cd <- sub("_ALL", "", cd)
    name <- ggrid[which(ggrid$code == cd), 2]
    return(enc(name))
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
    group_by(oblast) %>%
    mutate(mva7 = rollapply(cases, 7, mean, align = "right", fill = NA)) %>%
    mutate(i100k = 100000 * 7 * mva7 / as.integer(o_pop(oblast)))

dummy <- data.frame(date = as.Date("2020-11-08"), # for fixing y lower limit
                    oblast = otab %>% distinct(oblast) %>% pull(),
                    mva7 = 0,
                    cases = 0,
                    i100k = 0)

################################################################################
# plot (boolean argument: whether to plot incidence/100K instead of raw incid. #
################################################################################
oblasts_plot <- function(incid_100k) {
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
        geom_line() +
        scale_x_date(date_labels = "%m", date_breaks = "1 month") +
        geom_blank(data = dummy) +
        facet_geo(~ oblast, grid = ggrid, scales = scales)
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
}