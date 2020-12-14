# plot BG oblast data in a map: incidence (new cases) & incidence per 100K

library(magrittr)

extrafont::loadfonts(device = "win") # comment out to use default fonts
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows
# enc <- function(x) x

source(file.path("R", "bg_opendata.R")) # sets gen_data, age_data, obl_data

# NSI 2019 data
pops <- c("82835", "236305", "171809", "215477", "108018", "172262", "159470",
          "232568", "127001", "110789", "469885", "110914", "106598", "226671",
          "122546", "1328790", "409265", "313396", "119190", "184119", "117335",
          "252776", "225317", "666801", "116915", "158204", "103532", "302694")

##### visuals config
line_sizes <- c(0.5, 0.7)
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
obl_colors <- scales::hue_pal()(length(pops))
obl_colors[which(sort(ggrid$name) == enc("София-град"))] <- "black"
obl_colors[which(sort(ggrid$name) == enc("Пловдив"))] <- "#CC0000"
obl_colors[which(sort(ggrid$name) == enc("Варна"))] <- "#0000CC"
obl_colors[which(sort(ggrid$name) == enc("Бургас"))] <- "#008800"
big_oblasts <- c(enc("София-град"),
                 enc("Пловдив"),
                 enc("Варна"),
                 enc("Бургас"))
gtheme_count <- ggplot2::theme(text = ggplot2::element_text(size = 14,
                                                   family =
                                             grDevices::windowsFont("Calibri")),
                               panel.grid.minor.x = ggplot2::element_blank(),
                               legend.position = "top",
                               strip.text = ggplot2::element_text(margin =
                                                   ggplot2::margin(3, 0, 3, 0)),
                               plot.title = ggplot2::element_text(hjust = 0.5,
                                                                 face = "bold"))
gtheme_i100k <- gtheme_count +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "#a8bad2"),
                   axis.title.y = ggplot2::element_text(color = "#587ba9"),
                   plot.title = ggplot2::element_text(hjust = 0.5,
                                                      face = "bold",
                                                      color = "#587ba9"),
                   panel.spacing.x = ggplot2::unit(17, "pt"),
                   panel.grid.minor.y = ggplot2::element_blank())
labs_count <- ggplot2::labs(x = enc("месец"),
                            y = enc("7-дневно средно"),
                            title = enc("Регистрирани нови случаи по области"),
                            caption = enc("данни: data.egov.bg"))
labs_i100k <- ggplot2::labs(x = enc("месец"),
                            y = enc("заболеваемост на 100 хил."),
                            title = paste(
                                enc("7-дневна заболеваемост по области"),
                                enc("(регистрирани нови случаи на 100 хил.)")
                            ),
                            caption = enc("данни: data.egov.bg, НСИ"))
labs_no_facet <- ggplot2::labs(x = enc("дата на докладване (седмица)"))
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
    dplyr::select(!ends_with("_ACT")) %>%
    dplyr::rename_with(~ sapply(.x, o_name)) %>%
    dplyr::mutate(date = as.Date(date)) %>%
    tidyr::pivot_longer(cols = !matches("date"),
                        names_to = "oblast",
                        values_to = "cases") %>%
    dplyr::group_by(oblast) %>%
    dplyr::mutate(mva7 = zoo::rollapply(cases,
                                        7,
                                        mean,
                                        align = "right",
                                        fill = NA)) %>%
    dplyr::mutate(i100k = 100000 * 7 * mva7 / as.integer(o_pop(oblast)))

dummy <- data.frame(date = as.Date("2020-11-08"), # for fixing y lower limit
                    oblast = otab %>% dplyr::distinct(oblast) %>% dplyr::pull(),
                    mva7 = 0,
                    cases = 0,
                    i100k = 0)

################################################################################
# oblast incidence plot; arguments:                                            #
# - incid_100k: whether to plot incidence/100K instead of raw incid (def FALSE)#
# - facet: whether to geo split into small charts (default TRUE)               #
################################################################################
oblasts_plot <- function(incid_100k, facet = TRUE) {
    vy <- ifelse(incid_100k, "i100k", "mva7")
    plt <- ggplot2::ggplot(data = otab,
                           mapping = ggplot2::aes(
                               x = date,
                               y = .data[[vy]],
                               color = oblast,
                               fontface = ifelse(oblast %in%
                                                     big_oblasts,
                                                 "bold",
                                                 "plain"),
                               label = sprintf("%s (%d)",
                                               o_short(oblast),
                                               round(.data[[vy]]))
                           ))
    if (incid_100k) {
        plt <- plt + labs_i100k + gtheme_i100k
        scales <- "fixed"
    } else {
        plt <- plt + labs_count + gtheme_count
        scales <- "free_y"
    }
    plt <- plt +
        ggplot2::guides(color = FALSE) +
        ggplot2::geom_line(mapping =
                    ggplot2::aes(size = ifelse(!(oblast %in% big_oblasts),
                                               ifelse(facet, "B", "A"),
                                               "B"))) +
        ggplot2::scale_size_manual(values = line_sizes, guide = FALSE) +
        ggplot2::scale_color_manual(values = obl_colors)
    if (facet) {
        plt <- plt +
            ggplot2::scale_x_date(date_labels = "%m", date_breaks = "1 month") +
            ggplot2::geom_blank(data = dummy) +
            geofacet::facet_geo(~ oblast, grid = ggrid, scales = scales)
    } else {# no facet
        set.seed(42)
        dates_with_mva <- otab %>%
            dplyr::ungroup() %>%
            dplyr::filter(!is.na(mva7)) %>%
            dplyr::arrange(date) %>%
            dplyr::select("date")
        first_sunday <- dates_with_mva %>%
            dplyr::filter(weekdays(date) == "Sunday") %>%
            dplyr::slice_head() %>%
            dplyr::pull()
        first_mva7 <- dates_with_mva %>%
            dplyr::slice_head() %>%
            dplyr::pull()
        plot_end_date <- tail(otab$date, n = 1)
        days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
        last_sunday_inc <- plot_end_date + days_till_sunday
        plt <- plt +
            ggplot2::scale_x_date(breaks = seq(first_sunday,
                                               last_sunday_inc,
                                               by = "7 days"),
                                  limits = c(first_mva7, last_sunday_inc + 4),
                                  date_labels = "%d.%m. (%U)",
                                  expand = ggplot2::expansion(mult = c(0.02,
                                                                       0.26))) +
            ggrepel::geom_text_repel(data = otab %>%
                                dplyr::filter(date == plot_end_date),
                            size = 3.6,
                            nudge_x = 20,
                            hjust = 0,
                            direction = "y",
                            point.padding = NA,
                            box.padding = ggplot2::unit(0.6, units = "pt"),
                            segment.color	= "dark gray",
                            segment.size = 0.2,
                            segment.alpha	= 0.5,
                            show.legend = FALSE) +
            labs_no_facet +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                               hjust = 1))
    }
    return(plt)
}

################################################################################
# example output                                                               #
################################################################################
save_all <- function() {
    w <- 11; h <- 7
    ggplot2::ggsave(file = "oblasts_i100k.svg",
                    width = w, height = h,
                    plot = oblasts_plot(incid_100k = TRUE))
    ggplot2::ggsave(file = "oblasts_count.svg",
                    width = w, height = h,
                    plot = oblasts_plot(incid_100k = FALSE))
    ggplot2::ggsave(file = "oblasts_i_cmp.svg",
                    width = w, height = h,
                    plot = oblasts_plot(incid_100k = TRUE, facet = FALSE))
    ggplot2::ggsave(file = "oblasts_c_cmp.svg",
                    width = w, height = h,
                    plot = oblasts_plot(incid_100k = FALSE, facet = FALSE))
}