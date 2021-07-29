#' @importFrom magrittr %>%

make_oblasts_vis <- function(process_data = FALSE) {
    if (!process_data) {
        vis <- list()
        vis_f <- function() {
            if (length(vis) == 0)
                vis <<- make_oblasts_vis(process_data = TRUE)
            return(vis)
        }
        return(vis_f)
    }

    font_size <- getOption("c19bg.font_size") * getOption("c19bg.font_scale")
    font_family <- getOption("c19bg.font_family")
    gtheme_count <- ggplot2::theme(
        text = ggplot2::element_text(
            size = font_size,
            family = font_family
        ),
        #panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "top",
        strip.text = ggplot2::element_text(margin =
                                               ggplot2::margin(3, 0, 3, 0)),
        plot.title = ggplot2::element_text(hjust = 0.5,
                                           face = "bold")
    )
    gtheme_i100k <- gtheme_count + ggplot2::theme(
        strip.background = ggplot2::element_rect(fill = "#a8bad2"),
        axis.title.y = ggplot2::element_text(color = "#587ba9"),
        plot.title = ggplot2::element_text(hjust = 0.5,
                                           face = "bold",
                                           color = "#587ba9"),
        panel.spacing.x = ggplot2::unit(17, "pt"),
        panel.grid.minor.y = ggplot2::element_blank()
        )
    vis <- list(
        line_sizes = c(0.5, 0.7),
        labs_count = ggplot2::labs(
            x = tra("mesec"),
            y = tra("7-dnevno sredno"),
            title = tra("Registrirani novi slucai po oblasti"),
            caption = tra("danni: data.egov.bg")
        ),
        labs_i100k = ggplot2::labs(
            x = tra("mesec"),
            y = tra("zabolevaemost na 100 hil."),
            caption = tra("danni: data.egov.bg, NSI")
        ),
        labs_no_facet = ggplot2::labs(x = tra("data na dokladvane (sedmica)")),
        theme_count = gtheme_count,
        theme_i100k = gtheme_i100k,
        font_family = font_family,
        font_size = font_size,
        font_size_lab = 4 * getOption("c19bg.font_scale")
    )
    return(vis)
}

oblasts_vis <- make_oblasts_vis()

oblasts_tidy <- function(country_data, nsi_data, days) {
    ggrid <- country_data$subdiv_grid
    pops <- nsi_data$oblasts_pops
    o_name <- function(cd) { # return human field names
        if (cd == "date") return(cd)
        cd <- sub("_ALL", "", cd)
        name <- ggrid %>%
            dplyr::filter(code == cd) %>%
            dplyr::pull(name)
        return(name)
    }

    o_pop <- function(oblast) { # return pop
        return(pops[match(oblast, ggrid$name)])
    }

    otab <- country_data$subdivs %>%
        dplyr::select(!ends_with("_ACT")) %>%
        dplyr::rename_with(~ sapply(.x, o_name)) %>%
        dplyr::mutate(date = as.Date(date)) %>%
        tidyr::pivot_longer(cols = !matches("date"),
                            names_to = "oblast",
                            values_to = "cases") %>%
        dplyr::group_by(oblast) %>%
        dplyr::mutate(mva7 = zoo::rollapply(cases,
                                            days,
                                            mean,
                                            align = "right",
                                            fill = NA)) %>%
        dplyr::mutate(i100k = 100000 * days * mva7 / as.integer(o_pop(oblast)))
    return(otab)
}


#' Plot oblast incidence (per 100K or raw), faceted (map) or lumped ("spaghetti
#' plot").
#'
#' @param incid_100k whether to plot raw incidence (case counts) or per 100K
#' @param facet whether to facet plot (individual plots on BG map)
#' @param first_facet_date starting date for facet plots
#' @param days incidence over how many days
#' @param country_data country data
#' @param nsi_data nsi data
#'
#' @export
#' @family plot funcs
c19_oblasts <- function(
    incid_100k,
    facet = TRUE,
    days = 7,
    country_data = c19_bg_data(),
    nsi_data = c19_nsi_data(),
    first_facet_date = as.Date("2020-10-01")
) {

    o_short <- function(x) { # short oblast name
        x <- sub(tra("Veliko"), tra("V."), x)
        x <- sub(tra("Stara"), tra("St."), x)
        x <- sub(tra("Sofijska oblast"), tra("Sof. obl."), x)
        return(x)
    }

    otab <- oblasts_tidy(country_data, nsi_data, days)
    vis <- oblasts_vis()
    ggrid <- country_data$subdiv_grid
    obl_colors <- scales::hue_pal()(nrow(country_data$subdiv_grid))
    big_oblasts <- c(tra("Sofia-grad"),
                     tra("Plovdiv"),
                     tra("Varna"),
                     tra("Burgas"))
    obl_colors[which(sort(ggrid$name) == tra("Sofia-grad"))] <- "black"
    obl_colors[which(sort(ggrid$name) == tra("Plovdiv"))] <- "#CC0000"
    obl_colors[which(sort(ggrid$name) == tra("Varna"))] <- "#0000CC"
    obl_colors[which(sort(ggrid$name) == tra("Burgas"))] <- "#008800"
    dummy <- data.frame(
        date = as.Date("2020-11-08"), # for fixing y lower limit
        oblast = otab %>% dplyr::distinct(oblast) %>% dplyr::pull(),
        mva7 = 0,
        cases = 0,
        i100k = 0
    )
    vy <- ifelse(incid_100k, "i100k", "mva7")
    plt <- ggplot2::ggplot(
        data = otab,
        mapping = ggplot2::aes(
            x = date,
            y = .data[[vy]],
            color = oblast,
            fontface = ifelse(oblast %in% big_oblasts, "bold", "plain"),
            label = sprintf("%s (%d)", o_short(oblast), round(.data[[vy]]))
        )
    )
    if (incid_100k) {
        plt <- plt +
            vis$labs_i100k +
            ggplot2::labs(
                title = paste(
                    sprintf(tra("%d-dnevna zabolevaemost po oblasti"), days),
                    tra("(registrirani novi slucai na 100 hil.)")
                )
            ) +
            vis$theme_i100k
        scales <- "fixed"
    } else {
        plt <- plt + vis$labs_count + vis$theme_count
        scales <- "free_y"
    }
    plt <- plt +
        ggplot2::guides(color = FALSE) +
        ggplot2::geom_line(
            mapping = ggplot2::aes(size = ifelse(!(oblast %in% big_oblasts),
                                                 ifelse(facet, "B", "A"),
                                                 "B"))
        ) +
        ggplot2::scale_size_manual(values = vis$line_sizes, guide = FALSE) +
        ggplot2::scale_color_manual(values = obl_colors)
    if (facet) {
        plt <- plt +
            ggplot2::scale_x_date(date_labels = "%m",
                                  date_breaks = "2 months",
                                  limits = c(first_facet_date, NA)) +
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
        first_mva7_date <- dates_with_mva %>%
            dplyr::slice_head() %>%
            dplyr::pull()
        plot_end_date <- utils::tail(otab$date, n = 1)
        days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
        last_sunday_inc <- plot_end_date + days_till_sunday
        plt <- plt +
            ggplot2::scale_x_date(breaks = seq(first_sunday,
                                               last_sunday_inc,
                                               by = "14 days"),
                                  limits = c(first_mva7_date,
                                             last_sunday_inc + 4),
                                  date_labels = "%d.%m. (%V)",
                                  expand = ggplot2::expansion(mult = c(0.02,
                                                                       0.19))) +
            ggrepel::geom_text_repel(
                data = otab %>%
                    dplyr::filter(date == plot_end_date),
                family = vis$font_family,
                size = vis$font_size_lab,
                nudge_x = 25,
                hjust = 0,
                direction = "y",
                point.padding = NA,
                box.padding = ggplot2::unit(0.12, units = "line"),
                max.overlaps = Inf,
                segment.color	= "dark gray",
                segment.size = 0.3,
                segment.alpha	= 0.5,
                max.time = 1,
                max.iter = 100000,
                bg.colour = "#ebebeb",
                show.legend = FALSE
            ) +
            vis$labs_no_facet +
            ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45,
                                                               hjust = 1))
    }
    return(plt)
}

#' Saves the four oblasts plots (spaghetti / faceted versions of case counts
#' and incidence per 100K).
#'
#' @param ... Passed export params: w (width), h (height), file_ext (".svg",
#'            ".png", ".jpg"; others may work as well). Rest passed to ggplot2,
#'            e.g. quality for JPEG output.
#'
#' @export
#' @examples
#' \dontrun{
#' c19_oblasts_save() # default is SVG
#' c19_oblasts_save(file_ext = ".png", dpi = 300)
#' }
#' @family output funcs
c19_oblasts_save <- function(...) {
    charts <- list(
        list(file = "C02_oblasts_i100k", i = TRUE, f = TRUE),
        list(file = "C03_oblasts_count", i = FALSE, f = TRUE),
        list(file = "C02_oblasts_i_cmp", i = TRUE, f = FALSE),
        list(file = "C03_oblasts_c_cmp", i = FALSE, f = FALSE)
    )
    for (c in charts) {
        export(file = c$file,
               ...,
               plot = c19_oblasts(incid_100k = c$i, facet = c$f))
    }
}
