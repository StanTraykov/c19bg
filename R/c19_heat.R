# heatmap incidence per 100K in age bands

#' @importFrom magrittr %>%

heat_tidy <- function(atab) {
    # from https://www.nsi.bg/bg/content/2977/%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BF%D0%BE-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8-%D1%80%D0%B0%D0%B9%D0%BE%D0%BD%D0%B8-%D0%B2%D1%8A%D0%B7%D1%80%D0%B0%D1%81%D1%82-%D0%BC%D0%B5%D1%81%D1%82%D0%BE%D0%B6%D0%B8%D0%B2%D0%B5%D0%B5%D0%BD%D0%B5-%D0%B8-%D0%BF%D0%BE%D0%BB
    # population struct 0-19, 20-29, 30-39, 40-49,
    #                   50-59, 60-69, 70-79, 80-89, 90+, total
    pop_struct <- c(1315235, 692250, 956388, 1055350,
                    953355, 938635, 701964, 301703, 36602, 6951482)
    str_all <- tra("vsicki")
    atab[, 11] <- rowSums(atab[, 2:10])
    names(atab)[11] <- str_all
    # 7-day sums; filter only mondays to get approx. real week data
    atab <- atab %>%
        dplyr::mutate(dplyr::across(!matches("date"),
                                    function(x) zoo::rollapply(x,
                                                               7,
                                                               sum,
                                                               align = "right",
                                                               fill = NA))) %>%
        dplyr::filter(weekdays(date, abbreviate = FALSE) == "Monday",
                      !is.na(`0-19`))

    # divide by pop struct
    atab[, -1] <- sweep(atab[, -1] * 100000, MARGIN = 2, pop_struct, `/`)
    # make longer and add week number
    atab <- atab %>%
        tidyr::pivot_longer(cols = tidyr::matches(paste0("0|", str_all)),
                            names_to = "group",
                            values_to = "incidence") %>%
        dplyr::mutate(week = format(date - 1, "%G-%V")) #week-based yr; ISO week
    return(atab)
}

#' Plot incidence/100K heat map along age bands.
#'
#' @param country_data country data
#'
#' @export
#' @family plot funcs
c19_heat <- function(country_data = c19_bg_data()) {
    atab <- heat_tidy(country_data$age)
    plt <- ggplot2::ggplot(data = atab,
                           ggplot2::aes(x = week,
                                        y = group,
                                        fill = incidence,
                                        label = round(incidence))) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(
            family = getOption("c19bg.font_family"),
            size = 3.6 * getOption("c19bg.font_scale")
        ) +
        ggplot2::geom_hline(yintercept = 1.5, size = 1.5) +
        ggplot2::scale_fill_viridis_c() +
        ggplot2::theme_minimal() +
        ggplot2::theme(
            text = ggplot2::element_text(
                size = getOption("c19bg.font_size") *
                    getOption("c19bg.font_scale"),
                family = getOption("c19bg.font_family")
            ),
            panel.grid = ggplot2::element_blank(),
            plot.title = ggplot2::element_text(hjust = 0.5,
                                               face = "bold"),
            axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        ) +
        ggplot2::labs(
            title = paste(tra("Sedmicna zabolevaemost na COVID-19"),
                          tra("(registrirani novi slucai na 100 hil.)")),
            caption = paste(tra("*dasno podravnena 7-dnevna suma"),
                            tra("spramo dokladvanite v ponedelnik"),
                            tra("na sledvasata sedmica;"),
                            tra("danni: data.egov.bg, NSI")),
            fill = tra("c.100K"),
            x = tra("kalendarna sedmica*"),
            y = tra("grupa")
        )
    return(plt)
}

#' Saves the incidence heat map.
#'
#' @param ... Passed export params, incl. w (width). Rest passed to ggplot2,
#'            e.g. dpi, quality for JPEG output.
#' @param file_ext file extension (e.g. ".png", ".jpg"). ".svg" gets changed
#'                 to ".png" currently as the SVG output is unsatisfactory.
#' @param h height in inches
#'
#' @export
#' @examples
#' \dontrun{
#' c19_heat_save() # default is png for screen reading
#' c19_heat_save(dpi = 300) # e.g. for print
#' c19_heat_save(file_ext = ".jpg", w = 12, h = 4.5, dpi = 125, quality = 100)
#' }
#' @family output funcs

c19_heat_save <- function(file_ext = ".png", h = 5.5, ...) {
    if (file_ext == ".svg") {
        message("c19_heat_save: changing .svg output to .png")
        file_ext <- ".png"
    }
    export(plot = c19_heat(),
           file = "C01_heat",
           file_ext = file_ext,
           h = h,
           ...)
}