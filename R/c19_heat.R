# heatmap incidence per 100K in age bands

#' @importFrom magrittr %>%

heat_tidy <- function(atab, age_struct, first_wk, wday, wrate) {
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
        dplyr::filter(weekdays(date, abbreviate = FALSE) == wday,
                      !is.na(`0-19`))

    # divide by pop age struct
    atab[, -1] <- sweep(atab[, -1] * 100000, MARGIN = 2, age_struct, `/`)
    if (wrate) {
        atab[-1, -1] <- atab[-1, -1] / atab[-nrow(atab), -1] - 1
        atab <- atab[-1, ]
    }
    # make longer and add week number
    first_wk <- sprintf("2020-%02.f", first_wk)
    atab <- atab %>%
        tidyr::pivot_longer(cols = tidyr::matches(paste0("0|", str_all)),
                            names_to = "group",
                            values_to = "incidence") %>%
        #week_based_yr-ISO week
        dplyr::mutate(week = format(date - 7, "%G-%V")) %>%
        dplyr::filter(week >= first_wk)
    return(atab)
}

#' Plot incidence/100K heat map along age bands.
#'
#' @param wday weekday to right-align sums to ("Monday", "Tuesday", "Today",
#'             etc.)
#' @param wrate whether to plot weekly growth rates (percentages) instead of
#'              incidence
#' @param first_wk first week in 2020 to plot
#' @param country_data country data
#' @param nsi_data nsi data
#'
#' @export
#' @family plot funcs
c19_heat <- function(
    wday = "Monday",
    wrate = FALSE,
    first_wk = 1,
    country_data = c19_bg_data(),
    nsi_data = c19_nsi_data()
) {
    if (wday == "Today")
        wday <- weekdays(Sys.Date(), abbreviate = FALSE)
    if (!wday %in% weekdays(as.Date("2000-01-03") + 0:6, abbreviate = FALSE))
        stop(paste("invalid weekday:", wday))
    atab <- heat_tidy(country_data$age,
                      nsi_data$age_struct_10,
                      first_wk,
                      wday,
                      wrate)
    if (wrate) {
        lab_fun <- function(x) signif_pad(x * 100, digits = 3)
        tile_fill <- ggplot2::scale_fill_distiller(palette = "Spectral")
    } else {
        lab_fun <- function(x) signif_pad(x, digits = 3)
        tile_fill <- ggplot2::scale_fill_viridis_c()
    }
    plt <- ggplot2::ggplot(data = atab,
                           ggplot2::aes(x = week,
                                        y = group,
                                        fill = incidence,
                                        label = lab_fun(incidence))) +
        ggplot2::geom_tile() +
        ggplot2::geom_text(
            family = getOption("c19bg.font_family"),
            size = 3.1 * getOption("c19bg.font_scale"),
            angle = 90
        ) +
        ggplot2::geom_hline(yintercept = 1.5, size = 1.5) +
        tile_fill +
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
            title = ifelse(wrate,
                           tra("Sedmicen prirast (%) po vazrastovi grupi"),
                           paste(tra("Sedmicna zabolevaemost na COVID-19"),
                                 tra("(registrirani novi slucai na 100 hil.)"))
            ),
            caption = paste(tra("*dasno podravnena 7-dnevna suma"),
                            sprintf(tra("spramo dokladvanite v %s"), tra(wday)),
                            tra("na sledvasata sedmica;"),
                            tra("danni: data.egov.bg, NSI")),
            fill = ifelse(wrate, tra("prirast"), tra("c.100K")),
            x = tra("kalendarna sedmica*"),
            y = tra("grupa")
        )
    return(plt)
}

#' Save the incidence heat map.
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
    export(plot = c19_heat(wrate = TRUE),
           file = "C01_heatWrate",
           file_ext = file_ext,
           h = h,
           ...)
}
