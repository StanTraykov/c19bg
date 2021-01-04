#' @importFrom magrittr %>%

make_r_plot_vis <- function(process_data = FALSE) {
    if (!process_data) {
        vis <- list()
        vis_f <- function() {
            if (length(vis) == 0)
                vis <<- make_r_plot_vis(process_data = TRUE)
            return(vis)
        }
        return(vis_f)
    }

    line_sz <- 0.5
    lty_norm <- "solid"
    lty_mva <- "dotted"
    clr_leg <- list(linetype = c(lty_norm, lty_norm, lty_mva, lty_norm),
                    size = c(line_sz, line_sz, line_sz, line_sz))
    clr <- list(cri = "#0082df66",
                cri_txt = "#0082df99",
                reg_c = "#77777777",
                reg_s = "#44444499",
                r_med = "#0070c0",
                tst = "#00b050",
                pos = "red 3")
    font_size <- getOption("c19bg.font_size") * getOption("c19bg.font_scale")
    font_family <- getOption("c19bg.font_family")
    plot_theme <- ggplot2::theme(
        text = ggplot2::element_text(
            size = font_size,
            family = font_family
        ),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.major.x = ggplot2::element_blank(),
        legend.position = "top",
        legend.title = ggplot2::element_blank(),
        legend.spacing = ggplot2::unit(0, units = "pt"),
        legend.margin = ggplot2::margin(0, 0, 0, 0),
        plot.title = ggplot2::element_text(hjust = 0.5,
                                           face = "bold"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5,
                                              size = font_size * 11.3 / 14),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        axis.text.y.right = ggplot2::element_text(size = font_size)
    )
    plot_labels <- ggplot2::labs(
        title = paste(tra("Ocenka na reproduktivno cislo Rt"),
                      tra("(time-varying instantaneous)"),
                      tra("za Balgaria")),
        subtitle = paste(tra("razpredelenie generacionni vremena po"),
                         tra("Ferretti et al. (sredno 5.5, otcetena"),
                         tra("nesigurnost 4-7); 7-dnevni prozorci, CrI"),
                         tra("po Cori et al. (vz. github uiki)")),
        caption = tra("izh. danni: data.egov.bg, NOS"),
        x = tra("data na dokladvane (sedmica)")
    )
    clr_labels <- c(tra("testove (7 dni)"),
                    tra("pozitivnost (7 dni)"),
                    tra("reg. slucai (sr. 7 dni)"),
                    tra("Rt mediana"))
    fill_labels <- c(tra("95% CrI"),
                     tra("reg. slucai"),
                     tra("reg. sl. (ned.)"))
    vis <- list(
        skip_to = 20, # do not include first few days in scaling calc's
        tick_choice = c(10, 15, 20, 25, 50, 75) *
            rep(c(1, 10, 100, 1000), each = 6),
        tst_choice = c(1, 2, 5) * rep(c(1, 10, 100, 1000), each = 3),
        line_sz = line_sz,
        fill_leg = list(alpha = c(0x66, 0x77, 0x99) / (0xFF * 1.5)),
        lty_norm = lty_norm,
        lty_mva = lty_mva,
        lty_norm = lty_norm,
        lty_mva = lty_mva,
        clr_leg = clr_leg,
        clr = clr,
        theme = plot_theme,
        labels = plot_labels,
        lab_y = tra("registrirani slucai, testove (*%d)"),
        clr_labels = clr_labels,
        fill_labels = fill_labels,
        sec_axis_name = tra("reproduktivno cislo, pozitivnost"),
        font_family = font_family,
        font_size = font_size,
        font_size_R = 3.7 * getOption("c19bg.font_scale"),
        font_size_poslab = 2.7 * getOption("c19bg.font_scale")
    )
    return(vis)
}

r_plot_vis <- make_r_plot_vis()

r_plot_tidy <- function(country_data) {
    estr_csv <- "estR.csv"
    if (!datafile_exists(estr_csv))
        stop("can't plot R without R estimate; run c19_estimate_r() first")
    rtab <- tib_read_csv(
        file = estr_csv,
        col_types = paste0("ii", strrep("d", 9))
    )
    rtab <- rbind(NA, NA, NA, NA, NA, NA, rtab, NA)
    ftab <- country_data$gen_inc_hist %>%
        dplyr::mutate(s7_nc = zoo::rollsum(new_cases,
                                           7,
                                           align = "right",
                                           fill = NA)) %>%
        dplyr::mutate(s7_nt = zoo::rollsum(new_tests,
                                           7,
                                           align = "right",
                                           fill = NA)) %>%
        dplyr::mutate(posit7 = (s7_nc / s7_nt)) %>%
        dplyr::mutate(rollmean7 = s7_nc / 7) %>%
        dplyr::mutate(is_sun = ifelse(weekdays(date,
                                               abbreviate = FALSE) == "Sunday",
                               "yes",
                               "no"))
    return(dplyr::bind_cols(ftab, rtab))
}

#' Plot R, incl. daily cases and 7d-rolling mean, 7d testing and positivity.
#'
#' R needs to be estimated first using \code{\link{c19_estimate_r}()}.
#'
#' @param country_data country data
#'
#' @export
#' @family plot funcs
#' @seealso \code{\link{c19_estimate_r}()}
c19_r_plot <- function(country_data = c19_bg_data()) {
    ftab <- r_plot_tidy(country_data)
    vis <- r_plot_vis()
    sundays_only <- ftab %>% dplyr::filter(is_sun == "yes")
    first_sunday <- sundays_only %>%
        dplyr::slice_head() %>%
        dplyr::pull(date)
    plot_end_date <- utils::tail(ftab$date, n = 1)
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    secondary <- ggplot2::sec_axis(name = vis$sec_axis_name,
                                   trans = ~ . / r_scale)
    clr_guide <- ggplot2::guide_legend(override.aes = vis$clr_leg)
    fill_guide <- ggplot2::guide_legend(override.aes = vis$fill_leg)
    ttab <- ftab %>% dplyr::filter(date >= ftab$date[1] + vis$skip_to)
    cmx <- max(ttab %>% dplyr::pull(new_cases),
               na.rm = TRUE)
    rmx <- max(ttab %>% dplyr::pull(`Quantile.0.975(R)`),
               na.rm = TRUE)
    pmx <- max(ttab %>% dplyr::pull(s7_nt),
               na.rm = TRUE)
    tick_choice <- vis$tick_choice
    r_scale <- tick_choice[tick_choice >= cmx / rmx][1]
    c_max <- r_scale * rmx # cases axis limit
    c_by <- r_scale        # cases axis tick
    tst_choice <- vis$tst_choice
    tst_scale <- tst_choice[tst_choice >= pmx / c_max][1]
    plt <- ggplot2::ggplot(data = ftab, mapping = ggplot2::aes(x = date))
    plt <- plt +
        ggplot2::geom_col(mapping = ggplot2::aes(y = new_cases, fill = is_sun),
                          width = 0.9) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = rollmean7,
                                                  color = "C_mva"),
                           linetype = vis$lty_mva,
                           size = vis$line_sz) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = `Median(R)` * r_scale,
                                                  color = "D_med"),
                           linetype = vis$lty_norm,
                           size = vis$line_sz) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = s7_nt / tst_scale,
                                                  color = "A_tst"),
                           linetype = vis$lty_norm,
                           size = vis$line_sz) +
        ggplot2::geom_line(mapping = ggplot2::aes(y = posit7 * r_scale,
                                                  color = "B_pos"),
                           linetype = vis$lty_norm,
                           size = vis$line_sz) +
        ggplot2::geom_ribbon(
            mapping = ggplot2::aes(ymin = `Quantile.0.025(R)` * r_scale,
                                   ymax = `Quantile.0.975(R)` * r_scale,
                                   fill = "A_ribbon")
        ) +
        # % positivity labels
        ggplot2::geom_text(
            data = sundays_only %>%
                dplyr::filter(!is.na(posit7)),
            mapping = ggplot2::aes(x = date - 3.5,
                                   y = 0,
                                   color = "B_pos",
                                   label = paste0(round(100 * posit7),
                                                  "%")),
            vjust = 1.3,
            size = vis$font_size_poslab
        ) +
        # % positivity labels label
        ggplot2::geom_text(
            data = sundays_only %>%
                dplyr::filter(is.na(posit7)) %>%
                dplyr::slice_tail(),
            mapping = ggplot2::aes(
                x = date,
                y = 0,
                color = "B_pos",
                label = tra("sedmicna pozitivnost: ")
            ),
            vjust = 1.3,
            hjust = 1,
            size = vis$font_size_poslab
        ) +
        # R median label
        shadowtext::geom_shadowtext(
            data = ftab %>%
                dplyr::filter(date == plot_end_date - 1),
            mapping = ggplot2::aes(
                x = date,
                y = `Median(R)` * r_scale,
                color = "D_med",
                label = format(round(`Median(R)`, 2), nsmall = 2)
            ),
            family = vis$font_family,
            size = vis$font_size_R,
            bg.color = "#ebebeb",
            nudge_x = 5
        ) +
        # R CrI 95% lower
        shadowtext::geom_shadowtext(
            data = ftab %>%
                dplyr::filter(date == plot_end_date - 1),
            mapping = ggplot2::aes(
                x = date,
                y =  r_scale * min(`Quantile.0.025(R)`, `Median(R)` - 0.1),
                label = format(round(`Quantile.0.025(R)`, 2), nsmall = 2)
            ),
            color = vis$clr$cri_txt,
            family = vis$font_family,
            size = vis$font_size_R,
            bg.color = "#ebebeb",
            nudge_x = 5
        ) +
        # R CrI 95% upper
        shadowtext::geom_shadowtext(
            data = ftab %>%
                dplyr::filter(date == plot_end_date - 1),
            mapping = ggplot2::aes(
                x = date,
                y = r_scale * max(`Quantile.0.975(R)`, `Median(R)` + 0.1),
                label = format(round(`Quantile.0.975(R)`, 2), nsmall = 2)
            ),
            color = vis$clr$cri_txt,
            family = vis$font_family,
            size = vis$font_size_R,
            bg.color = "#ebebeb",
            nudge_x = 5
        ) +
        ggplot2::scale_fill_manual(
            name = 2,
            values = c(vis$clr$cri, vis$clr$reg_c, vis$clr$reg_s),
            labels = vis$fill_labels,
            guide = fill_guide
        ) +
        ggplot2::scale_color_manual(
            name = 1,
            values = c(vis$clr$tst, vis$clr$pos, "black", vis$clr$r_med),
            labels = vis$clr_labels,
            guide = clr_guide
        ) +
        ggplot2::scale_y_continuous(
            breaks = seq(0, c_max, by = c_by),
            labels = scales::label_number(),
            limits = c(0, c_max),
            sec.axis = secondary,
            expand = ggplot2::expansion(mult = c(0.025, 0.015))
        ) +
        ggplot2::scale_x_date(
            breaks = seq(first_sunday,
                         last_sunday_inc,
                         by = "7 days"),
            limits = c(ftab$date[1], last_sunday_inc + 4),
            date_labels = "%d.%m. (%V)",
            expand = ggplot2::expansion(mult = c(0.025, 0), add = c(-1, 4))
        ) +
        vis$labels +
        ggplot2::labs(y = sprintf(vis$lab_y, tst_scale)) +
        vis$theme
    return(plt)
}

#' Saves the R plot.
#'
#' @param ... Passed export params: w (width), h (height), file_ext (".svg",
#'            ".png", ".jpg"; others may work as well). Rest passed to ggplot2,
#'            e.g. dpi, quality for JPEG output.
#'
#' @export
#' @examples
#' \dontrun{
#' c19_r_plot_save() # default is SVG
#' c19_r_plot_save(file_ext = ".png", dpi = 300)
#' c19_r_plot_save(file_ext = ".jpg", dpi = 125, quality = 90)
#' }
#' @family output funcs
c19_r_plot_save <- function(...) {
    c19_estimate_r()
    export(file = "C00_R", ..., plot = c19_r_plot())
}
