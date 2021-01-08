# various plots:
# cases/active ("cases")
# new cases + new deaths ("casedeaths")
# age plot ("age")
# disaggregated age plot ("dis")
# hospitalized / icu ("hospitalized")
# share of tests confirming new cases ("positivity" | "posag" | "pospcr")

#' @importFrom magrittr %>%

make_var_plot_vis <- function(process_data = FALSE) {
    if (!process_data) {
        vis <- list()
        vis_f <- function() {
            if (length(vis) == 0)
                vis <<- make_var_plot_vis(process_data = TRUE)
            return(vis)
        }
        return(vis_f)
    }

    translate <- function(x) {
        t <- list(mean = tra("-dnevno plavaso sredno"),
                  sum = tra("-dnevna suma"))
        res <- t[[x]]
        if (!is.null(res)) return(res)
        return(paste(tra("-dn. prozorec: "), x))
    }
    line_fields <- list(casesdeaths = "new_cases|new_deaths",
                        age = "([2-8]|^)0\\+|new_deaths",
                        dis = "0-|90\\+",
                        hospitalized = "active_cases",
                        positivity = "date|s7_nt$|posit7$|s7_nt_pcr|posit7_pcr",
                        pospcr = "date|posit7_pcr|s7_nt_pcr",
                        posag = "date|posit7_ag|s7_nt_ag")
    area_fields <- list(cases = "^cases$|active_cases",
                        hospitalized = "hospitalized|in_icu")
    tick_choice <- c(10, 20, 25, 50) * rep(c(1, 10, 100, 1000), each = 4)
    no_na <- c( # don't draw rows if these fields are NA
        age = "0+",
        dis = "0-19",
        positivity = "posit7",
        pospcr = "posit7_pcr",
        posag = "posit7_ag"
    )
    thin <- 0.5   # } lines
    thick <- 1    # }
    sz_norm <- list(rgx = "NONE", sizes = thin)
    line_sizes <- list(
        casesdeaths = sz_norm,
        age = list(rgx = "^[2-8]", sizes = c(thin, thick)),
        dis = sz_norm,
        hospitalized = sz_norm,
        positivity = sz_norm,
        pospcr = sz_norm,
        posag = sz_norm
    )
    type_norm <- list(rgx = "NONE", types = "solid")
    line_types <- list(
        casesdeaths = type_norm,
        age = type_norm,
        dis = type_norm,
        hospitalized = type_norm,
        positivity = list(rgx = "pcr", types = c("dotted", "solid")),
        pospcr = type_norm,
        posag = type_norm
    )
    casesdeaths_colors <- c("blue" = tra("novi slucai"),
                            "red" = tra("smartni slucai"))
    age_colors <- c("dark green",
                    rep("green", 3),
                    "blue",
                    rep("dark orange", 3),
                    "red")
    dis_colors <- c("lightgreen", "green1", "green4",
                    "#5555FF", "blue4", "violet",
                    "orange", "red", "red4")
    age_labels <- function(x) ifelse(x == "new_deaths",
                                     tra("smartni slucai"),
                                     x)
    age_guide <- ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(size = c(thick, rep(thin, 7), thick))
    )
    positivity_guide <- ggplot2::guide_legend(
        nrow = 1,
        override.aes = list(linetype = c(rep("solid", 2), rep("dotted", 2)))
    )
    cases_fills <- c("light blue" = tra("registrirani slucai"),
                     "dark blue" = tra("aktivni slucai"))
    hospitalized_fills <- c("dark golden rod 1" = tra("hospitalizirani"),
                            "dark red" = tra("ot tah v intenzivno"))
    hospitalized_colors <- c("black" = tra("aktivni slucai"))
    positivity_colors4 <- c(
        "green 3" = tra("broj testove"),
        "red 3" = tra("pozitivni"),
        "green 3" = tra("broj PCR"),
        "red 3" = tra("pozitivni PCR")
    )
    positivity_colors2 <- c(
        "green 3" = tra("broj testove"),
        "red 3" = tra("pozitivni")
    )
    make_scale <- function(sf, x) sf(values = names(x), labels = unname(x))
    plot_colors <- c(
        casesdeaths = make_scale(ggplot2::scale_color_manual,
                                 casesdeaths_colors),
        age = ggplot2::scale_color_manual(values = age_colors,
                                          labels = age_labels,
                                          guide = age_guide),
        hospitalized = make_scale(ggplot2::scale_color_manual,
                                  hospitalized_colors),
        positivity = ggplot2::scale_color_manual(
            values = names(positivity_colors4),
            labels = unname(positivity_colors4),
            guide = positivity_guide
        ),
        pospcr = make_scale(ggplot2::scale_color_manual,
                            positivity_colors2),
        posag = make_scale(ggplot2::scale_color_manual,
                           positivity_colors2),
        dis = ggplot2::scale_color_manual(
            values = dis_colors,
            guide = ggplot2::guide_legend(nrow = 1)
        )
    )

    plot_fills <- c(cases = make_scale(ggplot2::scale_fill_manual,
                                       cases_fills),
                    hospitalized = make_scale(ggplot2::scale_fill_manual,
                                              hospitalized_fills))
    x_label <- tra("data na dokladvane (sedmica)")
    plot_labels <- list(
        casesdeaths = ggplot2::labs(
            title = paste(tra("Registrirani novi slucai i"),
                          tra("smartni slucai ot COVID-19")),
            caption = tra("danni: NOS, data.egov.bg"),
            color = tra("dnevno"),
            x = x_label,
            y = tra("registrirani slucai")
        ),
        age = ggplot2::labs(
            title = paste(tra("Registrirani novi slucai"),
                          tra("(vazrastovi grupi) i "),
                          tra("smartni slucai")),
            caption = tra("danni: data.egov.bg"),
            color = tra("dnevno"),
            x = x_label,
            y = tra("registrirani slucai")
        ),
        dis = ggplot2::labs(
            title = paste(tra("Registrirani novi slucai"),
                          tra("(dezagregirani vazrastovi grupi)")),
            caption = tra("danni: data.egov.bg"),
            color = tra("dnevno"),
            x = x_label,
            y = tra("registrirani slucai")
        ),
        cases = ggplot2::labs(
            title = tra("Registrirani i aktivni slucai na COVID-19"),
            caption = tra("danni: NOS, data.egov.bg"),
            fill = NULL,
            x = x_label,
            y = NULL
        ),
        hospitalized = ggplot2::labs(
            title = tra("Hospitalizirani slucai na COVID-19"),
            caption = tra("danni: NOS, data.egov.bg"),
            fill = NULL,
            color = NULL,
            x = x_label,
            y = tra("hospitalizirani")
        ),
        positivity = ggplot2::labs(
            title = tra("Pozitivnost PCR + Ag (novi slucai . broj testove)"),
            caption = tra("danni: NOS, data.egov.bg"),
            color = tra("za poslednite 7 dni"),
            x = x_label,
            y = tra("broj testove")
        ),
        pospcr = ggplot2::labs(
            title = tra("Pozitivnost PCR (novi slucai . broj testove)"),
            caption = tra("danni: NOS, data.egov.bg"),
            color = tra("za poslednite 7 dni"),
            x = x_label,
            y = tra("broj testove")
        ),
        posag = ggplot2::labs(
            title = tra("Pozitivnost Ag (novi slucai . broj testove)"),
            caption = tra("danni: data.egov.bg"),
            color = tra("za poslednite 7 dni"),
            x = x_label,
            y = tra("broj testove")
        )
    )
    plot_order <- list(
        cases = c("cases", "active_cases"),
        hospitalized = c("hospitalized", "in_icu", "active_cases"),
        positivity = c("s7_nt", "posit7", "s7_nt_pcr", "posit7_pcr"),
        pospcr = c("s7_nt_pcr", "posit7_pcr"),
        posag = c("s7_nt_ag", "posit7_ag")
    )
    plot_sec_y <- list(
        casesdeaths = list(label = tra("smartni slucai"),
                           vars = "new_deaths",
                           scale = 0.04),
        age = list(label = tra("smartni slucai"),
                   vars = "new_deaths",
                   scale = 0.04),
        hospitalized = list(label = tra("aktivni slucai"),
                            vars = "active_cases",
                            scale = 10),
        positivity = list(label = tra("novodokazani slucai"),
                          vars = c("posit7", "posit7_pcr"),
                          scale = 0.000005,
                          type = "percent"),
        pospcr = list(label = tra("novodokazani slucai"),
                      vars = "posit7_pcr",
                      scale = 0.000005,
                      type = "percent"),
        posag = list(label = tra("novodokazani slucai"),
                     vars = "posit7_ag",
                     scale = 0.000005,
                     type = "percent")
    )
    plot_x_min <- list(
        positivity = list(posit7_pcr = as.Date("2020-12-23"),
                          s7_nt_pcr = as.Date("2020-12-23"))
    )
    font_size <- getOption("c19bg.font_size") * getOption("c19bg.font_scale")
    font_family <- getOption("c19bg.font_family")
    plot_theme <- ggplot2::theme(
        text = ggplot2::element_text(
            size = font_size,
            family = font_family
        ),
        panel.grid.minor.x = ggplot2::element_blank(),
        legend.position = "top",
        plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
        axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
        panel.grid.minor.y = ggplot2::element_blank()
    )
    vis <- list(
        translate = translate,
        fields = list(line = line_fields, area = area_fields),
        tick_choice = tick_choice,
        no_na = no_na,
        line_sizes = line_sizes,
        line_types = line_types,
        colors = list(line = plot_colors, area = plot_fills),
        labels = plot_labels,
        order = plot_order,
        sec_y = plot_sec_y,
        plot_x_min = plot_x_min,
        theme = plot_theme,
        font_family = font_family,
        font_size = font_size,
        font_size_lineleg = 4 * getOption("c19bg.font_scale")
    )
    return(vis)
}

var_plot_vis <- make_var_plot_vis()

var_plot_tidy <- function(country_data) {
    # age table
    atab <- country_data$age
    for (s in c(8:1)) {
        vals <- as.integer(rowSums(atab[(s + 1):10]))
        field_name <- paste0(ifelse(s > 1, s, ""), "0+")
        atab[field_name] <- vals
    }
    # history + age data, clean wide table
    ftab <- dplyr::left_join(country_data$gen_inc_hist, atab, by = "date")
    roll7 <- function(x) zoo::rollsum(x, 7, align = "right", fill = NA)
    ftab <- ftab %>%
        # all tests
        dplyr::mutate(
            s7_nc = roll7(new_cases),
            s7_nt = roll7(new_tests),
            posit7 = s7_nc / s7_nt,
            s7_nc_pcr = roll7(new_pcr_cases),
            s7_nt_pcr = roll7(new_pcr_tests),
            posit7_pcr = s7_nc_pcr / s7_nt_pcr,
            s7_nc_ag = roll7(new_ag_cases),
            s7_nt_ag = roll7(new_ag_tests),
            posit7_ag = s7_nc_ag / s7_nt_ag,
            posit_ag = new_ag_cases / new_ag_tests
        )
    return(ftab)
}

#' Various plots from data.gov.bg data.
#'
#' @param chart one of: "casesdeaths" | "age" | "dis" | "cases" |
#' "hospitalized" | "positivity" | "pospcr" | "posag"
#' @param roll_func function to roll, e.g. mean (default NULL = raw daily)
#' @param roll_window window for roll func in days (e.g. 7, 14)
#' @param roll_align rolling alignment (default: "right")
#' @param roll_fill fill missing values for rolling function (default: NA)
#' @param line_legend regular expression specifying lines that should be
#'                    labelled in-plot
#' @param country_data country data
#'
#' @export
#' @family plot funcs
c19_var_plot <- function(
    chart,
    roll_func = NULL,
    roll_window = 7,
    roll_align = "right",
    roll_fill = NA,
    line_legend = NULL,
    country_data = c19_bg_data()
) {
    set.seed(42)
    vis <- var_plot_vis()
    line_fields <- vis$fields$line[[chart]]
    area_fields <- vis$fields$area[[chart]]
    all_fields <- paste(c("date", line_fields, area_fields), collapse = "|")
    ptab <- var_plot_tidy(country_data) %>%
        dplyr::select(dplyr::matches(all_fields))
    # scale secondary axis values
    if (chart %in% names(vis$sec_y)) {
        for (v in vis$sec_y[[chart]]$vars) {
            ptab <- ptab %>% # "{v}" := and .data[[v]] provide indirection
                dplyr::mutate("{v}" := .data[[v]] / vis$sec_y[[chart]]$scale)
        }
    }
    # remove values before min_x
    if (chart %in% names(vis$plot_x_min)) {
        for (v in names(vis$plot_x_min[[chart]])) {
            min_date <- vis$plot_x_min[[chart]][[v]]
            ptab <- ptab %>%
                dplyr::mutate("{v}" := ifelse(date >= min_date,
                                              .data[[v]],
                                              NA))
        }
    }
    if (!is.null(roll_func)) {
        ptab <- ptab %>%
            dplyr::mutate(dplyr::across(
                !matches("date"),
                function(x) zoo::rollapply(x,
                                           roll_window,
                                           roll_func,
                                           align = roll_align,
                                           fill = roll_fill)
            ))
        legend_main <- paste0(roll_window,
                              vis$translate(deparse(substitute(roll_func))))
    } else {
        legend_main <- NULL
    }
    if (chart %in% names(vis$no_na)) {
        field <- vis$no_na[[chart]]
        ptab <- ptab %>% dplyr::filter(!is.na(.data[[field]]))
    }
    ptab <- ptab %>%
        tidyr::pivot_longer(cols = !matches("date"),
                            names_to = "metric",
                            values_to = "value")
    if (chart %in% names(vis$order))
        ptab <- ptab %>%
            dplyr::mutate(
                metric = forcats::fct_relevel(metric, vis$order[[chart]])
            )
    plot_start_date <- utils::head(ptab$date, n = 1)
    plot_end_date <- utils::tail(ptab$date, n = 1)
    first_sunday <- ptab %>%
        dplyr::select("date") %>%
        dplyr::filter(weekdays(date, abbreviate = FALSE) == "Sunday") %>%
        dplyr::slice_head() %>%
        dplyr::pull()
    days_till_sunday <- 7 - lubridate::wday(plot_end_date, week_start = 1)
    last_sunday_inc <- plot_end_date + days_till_sunday
    if (length(first_sunday) == 0 || first_sunday == last_sunday_inc) {
        first_sunday <- plot_start_date -
            lubridate::wday(plot_start_date, week_start = 1)
    }

    visibility_min <- 1.05 * max(ptab$value, na.rm = TRUE)
    tick_choice <- vis$tick_choice
    tick_by <- tick_choice[tick_choice >= visibility_min / 7][1]
    tick_max <- 10 * tick_by
    plt <- ggplot2::ggplot()
    if (chart %in% names(vis$fields$area)) {
        plt <- plt +
            ggplot2::geom_area(
                data = ptab %>%
                    dplyr::filter(stringr::str_detect(metric, area_fields)),
                mapping = ggplot2::aes(x = date,
                                       y = value,
                                       fill = metric),
                position = "identity"
            )
    }
    if (chart %in% names(vis$fields$line)) {
        plt <- plt +
            ggplot2::geom_line(
                data = ptab %>%
                    dplyr::filter(stringr::str_detect(metric, line_fields)),
                mapping = ggplot2::aes(
                    x = date,
                    y = value,
                    color = metric,
                    size = ifelse(
                        stringr::str_detect(metric,
                                            vis$line_sizes[[chart]]$rgx),
                        "size1",
                        "size2"
                    ),
                    linetype = ifelse(
                        stringr::str_detect(metric,
                                            vis$line_types[[chart]]$rgx),
                        "type1",
                        "type2"
                    )
                )
            ) +
            ggplot2::scale_size_manual(
                values = vis$line_sizes[[chart]]$sizes,
                guide = FALSE
            ) +
            ggplot2::scale_linetype_manual(
                values = vis$line_types[[chart]]$types,
                guide = FALSE
            )
    }
    if (!is.null(line_legend)) {
        plt <- plt +
            ggrepel::geom_text_repel(
                data = ptab %>%
                    dplyr::filter(date == plot_end_date,
                                  stringr::str_detect(metric, line_legend)),
                mapping = ggplot2::aes(x = date,
                                       y = value,
                                       color = metric,
                                       label = metric),
                family = vis$font_family,
                size = vis$font_size_lineleg,
                direction = "y",
                nudge_x = 2,
                box.padding = ggplot2::unit(0.12, units = "line"),
                hjust = 0,
                max.overlaps = Inf,
                segment.color	= "dark gray",
                segment.size = 0.3,
                segment.alpha	= 0.5,
                bg.colour = "#ebebeb",
                show.legend = FALSE
            )
        exp_fix <- 5
    } else {
        exp_fix <- 0
    }
    if (chart %in% names(vis$sec_y)) {
        if (!is.null(vis$sec_y[[chart]]$type))
            sec_labels <- scales::label_percent()
        else
            sec_labels <- scales::label_number()
        secondary <- ggplot2::sec_axis(
            name = vis$sec_y[[chart]]$label,
            trans = ~ . * vis$sec_y[[chart]]$scale,
            breaks = seq(0,
                         vis$sec_y[[chart]]$scale * tick_max,
                         by = vis$sec_y[[chart]]$scale * tick_by),
            labels = sec_labels
        )
    } else {
        secondary <- ggplot2::waiver()
    }
    plt <- plt +
        vis$colors$line[chart] +
        vis$colors$area[chart] +
        ggplot2::scale_y_continuous(
            breaks = seq(0, tick_max, by = tick_by),
            limits = c(0, NA),
            expand = ggplot2::expansion(mult = c(0.025, 0.05)),
            labels = scales::label_number(),
            sec.axis = secondary
        ) +
        ggplot2::scale_x_date(
            breaks = seq(first_sunday,
                         last_sunday_inc,
                         by = "7 days"),
            limits = c(min(plot_start_date, first_sunday),
                       last_sunday_inc + 4),
            date_labels = "%d.%m. (%V)",
            expand = ggplot2::expansion(mult = c(0.025, 0),
                                        add = c(0, 1 + exp_fix))
        ) +
        vis$labels[[chart]]
    if (!is.null(legend_main))
        plt <- plt + ggplot2::labs(color = legend_main)
    plt <- plt + vis$theme
    return(plt)
}

#' Saves various BG plots based on data.gov.bg.
#'
#' @param ... Passed export params: w (width), h (height), file_ext (".svg",
#'            ".png", ".jpg"; others may work as well). Rest passed to ggplot2,
#'            e.g. quality for JPEG output.
#'
#' @export
#' @examples
#' \dontrun{
#' c19_var_plot_save() # default is SVG
#' c19_var_plot_save(file_ext = ".png", dpi = 300)
#' c19_var_plot_save(file_ext = ".jpg", dpi = 125, quality = 100)
#' }
#' @family output funcs
c19_var_plot_save <- function(...) {
    export(file = "C09_pos",
           ...,
           plot = c19_var_plot("positivity"))
    export(file = "C09_pos_pcr",
           ...,
           plot = c19_var_plot("pospcr"))
    export(file = "C09_pos_ag",
           ...,
           plot = c19_var_plot("posag"))
    export(
        file = "C04_cd",
        ...,
        plot = c19_var_plot("casesdeaths",
                            roll_func = mean,
                            roll_window = 7)
    )
    export(file = "C08_cases",
           ...,
           plot = c19_var_plot("cases"))
    export(file = "C07_hospitalized",
           ...,
           plot = c19_var_plot("hospitalized"))
    export(file = "C05_age_7",
           ...,
           plot = c19_var_plot("age",
                               roll_func = mean,
                               roll_window = 7,
                               line_legend = "0"))
    export(file = "C05_age_dis",
           ...,
           plot = c19_var_plot("dis",
                               roll_func = mean,
                               roll_window = 7,
                               line_legend = "."))
    export(
        file = "C06_age_1",
        ...,
        plot = c19_var_plot("age", line_legend = "0")
    )
}
