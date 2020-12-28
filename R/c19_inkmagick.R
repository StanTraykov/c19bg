# Production quality output -- using Inkscape for rasterization & ImageMagick
# for JPEG compression yields slightly better results but is not really
# necessary.
#
# * See the example functions at end of scripts for alternative simple output.
#   -> You can run them all via save_all_charts() in helper.R
#
# * The return value of plot functions can also be used to:
#   -> save to desired file format (SVG/PNG/JPG) via ggsave()
#   -> plot to screen via plot() / print()

#' @export
c19_inkmagick <- function(var = TRUE, eu = TRUE, r = TRUE, d_all = FALSE) {
    out_parent <- getOption("c19bg.output_dir")
    out_dir <- file.path(out_parent, format(Sys.time(), "%b%d"))

    ##### helper funcs
    dirs <- list(output = out_parent,
                 main = out_dir,
                 svg = file.path(out_dir, "svg"),
                 png = file.path(out_dir, "png"),
                 jpg = file.path(out_dir, "jpg"))

    filenames <- function(fname) {
        fn <- list(svg = paste0(file.path(dirs$svg, fname), ".svg"),
                   png = paste0(file.path(dirs$png, fname), ".png"),
                   jpg = paste0(file.path(dirs$jpg, fname), ".jpg"))
        return(fn)
    }

    runcmd <- function(cmd) {
        message(paste0("cmd> ", cmd))
        system(cmd)
    }

    export <- function(file,
                       plot = NULL,
                       make_svg = TRUE,
                       make_png = TRUE,
                       make_jpg = TRUE,
                       width = getOption("c19bg.output")$width,
                       height = getOption("c19bg.output")$height,
                       pix_width = getOption("c19bg.output")$pixwidth,
                       inkopts = getOption("c19bg.output")$inkopts,
                       mgkopts =  getOption("c19bg.output")$mgkopts) {
        fn <- filenames(file)
        if (make_svg)
            ggplot2::ggsave(file = fn$svg,
                            width = width,
                            height = height,
                            plot = plot)
        if (make_png)
            runcmd(paste(getOption("c19bg.output")$inkscape,
                         fn$svg,
                         sprintf(inkopts, pix_width),
                         fn$png))
        if (make_jpg)
            runcmd(paste(getOption("c19bg.output")$magick,
                         fn$png, mgkopts,
                         fn$jpg))
    }

    ##### export stuff
    for (d in dirs)
        if (!file.exists(d))
            dir.create(d, recursive = TRUE)

    if (var) {
        export(file = "C09_pos", plot = c19_var_plot("positivity"))
        export(file = "C09_pos_pcr", plot = c19_var_plot("pospcr"))
        export(file = "C09_pos_ag", plot = c19_var_plot("posag"))
        export(
            file = "C04_cd",
            plot = c19_var_plot("casesdeaths",
                            roll_func = mean,
                            roll_window = 7)
        )
        export(file = "C08_cases", plot = c19_var_plot("cases"))
        export(file = "C07_hospitalized", plot = c19_var_plot("hospitalized"))
        export(file = "C05_age_7", plot = c19_var_plot("age",
                                                   roll_func = mean,
                                                   roll_window = 7,
                                                   line_legend = "0"))
        export(file = "C05_age_dis", plot = c19_var_plot("dis",
                                                     roll_func = mean,
                                                     roll_window = 7,
                                                     line_legend = "."))
        export(
            file = "C06_age_1",
            plot = c19_var_plot("age", line_legend = "0")
        )

        heat_map <- c19_heat()
        ggplot2::ggsave(file = filenames("C01_heat")$jpg,
                        width = 11, height = 5.5, quality = 100, dpi = 125,
                        plot = heat_map)
        ggplot2::ggsave(file = filenames("C01_heat")$png,
                        width = 11, height = 5.5, dpi = 125,
                        plot = heat_map)

        charts <- list(
            list(file = "C02_oblasts_i100k", i = TRUE, f = TRUE),
            list(file = "C03_oblasts_count", i = FALSE, f = TRUE),
            list(file = "C02_oblasts_i_cmp", i = TRUE, f = FALSE),
            list(file = "C03_oblasts_c_cmp", i = FALSE, f = FALSE)
        )
        for (c in charts) {
            export(file = c$file,
                   plot = c19_oblasts(incid_100k = c$i, facet = c$f))
        }
    }
    if (eu) {
        export(plot = c19_eu_weekly(indicator = "tests_100k", top_n = 100),
               file = "C14_cmp_tst_eurp")
        export(
            plot = c19_eu_weekly(
                indicator = "positivity",
                top_n = 100,
                label_fun = function(x) sprintf("%.1f%%", 100 * x),
                axis_labels = scales::label_percent()
            ),
            file = "C15_cmp_pos_eurp"
        )
        export(plot = c19_eu_weekly(indicator = "hosp_1m", top_n = 100),
               file = "C13_cmp_h_eurp")
        export(plot = c19_eu_weekly(indicator = "em_1m"),
               file = "C12_exd1m_eurp")
        export(plot = c19_eu_weekly(indicator = "r14_cases", lower_y = 0),
               file = "C10_cmp_i_wrld")
        export(plot = c19_eu_weekly(indicator = "r14_deaths", lower_y = 0),
               file = "C10_cmp_d_wrld")
        export(
            plot = c19_eu_weekly(
                indicator = "r14_cases",
                continents = "Europe",
                lower_y = 0
            ),
            file = "C11_cmp_i_eurp"
        )
        export(
            plot = c19_eu_weekly(
                indicator = "r14_deaths",
                continents = "Europe",
                lower_y = 0
            ),
            file = "C11_cmp_d_eurp"
        )
        export(file = "D00_BG_t", plot = c19_deaths_total("BG"))
        export(file = "D00_map", plot = c19_deaths_map())
        export(file = "D00_cmp",
               height = 8,
               width = 14.4,
               pix_width = 1800,
               plot = c19_deaths_factor())
        eu_codes <- c19_eu_data()$eu_codes
        for (n in seq_along(eu_codes)) {
            pn <- stringr::str_pad(n, 2, pad = "0")
            cd <- eu_codes[n]
            if (d_all || (cd == "BG"))
                export(file = paste0("D", pn, "_", cd),
                       plot = c19_deaths_age(cd))
        }
    }
    if (r) {
            c19_estimate_r()
            export(file = "C00_R", plot = c19_r_plot())
    }

    invisible()
}
