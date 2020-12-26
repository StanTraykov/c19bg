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

if (!exists("skip_var")) skip_var <- FALSE
if (!exists("skip_demo")) skip_demo <- FALSE
if (!exists("skip_dall")) skip_dall <- FALSE
if (!exists("skip_r")) skip_r <- FALSE

# set this to installation/platform-specific locations
inkscape <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
magick <- "magick"
inkopts <- "-w %d --export-filename"
mgkopts <- "-quality 100"
out_dir <- format(Sys.time(), "%b%d")

source(file.path("R", "helper.R"))
load_all_scripts()

##### helper funcs
dirs <- list(main = out_dir,
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
    cat(paste0("cmd> ", cmd, "\n"))
    system(cmd)
}

export <- function(file,
                   plot = NULL,
                   make_svg = TRUE,
                   make_png = TRUE,
                   make_jpg = TRUE,
                   width = 11,
                   height = 7,
                   pix_width = 1375) {
    fn <- filenames(file)
    if (make_svg)
        ggplot2::ggsave(file = fn$svg,
                        width = width,
                        height = height,
                        plot = plot)
    if (make_png)
        runcmd(paste(inkscape,
                     fn$svg,
                     sprintf(inkopts, pix_width),
                     fn$png))
    if (make_jpg)
        runcmd(paste(magick, fn$png, mgkopts, fn$jpg))
}

##### export stuff
for (d in dirs)
    if (!file.exists(d))
        dir.create(d)

bg_data <- get_bg_data()

if (!skip_var) {
    export(file = "C09_pos", plot = var_plot(bg_data, "positivity"))
    export(file = "C09_pos_pcr", plot = var_plot(bg_data, "pospcr"))
    export(file = "C09_pos_ag", plot = var_plot(bg_data, "posag"))
    export(
        file = "C04_cd",
        plot = var_plot(bg_data,
                        "casesdeaths",
                        roll_func = mean,
                        roll_window = 7)
    )
    export(file = "C08_cases", plot = var_plot(bg_data, "cases"))
    export(file = "C07_hospitalized", plot = var_plot(bg_data, "hospitalized"))
    export(file = "C05_age_7", plot = var_plot(bg_data,
                                               "age",
                                               roll_func = mean,
                                               roll_window = 7,
                                               line_legend = "0"))
    export(file = "C05_age_dis", plot = var_plot(bg_data,
                                                 "dis",
                                                 roll_func = mean,
                                                 roll_window = 7,
                                                 line_legend = "."))
    export(
        file = "C06_age_1",
        plot = var_plot(bg_data, "age", line_legend = "0")
    )

    heat_map <- hplot(bg_data)
    ggplot2::ggsave(file = filenames("C01_heat")$jpg,
                    width = 11, height = 5.5, quality = 100, dpi = 125,
                    plot = heat_map)
    ggplot2::ggsave(file = filenames("C01_heat")$png,
                    width = 11, height = 5.5, dpi = 125,
                    plot = heat_map)

    export(file = "C03_oblasts_count", plot = oblasts_plot(bg_data,
                                                           incid_100k = FALSE))
    export(file = "C03_oblasts_c_cmp", plot = oblasts_plot(bg_data,
                                                           incid_100k = FALSE,
                                                           facet = FALSE))
    export(file = "C02_oblasts_i100k", plot = oblasts_plot(bg_data,
                                                           incid_100k = TRUE))
    export(file = "C02_oblasts_i_cmp", plot = oblasts_plot(bg_data,
                                                           incid_100k = TRUE,
                                                           facet = FALSE))
}
if (!skip_dall) {
    eud <- get_eu_data()
    export(plot = wk_plot(eud, indicator = "tests_100k", top_n = 100),
           file = "C14_cmp_tst_eurp")
    export(plot = wk_plot(eud,
                          indicator = "positivity",
                          top_n = 100,
                          label_fun = function(x) sprintf("%.1f%%", 100 * x),
                          axis_labels = scales::label_percent()),
           file = "C15_cmp_pos_eurp")
    export(plot = wk_plot(eud,
                          indicator = "hosp_1m", top_n = 100),
           file = "C13_cmp_h_eurp")
    export(plot = wk_plot(eud,
                          indicator = "em_1m"),
           file = "C12_exd1m_eurp")
    export(plot = wk_plot(eud,
                          indicator = "r14_cases",
                          lower_y = 0),
           file = "C10_cmp_i_wrld")
    export(plot = wk_plot(eud,
                          indicator = "r14_deaths", lower_y = 0),
           file = "C10_cmp_d_wrld")
    export(plot = wk_plot(eud,
                          indicator = "r14_cases",
                          continents = "Europe",
                          lower_y = 0),
           file = "C11_cmp_i_eurp")
    export(plot = wk_plot(eud,
                          indicator = "r14_deaths",
                          continents = "Europe",
                          lower_y = 0),
           file = "C11_cmp_d_eurp")
    export(file = "D00_BG_t", plot = tplot(eud, "BG"))
    export(file = "D00_map", plot = mplot(eud))
    export(file = "D00_cmp",
           height = 8,
           width = 14.4,
           pix_width = 1800,
           plot = fplot(eud))
    if (!skip_demo) {
        for (n in seq_along(eud$eu_codes)) {
            pn <- stringr::str_pad(n, 2, pad = "0")
            export(file = paste0("D", pn, "_", eud$eu_codes[n]),
                   plot = cplot(eud, eud$eu_codes[n]))
        }
    }
}
r_calc_csv <- file.path("data", "estR.csv")
hfile <- file.path("data", ".Rhash")
if (!skip_r) {
    old_hash <- ""
    if (file.exists(hfile) && file.exists(r_calc_csv)) {
        old_hash = scan(hfile, what = "")
    }
    new_hash <- digest::sha1(file.path("data", "bg_gen.csv"))
    if (old_hash != new_hash) {
        cat("calculating R...\n")
        estimate_r(bg_data)
        write(new_hash, hfile)
    } else {
        cat("skipping R calc (data unchanged)\n")
    }
}
if (file.exists(r_calc_csv)) {
    export(file = "C00_R", plot = r_plot(bg_data))
}
