# Production quality output -- using Inkscape for rasterization & ImageMagick
# for JPEG compression yields slightly better results but is not really
# necessary.
#
# - See the save_all() example functions in various files for quick HQ output.
# - Alternatively just ggsave() to target format or plot() to screen.
#
# To skip some of the time-consuming stuff:
# > skip_demo <- TRUE; skip_r <- TRUE; source_d("output_all.R")

library(tools)
library(stringr)

if (!exists("skip_var")) skip_var <- FALSE
if (!exists("skip_demo")) skip_demo <- FALSE
if (!exists("skip_r")) skip_r <- FALSE

# set this to installation/platform-specific locations
inkscape <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
magick <- "magick"
inkopts <- "-w 1375 --export-filename"
mgkopts <- "-quality 100"
out_dir <- format(Sys.time(), "%b%d")
source_d <- function(file) source(file.path("R", file))

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
                   make_jpg = TRUE) {
    fn <- filenames(file)
    if (make_svg)
        ggsave(file = fn$svg,
               width = 11, height = 7,
               plot = plot)
    if (make_png)
        runcmd(paste(inkscape, fn$svg, inkopts, fn$png))
    if (make_jpg)
        runcmd(paste(magick, fn$png, mgkopts, fn$jpg))
}

##### export stuff
for (d in dirs)
    if (!file.exists(d))
        dir.create(d)

if (!skip_var) {
    source_d("var_plot.R")
    export(file = "C09_posit", plot = var_plot("positivity"))
    export(file = "C04_cd", plot = var_plot("casesdeaths",
                                               roll_func = mean,
                                               roll_window = 7))
    export(file = "C08_cases", plot = var_plot("cases"))
    export(file = "C07_hospitalized", plot = var_plot("hospitalized"))
    export(file = "C05_age_7", plot = var_plot("age",
                                         roll_func = mean,
                                         roll_window = 7,
                                         line_legend = "0"))
    export(file = "C06_age_1", plot = var_plot("age", line_legend = "0"))
    
    source_d("heat.R")
    heat_map <- hplot()
    ggsave(file = filenames("C01_heat")$jpg,
           width = 11, height = 5.5, quality = 100, dpi = 125,
           plot = heat_map)
    ggsave(file = filenames("C01_heat")$png,
           width = 11, height = 5.5, dpi = 125,
           plot = heat_map)
    
    source_d("oblasts.R")
    export(file = "C03_oblasts_count", plot = oblasts_plot(incid_100k = FALSE))
    export(file = "C02_oblasts_i100k", plot = oblasts_plot(incid_100k = TRUE))
}
if (!skip_demo) {
    source_d("demo.R")
    for (c in names(cnames))
        export(file = paste0("D",
                             str_pad(which(sort(cnames) == cnames[c]),
                                     2,
                                     pad = "0"),
                             "_",
                             c),
               plot = cplot(c))
    export(file = "D00_BG_t", plot = tplot("BG"))
    export(file = "D00_map", plot = mplot())
}
if (!skip_r) {
    cat("calculating R...\n")
    source_d("estR.R")
}
if(file.exists(file.path("data", "estR.csv"))) {
    source_d("r_plot.R")
    export(file = "C00_R", plot = r_plot())
}
