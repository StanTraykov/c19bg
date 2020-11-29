# Production quality output -- using inkscape & imagemagick yields slightly
# better results but is not really necessary.
#
# - see the save_all() example functions in various files
# - alternatively just ggsave() to target format or plot() to screen

library(tools)

# set this to your installation/platform-specific locations
inkscape <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
magick <- "magick"
inkopts <- "-w 1375 --export-filename"
mgkopts <- "-quality 100"
out_dir <- format(Sys.time(), "%b%d")

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

source("var_plot.R")
export(file = "D09_posit", plot = var_plot("positivity"))
export(file = "D04_cd", plot = var_plot("casesdeaths",
                                           roll_func = mean,
                                           roll_window = 7))
export(file = "D08_cases", plot = var_plot("cases"))
export(file = "D07_hospitalized", plot = var_plot("hospitalized"))
export(file = "D05_age_7", plot = var_plot("age",
                                     roll_func = mean,
                                     roll_window = 7,
                                     line_legend = "0"))
export(file = "D06_age_1", plot = var_plot("age", line_legend = "0"))

source("heat.R")
ggsave(file = filenames("D01_heat")$jpg,
       width = 11, height = 5.5, quality = 100, dpi = 125,
       plot = hplot())

source("oblasts.R")
export(file = "D03_oblasts_count", plot = oblasts_plot(incid_100k = FALSE))
export(file = "D02_oblasts_i100k", plot = oblasts_plot(incid_100k = TRUE))

source("demo.R")
for (c in names(cnames))
    export(file = paste0(which(sort(cnames) == cnames[c]), "_", c),
           plot = cplot(c))
export(file = "0_map", plot = mplot())

cat("calculating R...\n")
source("estR.R")
