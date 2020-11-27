# this script just outputs everything
# using inkscape & imagemagick yields slightly better results but is not really
# necessary (you can just ggsave to target format)

library(tools)

inkscape <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
magick <- "magick"
inkopts <- "-w 1375 --export-filename"
mgkopts <- "-quality 100"
out_dir <- format(Sys.time(), "%b%d")

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

dirs <- list(main = out_dir,
             svg = file.path(out_dir, "svg"),
             png = file.path(out_dir, "png"),
             jpg = file.path(out_dir, "jpg"))
for (d in dirs)
    if (!file.exists(d))
        dir.create(d)

source("demo.R")
for (c in names(cnames))
    export(file = paste0(which(sort(cnames) == cnames[c]), "_", c),
           plot = cplot(c))
export(file = "0_map", plot = mplot())

source("heat.R")
ggsave(file = filenames("heat")$jpg,
       width = 11, height = 5.5, quality = 100, dpi = 125,
       plot = hplot())

source("oblasts.R")
export(file = "oblasts_count", plot = oblasts_plot(incid_100k = FALSE), )
export(file = "oblasts_i100k", plot = oblasts_plot(incid_100k = TRUE))

source("ag7d.R")
fn <- filenames("age7_plot")
save_cd_plot(fn$svg, 7)
export(file = "age7_plot", make_svg = FALSE)

cat("calculating R...\n")
source("estR.R")
