#' @importFrom magrittr %>%

.onLoad <- function(libname, pkgname) {
    op <- options()
    op.c19bg <- c19bg_setup()
    toset <- !(names(op.c19bg) %in% names(op))
    if (any(toset)) options(op.c19bg[toset])

    invisible()
}

.onAttach <- function(libname, pkgname) {
    packageStartupMessage("Attached: c19bg")
}

c19bg_setup <- function() {
    parent_dir <- "c19bg" # for plots and downloads
    # general config
    cfg <- list(
        c19bg.data_dir = file.path(parent_dir, "data"),
        c19bg.output_dir = file.path(parent_dir, "plots"),
        c19bg.font_family = "Calibri", # font; uses default, if not found
        c19bg.font_size = 14, # base size (prefer changing font_scale below)
        c19bg.font_scale = 1, # adjust font size everywhere by this factor

        # set this to installation/platform-specific locations
        c19bg.output = list(
            inkopts = "-w %d --export-filename",
            mgkopts = "-quality 100",
            pixwidth = 1375,
            width = 11,
            height = 7
        )
    )

    # platform-dependent init / config
    os_type <- .Platform$OS.type
    if (os_type == "windows") {
        # Inkscape usually to be found here
        inkpath <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
        cfg$c19bg.output$inkscape <- inkpath
        # ImageMagick folder changes with version but executable often in path,
        # so we try
        cfg$c19bg.output$magick = "magick"
    } else { # unix
        cfg$c19bg.output$inkscape = "inkscape"
        cfg$c19bg.output$magick = "magick"
    }
    return(cfg)
}

tra <- function(x) {
    tr <- intern_data$trans_bg %>%
        dplyr::filter(id == x) %>%
        dplyr::pull(translation)
    if (length(tr) == 0) {
        warning("MISSING TRANSLATION FOR:\n\t", x)
        return(x)
    }
    return(tr)
}
