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
        c19bg.data_dir = file.path(system.file(package = "c19bg",
                                               mustWork = TRUE),
                                   "data"),
        c19bg.down_dir = file.path(parent_dir, "downloads"),
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
        inkpath <- "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\""
        cfg$c19bg.output$inkscape <- inkpath
        cfg$c19bg.output$magick = "magick"

        # fileEncoding does not work on Windows (w/default codepage settings)

        enc_f <- function(x) { # Windows Unicode hack
            if (typeof(x) == "character")
                return(iconv(x, from = "UTF-8", to = "UTF-8"))
            return(x)
        }

        read_unicode <- function(frame) {
            return(frame %>%
                       dplyr::mutate(dplyr::across(dplyr::everything(), enc_f)))
        }

        # these read files using native encoding and convert after
        cfg$c19bg.rt <- function(...) {
            t <- utils::read.table(...,
                            na.strings = "",
                            stringsAsFactors = FALSE)
            return(read_unicode(t))
        }
        cfg$c19bg.rd <- function(...) {
            t <- utils::read.delim(...,
                            na.strings = "",
                            stringsAsFactors = FALSE)
            return(read_unicode(t))
        }
        cfg$c19bg.rc <- function(...) {
            t <- utils::read.csv(...,
                          na.strings = "",
                          stringsAsFactors = FALSE)
            return(read_unicode(t))
        }
    } else if (os_type == "unix") {
        cfg$c19bg.output$inkscape = "inkscape"
        cfg$c19bg.output$magick = "magick"

        # NOT TESTED but this should probably work?
        cfg$c19bg.rt <- function(...) {
            t <- utils::read.table(...,
                            fileEncoding = "UTF-8-BOM",
                            na.strings = "",
                            stringsAsFactors = FALSE)
            return(t)
        }
        cfg$c19bg.rd <- function(...) {
            t <- utils::read.delim(...,
                            fileEncoding = "UTF-8-BOM",
                            na.strings = "",
                            stringsAsFactors = FALSE)
            return(t)
        }
        cfg$c19bg.rc <- function(...) {
            t <- utils::read.csv(...,
                          fileEncoding = "UTF-8-BOM",
                          na.strings = "",
                          stringsAsFactors = FALSE)
            return(t)
        }
    }

    data_dir <- getOption("c19bg.data_dir", default = cfg$c19bg.data_dir)
    trans <- cfg$c19bg.rc(file.path(data_dir, "trans_bg.csv"))
    tra_f <- function(x) {
        ret <- trans %>% dplyr::filter(id == x) %>% dplyr::pull(translation)
        if (length(ret) == 0) {
            warning("MISSING TRANSLATION FOR:\n\t", x)
            return(x)
        }
        return(ret)
    }

    cfg$c19bg.tra <- tra_f
    return(cfg)
}

tra <- function(x) {
    getOption("c19bg.tra")(x)
}

#'@export
c19_reload <- function(redownload = FALSE) {
    c19_bg_data(reload = TRUE, redownload = redownload)
    c19_eu_data(reload = TRUE, redownload = redownload)
    invisible()
}
