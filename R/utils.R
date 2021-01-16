#' Reload all data from disk (or from the Internet).
#'
#' Equivalent to calling
#' \code{\link{c19_bg_data}(reload = TRUE, redownload = redownload)}
#' and
#' \code{\link{c19_eu_data}(reload = TRUE, redownload = redownload)}
#' @param redownload whether to refresh all datasets from the Internet.
#'
#'@export
#'@examples
#'\dontrun{
#'c19_reload()
#'c19_reload(redownload = TRUE)
#'}
#'@family data funcs
c19_reload <- function(redownload = FALSE) {
    c19_bg_data(reload = TRUE, redownload = redownload)
    c19_eu_data(reload = TRUE, redownload = redownload)
    invisible()
}

export <- function(
    file,
    file_ext = ".svg",
    w = getOption("c19bg.output")$width,
    h = getOption("c19bg.output")$height,
    scale_h = 1,
    scale_w = 1,
    ...
) {
    out_dir <- file.path(getOption("c19bg.output_dir"), "save")
    if (!file.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
    file <- file.path(out_dir, paste0(file, file_ext))
    message(paste("exporting:", file))
    ggplot2::ggsave(file = file,
                    width = scale_w * w,
                    height = scale_h * h,
                    ...)
}

#' Calls output funcs with some default params.
#'
#' Output files will be located in c19bg/plots/save under current working
#' directory. Downloaded and calculated data will be stored in c19bg/data.
#'
#' @param file_ext extension (e.g. ".svg", ".png", ".jpg")
#' @param dpi dots per inch
#' @param funs functions to call (default: all)
#' @param dl whether to refresh all data sets from the Internet
#' @param rl whether to refresh all data sets from disk
#' @param ... passed to individual save functions
#'
#' @export
#' @examples
#' \dontrun{
#' c19_save_all() # default is ".svg"
#' c19_save_all(file_ext = ".png")
#' c19_save_all(file_ext = ".jpg", quality = 100)
#' c19_save_all(file_ext = ".jpg", w = 10, h = 10, dpi = 200) #2000x2000px
#' c19_save_all(file_ext = ".png", dpi = 300)

#' }
#' @family output funcs
c19_save_all <- function(file_ext = ".svg",
                         dpi = 125,
                         funs = c(
                             "c19_var_plot_save",
                             "c19_heat_save",
                             "c19_oblasts_save",
                             "c19_eu_plots_save",
                             "c19_r_plot_save"
                         ),
                         dl = TRUE,
                         rl = TRUE,
                         ...) {
    # load fonts on Windows to use the option-supplied font for bitmap output
    if (.Platform$OS.type == "windows" &&
        "extrafont" %in% rownames(utils::installed.packages())) {
        extrafont::loadfonts(device = "win")
    }

    if (dl || rl) {
        message(paste("Reloading from",
                      ifelse(dl, "the Internet.", "disk.")))
        c19_reload(redownload = dl)
    }

    for (f in funs)
        do.call(f, args = list(file_ext = file_ext, dpi = dpi, ...))
    invisible()
}

setup_data_dir <- function() {
    data_dir <- getOption("c19bg.data_dir")
    if (!file.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
    return(data_dir)
}

datafile_exists <- function(file) {
    data_dir <- getOption("c19bg.data_dir")
    return(file.exists(file.path(data_dir, file)))
}

data_path <- function(file) {
    data_dir <- setup_data_dir()
    return(file.path(data_dir, file))
}

download <- function(url, file, zip = FALSE) {
    data_dir <- setup_data_dir()
    full <- file.path(data_dir, file)
    down <- full
    if (zip) down <- paste0(down, "_tmp")
    utils::download.file(url, down)
    if (zip) {
        R.utils::gzip(filename = down,
                      destname = full,
                      overwrite = TRUE,
                      remove = TRUE)
    }
    return(full)
}

tib_write_csv <- function(tib, file) {
    if (!tibble::is_tibble(tib))
        stop("non-tibble given for writing")
    data_dir <- setup_data_dir()
    readr::write_csv(tib,
                     file.path(data_dir, file))
}

tib_read_csv <- function(file, ...) {
    data_dir <- getOption("c19bg.data_dir")
    r <- readr::read_csv(file.path(data_dir, file),
                         trim_ws = FALSE,
                         na = "",
                         ...)
    return(r)
}

tib_read_tsv <- function(file, ...) {
    data_dir <- getOption("c19bg.data_dir")
    r <- readr::read_tsv(file.path(data_dir, file),
                         trim_ws = FALSE,
                         na = "",
                         ...)
    return(r)
}

signif_pad <- function(x, ...) {
    fmt <- formatC(signif(x, ...), format = "fg", flag = "#", ...)
    return(sub("\\.$", "", fmt))
}
