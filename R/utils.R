#'@export
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
    file = file.path(out_dir, paste0(file, file_ext))
    message(paste("exporting:", file))
    ggplot2::ggsave(file = file,
                    width = scale_w * w,
                    height = scale_h * h,
                    ...)
}

#' @export
c19_save_all <- function(file_ext = ".svg", dpi = 125, ...) {
    # load fonts on Windows to use the option-supplied font for bitmap output
    if (.Platform$OS.type == "windows" &&
        "extrafont" %in% rownames(utils::installed.packages())) {
        extrafont::loadfonts(device = "win")
    }
    save_funcs <- c(
        "c19_var_plot_save",
        "c19_heat_save",
        "c19_oblasts_save",
        "c19_eu_plots_save",
        "c19_r_plot_save"
    )
    for (f in save_funcs)
        do.call(f, args = list(file_ext = file_ext, dpi = dpi, ...))
    invisible()
}
