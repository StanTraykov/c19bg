#' @export
c19_save_all <- function() {
    # load fonts on Windows to use the option-supplied font for bitmap output
    if (.Platform$OS.type == "windows" &&
        "extrafont" %in% rownames(installed.packages())) {
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
        do.call(f, args = list())
}
