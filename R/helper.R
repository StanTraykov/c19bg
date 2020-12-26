# instead of pkg (need to rework unicode handling before publishing as pkg)

load_all_scripts <- function() {
    r_dir <- "R"
    scripts <- c(
        "var_plot.R",
        "heat.R",
        "oblasts.R",
        "demo.R",
        "r_plot.R",
        "estR.R",
        "bg_opendata.R",
        "eu_opendata.R"
    )
    for (s in scripts) {
        source(file.path(r_dir, s))
    }
}

save_all_charts <- function() {
    save_funcs <- c(
        "var_plot_save",
        "heat_save",
        "oblasts_save",
        "demo_save",
        "estimate_r",
        "r_plot_save"
    )
    for (f in mget(save_funcs, ifnotfound = "not found")) {
        if (typeof(f) != "closure") {
            cat("loading scripts first...\n")
            load_all_scripts()
            break
        }
    }
    for (f in save_funcs)
        do.call(f, args = list())
}