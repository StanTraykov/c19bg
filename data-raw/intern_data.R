rd_in <- function(file) {
    readr::read_csv(file.path("data-raw", paste0(file, ".csv")),
                    trim_ws = FALSE,
                    na = "")
}

intern_data = list(
    test = c(1,2,4),
    bg_cnames = rd_in("bg_cnames"),
    ccodes = rd_in("ccodes"),
    pre_opendata = rd_in("pre_opendata"),
    oblasts = rd_in("oblasts"),
    trans_bg = rd_in("trans_bg")
)

usethis::use_data(intern_data, overwrite = TRUE, internal = TRUE)
