# estimate R and output to CSV
# using EpiEstim package Cori et al. https://doi.org/10.1093/aje/kwt133

#' Estimate R values.
#'
#' @param country_data test country data (default: c19_bg_data())
#'
#' @export
c19_estimate_r <- function(country_data = c19_bg_data()) {
    nc <- country_data$gen_inc_hist$new_cases
    down_dir <- getOption("c19bg.down_dir")
    res_csv <- file.path(down_dir, "estR.csv")
    hfile <- file.path(down_dir, ".Rhash")
    old_hash <- ""
    if (file.exists(hfile) && file.exists(res_csv)) {
        old_hash = scan(hfile, what = "")
    }
    new_hash <- digest::sha1(nc)
    if (old_hash == new_hash) {
        message("skipping R calc (data unchanged)\n")
        return()
    }
    message("calculating R...\n")
    # COVID-19 generation times gamma distribution (transformed parameters)
    # per Ferretti et al. https://doi.org/10.1101/2020.09.04.20188516
    mu <- 5.509073
    stdev <- 2.112571
    # exploration parameters (see Cori et al. Web Appendix 4)
    mu_sd <- 5
    mu_trunc <- 1.5
    stdev_sd <- 0.5
    stdev_trunc <- 1
    # calculate; window = 7 days
    win <- 7
    skip_to <- 2
    start <- seq(skip_to, length(nc) - win + 1)
    end <- start + win - 1
    res <- EpiEstim::estimate_R(
        nc,
        method = "uncertain_si",
        config = EpiEstim::make_config(list(
            seed = 12345,
            t_start = start,
            t_end = end,
            n1 = 1000,
            n2 = 1000,
            mean_si = mu,
            std_mean_si = mu_sd,
            min_mean_si = mu - mu_trunc,
            max_mean_si = mu + mu_trunc,
            std_si = stdev,
            std_std_si = stdev_sd,
            min_std_si = stdev - stdev_trunc,
            max_std_si = stdev + stdev_trunc
        ))
    )
    # save csv, input hash
    if (!file.exists(down_dir)) dir.create(down_dir, recursive = TRUE)
    write.csv(res[1], res_csv, row.names = FALSE)
    write(new_hash, hfile)
}
