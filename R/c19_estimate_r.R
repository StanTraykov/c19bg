# estimate R and output to CSV
# using EpiEstim package Cori et al. https://doi.org/10.1093/aje/kwt133

#' Estimate reproductive number for Bulgaria.
#'
#' Uses package EpiEstim by Cori et al. https://doi.org/10.1093/aje/kwt133
#' Gen. times per Ferretti et al. https://doi.org/10.1101/2020.09.04.20188516
#' Explores a range of gen time distributions that are likely for Bulgaria.
#' See longer explanation in repo wiki: https://github.com/StanTraykov/c19bg
#'
#' Also stores a hash of the input data (.Rhash in data dir) and skips
#' calculation if hash matches and estR.csv is already present.
#'
#' @param country_data country data
#'
#' @export
#' @examples
#' \dontrun{
#' c19_estimate_r() # save to csv file
#' result <- c19_estimate_r() # also save to variable
#' }
#' @seealso \code{\link{c19_r_plot}()}
c19_estimate_r <- function(country_data = c19_bg_data()) {
    nc <- country_data$gen_inc_hist$new_cases
    estr_csv <- "estR.csv"
    hfile <- data_path(".Rhash")
    old_hash <- ""
    if (file.exists(hfile) && datafile_exists(estr_csv)) {
        old_hash <- scan(hfile, what = "")
    }
    new_hash <- digest::sha1(nc)
    if (old_hash == new_hash) {
        message("skipping R calc (data unchanged)")
        return()
    }
    message("calculating R...")
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
    tib_write_csv(tibble::as_tibble(res$R), estr_csv)
    write(new_hash, hfile)
    invisible(res)
}
