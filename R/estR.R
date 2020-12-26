# estimate R and output to CSV
# using EpiEstim package Cori et al. https://doi.org/10.1093/aje/kwt133

estimate_r <- function() {
    nc <- get_bg_data()$gen_inc_hist$new_cases
    #### COVID-19 generation times gamma distribution (transformed parameters)
    #### per Ferretti et al. https://doi.org/10.1101/2020.09.04.20188516
    mu <- 5.509073; stdev <- 2.112571
    #### exploration parameters
    mu_sd <- 5; mu_trunc <- 1.5
    stdev_sd <- 0.5; stdev_trunc <- 1
    #### calculate; window = 7 days
    win <- 7; skip_to <- 2
    start <- seq(skip_to, length(nc) - win + 1)
    end <- start + win - 1
    res <- EpiEstim::estimate_R(
        nc,
        method = "uncertain_si",
        config = EpiEstim::make_config(list(
            seed = 12345, t_start = start, t_end = end, n1 = 1000, n2 = 1000,
            mean_si = mu, std_mean_si = mu_sd,
            min_mean_si = mu - mu_trunc, max_mean_si = mu + mu_trunc,
            std_si = stdev, std_std_si = stdev_sd,
            min_std_si = stdev - stdev_trunc, max_std_si = stdev + stdev_trunc))
    )
    #### save csv
    write.csv(res[1], file.path("data", "estR.csv"), row.names = FALSE)
}