library(EpiEstim) # Cori et al. https://doi.org/10.1093/aje/kwt133
library(ggplot2)
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows

# download @ https://data.egov.bg/data/view/492e8186-0d00-43fb-8f5e-f2b0b183b64f
gen_data <- enc("Обща статистика за разпространението.csv") # open data: general spread statistics
gtab <- read.csv(file = gen_data)
# case counts before open data (2020-03-08 - 2020-06-05)
nc_old <- c(4, 0, 2, 1, 16, 8, 10, 10, 11, 19, 11, 15, 20, 36, 22, 16,
            17, 24, 22, 29, 38, 15, 13, 40, 23, 35, 28, 18, 28, 18, 28, 16,
            25, 17, 26, 14, 10, 28, 34, 53, 46, 32, 16, 35, 46, 49, 73, 91,
            59, 53, 63, 36, 48, 59, 49, 39, 24, 34, 52, 74, 51, 43, 49, 44,
            25, 33, 46, 31, 38, 37, 36, 24, 24, 33, 39, 41, 36, 19, 6, 10,
            17, 17, 8, 14, 14, 6, 19, 22, 25, 42)
nc <- c(nc_old, gtab[[6]])
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
res <- estimate_R(nc, method = "uncertain_si", config = make_config(list(
    seed = 12345, t_start = start, t_end = end, n1 = 1000, n2 = 1000,
    mean_si = mu, std_mean_si = mu_sd,
    min_mean_si = mu - mu_trunc, max_mean_si = mu + mu_trunc,
    std_si = stdev, std_std_si = stdev_sd,
    min_std_si = stdev - stdev_trunc, max_std_si = stdev + stdev_trunc)))
#### plot & save csv
plot(res)
write.csv(res[1], "estR.csv", row.names=FALSE)