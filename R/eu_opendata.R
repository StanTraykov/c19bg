library(magrittr)

enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows

# download data, if missing; return local filename
make_eu_data <- function() {
    dl_missing <- function(url, filename, zip_local = FALSE) {
        local_fn <- file.path("data", filename)
        if (zip_local)
            down_dest <- paste0(local_fn, ".temp")
        else
            down_dest <- local_fn
        if (!file.exists(local_fn)) {
            download.file(url, down_dest)
            if (zip_local) {
                R.utils::gzip(down_dest,
                              destname = local_fn,
                              overwrite = TRUE,
                              remove = TRUE)
            }
        }
        return(local_fn)
    }
    
    # ECDC nat'l case death
    local_ncd_file <- dl_missing(
        url = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",
        filename = "ecdc_ncd.csv.gz",
        zip_local = TRUE
    )
    
    # ECDC hosp
    local_hosp_file <- dl_missing(
        url = paste0("https://opendata.ecdc.europa.eu/covid19/",
                     "hospitalicuadmissionrates/csv/data.csv"),
        filename = "ecdc_hosp.csv.gz",
        zip_local = TRUE
    )
    
    # EUROSTAT deaths 10yr bands
    deaths_file <- "demo_r_mwk_10.tsv.gz"
    local_deaths_file <- dl_missing(
        url = paste0("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod/",
                     "BulkDownloadListing?file=data/",
                     deaths_file),
        filename = deaths_file
    )
    
    # ECDC testing
    local_tstg_file <- dl_missing(
        url = "https://opendata.ecdc.europa.eu/covid19/testing/csv",
        filename = "ecdc_tstg.csv.gz",
        zip_local = TRUE
    )
    
    # country names
    bg_names <- read.csv(file.path("prog_data", "bg_cnames.csv"), na.strings = "")
    bg_names <- bg_names %>%
        dplyr::mutate(bg_name = enc(bg_name)) %>%
        dplyr::rename(geo_name = bg_name)
    codes_tab <- read.csv(file.path("prog_data", "ccodes.csv"), na.strings = "")
    codes_tab <- codes_tab %>%
        dplyr::left_join(bg_names, by = "tl_code") # use Bulgarian names
    
    # return geo names in target language from EU 2-letter or ISO 3-letter codes
    get_geo_names <- function(codes) {
        ret <- sapply(codes,
                      function(x) codes_tab %>%
                          dplyr::filter(geo == x | tl_code == x) %>%
                          dplyr::pull(geo_name))
        return(ret)
    }
    
    # for deaths comparison / eu map
    eu_codes <- c(
        "IS", "NO", "SE", "FI", "EE", "UK", "DK", "NL", "LV", "LT", "DE", "PL",
        "BE", "CZ", "SK", "FR", "AT", "LU", "RS", "RO", "HU", "SI", "ES", "PT",
        "CH", "BG", "ME", "HR", "IT", "AL", "EL", "MT", "CY"
    )
    
    # eu map grid
    egrid <- data.frame(
        row = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4,
                4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 7, 7),
        col = c(1, 4, 5, 6, 7, 1, 4, 3, 7, 7, 4, 6, 3, 5, 5, 2, 4,
                3, 6, 7, 5, 4, 2, 1, 3, 7, 6, 5, 3, 6, 7, 2, 7),
        code = eu_codes,
        name = get_geo_names(eu_codes)
    )
    
    # rearrange EU codes in target language alphabetical order
    eu_codes <- codes_tab %>% 
        dplyr::filter(geo %in% eu_codes) %>%
        dplyr::arrange(geo_name) %>%
        dplyr::pull(geo)
    
    # eurostat deaths tab
    agrp_names <- function(x) { # age group labels
        x <- sub("Y_LT10", "00-09", x)
        x <- sub("Y_GE80", "80+", x)
        x <- sub("Y_GE90", "90+", x)
        x <- sub("Y", "", x)
        return(x)
    }
    
    dtab <- read.delim(gzfile(local_deaths_file))
    dtab <- cbind(stringr::str_split_fixed(dtab[[1]], ",", 4), dtab[, -1])
    names(dtab) <- c("age", "sex", "unit", "geo", names(dtab)[-(1:4)])
    dtab <- dtab %>%
        dplyr::select(dplyr::matches("(201[0-9]|2020)W[0-5]|age|geo|sex")) %>%
        tidyr::pivot_longer(cols = tidyr::matches("20..W"),
                            names_to = c("year", "week"),
                            names_pattern = "X(....)W(..)",
                            names_transform = list(year = as.integer,
                                                   week = as.integer),
                            values_to = "deaths") %>%
        dplyr::mutate(deaths = as.integer(gsub("[: pe]", "", deaths)),
                      age = agrp_names(age)) %>%
        dplyr::arrange(year, week, age)
    
    mean_tab <- dtab %>% # mean 2015-2019 & mean 2015-2019* *with NA removed
        dplyr::filter(sex == "T",
                      year >= 2015,
                      year <= 2019,
                      age == "TOTAL") %>%
        dplyr::group_by(geo, week) %>%
        dplyr::summarize(mean_deaths = mean(deaths),
                         mean_deaths_star = mean(deaths, na.rm = TRUE))
    tt_tab <- dtab %>% # 2020 deaths
        dplyr::filter(sex == "T",
                      year == 2020,
                      age == "TOTAL") %>%
        dplyr::select("geo", "week", "deaths") %>%
        dplyr::rename(d_2020 = deaths)
    last_bg_wk <- tt_tab %>%
        dplyr::filter(geo == "BG", d_2020 > 0) %>%
        dplyr::summarize(max(week)) %>%
        dplyr::pull()
    cmp_tab <- dplyr::left_join(mean_tab, tt_tab, by = c("geo", "week")) %>%
        dplyr::mutate(excess_deaths = d_2020 - mean_deaths) %>%
        dplyr::mutate(excess_deaths_star = d_2020 - mean_deaths_star) %>%
        dplyr::ungroup()
    
    ## ECDC nat'l cases / deaths
    ncd_tab <- read.csv(gzfile(local_ncd_file), na.strings = "")
    names(ncd_tab)[1] = "country" # BOM removal (fileEncoding broken on Win)
    ncd_tab <- ncd_tab %>%
        dplyr::rename(tl_code = country_code) %>%
        dplyr::right_join(
            codes_tab %>% dplyr::select("tl_code", "geo", "geo_name"),
            by = "tl_code"
        ) %>%
        dplyr::mutate(year = as.integer(substr(year_week, 1, 4)),
                      week = as.integer(substr(year_week, 6, 7)))
    
    # weekly COVID cases/deaths from ECDC
    cd_tab <- ncd_tab %>%
        tidyr::pivot_wider(names_from = "indicator",
                           values_from = c("weekly_count",
                                           "cumulative_count",
                                           "rate_14_day")) %>%
        dplyr::rename(cases = weekly_count_cases,
                      covid_deaths = weekly_count_deaths,
                      r14_cases = rate_14_day_cases,
                      r14_deaths = rate_14_day_deaths,
                      cml_cases = cumulative_count_cases,
                      cml_deaths = cumulative_count_deaths)
    
    # EUROSTAT & ECDC data incl. factors & excess mortality per 1M
    factor_tab <- dplyr::left_join(cd_tab, cmp_tab, by = c("geo", "week")) %>%
        dplyr::mutate(ed_covid_factor = excess_deaths / covid_deaths) %>%
        dplyr::mutate(ed_factor = d_2020 / mean_deaths) %>%
        dplyr::mutate(em_1m = 1000000 * excess_deaths_star / population)
    
    # add hospit. per ECDC
    hosp_tab <- read.csv(gzfile(local_hosp_file))
    names(hosp_tab)[1] = "country"
    hosp_tab <- hosp_tab %>%
        dplyr::filter(indicator == "Daily hospital occupancy") %>%
        dplyr::rename(hosp_count = value) %>%
        dplyr::select("country", "date", "year_week", "hosp_count") %>%
        dplyr::mutate(
            date = as.Date(date),
            year = as.integer(substr(year_week, 1, 4)),
            week = as.integer(substr(year_week, 7, 8))
        ) %>%
        dplyr::group_by(country, year, week) %>%
        dplyr::slice_tail() %>%
        dplyr::ungroup() %>%
        dplyr::select("country", "year", "week", "hosp_count")
    factor_tab <- factor_tab %>%
        dplyr::left_join(hosp_tab, by = c("country", "year", "week")) %>%
        dplyr::mutate(hosp_1m = 1000000 * hosp_count / population)
    
    # add testing per ECDC
    tstg_tab <- read.csv(gzfile(local_tstg_file))
    names(tstg_tab)[1] = "country"
    tstg_tab <- tstg_tab %>%
        dplyr::mutate(
            year = as.integer(substr(year_week, 1, 4)),
            week = as.integer(substr(year_week, 7, 8)),
            # note that ECDC positivity is next Monday's 7-day sum of positives,
            # divided by Sunday's 7-day sum of tests (different from BG charts)
            positivity = positivity_rate / 100
        ) %>%
        dplyr::rename(
            geo = country_code,
            tests = tests_done,
            tests_100k = testing_rate
        ) %>%
        dplyr::select(
            "geo", "year", "week", "tests", "tests_100k", "positivity"
        )
    factor_tab <- factor_tab %>%
        dplyr::left_join(tstg_tab, by = c("geo", "year", "week"))
    
    last_week <- function(code) {
        last_wk <- tt_tab %>%
            dplyr::filter(geo == code, d_2020 > 0) %>%
            dplyr::summarize(max(week)) %>%
            dplyr::pull()
        return(last_wk)
    }
    
    eu_data <- list(
        get_geo_names = get_geo_names,
        eu_codes = eu_codes,
        eu_map_grid = egrid,
        eurostat_deaths = dtab %>%
            dplyr::left_join(codes_tab %>% dplyr::select("geo", "geo_name"),
                             by = "geo"),
        factor_tab = factor_tab,
        last_week = last_week
    )
    
    return(function() return(eu_data))
}

get_eu_data <- make_eu_data()