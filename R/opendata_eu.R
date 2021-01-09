# download data, if missing; return local filename

#' @importFrom magrittr %>%

process_eu_data <- function(redownload = FALSE) {
    tib_eu <- function(url, file, zip = FALSE, tab_delim = FALSE, ...) {
        if (redownload || !datafile_exists(file)) {
            download(url, file, zip)
        }
        if (tab_delim)
            t <- tib_read_tsv(file, ...)
        else
            t <- tib_read_csv(file, ...)
        return(t)
    }

    # ECDC nat'l case death
    ncd_tab <- tib_eu(
        url = "https://opendata.ecdc.europa.eu/covid19/nationalcasedeath/csv",
        file = "ecdc_ncd.csv.gz",
        zip = TRUE,
        col_types = "cccdcdcddc"
    )
    # ECDC hosp
    hosp_tab <- tib_eu(
        url = paste0("https://opendata.ecdc.europa.eu/covid19/",
                     "hospitalicuadmissionrates/csv/data.csv"),
        file = "ecdc_hosp.csv.gz",
        zip = TRUE,
        col_types = "ccDcd__"
    )
    # EUROSTAT deaths 10yr bands
    dtab <- tib_eu(
        url = paste0("https://ec.europa.eu/eurostat/estat-navtree-portlet-prod",
                     "/BulkDownloadListing?file=data/demo_r_mwk_10.tsv.gz"),
        file = "eurostat_dmwk10.tsv.gz",
        tab_delim = TRUE,
        col_types = readr::cols()
    )
    # ECDC testing
    tstg_tab <- tib_eu(
        url = "https://opendata.ecdc.europa.eu/covid19/testing/csv",
        file = "ecdc_tstg.csv.gz",
        zip = TRUE,
        col_types = "cccddddd_"
    )

    # country names
    bg_names <- intern_data$bg_cnames %>%
        dplyr::rename(geo_name = bg_name)
    codes_tab <- intern_data$ccodes %>%
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

    dtab <- cbind(stringr::str_split_fixed(dtab[[1]], ",", 4), dtab[, -1])
    names(dtab) <- c("age", "sex", "unit", "geo", names(dtab)[-(1:4)])
    dtab <- dtab %>%
        dplyr::select(dplyr::matches("(201[0-9]|2020)W[0-5]|age|geo|sex")) %>%
        tidyr::pivot_longer(cols = tidyr::matches("20..W"),
                            names_to = c("year", "week"),
                            names_pattern = "(....)W(..)",
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
    cmp_tab <- dplyr::left_join(mean_tab, tt_tab, by = c("geo", "week")) %>%
        dplyr::mutate(excess_deaths = d_2020 - mean_deaths) %>%
        dplyr::mutate(excess_deaths_star = d_2020 - mean_deaths_star) %>%
        dplyr::ungroup()

    ## ECDC nat'l cases / deaths
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
    hosp_tab <- hosp_tab %>%
        dplyr::filter(indicator == "Daily hospital occupancy") %>%
        dplyr::rename(hosp_count = value) %>%
        dplyr::select("country", "date", "year_week", "hosp_count") %>%
        dplyr::mutate(
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

    return(eu_data)
}

c19_make_eu_data <- function() {
    processed_data <- list()
    get_data <- function(reload = FALSE, redownload = FALSE) {
        if ((length(processed_data) == 0) || reload || redownload)
            processed_data <<- process_eu_data(redownload = redownload)
        invisible(processed_data)
    }
    return(get_data)
}

#' Provides access to ECDC/EUROSTAT COVID-19 data.
#'
#' @param reload reload from disk
#' @param redownload refresh from internet
#'
#' @export
#' @family data funcs
c19_eu_data <- c19_make_eu_data()