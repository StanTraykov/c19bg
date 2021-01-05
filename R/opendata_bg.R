# save BG open data

#' @importFrom magrittr %>%

resources_to_csv <- function(redownload = FALSE, sleep_time = 0.3) {
    dot_csv <- function(x) paste0(x, ".csv")
    api_url <- "https://data.egov.bg/api/getResourceData"
    resources <- list(bg_gen = "e59f95dd-afde-43af-83c8-ea2916badd19",
                      bg_obl = "cb5d7df0-3066-4d7a-b4a1-ac26525e0f0c",
                      bg_age = "8f62cfcf-a979-46d4-8317-4e1ab9cbd6a8",
                      bg_tst = "0ce4e9c3-5dfc-46e2-b4ab-42d840caab92")
    for (res_id in names(resources)) {
        if (redownload || !datafile_exists(dot_csv(res_id))) {
            uri <- resources[[res_id]]
            message(paste("Getting data.egov.bg uri:", uri))
            api_data <- list(resource_uri = uri)
            resp <- httr::POST(api_url, body = api_data, encode = "json")
            parsed_data <- httr::content(resp, as = "parsed")[["data"]]
            fields <-  purrr::transpose(parsed_data[-1],
                                        .names = parsed_data[[1]])
            tib <- tibble::as_tibble(lapply(fields, unlist))
            tib_write_csv(tib, dot_csv(res_id))
            Sys.sleep(sleep_time)
        }
    }
}

process_bg_data <- function(redownload = FALSE) {
    resources_to_csv(redownload)

    ##### age tab
    age_tab <- tib_read_csv(
        file = "bg_age.csv",
        col_types = paste0("D", strrep("i", 9))
    )
    # repl. totals with daily incidence (losing one day of data)
    age_tab[-1, -1] <- age_tab[-1, -1] - age_tab[-nrow(age_tab), -1]
    age_tab <- age_tab[-1, ]
    names(age_tab)[1] <- "date"
    names(age_tab) <- gsub(" ", "", names(age_tab))

    ##### oblasts tab
    obl_tab <- tib_read_csv(
        file = "bg_obl.csv",
        col_types = paste0("D", strrep("i", 56)))
    # repl. totals with daily incidence (losing one day of data)
    obl_tab[-1, -1] <- obl_tab[-1, -1] - obl_tab[-nrow(obl_tab), -1]
    obl_tab <- obl_tab[-1, ]
    names(obl_tab)[1] <- "date"

    ##### gen tab (+ test tab)
    gen_tab <- tib_read_csv(
        file = "bg_gen.csv",
        col_types = paste0("D", strrep("i", 11))
    )
    tst_tab <- tib_read_csv(
        file = "bg_tst.csv",
        col_types = paste0("D", strrep("i", 12))
    )
    # new_tests_GEN is an inconsistent field containing PCR tests until Jan 5
    # (=all tests until Dec 24) and then all tests from Jan 6, 2021.
    names(gen_tab) <- c(
        "date", "tests", "new_tests_GEN", "cases", "active_cases", "new_cases",
        "hospitalized", "in_icu", "recovered", "newly_recovered", "deaths",
        "new_deaths"
    )
    names(tst_tab) <- c(
        "date", "tests", "pcr_tests", "ag_tests", "new_tests", "new_pcr_tests",
        "new_ag_tests", "cases", "pcr_cases", "ag_cases", "new_cases",
        "new_pcr_cases", "new_ag_cases"
    )
    begin_ag_tests <- as.Date("2020-12-24")

    # gen_tst_by <- c("date", "tests", "cases", "new_cases", "new_pcr_cases")
    # ideally, above should work, but open data is inconsistent.
    gen_tst_by <- c("date", "tests", "cases", "new_cases")

    # override new_ag_tests. open data is bogus.
    tst_tab$new_ag_tests[-1] <- tst_tab$ag_tests[-1] -  #TODO remove
        tst_tab$ag_tests[-nrow(tst_tab)]                #TODO remove

    gen_tab <- gen_tab %>%
        dplyr::left_join(
            tst_tab,
            by = gen_tst_by
        ) %>%
        dplyr::mutate(
            new_pcr_tests = ifelse(date >= begin_ag_tests,
                                   new_pcr_tests,
                                   new_tests_GEN),
            new_tests = new_pcr_tests + ifelse(date >= begin_ag_tests,
                                               new_ag_tests,
                                               0),
            new_pcr_cases = ifelse(date >= begin_ag_tests,
                                   new_pcr_cases,
                                   new_cases),
            pcr_cases = ifelse(date >= begin_ag_tests, pcr_cases, cases),
            pcr_tests = ifelse(date >= begin_ag_tests, pcr_cases, tests)
        )

    if (!identical(as.integer(unname(rowSums(age_tab[, 2:10]))),
                   unname(gen_tab$new_cases[-1])))
        stop("failed sanity check: new case counts != summed age brackets")

    ##### gen tab incl. historical data
    hist_tab <- intern_data$pre_opendata %>%
        dplyr::mutate(
            date = as.Date(date),
            new_pcr_tests = new_tests,
            new_pcr_cases = new_cases,
            pcr_cases = cases
        )
    hgn_tab <- dplyr::bind_rows(hist_tab, gen_tab)

    ##### oblasts grid
    ggrid <- intern_data$oblasts

    ##### bg_data
    bg_data <- list(
        general = gen_tab,
        gen_inc_hist = hgn_tab,
        age = age_tab,
        subdivs = obl_tab,
        subdiv_grid = ggrid
    )
    return(bg_data)
}

c19_make_bg_data <- function() {
    processed_data <- list()
    get_data <- function(reload = FALSE, redownload = FALSE) {
        if ((length(processed_data) == 0) || reload || redownload)
            processed_data <<- process_bg_data(redownload = redownload)
        invisible(processed_data)
    }
    return(get_data)
}

#' Provides access to Bulgarian COVID-19 data from data.egov.bg.
#'
#' @param reload reload from disk
#' @param redownload refresh from internet
#'
#' @export
#' @family data funcs
c19_bg_data <- c19_make_bg_data()
