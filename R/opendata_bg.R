# save BG open data

#' @importFrom magrittr %>%

process_bg_data <- function() {
    down_dir <- getOption("c19bg.down_dir")
    data_dir <- getOption("c19bg.data_dir")
    if (!file.exists(down_dir)) dir.create(down_dir, recursive = TRUE)

    csv_name <- function(x) file.path(down_dir, paste0(x, ".csv"))
    read_tab <- getOption("c19bg.rt")
    read_csv <- getOption("c19bg.rc")

    resources_to_csv <- function(resources, sleep_time = 0.3) {
        api_url <- 'https://data.egov.bg/api/getResourceData'
        resources <- list(bg_gen = "e59f95dd-afde-43af-83c8-ea2916badd19",
                          bg_obl = "cb5d7df0-3066-4d7a-b4a1-ac26525e0f0c",
                          bg_age = "8f62cfcf-a979-46d4-8317-4e1ab9cbd6a8",
                          bg_tst = "0ce4e9c3-5dfc-46e2-b4ab-42d840caab92")
        for (fn in names(resources)) {
            full_name <- csv_name(fn)
            if (!file.exists(full_name)) {
                uri <- resources[[fn]]
                api_data <- list(resource_uri = uri)
                resp <- httr::POST(api_url, body = api_data, encode = "json")
                parsed_data <- httr::content(resp, as = "parsed")[["data"]]
                fields <- purrr::transpose(parsed_data[-1])
                names(fields) <- parsed_data[[1]]
                df <- data.frame(sapply(fields, unlist))
                write.csv(df, full_name, row.names = FALSE)
                Sys.sleep(sleep_time)
            }
        }
    }

    resources_to_csv(resources)

    ##### age tab
    age_tab <- read_csv(csv_name("bg_age")) # cases by age band
    # repl. totals with daily incidence (losing one day of data)
    age_tab[-1, -1] <- age_tab[-1, -1] - age_tab[-nrow(age_tab), -1]
    age_tab <- age_tab[-1, ]
    age_tab[, 1] <- as.Date(age_tab[, 1])
    names(age_tab)[1] <- "date"
    names(age_tab)[10] <- "90+"
    names(age_tab) <- sub("X([0-9]+)\\.\\.\\.([0-9]+)", "\\1-\\2",
                          names(age_tab))

    ##### oblasts tab
    obl_tab <- read_csv(file = csv_name("bg_obl"))
    # repl. totals with daily incidence (losing one day of data)
    obl_tab[-1, -1] <- obl_tab[-1, -1] - obl_tab[-nrow(obl_tab), -1]
    obl_tab <- obl_tab[-1, ]
    obl_tab[, 1] <- as.Date(obl_tab[, 1])
    names(obl_tab)[1] <- "date"

    ##### gen tab (+ test tab)
    gen_tab <- read_csv(file = csv_name("bg_gen"))
    tst_tab <- read_csv(file = csv_name("bg_tst"))
    names(gen_tab) <- c(
        "date", "tests", "new_pcr_tests", "cases", "active_cases", "new_cases",
        "hospitalized", "in_icu", "recovered", "newly_recovered", "deaths",
        "new_deaths"
    )
    names(tst_tab) <- c(
        "date", "tests", "pcr_tests", "ag_tests", "new_tests", "new_pcr_tests",
        "new_ag_tests", "cases", "pcr_cases", "ag_cases", "new_cases",
        "new_pcr_cases", "new_ag_cases"
    )
    gen_tab[, 1] <- as.Date(gen_tab[, 1])
    tst_tab[, 1] <- as.Date(tst_tab[, 1])
    begin_ag_tests = as.Date("2020-12-24")


    # gen_tst_by <- c("date", "tests", "cases", "new_cases", "new_pcr_cases")
    # TODO remove the following and uncomment above when the open data about
    # new_pcr_tess gets fixed.
    gen_tst_by <- c("date", "tests", "cases", "new_cases") #TODO remove
    tst_tab <- tst_tab %>% dplyr::select(-"new_pcr_tests") #TODO remove


    # TODO remove the following when open data about daily ag tests gets fixed.
    # We now calculate dialy ag tests because the open data field is cumulative
    # instead of daily (new_ag_tests wrongly matches ag_tests).
    tst_tab$new_ag_tests[-1] = tst_tab$new_ag_tests[-1] -  #TODO remove
        tst_tab$new_ag_tests[-nrow(tst_tab)]               #TODO remove


    gen_tab <- gen_tab %>%
        dplyr::left_join(
            tst_tab,
            by = gen_tst_by
        ) %>%
        dplyr::mutate(
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
    hist_tab <- read_csv(file.path(data_dir, "pre_opendata.csv")) %>%
        dplyr::mutate(
            date = as.Date(date),
            new_pcr_tests = new_tests,
            new_pcr_cases = new_cases,
            pcr_cases = cases
        )
    hgn_tab <- dplyr::bind_rows(hist_tab, gen_tab)

    ##### oblasts grid
    ggrid <- read_tab(file.path(data_dir, "oblasts.cfg"), header = TRUE)

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

#' @export
make_c19_bg_data <- function() {
    processed_data <- list()
    get_data <- function() {
        if (length(processed_data) == 0)
            processed_data <<- process_bg_data()
        return(processed_data)
    }
    return(get_data)
}

#' @export
c19_bg_data <- make_c19_bg_data()
