# save BG open data

library(magrittr)

enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows

make_bg_data <- function() {
    csv_name <- function(x) file.path("data", paste0(x, ".csv"))
    
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
    age_tab <- read.csv(csv_name("bg_age")) # cases by age band
    # repl. totals with daily incidence (losing one day of data)
    age_tab[-1, -1] <- age_tab[-1, -1] - age_tab[-nrow(age_tab), -1] 
    age_tab <- age_tab[-1, ]                                   
    age_tab[, 1] <- as.Date(age_tab[, 1])
    names(age_tab)[1] <- "date"
    names(age_tab)[10] <- "90+"
    names(age_tab) <- sub("X([0-9]+)\\.\\.\\.([0-9]+)", "\\1-\\2",
                          names(age_tab))
    
    ##### oblasts tab
    obl_tab <- read.csv(file = csv_name("bg_obl"))
    # repl. totals with daily incidence (losing one day of data)
    obl_tab[-1, -1] <- obl_tab[-1, -1] - obl_tab[-nrow(obl_tab), -1]
    obl_tab <- obl_tab[-1, ]
    obl_tab[, 1] <- as.Date(obl_tab[, 1])
    names(obl_tab)[1] <- "date"
    
    ##### gen tab (+ test tab)
    gen_tab <- read.csv(file = csv_name("bg_gen"))
    tst_tab <- read.csv(file = csv_name("bg_tst"))
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
    gen_tab <- gen_tab %>%
        dplyr::left_join(
            tst_tab,
            by = c("date", "tests", "cases", "new_cases", "new_pcr_tests")
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
    hist_tab <- read.csv(file.path("historical_data", "pre_opendata.csv")) %>%
        dplyr::mutate(
            date = as.Date(date),
            new_pcr_tests = new_tests,
            new_pcr_cases = new_cases,
            pcr_cases = cases
        )
    hgn_tab <- dplyr::bind_rows(hist_tab, gen_tab)
    
    ##### oblasts grid
    ggrid <- data.frame( # bg oblast grid from geofacet pkg; modified codes/names
        code = c("VID", "PVN", "DOB", "RSE", "SLS", "SHU",
                 "VRC", "VTR", "MON", "RAZ", "VAR", "TGV", "GAB", "SFO", "LOV",
                 "SOF", "BGS", "SZR", "PER", "SLV", "JAM", "PAZ", "HKV", "PDV",
                 "KNL", "KRZ", "SML", "BLG"),
        name = c(enc("Видин"), enc("Плевен"), enc("Добрич"), enc("Русе"),
                 enc("Силистра"), enc("Шумен"), enc("Враца"),
                 enc("Велико Търново"), enc("Монтана"), enc("Разград"),
                 enc("Варна"), enc("Търговище"), enc("Габрово"),
                 enc("Софийска област"), enc("Ловеч"), enc("София-град"),
                 enc("Бургас"), enc("Стара Загора"), enc("Перник"),
                 enc("Сливен"), enc("Ямбол"), enc("Пазарджик"),
                 enc("Хасково"), enc("Пловдив"), enc("Кюстендил"),
                 enc("Кърджали"), enc("Смолян"), enc("Благоевград")),
        row = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 4L, 4L,
                4L, 4L, 4L, 5L, 5L, 5L, 5L, 5L, 6L, 6L, 6L),
        col = c(1L, 3L, 6L, 4L, 5L, 5L, 2L, 3L, 1L, 4L, 5L, 4L, 3L, 1L, 2L, 2L, 5L,
                3L, 1L, 4L, 5L, 2L, 4L, 3L, 1L, 4L, 3L, 2L))
    
    ##### bg_data
    bg_data <- list(
        general = gen_tab,
        gen_inc_hist = hgn_tab,
        age = age_tab,
        subdivs = obl_tab,
        subdiv_grid = ggrid
    )
    return(function() return(bg_data))
}

get_bg_data <- make_bg_data()