# save BG open data

api_url <- 'https://data.egov.bg/api/getResourceData'
resources <- list(bg_gen = "e59f95dd-afde-43af-83c8-ea2916badd19",
                  bg_obl = "cb5d7df0-3066-4d7a-b4a1-ac26525e0f0c",
                  bg_age = "8f62cfcf-a979-46d4-8317-4e1ab9cbd6a8")

csv_name <- function(x) file.path("data", paste0(x, ".csv"))

resources_to_csv <- function(resources, sleep_time = 1) {
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
gen_data <- csv_name("bg_gen")
age_data <- csv_name("bg_age")
obl_data <- csv_name("bg_obl")