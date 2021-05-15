# NSI open data

#' @importFrom magrittr %>%

process_nsi_data <- function(redownload = FALSE) {
    # wkd_tab <- tib_read_csv2(file = "nsi_wkd.csv",
    #                          col_names = FALSE,
    #                          col_types = paste0(strrep("c", 162), "_"))
    # names(wkd_tab)[1:3] <- c("year", "oblast", "age")
    # names(wkd_tab)[-(1:3)] <- paste(wkd_tab[1,-(1:3)],
    #                                 wkd_tab[2,-(1:3)],
    #                                 sep = ":")
    # wkd_tab <- wkd_tab[-(1:2),] %>%
    #     dplyr::mutate(
    #         age = gsub(paste(" |", tra("nsi_gdot")),"", age),
    #         oblast = sub(tra("nsi_sofia_obl"), tra("Sofijska oblast"), oblast),
    #         oblast = sub(tra("nsi_sofia_grad"), tra("Sofia-grad"), oblast)
    #     ) %>%
    #     tidyr::pivot_longer(
    #         cols = tidyr::matches(tra("nsi_week")),
    #         names_to = c("week", "sex"),
    #         names_pattern = paste(tra("nsi_week"), "([0-9]+):(.*)"),
    #         values_to = "deaths"
    #     )
    ##### nsi_data
    nsi_data <- list(
        # from https://www.nsi.bg/bg/content/2977/%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BF%D0%BE-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8-%D1%80%D0%B0%D0%B9%D0%BE%D0%BD%D0%B8-%D0%B2%D1%8A%D0%B7%D1%80%D0%B0%D1%81%D1%82-%D0%BC%D0%B5%D1%81%D1%82%D0%BE%D0%B6%D0%B8%D0%B2%D0%B5%D0%B5%D0%BD%D0%B5-%D0%B8-%D0%BF%D0%BE%D0%BB
        # age struct 10      0-19,   20-29,   30-39,   40-49,   50-59,
        #                   60-69,   70-79,   80-89,     90+,   total
        age_struct_10 = c(1315235,  692250,  956388, 1055350,  953355,
                           938635,  701964,  301703,   36602, 6951482),
        # 2019 oblasts pop
        oblasts_pops = c( 82835, 236305, 171809,  215477, 108018, 172262,
                         159470, 232568, 127001,  110789, 469885, 110914,
                         106598, 226671, 122546, 1328790, 409265, 313396,
                         119190, 184119, 117335,  252776, 225317, 666801,
                         116915, 158204, 103532,  302694)
        #weekly_deaths = wkd_tab
    )
    return(nsi_data)
}

c19_make_nsi_data <- function() {
    processed_data <- list()
    get_data <- function(reload = FALSE, redownload = FALSE) {
        if ((length(processed_data) == 0) || reload || redownload)
            processed_data <<- process_nsi_data(redownload = redownload)
        invisible(processed_data)
    }
    return(get_data)
}

#' Provide access to NSI data
#'
#' @param reload reload from disk
#' @param redownload refresh from internet
#'
#' @export
#' @family data funcs
c19_nsi_data <- c19_make_nsi_data()
