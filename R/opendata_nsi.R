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
        # 2020 age struct
        # from https://www.nsi.bg/bg/content/2977/%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BF%D0%BE-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8-%D1%80%D0%B0%D0%B9%D0%BE%D0%BD%D0%B8-%D0%B2%D1%8A%D0%B7%D1%80%D0%B0%D1%81%D1%82-%D0%BC%D0%B5%D1%81%D1%82%D0%BE%D0%B6%D0%B8%D0%B2%D0%B5%D0%B5%D0%BD%D0%B5-%D0%B8-%D0%BF%D0%BE%D0%BB
        # age struct 10y  0-19,   20-29,   30-39,   40-49,   50-59,
        #                 60-69,   70-79,   80-89,     90+,   total
        age_struct_10 = c(1313252,  670806,  953393, 1050347,  956606,
                           919675,  716566,  298212,   37691, 6916548),
        # 2020 oblasts pop
        # from https://www.nsi.bg/bg/content/2975/%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BF%D0%BE-%D0%BE%D0%B1%D0%BB%D0%B0%D1%81%D1%82%D0%B8-%D0%BE%D0%B1%D1%89%D0%B8%D0%BD%D0%B8-%D0%BC%D0%B5%D1%81%D1%82%D0%BE%D0%B6%D0%B8%D0%B2%D0%B5%D0%B5%D0%BD%D0%B5-%D0%B8-%D0%BF%D0%BE%D0%BB
        #                   VID,    PVN,    DOB,     RSE,    SLS,    SHU,
        #                   VRC,    VTR,    MON,     RAZ,    VAR,    TGV,
        #                   GAB,    SFO,    LOV,     SOF,    BGS,    SZR,
        #                   PER,    SLV,    JAM,     PAZ,    HKV,    PDV,
        #                   KNL,    KRZ,    SML,     BLG
        oblasts_pops = c( 81212, 233438, 170298,  212729, 106852, 171781,
                         157637, 229718, 125395,  109810, 470124, 110027,
                         105788, 238476, 122490, 1308412, 409750, 311400,
                         120426, 182551, 116486,  251300, 223625, 666398,
                         116619, 160781, 101887,  301138)
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
