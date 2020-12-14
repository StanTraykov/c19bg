# heatmap incidence per 100K in age bands

library(tidyverse)
library(zoo)
library(extrafont)            # } comment these out to use default fonts
loadfonts(device = "win")     # }
enc <- function(x) iconv(x, from = "UTF-8", to = "UTF-8") # UC hack for Windows

##### data
source(file.path("R", "bg_opendata.R")) # sets gen_data, age_data, obl_data

atab <- read.csv(file = age_data)
# from https://www.nsi.bg/bg/content/2977/%D0%BD%D0%B0%D1%81%D0%B5%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BF%D0%BE-%D1%81%D1%82%D0%B0%D1%82%D0%B8%D1%81%D1%82%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B8-%D1%80%D0%B0%D0%B9%D0%BE%D0%BD%D0%B8-%D0%B2%D1%8A%D0%B7%D1%80%D0%B0%D1%81%D1%82-%D0%BC%D0%B5%D1%81%D1%82%D0%BE%D0%B6%D0%B8%D0%B2%D0%B5%D0%B5%D0%BD%D0%B5-%D0%B8-%D0%BF%D0%BE%D0%BB
# population struct 0-19, 20-29, 30-39, 40-49,
#                   50-59, 60-69, 70-79, 80-89, 90+, total
pop_struct <- c(1315235, 692250, 956388, 1055350,
                953355, 938635, 701964, 301703, 36602, 6951482)
str_all <- enc("всички")

##### tidy
atab[-1, -1] <- atab[-1, -1] - atab[-nrow(atab), -1] # repl. totals w/incidence
atab <- atab[-1, ]                                   # (losing one day of data)
names(atab)[1] <- "date"
names(atab)[10] <- "90+"
names(atab) <- sub("X([0-9]+)\\.\\.\\.([0-9]+)", "\\1-\\2", names(atab))
atab[, 1] <- as.Date(atab[, 1])
atab[, 11] <- rowSums(atab[, 2:10])
names(atab)[11] <- str_all
# 7-day sums; filter only mondays to get approx. real week data
atab <- atab %>%
    mutate(across(!matches("date"),
                  function(x) rollapply(x,
                                        7,
                                        sum,
                                        align = "right",
                                        fill = NA))) %>%
    filter(weekdays(date) == "Monday", !is.na(`0-19`))

# divide by pop struct
atab[, -1] <- sweep(atab[, -1] * 100000, MARGIN = 2, pop_struct, `/`)
# make longer and add week number
atab <- atab %>%
    pivot_longer(cols = matches(paste0("0|", str_all)),
                 names_to = "group",
                 values_to = "incidence") %>%
    mutate(week = lubridate::isoweek(date) - 1)

################################################################################
# incidence/100K heatmap                                                       #
################################################################################
hplot <- function() {
    plt <- ggplot(data = atab,
                  aes(x = week,
                      y = group,
                      fill = incidence,
                      label = round(incidence))) +
        geom_tile() +
        geom_text(size = 3.6) +
        geom_hline(yintercept = 1.5, size = 1.5) +
        scale_fill_viridis_c() +
        #scale_fill_distiller(direction = 1) +
        theme_minimal() +
        theme(text = element_text(size = 14,
                                  family = windowsFont("Calibri")),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = 0.5,
                                        face = "bold")) +
        scale_x_continuous(breaks = min(atab[, 4]):max(atab[, 4])) +
        labs(title = paste(enc("Седмична заболеваемост на COVID-19"),
                           enc("(регистрирани нови случаи на 100 хил.)")),
             caption = paste(enc("*дясно подравнена 7-дневна сума спрямо"),
                             enc("докладваните в понеделник на следващата"),
                             enc("седмица; данни: data.egov.bg, НСИ")),
             fill = enc("c/100K"),
             x = enc("календарна седмица*"),
             y = enc("група"))
}

################################################################################
# example output                                                               #
################################################################################

save_all <- function() {
    ggsave(file = "heat.png", width = 11, height = 5, plot = hplot())
}