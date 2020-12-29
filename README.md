# c19bg

Пакет за генериране на COVID-19 графики от отворените данни на България, EUROSTAT, ECDC, НСИ.

## Уеб страница

Графиките са достъпни на https://stantraykov.github.io/c19bg/

## Ако сте един от тях... 

Ако работите за държавата и имате нещо общо с обработката и публикацията на COVID-19 данни, изпълнете гражданския си дълг и станете [whistleblower](https://bg.wikipedia.org/wiki/%D0%A0%D0%B0%D0%B7%D0%BE%D0%B1%D0%BB%D0%B8%D1%87%D0%B8%D1%82%D0%B5%D0%BB). Разяснете на обществото как се взимат решенията да се скриват елементарни показатели (напр. географска и възрастова разбивка на смъртните случаи, но и много други).

## Инсталация

За инсталация директно от *GitHub*, въведете в [R](https://www.r-project.org/) или [RStudio](https://rstudio.com/):

```R
devtools::install_github("StanTraykov/c19bg")
```
Ако нямате пакет `devtools`, може да го инсталирате в R с `install.packages("devtools")`. Повече информация ще намерите на [GitHub страницата на devtools](https://github.com/r-lib/devtools).

### Опционално: extrafont

Еднократно и незадължително. Позволява ползването на шрифтове при извеждането на екран и записването в растерни формати (PNG, JPEG). **Не влияе** на векторния SVG изход (вкл. по-нататъшна обработка с външни програми). Единствената засегната графика при нормална употреба (генериране на всички графики) е хийтмапът на заболеваемостта.

```R
install.packages("extrafont")
extrafont::font_import() # отнема време
```

## Генериране на всички графики

```R
library(c19bg)

# сравнително бързо генериране на SVG
c19_save_all()

# SVG изход + растеризация с Inkscape, JPEG компресия
# с ImageMagick -- по-бавен вариант, трябва да ги имате
# инсталирани. Вижте раздел Опции по-долу.
c19_inkmagick()  # генерира SVG, PNG и JPEG файлове

# вкл. графики за умирания в други страни (ЕС+)
c19_inkmagick(d_all = TRUE) 

# презареждане на данните
c19_reload() # от локално свалени файлове
c19_reload(redownload = TRUE) # от Интернет
```

Пакетът ще се свърже с data.egov.bg, ECDC, EUROSTAT, за да вземе необходимите данни. Данните и графиките ще се запишат в подпапка `c19bg` на текущата (обикновено `Documents` при отваряне на R под Windows). Изчислението на R трае известно време.

## Генериране на единични графики

```R
library(c19bg)

# слчуаи по възрастови групи
my_plot <- c19_var_plot("age", roll_func = mean, roll_window = 7, line_legend = "0")

# извеждане на екран
my_plot # или print(my_plot) в не-интерактивен режим

# запис във файл
if (.Platform$OS.type == "windows" &&
        "extrafont" %in% rownames(installed.packages())) {
    extrafont::loadfonts(device = "win")
}
ggplot2::ggsave(file = "my_plot.png", width = 11, height = 7, plot = my_plot)

# R-графика на екран
# (отнема време, освен ако R вече не е изчислен) 
c19_r_plot() 

# помощ
?c19_var_plot
?c19_eu_weekly
?c19_oblasts
?c19_r_plot
#...
```

## Опции

```R
# промяна на шрифт 
options(c19bg.font_family = "Calibri")
options(c19bg.font_scale = 1) # скалиране на всички текстове (напр. 0.8, 1.1)
options(c19bg.font_size = 14) # базов размер (пробвайте първо font_scale)

options(c19bg.output_dir = "c19bg/plots")
options(c19bg.down_dir = "c19bg/downloads")

# зa c19_inkmagick()
options(c19bg.output = list(
    inkopts = "-w %d --export-filename",
    mgkopts = "-quality 100", 
    pixwidth = 1375,
    width = 11,
    height = 7,
    inkscape = "\"C:\\Program Files\\Inkscape\\bin\\inkscape.exe\"", 
    magick = "magick"  # работи, ако е в PATH
))

# изобразяване на всички опции за c19bg
names(options())[grep("c19bg",names(options()))]
```

## Данни
```R
library(c19bg)

# Данните могат да се достъпят с тези две функции
# (те се използват и от графичните фукнции)

eu_data <- c19_eu_data()
bg_data <- c19_bg_data()

# При първо изпълнение, те свалят и обработват данни от
# data.egov.bg, ECDC, и EUROSTAT. Това трае известно време.
# Последващи повиквания са евтини, освен ако не се изиска
# презареждане.

c19_eu_data(reload = TRUE)
c19_bg_data(reload = TRUE)

# Съдържанието може да се разглежда/задълбава в браузера
# на данни на RStudio.

# ECDC/EUROSTAT седмични случаи, смъртни случаи от COVID-19,
# свръхсмъртност, фактори на надвишаване, брой хоспитализирани,
# тестове, позитивност

View(eu_data$factor_tab)

# Умирания по седмици от EUROSTAT

View(eu_data$eurostat_deaths)

# За България

View(bg_data$gen_inc_hist) # обща, вкл. ограничен набор данни
                           # от преди да отворят данните
View(bg_data$age)          # по възраст (днвени)
View(bg_data$subdivs)      # по области (дневни)
```

Примерна заявка (плаващо средно 7 дни по области)
```R
library(dplyr)
library(tidyr)
library(c19bg)

bg_data <- c19_bg_data()
oblasts_table <- bg_data$subdivs %>%
    select(!ends_with("_ACT")) %>%
    mutate(date = as.Date(date)) %>%
    pivot_longer(cols = !matches("date"),
                 names_to = "oblast",
                 values_to = "cases") %>%
    group_by(oblast) %>%
    mutate(mva7 = zoo::rollapply(cases,
                                 7,
                                 mean,
                                 align = "right",
                                 fill = NA))
```
## Уики

* [Методика за изчисление на R](https://github.com/StanTraykov/C19_BG/wiki/%D0%9C%D0%B5%D1%82%D0%BE%D0%B4%D0%B8%D0%BA%D0%B0-%D0%B7%D0%B0-%D0%B8%D0%B7%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BD%D0%B0-R)
