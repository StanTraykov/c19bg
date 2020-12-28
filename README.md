# Пакет за генериране на COVID-19 графики от отворените данни на България, EUROSTAT, ECDC, НСИ.

## Уеб страница

Графиките са достъпни на https://stantraykov.github.io/c19bg/

## Ако сте един от тях... 

Ако работите за държавата и имате нещо общо с обработката и публикацията на COVID-19 данни, изпълнете гражданския си дълг и станете [whistleblower](https://bg.wikipedia.org/wiki/%D0%A0%D0%B0%D0%B7%D0%BE%D0%B1%D0%BB%D0%B8%D1%87%D0%B8%D1%82%D0%B5%D0%BB). Разяснете на обществото как се взимат решенията да се скриват елементарни показатели (напр. географска и възрастова разбивка на смъртните случаи, но и много други).

## Инсталация

За инсталация директно от github, въведете в R или R Studio:

```R
devtools::install_github("StanTraykov/c19bg")
```
Ако нямате `devtools`, може да го инсталирате с `install.packages("devtools")`. Повече информация ще намерите на [GitHub страницата на devtools](https://github.com/r-lib/devtools).

### Импорт на шрифтове в R

Еднократно и незадължително, но препоръчително за по-добре излгеждащи графики.

```R
extrafont::font_import()
```
## Генериране на всички графики

```R
library(c19bg)

# сравнително бързо генериране на SVG
c19_save_all()

# растеризация с Inkscape, JPEG компресия с ImageMagick
# по-бавен вариант, трябва да ги имате инсталирани
c19_inkmagick()

# вкл. графики за умирания в други страни (ЕС+)
c19_inkmagick(d_all = TRUE) 
```

Пакетът ще се свърже с data.egov.bg, ECDC, EUROSTAT, за да вземе необходимите данни. Данните и графиките ще се запишат в подпапка `c19bg` на текущата (обикновено `Documents` при отваряне на R под Windows). Изчислението на R трае известно време.

*Забележка: Изтрийте файловете* `demo_*.tsv.gz`, `ecdc_*.csv.gz`, `bg_*.csv` *от папка* `downloads`*, ако желаете данните да се обновят.*

## Генериране на индивидуални графики

```R
library(c19bg)

# слчуаи по възрастови групи
my_plot <- c19_var_plot("age", roll_func = mean, roll_window = 7, line_legend = "0")

# извеждане на екран
my_plot # или print(my_plot) в не-интерактивен режим

# запис във файл
ggplot2::ggsave(file = "my_plot.png", width = 11, height = 7, plot = my_plot)

# R-графика на екран
# (отнема време, освен ако R вече не е изчислен) 
c19_r_plot() 

# помощ
?c19_var_plot
?c19_eu_weekly
?c19_oblasts
?c19_r_plot
```

## Уики

* [Методика за изчисление на R](https://github.com/StanTraykov/C19_BG/wiki/%D0%9C%D0%B5%D1%82%D0%BE%D0%B4%D0%B8%D0%BA%D0%B0-%D0%B7%D0%B0-%D0%B8%D0%B7%D1%87%D0%B8%D1%81%D0%BB%D0%B5%D0%BD%D0%B8%D0%B5-%D0%BD%D0%B0-R)
