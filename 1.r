# Задание 1. Вариант 1

# для региона 30 рассчитайте урожайность пшеницы в 2001 году,
# взяв для рассчета средние суммы активных температур за предыдущие 8 лет,
# с 12 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца,
# когда среднедневные температуры были выше 8 градусов, но учитывая, что посев не может начаться раньше середины апреля,
# а вегетация составляет 3 месяца
# Регион 30 - Астраханская область, координаты столицы: 46.357843, 48.056044

# Подключим библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузим данные о метеостанциях:
#station_data = ghcnd_stations()
# Сохраним в файл "station_data.сsv".
#write.csv(station_data, "station_data.csv")

# Сохраним данные в вектор для дальнейшей работы:
station_data = read.csv("station_data.csv")
# Зададим название и координаты столицы региона:
astrakhan = data.frame(id = "ASTRAKAN", latitude = 46.357843,  longitude = 48.056044)
# Выполним поиск 7 ближайших метеостанций с данными по среднемесячной температуре за 1993-2000 год
#   и сохраним в вектор.
astrakhan_around = meteo_nearby_stations(lat_lon_df = astrakhan, station_data = station_data,
                                        limit = 12, var = "TAVG",
                                        year_min = 1993, year_max = 2000)

# Создадим таблицу, в которую будем сохранять данные со всех станций:
all_data = tibble()
di = tibble()
for (i in 1:12)
{
  # Определим станцию из 7 ближайших:
  astrakhan_id = astrakhan_around[["ASTRAKAN"]][["id"]][i]
  # Загрузим данные для станции:
  data = meteo_tidy_ghcnd(stationid = astrakhan_id,
                          var="TAVG",
                          date_min="1993-01-01",
                          date_max="2001-12-31")
  
  all_data = bind_rows(all_data, data %>%
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         summarise (tavg = sum(tavg[tavg>50])/10 )
                       )
  
  #найдем d для 2001 года
  di = bind_rows( di, data %>%
        mutate(year = year(date), month = month(date)) %>%
        filter(year == "2001") %>%
        group_by(month) %>%
        summarise (d = length(tavg[tavg>80])/31)
        )
  di = di %>% group_by(month) %>% summarise (d = mean(d))
}


# Изменения в таблице сохранятся в векторе clean_data.
clean_data = all_data %>%
  # Добавим колонку month для группировки данных:
  group_by(month) %>%
  # Найдем месячный d и cумму активных тмператур для каждой станции:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #добавим данные из таблицы с показателем d
  inner_join(di) %>%
  # Добавим колонки для расчета:
  mutate (a = af, b = bf) %>%
  # Рассчитаем урожайность для каждого месяца:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) ) %>%
  filter (month>=4 & month<=7)
#Согласно расчету, урожайность пшеницы в Астраханской области в 2001 году составила (ц/га):
Yield = sum(clean_data$fert); Yield