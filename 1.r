# ������� 1. ������� 1

# ��� ������� 30 ����������� ����������� ������� � 2001 ����,
# ���� ��� �������� ������� ����� �������� ���������� �� ���������� 8 ���,
# � 12 ��������� ������������ �� ��������� ������� di ��������������, ��� ���� ������,
# ����� ������������� ����������� ���� ���� 8 ��������, �� ��������, ��� ����� �� ����� �������� ������ �������� ������,
# � ��������� ���������� 3 ������
# ������ 30 - ������������ �������, ���������� �������: 46.357843, 48.056044

# ��������� ����������:
library(tidyverse)
library(rnoaa)
library(lubridate)

# �������� ������� � ������� ��� �������:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 #  ����������� ������������� ���
Qj = 1600 # ������������ ������ ��������
Lj = 2.2 #  ����� ������ �������� � �������� ���������
Ej = 25 #   ����������� ��������� ��������

# �������� ������ � �������������:
#station_data = ghcnd_stations()
# �������� � ���� "station_data.�sv".
#write.csv(station_data, "station_data.csv")

# �������� ������ � ������ ��� ���������� ������:
station_data = read.csv("station_data.csv")
# ������� �������� � ���������� ������� �������:
astrakhan = data.frame(id = "ASTRAKAN", latitude = 46.357843,  longitude = 48.056044)
# �������� ����� 7 ��������� ������������ � ������� �� �������������� ����������� �� 1993-2000 ���
#   � �������� � ������.
astrakhan_around = meteo_nearby_stations(lat_lon_df = astrakhan, station_data = station_data,
                                        limit = 12, var = "TAVG",
                                        year_min = 1993, year_max = 2000)

# �������� �������, � ������� ����� ��������� ������ �� ���� �������:
all_data = tibble()
di = tibble()
for (i in 1:12)
{
  # ��������� ������� �� 7 ���������:
  astrakhan_id = astrakhan_around[["ASTRAKAN"]][["id"]][i]
  # �������� ������ ��� �������:
  data = meteo_tidy_ghcnd(stationid = astrakhan_id,
                          var="TAVG",
                          date_min="1993-01-01",
                          date_max="2001-12-31")
  
  all_data = bind_rows(all_data, data %>%
                         mutate(year = year(date), month = month(date)) %>%
                         group_by(month, year) %>%
                         summarise (tavg = sum(tavg[tavg>50])/10 )
                       )
  
  #������ d ��� 2001 ����
  di = bind_rows( di, data %>%
        mutate(year = year(date), month = month(date)) %>%
        filter(year == "2001") %>%
        group_by(month) %>%
        summarise (d = length(tavg[tavg>80])/31)
        )
  di = di %>% group_by(month) %>% summarise (d = mean(d))
}


# ��������� � ������� ���������� � ������� clean_data.
clean_data = all_data %>%
  # ������� ������� month ��� ����������� ������:
  group_by(month) %>%
  # ������ �������� d � c���� �������� ��������� ��� ������ �������:
  summarise(s = mean(tavg, na.rm = TRUE)) %>%
  #������� ������ �� ������� � ����������� d
  inner_join(di) %>%
  # ������� ������� ��� �������:
  mutate (a = af, b = bf) %>%
  # ���������� ����������� ��� ������� ������:
  mutate (fert = ((a + b * 1.0 * s) * d * Kf) / (Qj * Lj * (100-Ej)) ) %>%
  filter (month>=4 & month<=7)
#�������� �������, ����������� ������� � ������������ ������� � 2001 ���� ��������� (�/��):
Yield = sum(clean_data$fert); Yield