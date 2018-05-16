library(rvest)
library(ggplot2)
library(dplyr)
library(tidyr)
library(ggmap)
#RSelenium может доставить некоторые проблемы при установке. Решение - установка из Гитхаба пакетов binman и wdman
library(RSelenium)
library(devtools)
#devtools::install_github("ropensci/RSelenium")
#devtools::install_github("johndharrison/wdman")
#devtools::install_github("johndharrison/binman")

library(forecast)
library(zoo)
library(grid)
library(gridExtra)
library(caret)
library(xgboost)
library(mlr)
library(rJava)
library(ggthemes)




# Нас интересуют месяцы - март, апрель и май
mnth <- 3:5
# Дни
days <- seq(1, 31, 1)
# Цикл генерации страниц на сайте BlaBlaCar
url.t <- c()
urls <- c()
for(i in mnth){
  for(j in days){
    url <- paste0("https://www.blablacar.ru/ride-sharing/kazan/?db=",
                  j, "/", i,
                  "/2018&fn=Казань,+Россия&fc=55.8304307%7C49.0660806&fcc=RU&fp=0&tn=&sort=trip_date&order=asc&radius=15&limit=10&page=1&v=default")
    url.t <- c(url.t, url)
  }
  urls <- c(urls, url.t)
  url.t <- c()
}


# Удаление лишних ссылок (то есть тех дат, по которым BlaBlaCar отказывается выдавать данные, то есть с 1 по 19 марта)
urls <- urls[20:82]
urls <- urls[-43] # удаление 31 апреля


#### ПАРСИНГ ####

# Создание датасета для хранение полученных данных
blblcars <- data.frame(Name = character(),
                       Age = character(),
                       Date = character(),
                       Time = character(),
                       City = character(),
                       Price = character(),
                       stringsAsFactors = FALSE)

# Запуск сервера RSelenium №1
rD <- rsDriver(port = 4448L, browser = "chrome", version = "latest")
remDr <- rD$client

# Запуск сервера RSelenium №2 (альтернативный вариант на случай, если первый не будет работать)
driver<- rsDriver()
remDr <- driver[["client"]]

for (j in urls) {
  
  # Переход на страницу
  remDr$navigate(j)
  
  # Перерыв на 3 секунды, иначе браузер не успевает сформировать страницу
  Sys.sleep(3)
  
  # Получение данных со страницы
  html <- remDr$getPageSource()
  html <- read_html(html[[1]])
  
  # Имена
  names <- html_nodes(html, ".ProfileCard-info--name")
  names.i <- c()
  if (length(names) == 0) {
    names.i <- NA
  } else {
    for (i in 1:length(names)) {
      names.i[i] <- gsub(".*\n                            |\n.*", 
                         "", names[[i]])
    }
  }
  
  # Возраст
  age <- html_nodes(html, ".u-truncate+ .ProfileCard-info")
  age.i <- c()
  if (length(age) == 0) {
    age.i <- NA
  } else {
    for (i in 1:length(age)) {
      age.i[i] <- gsub(".*возраст: |<br/>.*", "", age[[i]])
    }
  }
  
  # Дата
  date <- html_nodes(html, ".time")
  date.i <- c()
  if (length(date) == 0) {
    date.i <- NA
  } else {
    for (i in 1:length(date)) {
      date.i[i] <- gsub(".*content=\"|\">.*", "", date[[i]])
    }
  }
  
  # Время
  time <- html_nodes(html, ".time")
  time.i <- c()
  if (length(time) == 0) {
    time.i <- NA
  } else {
    for (i in 1:length(time)) {
      time.i[i] <- gsub(".* - |\n.*", "", time[[i]])
    }
  }
  
  # Цена
  price <- html_nodes(html, ".price")
  price.i <- c()
  if (length(price) == 0) {
    price.i <- NA
  } else {
    for (i in 1:length(price)) {
      price.i[i] <- gsub(".*<span class=\"\">\n|\n.*", 
                         "", 
                         price[[i]])
    }
  }
  
  # Пункт назначения
  city <- html_nodes(html, ".trip-roads-stop~ .trip-roads-stop")
  city.i <- c()
  if (length(city) == 0) {
    city.i <- NA
  } else {
    for (i in 1:length(city)) {
      city.i[i] <- gsub("<span class=\"trip-roads-stop\">|</span>", "", city[[i]])
    }
  }
  
  # Сохранение в датасет
  blblcars.t <- data.frame(Name = names.i,
                           Age = age.i,
                           Date = date.i,
                           Time = time.i,
                           City = city.i,
                           Price = price.i,
                           stringsAsFactors = FALSE)
  
  # Добавление данных в итоговый датасет
  blblcars <- rbind(blblcars, blblcars.t)
  
}

# Закрытие сервера RSelenium
remDr$close()

# Сохранение данных
save(blblcars, file = "data/blblcars")


#### ОБРАБОТКА ДАННЫХ ####

# Загрузка данных
load("data/blblcars")


# Преобразование типов данных
cleanFun <- function(htmlString) {
  return(gsub("<.*?>", "", htmlString))
}
blblcars$Age <- sapply(blblcars$Age, cleanFun)
blblcars$Age <- as.integer(blblcars$Age)
blblcars$Price <- as.integer(gsub("[^0-9]", "", blblcars$Price))
blblcars$hours <- as.integer(gsub(":..", "", blblcars$Time))
blblcars$days <- weekdays(as.Date(blblcars$Date))


#### Самые популярные направления ####
bl.city <- blblcars %>% count(City)
bl.city$percents <- round(bl.city$n/sum(bl.city$n)*100, digits = 2)
bl.city <- bl.city %>% arrange(desc(n))

# 96 городов
length(unique(bl.city$City))

# а здесь мы заменяем названием станции метро Zhibek Zholy в Алмате, обозначенной на латинице и 
#рассматриваемой в качестве конечной точки маршрута, на название самого города
bl.city$City[47] <- "Алматы"

#### График "Топ-10 маршрутов из Казани на blablacar.ru" ####
ggplot(bl.city[1:10,], aes(x = reorder(City, n), 
                           y = percents, fill = percents))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_label(aes(label = paste0(percents, "%")), 
             size = 5, colour = "white", hjust = 1)+
  labs(title = "Чаще всего казанцы ездят в Уфу, Набережные Челны и Чебоксары",
       subtitle = "Топ-10 маршрутов из Казани на blablacar.ru (март-май 2018)",
       caption = "Источник: blablacar.ru Автор кода: silentio.su",
       x = "Города",
       y = "% от всех поездок")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14))+
  theme_economist_white()


#### Карта маршрутов из Казани на blablacar.ru ####

# Геолокация
bl.city <- na.omit(bl.city)
geo <- geocode(bl.city$City)
bl.city <- cbind(bl.city, as.data.frame(geo))
map <- get_map(location = "Kazan", maptype = "terrain", zoom = 1)

ggmap(map)+
  geom_point(data = bl.city, 
             aes(x = lon, y = lat,  size = percents),
             alpha = 1, colour = "red")+
  labs(title = "Карта маршрутов из Казани на blablacar.ru",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = " ",
       y = " ",
       size = "% поездок:")+
  theme(legend.position = "left",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 8), 
        axis.title.x = element_text(size = 8),
        axis.text.y = element_text(size = 8), 
        axis.title.y = element_text(size = 8),
        title = element_text(size = 14))



#### Средняя цена по Топ-10 направлений ####
bl.price.top <- blblcars %>% 
  filter(City %in% unique(bl.city$City[1:10])) %>% 
  select(City, Price)
bl.price.top <- full_join(bl.price.top, 
                          bl.price.top %>% 
                            group_by(City) %>% 
                            summarise(mean = mean(Price))
)
bl.price.top$mean <- round(bl.price.top$mean, digits = 0)
bl.price.top$mean <- paste0(bl.price.top$mean, " р.")
bl.price.top <- bl.price.top %>% unite(City, c(City, mean), sep = ", ")

#### График "Самый большой разброс цен на билеты в Москву, Нижний Новгород и Ижевск" ####
library(Hmisc)
ggplot(bl.price.top, aes(x = reorder(City, Price), y = Price))+
  stat_summary(geom = "line", group = 1, fun.data = "mean_cl_boot", size = 1,
               colour = "blue")+
  stat_summary(fun.data = "mean_cl_boot", colour = "red", size = 1)+
  labs(title = "Самый большой разброс цен - на билеты в Москву, Нижний Новгород и Ижевск",
       subtitle = "Средняя цена поездки из Казани на blablacar.ru (Топ-10 направлений)",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = "Направления и средняя цена",
       y = "Цена поездки, руб.")+
  theme(legend.position = "none",
        legend.text = element_text(size = 14),
        axis.text.x = element_text(size = 10, angle = 90), 
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14))



#### Самые дорогие направления ####
bl.price <- blblcars %>% 
  select(City, Price) %>% 
  group_by(City) %>% 
  summarise(price = mean(Price))
bl.price$price <- round(bl.price$price, digits = 0)
bl.price <- bl.price %>% arrange(desc(price))
bl.price$City[5] <- "Алматы"
#### График "Топ-10 самых дорогих маршрутов из г. Клинцы на blablacar.ru" ####
ggplot(bl.price[1:10,], aes(x = reorder(City, price), 
                            y = price, fill = price))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_label(aes(label = paste0(price, " р.")), 
             size = 5, colour = "white", hjust = 1)+
  labs(title = "Дороже всего уехать из Казани в Зеленогорск (Красноярский край) и Красноярск",
       subtitle = "Топ-10 самых дорогих маршрутов из Казани на blablacar.ru",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = "Направления",
       y = "Средняя цена поездки, руб.")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14))
#theme_fivethirtyeight()



#### Самые популярные водители ####
drivers <- blblcars %>% 
  select(Name, Age)
drivers$Age <- paste0("возраст: ", drivers$Age)
drivers <- drivers %>% unite(Name, c(Name, Age), sep = ", ")
drivers <- drivers %>% count(Name)
drivers$percents <- round(drivers$n/sum(drivers$n)*100, digits = 2)
drivers <- arrange(drivers, desc(n))
drivers$per.month <- round(drivers$n/2, digits = 0)

summary(as.factor(drivers$n))/sum(drivers$n)*100

#### График "Большинство водителей подвозят людей эпизодически" ####
ggplot(drivers[1:10,], aes(x = reorder(Name, n), 
                           y = percents, fill = percents))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_label(aes(label = paste0(per.month, " поезд./месяц")), 
             size = 3, colour = "white", hjust = 1)+
  labs(title = "Лидеры по количеству перевозок в месяц - ИП и организации",
       subtitle = "Топ-10 водителей по количеству поездок из Казани на blablacar.ru",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = "Водители",
       y = "Количество поездок в месяц")+
  theme(legend.position = "none",
        axis.text.x = element_text(size = 10), 
        axis.title.x = element_text(size = 10),
        axis.text.y = element_text(size = 10), 
        axis.title.y = element_text(size = 10),
        title = element_text(size = 10))



#### Самые популярные часы отправления для Топ-10 ####
bl.hours <- blblcars %>% 
  group_by(City) %>% 
  count(hours)
bl.hours <- ungroup(bl.hours)
# Добавление нулевых значений
for (i in unique(bl.hours$City)) {
  for (j in seq(0, 23, 1)) {
    if (!j %in% bl.hours$hours[bl.hours$City == i]) {
      bl.hours <- rbind(bl.hours, 
                        data.frame(City = i, hours = j, n = 0))
    }
  }
}
# Отбор Топ-10
bl.hours <- bl.hours %>%
  filter(City %in% bl.city$City[1:10])
bl.hours$percents <- round(bl.hours$n/sum(bl.hours$n)*100, digits = 2)

#### График "Распределение поездок из г. Казани на blablacar.ru по времени суток" ####
ggplot(bl.hours, aes(x = hours, y = percents, fill = City))+
  geom_bar(stat = "identity")+
  labs(title = "Легче всего уехать из Казани с полуночи до пяти утра",
       subtitle = "Распределение поездок из Казани на blablacar.ru по времени суток",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = "Часы (время суток)",
       y = "% от всех поездок (по Топ-10)",
       fill = "Направления:")+
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 14), 
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14))


#### Самые популярные дни отправления для Топ-10 ####
bl.days <- blblcars %>% 
  group_by(City) %>% 
  count(days)
bl.days <- ungroup(bl.days)
# Добавление нулевых значений
for (i in unique(bl.days$City)) {
  for (j in unique(bl.days$days)) {
    if (!j %in% bl.days$days[bl.days$City == i]) {
      bl.days <- rbind(bl.days, 
                       data.frame(City = i, days = j, n = 0))
    }
  }
}
# Отбор Топ-10
bl.days <- bl.days %>%
  filter(City %in% bl.city$City[1:10])
bl.days$percents <- round(bl.days$n/sum(bl.days$n)*100, digits = 2)
# Сортировка по дням недели
bl.days$days <- as.factor(bl.days$days)
bl.days$days <- factor(bl.days$days, levels = c("понедельник",
                                                "вторник",
                                                "среда",
                                                "четверг",
                                                "пятница",
                                                "суббота",
                                                "воскресенье"))

#### График "Распределение поездок из г. Клинцы на blablacar.ru по дням недели" ####
ggplot(bl.days, aes(x = days, 
                    y = percents, fill = City))+
  geom_bar(stat = "identity")+
  labs(title = "По средам легче всего уехать из Казани",
       subtitle = "Распределение поездок из Казани на blablacar.ru по дням недели",
       caption = "Источник: blablacar.ru  автор кода: silentio.su",
       x = "Дни недели",
       y = "% от всех поездок (по Топ-10)",
       fill = "Направления:")+
  theme(legend.position = "right",
        legend.text = element_text(size = 12),
        axis.text.x = element_text(size = 12), 
        axis.title.x = element_text(size = 14),
        axis.text.y = element_text(size = 14), 
        axis.title.y = element_text(size = 14),
        title = element_text(size = 14))



#### РАСПИСАНИЕ ####
tbls <- blblcars %>% 
  filter(City %in% bl.city$City[1:10]) %>% 
  group_by(City) %>% 
  select(City, days, Time, Price)

# Добавление средней цены
tbls <- full_join(tbls,
                  tbls %>% 
                    summarise(mean.price = round(mean(Price), digits = 0)), 
                  by = "City"
)
tbls <- tbls %>% select(-Price)

# Добавление наиболее вероятного дня недели
tbls <- full_join(tbls, 
                  tbls %>% 
                    count(days) %>% 
                    top_n(1, n), by = "City")
for (i in unique(tbls$City)) {
  tbls$days.y[tbls$City == i] <- paste0(unique(tbls$days.y[tbls$City == i]),
                                        collapse = ", ")
}
tbls <- tbls %>% select(-c(days.x, n))    

# Добавление наиболее вероятного времени
tbls <- full_join(tbls, 
                  tbls %>% 
                    count(Time) %>% 
                    top_n(1, n), by = "City")
for (i in unique(tbls$City)) {
  tbls$Time.y[tbls$City == i] <- paste0(unique(tbls$Time.y[tbls$City == i]),
                                        collapse = ", ")
}
tbls <- tbls %>% select(-c(Time.x, n))
tbls <- ungroup(tbls)
tbls <- unique(tbls)

tbls <- tbls[c("City", "days.y", "Time.y", "mean.price")]
colnames(tbls) <- c("Пункт назначения", 
                    "Дни недели", 
                    "Время отправления",
                    "Средняя цена поездки")
tbls <- tbls %>% arrange(`Пункт назначения`)
write.csv(tbls, file = "tbls.csv", row.names = F)
load("tbls.csv")
png("tbls.png", height = 50*nrow(tbls), width = 200*ncol(tbls))
grid.table(tbls)
dev.off()
