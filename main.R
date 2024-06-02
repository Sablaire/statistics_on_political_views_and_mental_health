# Установка и загрузка пакетов
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("tidyr")
#install.packages("scales")
#install.packages("ggplot2")
library(tidyverse)
library(dplyr)
library(tidyr)
library(scales)
library(ggplot2) 

# Установка рабочего каталога и загрузка данных
setwd("C:/Users/User/Desktop/R")
data <- read.csv("data.csv")

# Выбор и переименование столбцов
data1 <- data %>%
  select(q212813, q6689, q321, q18894, q5850, q23954, q1552, q425, q1290, q26582, q1287)

colnames(data1) <- c("polit_bel", "angry", "worrier", "insomnia", "suicidal_th", 
                     "alcohol_obs", "depression", "compultion", "guilt", "friganism", "mental_ill")

# Фильтрация данных для либералов
data1_liberal <- data1 %>% 
  filter(polit_bel == "Liberal / Left-wing")

# Подсчет количества либералов и общего количества строк
count_liberal <- nrow(data1_liberal)
total_count <- nrow(data1)
print(total_count)

# Вычисление процента либералов
percentage_liberal <- (count_liberal / total_count) * 100
print(percentage_liberal)

# Выбор нужных столбцов и удаление пропущенных значений
df_data_health <- data1 %>%
  select(polit_bel, mental_ill) %>%
  na.omit()

print(df_data_health)

# Преобразование данных в бинарные
data_health <- data1 %>%
  select(polit_bel, mental_ill) %>%
  mutate(
    mental_ill_binary = ifelse(mental_ill == "No", 0, 1)
  )

# По всем полит убеждениям
model <- glm(mental_ill_binary ~ polit_bel, data = data_health, family = binomial)

# Только по либералам
model_liberal <- glm(mental_ill_binary ~ 1, data = data_health %>% filter(polit_bel == "Liberal / Left-wing"), family = binomial)

# Результаты моделей
summary(model)
summary(model_liberal)

# Визуализация данных по всем полит убеждениям
#ggplot(data_health, aes(x = polit_bel, fill = factor(mental_ill_binary))) +
#  geom_bar(position = "fill") +
#  labs(x = "Political Beliefs", y = "Proportion", fill = "Mental Illness") +
#  ggtitle("Relationship between Mental Illness and Political Beliefs") +
#  theme_minimal()

# Визуализация данных только для либералов
#ggplot(data_health %>% filter(polit_bel == "Liberal / Left-wing"), aes(x = polit_bel, fill = factor(mental_ill_binary))) +
#  geom_bar(position = "fill") +
#  labs(x = "Political Beliefs", y = "Proportion", fill = "Mental Illness") +
#  ggtitle("Relationship between Mental Illness and Political Beliefs for Liberals") +
#  theme_minimal()

# Функция для расчета процентов
calc_percents <- function(data1) {
  data1 %>%
    group_by(polit_bel, mental_ill_binary) %>%
    summarise(count = n()) %>%
    group_by(polit_bel) %>%
    mutate(percent = count / sum(count) * 100)
}

# Расчет процентов
data_health_percents <- calc_percents(data_health)

# Визуализация данных по всем полит убеждениям с процентами на барах
ggplot(data_health_percents, aes(x = polit_bel, y = percent, fill = factor(mental_ill_binary))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Political Beliefs", y = "Percentage", fill = "Mental Illness") +
  ggtitle("Relationship between Mental Illness and Political Beliefs") +
  theme_minimal()

# Визуализация данных только для либералов с процентами на барах
data_health_liberal_percents <- calc_percents(data_health %>% filter(polit_bel == "Liberal / Left-wing"))

ggplot(data_health_liberal_percents, aes(x = polit_bel, y = percent, fill = factor(mental_ill_binary))) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(round(percent, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 3) +
  labs(x = "Political Beliefs", y = "Percentage", fill = "Mental Illness") +
  ggtitle("Relationship between Mental Illness and Political Beliefs for Liberals") +
  theme_minimal()
