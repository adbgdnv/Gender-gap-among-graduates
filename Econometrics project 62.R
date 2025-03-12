# Econometrics project 2024-25, HSE University, Saint Petersburg
# Group 62

# Setup ========================================================================

## Loading packages ------------------------------------------------------------

library(readxl)
library(dplyr)
library(ggplot2)
library(lmtest)
library(sandwich)
library(stargazer)


## Other settings --------------------------------------------------------------

### Setting common theme for all graphs
theme_set(theme_bw(base_size = 12, base_family = "Times"))
### Disabling exponential number formatting
options(scipen = 999)

# Data =========================================================================

## Loading ---------------------------------------------------------------------
df_specialty <- read_excel('data_graduates_specialty.xlsx')
glimpse(df_specialty)
df_specialty$year <- as.numeric(df_specialty$year)
# Предположим, что ваш датафрейм называется df
df_specialty <- df_specialty[df_specialty$gender != "Всего", ]#удаляем Всего в пол



df_specialty <- df_specialty %>%
  filter(!is.na(average_salary))

df_specialty <- df_specialty %>%
  filter(!is.na(gender))
unique_values <- lapply(df_specialty, unique)

## Clear  ----------------------------------------------------------------------
df_specialty <- df_specialty %>% select(-oktmo)
df_specialty <- df_specialty %>% select(-okato)

df_specialty$gender <- as.factor(df_specialty$gender) #gender to factor
df_specialty <- df_specialty %>%
  filter(!object_level %in% c("Страна", "Регион")) #deliting general data

## Create new column year of experience ========================================
df_specialty <- df_specialty %>%
  mutate(yearofexp = case_when(
    year == 2019 ~ 4,
    year == 2020 ~ 3,
    year == 2021 ~ 2,
    year == 2022 ~ 1,
    year == 2023 ~ 0,
    TRUE ~ NA_integer_  
  ))
unique_values <- lapply(df_specialty, unique)

#Descriptive analysis
# Создаем пользовательские метрики
custom_summary <- function(x) {
  data.frame(
    `Unique` = length(unique(x)),
    `Missing` = mean(is.na(x)) * 100,
    Mean = mean(x, na.rm = TRUE),
    SD = sd(x, na.rm = TRUE),
    Min = min(x, na.rm = TRUE),
    Median = median(x, na.rm = TRUE),
    Max = max(x, na.rm = TRUE)
  )
}

# Применяем функцию ко всем нужным столбцам
summary_table <- rbind(
  cbind(Column = "average_salary", custom_summary(df_specialty$average_salary)),
  cbind(Column = "percent_employed", custom_summary(df_specialty$percent_employed)),
  cbind(Column = "count_graduate", custom_summary(df_specialty$count_graduate))
  )


# Выводим таблицу
summary_table[, -1] <- sapply(summary_table[, -1], function(x) round(x, 2))
write.csv(summary_table, "summary_table.csv", row.names = FALSE)

#Считаем остальное
years_table <- table(df_specialty$year)
write.csv(years_table, "years_table.csv", row.names = FALSE)

object_name_table <- table(df_specialty$object_name)
write.csv(object_name_table, "object_name_table.csv", row.names = FALSE)

gender_table <- table(df_specialty$gender)
write.csv(gender_table, "gender_table.csv", row.names = FALSE)

study_area_table <- table(df_specialty$study_area)
write.csv(study_area_table, "study_area_table.csv", row.names = FALSE)

yearofexp_table <- table(df_specialty$yearofexp)
write.csv(yearofexp_table, "yearofexp_table.csv", row.names = FALSE)

## Графики ------------------------------------------------
bsp <- ggplot(df_specialty, aes(x = object_name, y = average_salary)) +
  geom_boxplot() +
  labs(title = "Боксплоты по зарплатам выпускников в федеральных округах",
       x = "Федеральный округ",
       y = "Зарплата выпускников") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Повернем метки оси X
ggsave("Боксплоты по зарплатам выпускников в федеральных округах.png", plot = bsp, width = 12, height = 6, dpi = 300)

bso <- ggplot(df_specialty, aes(x = study_area, y = average_salary)) +
  geom_boxplot() +
  labs(title = "Боксплоты по зарплатам выпускников по раным сферам",
       x = "Сфера образования выпускника",
       y = "Зарплата выпускников") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Повернем метки оси X
ggsave("Боксплоты по зарплатам выпускников по раным сферам.png", plot = bso, width = 12, height = 6, dpi = 300)

bsi <- ggplot(df_specialty, aes(x = gender, y = average_salary)) +
  geom_boxplot() +
  labs(title = "Боксплоты по зарплатам выпускников по полу",
       x = "Пол",
       y = "Зарплата выпускников") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Повернем метки оси X
ggsave("Боксплоты по зарплатам выпускников по полу.png", plot = bsi, width = 12, height = 6, dpi = 300)


# Строим scatter plot
bsu <- ggplot(df_specialty, aes(y = average_salary, x = percent_employed, color = gender)) +
  geom_point() +  # Добавляем точки
  labs(
    x = "Зарплата",
    y = "Опыт работы",
    title = "Зарплата по опыту работы с разделением по полу"
  ) +
  theme_minimal()  # Используем минималистичную тему для чистоты графика
ggsave("Скаттер Зарплата по опыту работы с разделением по полу.png", plot = bsu, width = 12, height = 6, dpi = 300)

bsy <- ggplot(df_specialty, aes(y = average_salary, x = percent_employed, color = gender)) +
  geom_point(alpha = 0.5) +  # Добавляем точки
  labs(
    x = "Зарплата",
    y = "Опыт работы",
    title = "Зарплата по опыту работы с разделением по полу"
  ) +
  scale_color_manual(values = c("Мужской" = "blue", "Женский" = "red")) +  # Задаем цвета вручную
  theme_minimal()  # Используем минималистичную тему
ggsave("Скаттер Зарплата по опыту работы с разделением по полу.png", plot = bsy, width = 12, height = 6, dpi = 300)



# Предположим, что ваш датасет называется df_specialty
# Преобразуем категориальные переменные в факторы
df_specialty$gender <- factor(df_specialty$gender)
df_specialty$education_level <- factor(df_specialty$education_level)
df_specialty$study_area <- factor(df_specialty$study_area)
df_specialty$object_level <- factor(df_specialty$object_level)

# Модели регрессии (подобно вашему примеру)
# Спецификация 1: гендер
spec_1 <- average_salary ~ gender

# Спецификация 2: гендер + стаж
spec_2 <- average_salary ~ gender + yearofexp

# Спецификация 3: гендер + стаж + уровень образования
spec_3 <- average_salary ~ gender + yearofexp + education_level

# Спецификация 4: гендер + стаж + уровень образования + область образования
spec_4 <- average_salary ~ gender + yearofexp + education_level + study_area

# Спецификация 5: гендер + стаж + уровень образования + область образования + уровень объекта образования
spec_5 <- average_salary ~ gender + yearofexp + education_level + study_area + object_name

# Построение моделей для всего датасета
reg_1_all <- lm(spec_1, data = df_specialty)
reg_2_all <- lm(spec_2, data = df_specialty)
reg_3_all <- lm(spec_3, data = df_specialty)
reg_4_all <- lm(spec_4, data = df_specialty)
reg_5_all <- lm(spec_5, data = df_specialty)

# Расчет стандартных ошибок с помощью ковариационной матрицы с коррекцией для гетероскедастичности
cov_1_all <- vcovHC(reg_1_all, type = "HC0")
se_1_all <- sqrt(diag(cov_1_all))
coeftest(reg_1_all, vcov = cov_1_all)

cov_2_all <- vcovHC(reg_2_all, type = "HC0")
se_2_all <- sqrt(diag(cov_2_all))
coeftest(reg_2_all, vcov = cov_2_all)

cov_3_all <- vcovHC(reg_3_all, type = "HC0")
se_3_all <- sqrt(diag(cov_3_all))
coeftest(reg_3_all, vcov = cov_3_all)

cov_4_all <- vcovHC(reg_4_all, type = "HC0")
se_4_all <- sqrt(diag(cov_4_all))
coeftest(reg_4_all, vcov = cov_4_all)

cov_5_all <- vcovHC(reg_5_all, type = "HC0")
se_5_all <- sqrt(diag(cov_5_all))
coeftest(reg_5_all, vcov = cov_5_all)


# Вывод результатов с помощью stargazer
stargazer(reg_1_all, reg_2_all, reg_3_all, reg_4_all, reg_5_all,
          se = list(se_1_all, se_2_all, se_3_all, se_4_all, se_5_all),
          column.labels = c("Model 1", "Model 2", "Model 3", "Model 4", "Model 5"),
          colnames = FALSE,
          type = "text",
          out = "results_all.txt")


# Функция для построения графика остатков против предсказанных значений
plot_residuals <- function(model, model_name) {
  # Вычисление предсказанных значений и остатков
  predicted <- predict(model, df_specialty)
  residuals <- residuals(model)
  
  # Преобразуем в датафрейм для удобства
  data_plot <- data.frame(predicted = predicted, residuals = residuals)
  
  # Построение графика
  p <- ggplot(data_plot, aes(x = predicted, y = residuals)) +
    geom_point(color = "blue", alpha = 0.5) +
    geom_hline(yintercept = 0, color = "red", linetype = "dashed") +
    labs(title = paste("Остатки vs Предсказанные значения -", model_name),
         x = "Предсказанные значения",
         y = "Остатки") +
    theme_minimal()
  
  return(p)
}

# Построение графиков для всех моделей
p1 <- plot_residuals(reg_1_all, "Model 1")
p2 <- plot_residuals(reg_2_all, "Model 2")
p3 <- plot_residuals(reg_3_all, "Model 3")
p4 <- plot_residuals(reg_4_all, "Model 4")
p5 <- plot_residuals(reg_5_all, "Model 5")

# Если вы хотите сохранить графики отдельно:
ggsave("residuals_model_1.png", p1, width = 8, height = 6)
ggsave("residuals_model_2.png", p2, width = 8, height = 6)
ggsave("residuals_model_3.png", p3, width = 8, height = 6)
ggsave("residuals_model_4.png", p4, width = 8, height = 6)
ggsave("residuals_model_5.png", p5, width = 8, height = 6)

# Если вы хотите объединить их в один график (например, с помощью patchwork):
library(patchwork)
combined_plot <- p1 + p2 + p3 + p4 + p5 + plot_layout(ncol = 2)
ggsave("residuals_combined.png", combined_plot, width = 16, height = 12)

unique_values <- lapply(df_specialty, unique)
write.csv(unique_values, "unique_values_table.csv")