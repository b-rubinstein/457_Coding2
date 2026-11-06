library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(scales)
library(cowplot)
library(modelsummary)

all_data <- read.csv('bobbleheads.csv')
mets_data <- subset(all_data, home_team == "New York Mets")


# Figure 7.1

order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mets_data <- mets_data |> mutate(day_of_week = factor(day_of_week, levels = order))
all_data <- all_data |> mutate(day_of_week = factor(day_of_week, levels = order))

comparison <-bind_rows(mets_data |> mutate(group = "New York Mets"),
                                           all_data |> mutate(group = "Full MLB"))

ggplot(comparison, aes(x = day_of_week, y = attend/1000, fill=group)) +
  geom_boxplot() +
  labs(
    title="Game Attendance by Day of Week",
    subtitle = '2012 Season Data - Full MLB vs Mets',
    x = "Day",
    y = "Attendance (thousands)"
  ) + theme_cowplot()


# Figure 7.2

order <- c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT")
mets_data <- mets_data |> mutate(month = factor(month, levels = order,
                                                labels = c("April", "May", "June", "July", "August", "September", "October")))

all_data <- all_data |> mutate(month = factor(month, levels = order,
                                                labels = c("April", "May", "June", "July", "August", "September", "October")))


comparison <-bind_rows(mets_data |> mutate(group = "New York Mets"),
                       all_data |> mutate(group = "Full MLB"))


ggplot(comparison, aes(x = month, y = attend/1000, fill=group)) +
  geom_boxplot() +
  labs(
    title="Game Attendance by Month",
    subtitle = '2012 Season Data - Full MLB vs Mets',
    x = "Month",
    y = "Attendance (thousands)"
  ) + theme_cowplot()

# Table 7.2

mets_model <- lm(attend ~ month + day_of_week + bobblehead, data = mets_data)
all_model <- lm(attend ~ month + day_of_week + bobblehead, data = all_data)

summary(mets_model)
summary(all_model)

models_list <- list("Mets" = mets_model,
                    "Full MLB" = all_model)

modelsummary(models_list, stars = TRUE,
             gof_map = c("nobs", "r.squared", "adj.r.squared"))


#COMPARISON BY DAY
mets_data <- mets_data |> group_by(day_of_week) |> summarize("Avg_Mets" = mean(attend))
all_data <- all_data |> group_by(day_of_week) |> summarize("Avg_All" = mean(attend))

diff <- left_join(mets_data, all_data, by = "day_of_week") |>
  mutate(difference = Avg_Mets - Avg_All) |> arrange(difference)
diff

#COMPARISON BY MONTH
mets_data <- mets_data |> group_by(month) |> summarize("Avg_Mets" = mean(attend))
all_data <- all_data |> group_by(month) |> summarize("Avg_All" = mean(attend))

diff <- left_join(mets_data, all_data, by = "month") |>
  mutate(difference = Avg_Mets - Avg_All) |> arrange(difference)
diff
