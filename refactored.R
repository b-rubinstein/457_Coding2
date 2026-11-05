library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(scales)
library(cowplot)

all_data <- read.csv('bobbleheads.csv')
mets_data <- subset(all_data, home_team == "New York Mets")
mets_data

# Figure 7.1

order <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
mets_data <- mets_data |> mutate(day_of_week = factor(day_of_week, levels = order))

ggplot(mets_data, aes(x = day_of_week, y = attend/1000)) +
  geom_boxplot(fill = 'violet') +
  labs(
    title="Mets Game Attendance by Day of Week",
    subtitle = '2012 Season Data',
    x = "Day",
    y = "Attendance (thousands)"
  ) + theme_cowplot()


# Figure 7.2

order <- c("APR", "MAY", "JUN", "JUL", "AUG", "SEP", "OCT")
mets_data <- mets_data |> mutate(month = factor(month, levels = order,
                                 labels = c("April", "May", "June", "July", "August", "September", "October")))

ggplot(mets_data, aes(x = month, y = attend/1000)) +
  geom_boxplot(fill = 'lightblue') +
  labs(
    title="Mets Game Attendance by Month",
    subtitle = '2012 Season Data',
    x = "Month",
    y = "Attendance (thousands)"
  ) + theme_cowplot()

