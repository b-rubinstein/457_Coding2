library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)
library(scales)

all_data <- read.csv('bobbleheads.csv')
mets_data <- subset(all_data, home_team == "New York Mets")
mets_data

# Figure 7.1
mets_data$ordered_day_of_week <- with(data=mets_data,
                                    ifelse ((day_of_week == "Monday"),1,
                                            ifelse ((day_of_week == "Tuesday"),2,
                                                    ifelse ((day_of_week == "Wednesday"),3,
                                                            ifelse ((day_of_week == "Thursday"),4,
                                                                    ifelse ((day_of_week == "Friday"),5,
                                                                            ifelse ((day_of_week == "Saturday"),6,7)))))))
mets_data$ordered_day_of_week <- factor(mets_data$ordered_day_of_week, levels=1:7,
                                      labels=c("Mon", "Tue", "Wed", "Thur", "Fri", "Sat", "Sun"))


with(data=mets_data,plot(ordered_day_of_week, attend/1000,
                       xlab = "Day of Week", ylab = "Attendance (thousands)",
                       col = "violet", las = 1))

# Figure 7.2
mets_data$ordered_month <- with(data=mets_data,
                                ifelse ((month == "APR"),4,
                                        ifelse ((month == "MAY"),5,
                                                ifelse ((month == "JUN"),6,
                                                        ifelse ((month == "JUL"),7,
                                                                ifelse ((month == "AUG"),8,
                                                                        ifelse ((month == "SEP"),9,10)))))))
mets_data$ordered_month <- factor(mets_data$ordered_month, levels=4:10,
                                  labels = c("April", "May", "June", "July", "Aug", "Sept", "Oct"))

with(data=mets_data,plot(ordered_month, attend/1000,
                         xlab = "Month", ylab = "Attendance (thousands)",
                         col = "lightblue", las = 1))


# Table 7.2

# use the full data set to obtain an estimate of the increase in
# attendance due to bobbleheads, controlling for other factors

my.model <- {attend ~ ordered_month + ordered_day_of_week + bobblehead}
my.model.fit <- lm(my.model, data = mets_data)# use all available data
print(summary(my.model.fit))
