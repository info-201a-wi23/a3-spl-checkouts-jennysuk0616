library(dplyr)
library(ggplot2)
library(stringr)

spl_df <- read.csv("~/Documents/INFO201/assignments/week-8-assignments/a3-spl-checkouts-jennysuk0616/INFO 201 A3 2017-2023-10-Checkouts-SPL-Data.csv")

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))


spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


rb_df <- spl_df %>%
  filter(str_detect(Creator, "Bradbury"))
rb_df$Title <- tolower(rb_df$Title)

rb_checkouts_per_month <- rb_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


rb_checkouts_per_month["Author"] <- "Ray Bradbury"

rb_df$Title[str_detect(rb_df$Title, "fahrenheit 451")] <- "fahrenheit 451"

go_df <- spl_df %>%
  filter(str_detect(Creator, "Orwell"))
go_df$Title <- tolower(go_df$Title)

go_checkouts_per_month <- go_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


go_checkouts_per_month["Author"] <- "George Orwell"

go_df$Title[str_detect(go_df$Title, "nineteen eighty-four")] <- "1984"

rb_go_df <- rbind(rb_checkouts_per_month, go_checkouts_per_month)


chart_2 <- ggplot(rb_go_df) +
  geom_line(aes(x = date, y = total_checkouts, color = Author)) +
  labs(title = "Fahrenheit 451 vs. 1984 Checkouts",
       x = "Year",
       y = "Total Checkouts")
chart_2

