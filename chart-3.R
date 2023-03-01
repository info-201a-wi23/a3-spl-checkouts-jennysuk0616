library(dplyr)
library(ggplot2)
library(stringr)

spl_df <- read.csv("~/Documents/INFO201/assignments/week-8-assignments/a3-spl-checkouts-jennysuk0616/INFO 201 A3 2017-2023-10-Checkouts-SPL-Data.csv")

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))


spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


jrrt_df <- spl_df %>%
  filter(str_detect(Creator, "Tolkien"))
jrrt_df$Title <- tolower(jrrt_df$Title)

jrrt_checkouts_per_month <- jrrt_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


jrrt_checkouts_per_month["Author"] <- "J. R. R. Tolkien"

jrrt_df$Title[str_detect(jrrt_df$Title, "fellowship")] <- "the fellowship of the ring"

as_df <- spl_df %>%
  filter(str_detect(Creator, "Sapkowski"))
as_df$Title <- tolower(as_df$Title)

as_checkouts_per_month <- as_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


as_checkouts_per_month["Author"] <- "Andrzej Sapkowski"

as_df$Title[str_detect(as_df$Title, "last wish")] <- "the last wish"

jrrt_as_df <- rbind(jrrt_checkouts_per_month, as_checkouts_per_month)

chart_3 <- ggplot(jrrt_as_df) +
  geom_line(aes(x = date, y = total_checkouts, color = Author)) +
  labs(title = "The Fellowship of the Ring vs. The Last Wish Checkouts",
       x = "Year",
       y = "Total Checkouts")
chart_3
