library(dplyr)
library(ggplot2)
library(stringr)

spl_df <- read.csv("~/Documents/INFO201/assignments/week-8-assignments/a3-spl-checkouts-jennysuk0616/INFO 201 A3 2017-2023-10-Checkouts-SPL-Data.csv")

spl_df <- spl_df %>%
  mutate(date = paste0(CheckoutYear, "-", CheckoutMonth, "-01"))


spl_df$date <- as.Date(spl_df$date, format = "%Y-%m-%d")


rr_df <- spl_df %>%
  filter(str_detect(Creator, "Riordan"))
rr_df$Title <- tolower(rr_df$Title)


rr_checkouts_per_month <- rr_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))


rr_checkouts_per_month["Author"] <- "Rick Riordan"

rr_df$Title[str_detect(rr_df$Title, "lightning thief")] <- "the lightning thief"

jkr_df <- spl_df %>%
  filter(str_detect(Creator, "Rowling"))
jkr_df$Title <- tolower(jkr_df$Title)

jkr_checkouts_per_month <- jkr_df %>%
  group_by(date) %>%
  summarize(total_checkouts = sum(Checkouts, na.rm = TRUE))

jkr_checkouts_per_month["Author"] <- "J. K. Rowling"

jkr_df$Title[str_detect(jkr_df$Title, "sorcerer's stone")] <- "harry potter and the sorcerer's stone"

rr_jkr_df <- rbind(rr_checkouts_per_month, jkr_checkouts_per_month)

chart_1 <- ggplot(rr_jkr_df) +
  geom_line(aes(x = date, y = total_checkouts, color = Author)) +
  labs(title = "The Lightning Thief vs. Harry Potter and the Sorcerer's Stone Checkouts",
       x = "Year",
       y = "Total Checkouts") 
chart_1
