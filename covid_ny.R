rm(list = ls())
cat("\014")

library(COVID19)
library(dplyr)
library(tidyverse)
library(ggplot2)


df <- covid19(
     country = "USA",
     level = 2,
     start = "2021-09-01",
     verbose = FALSE,
     end = Sys.Date())

# extract data just from NYS
df <- filter(df,
             df$administrative_area_level_2 == "New York")

## manipulate data ############

## CONFIRMED CASES BY DAY
confirmed <- NULL # create empty variable for following loop
for (i in 1:length(df$confirmed)) { # loop calculates new cases per day
        a <- df$confirmed[i]
        b <- df$confirmed[i + 1]
        c <- b - a
        confirmed[i] <- c
}
rm(a, b, c)

## DEATHS BY DAY
deaths <- NULL # create empty variable for following loop
for (i in 1:length(df$deaths)) { # loop calculates deaths per day
        d <- df$deaths[i]
        e <- df$deaths[i + 1]
        f <- e - d
        deaths[i] <- f
}
rm(d, e, f)

## TESTS DONE DAILY
tests <- NULL
for (i in 1:length(df$tests)) { # number of covid tests administered daily
        g <- df$tests[i]
        h <- df$tests[i + 1]
        j <- h - g
        tests[i] <- j
}
rm(g, h, j)

## CALCULATE POSITIVITY RATE
positives <- 100 * (confirmed / tests) # daily positivity rate

## PEOPLE VACCINATED
vaxxed <- NULL
for (i in 1:length(df$people_vaccinated)) {
        k <- df$people_vaccinated[i]
        l <- df$people_vaccinated[i + 1]
        m <- l - k
        vaxxed[i] <- m
}
rm(k, l, m)

## PEOPLE FULLY VACCINATED
fully <- NULL
for (i in 1:length(df$people_fully_vaccinated)) {
        n <- df$people_fully_vaccinated[i]
        o <- df$people_fully_vaccinated[i + 1]
        p <- o - n
        fully[i] <- p
}
rm(n, o, p)

rm(i)

## VAX STATUS DIFFERENCES
status_difference <- NULL
for (i in 1:length(df$people_vaccinated)) {
        q <- df$people_vaccinated[i]
        r <- df$people_fully_vaccinated[i]
        s <- q - r
        status_difference[i] <- s
}
rm(q, r, s)


# CREATE NEW DF USING NEW VARIABLES
df2 <- data.frame(df$date, confirmed, deaths, fully, positives, status_difference)
# make into a tibble
data <- as_tibble(df2)
rm(df2)

# plots ######################

chart <- ggplot() +
        geom_line( aes(x = df$date, y = confirmed), # add the space before aes
                   size = 2, color = "green") +
        geom_line( aes(x = df$date, y = deaths*250),
                   size = 1.5, color = "red") + #  scale deaths var on graph
        scale_y_continuous(
                name = "Confirmed Daily Cases",
                sec.axis = sec_axis(~./1000,
                                    name = "Daily Deaths") # to adjust second y axis
        ) +
        labs(x = "Date",
             title = "New York State COVID Data",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()


plot_confirmed <- ggplot() +
        geom_line(aes(df$date, confirmed)) +
        labs(x = "Date",
             y = "Confirmed Cases",
             title = "NYS Confirmed Cases",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_deaths <- ggplot() +
        geom_line(aes(df$date, deaths)) +
        labs(x = "Date",
             y = "Deaths",
             title = "NYS Daily COVID-19 Deaths",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_positives <- ggplot() +
        geom_line(aes(df$date, positives)) +
        labs(x = "Date",
             y = "Positivity Rate",
             title = "NYS COVID-19 Positivity Rate",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_vax <- ggplot() +
        geom_line(aes(df$date, df$people_vaccinated),
                  col = "red", size = 1, linetype = "dashed") +
        geom_line(aes(df$date, df$people_fully_vaccinated),
                  col = "green", size = 1) +
        labs(x = "Date",
             y = "Population",
             title = "NYS COVID-19 Vaccinations",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_icu <- ggplot() +
        geom_line(aes(df$date, df$icu)) +
        labs(x = "Date",
             y = "Number of ICU Beds in Use",
             title = "NYS COVID-19 ICU Hospitalizations",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_hospital <- ggplot() +
        geom_line(aes(df$date, df$hosp)) +
        labs(x = "Date",
             y = "Hospitalizations",
             title = "NYS COVID-19 Hospitalizations",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_medical <- ggplot() +
        geom_line( aes(x = df$date, y = df$hosp), # add the space before aes
                   size = 1, linetype = "dashed") +
        geom_line( aes(x = df$date, y = df$icu),
                   size = 1) + #  scale deaths var on graph
        scale_y_continuous(
                name = "Hospitalizations [- - -]",
                sec.axis = sec_axis(~./1,
                                    name = "ICU") # to adjust second y axis
        ) +
        labs(x = "Date",
             title = "New York State COVID Hospitalizations and ICUs",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_people_vaxxed <- ggplot() +
        geom_line( aes(x = df$date, y = df$people_vaccinated),
                   size = 1, linetype = "dashed", col = "red") +
        geom_line( aes(x = df$date, y = df$people_fully_vaccinated),
                   size = 1, col = "green") +
        scale_y_continuous(
                name = "One Vaccine [- - -]",
                sec.axis = sec_axis(~./1,
                                    name = "Fully Vaccinated")) +
        labs(x = "Date",
             title = "NYS COVID Vaccinations",
             subtitle = "01 SEP 2021 to present") +
        theme_minimal()

plot_status_difference <- ggplot() +
        geom_line(aes(x = df$date, y = status_difference)) +
        labs(title = "New York State Gap Between Partially and Fulled Vaxxed Status",
             subtitle = "01 SEP 2021 to present",
             x = "Date",
             y = "Difference in vax statuses") +
        theme_minimal()


