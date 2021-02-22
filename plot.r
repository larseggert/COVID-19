#! /usr/bin/env Rscript

if (!require("pacman"))
	install.packages("pacman", repos = "https://cloud.r-project.org/")
pacman::p_load(tidyverse, cowplot, lubridate, scales, wpp2019, RcppRoll)
pdf(NULL)

data = read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")

data = rename(data, Country = "Country/Region", State = "Province/State")

data = filter(data, is.na(State))

data$Country[ data$Country == "United Kingdom" ] <- "UK"

data = select(data, Country, matches("[0-9]+/[0-9]+/[0-9]+"))

data = filter(data, Country %in% commandArgs(trailingOnly=TRUE))

data = pivot_longer(data, cols=matches("[0-9]+/[0-9]+/[0-9]+"), names_to="Date", values_to="Count")

data = mutate(data, Date=mdy(Date))

data = filter(data, Count > 1)

data(pop)
pop$name[ pop$name == "United States of America" ] <- "US"
pop$name[ pop$name == "United Kingdom" ] <- "UK"

p = filter(pop, name %in% commandArgs(trailingOnly=TRUE))
p = select(p, name, "2020")
p = rename(p, Population = "2020")
p = mutate(p, Population = Population * 1000)
data = inner_join(data, p, by=c("Country"="name"))

data = mutate(data, Diff=Count-lag(Count, default=first(Count)),
              Diff14=roll_sum(Diff, 14, align = "right", fill = NA),
              Diff7=roll_sum(Diff, 7, align = "right", fill = NA),
              DiffFract=Diff/Count,
              Diff100K14=Diff14/Population*100000,
              Diff100K7=Diff7/Population*100000)

data = filter(data, Diff > 1)

print(data, n=1000)

f <- function(start, x) 10^(x-start)

p_cnt = ggplot(data, aes(x=Date, y=Count, group=Country, color=Country)) +
	scale_y_log10(limits=c(1, NA), labels=comma_format(accuracy=1)) +
	geom_point() +
	# geom_smooth() +
	labs(y="Cases") +
	theme_minimal_grid() + theme(legend.position="bottom")

p_diff = ggplot(data, aes(x=Date, y=Diff, group=Country, color=Country)) +
	scale_y_continuous(limits=c(-100, NA), labels=comma_format(accuracy=1)) +
	coord_cartesian(ylim=c(0, NA)) +
	geom_point() +
	geom_smooth() +
	labs(y="New cases") +
	theme_minimal_grid() + theme(legend.position="bottom")

p_difffract = ggplot(data, aes(x=Date, y=DiffFract, group=Country, color=Country)) +
	scale_y_continuous(limits=c(-1, NA), labels=comma_format(accuracy=1)) +
	coord_cartesian(ylim=c(0, NA)) +
	geom_point() +
	# geom_smooth() +
	theme_minimal_grid() + theme(legend.position="bottom")

p_diff100k14 = ggplot(data, aes(x=Date, y=Diff100K14, group=Country, color=Country)) +
	scale_y_continuous(limits=c(-1, NA), labels=comma_format(accuracy=1)) +
	coord_cartesian(ylim=c(0, NA)) +
	geom_point() +
	# geom_smooth() +
	labs(y="New cases per 100K in last 14 days") +
	theme_minimal_grid() + theme(legend.position="bottom")

p_diff100k7 = ggplot(data, aes(x=Date, y=Diff100K7, group=Country, color=Country)) +
	scale_y_continuous(limits=c(-1, NA), labels=comma_format(accuracy=1)) +
	coord_cartesian(ylim=c(0, NA)) +
	geom_point() +
	# geom_smooth() +
	labs(y="New cases per 100K in last 7 days") +
	theme_minimal_grid() + theme(legend.position="bottom")

final = plot_grid(p_cnt, p_diff, p_diff100k14, p_diff100k7, nrow=2)

save_plot("corona.pdf", final, base_width=10, base_height=10)
