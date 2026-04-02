
ggplot(data = tree_dat, 
       mapping = aes(x = yday(Date), y = CH4_lin_flux.estimate, color = Species))+
  geom_point() +
  facet_grid(Species ~ Plot,
             scales = "free") +# Add a layer of points
  labs(title = "Practice") 

tree_dat %>%
  filter(Plot == "Control",
         CH4_lin_flux.estimate > -0.05 &
           CH4_lin_flux.estimate < 0.21) %>%
  ggplot(aes(x = yday(Date),
             y = CH4_lin_flux.estimate,
             color = CO2_lin_flux.estimate)) +
  geom_point() +
  facet_grid(.~Species)

tree_dat %>%
  filter(Plot == "Control",
         Species == "Tulip poplar") %>%
  ggplot(aes(x = yday(Date),
             y = CH4_lin_flux.estimate,
             color = ID)) +
  geom_point() +
  facet_wrap(.~Year)


events <- tribble(
  ~Year, ~Date,           ~Event,
#  2021, ymd("2021-08-25"), "Test event",
#  2021, ymd("2021-09-09"), "Test event",
  2022, ymd("2022-06-22"), "TEMPEST 1",
  2023, ymd("2023-06-06"), "TEMPEST 2",
  2023, ymd("2023-06-07"), "TEMPEST 2",
  2024, ymd("2024-06-11"), "TEMPEST 3",
  2024, ymd("2024-06-12"), "TEMPEST 3",
  2024, ymd("2024-06-13"), "TEMPEST 3"
) %>%
  mutate(JD = yday(Date))

library(dplyr)
library(ggplot2)
library(lubridate)

title_fun <- function(ID) {
  sprintf("Methane Flux — ID %s", paste(unique(ID), collapse = ", "))
}

# Filter first; use the filtered column to compute the title
df <- tree_dat %>%
  filter(CH4_rob_flux.estimate > 0, ID == "C1",
         Timepoint != "(none)",
         Year != "2021")

ggplot(df, aes(x = yday(Date),
               y = CH4_rob_flux.estimate,
               color = as.factor(Year))) +
  geom_point(size = 2) +
  geom_line(alpha = 0.7) +
  geom_vline(data = events,
             aes(xintercept = JD, color = as.factor(Year)),
             linetype = "dashed", alpha = 0.6) +
  labs(title = title_fun(df$ID), color = "Year")

