library(tidyverse)
library(tidytuesdayR)
library(ggplot2)
library(scales)
library(magrittr)
library(patchwork)
library(extrafont)

# Load Tidy Tuesday Data
datasets <- tidytuesdayR::tt_load(2021, 22)
drivers <- datasets$drivers
records <- datasets$records

plot_data <- records %>%
  dplyr::filter(
    type == "Three Lap",
    shortcut == "No"
  ) %>%
  dplyr::select(track, date, time) %>%
  dplyr::mutate(date = as.Date(date, format = "%Y-%m-%d")) %>%
  dplyr::group_by(track) %>%
  dplyr::mutate(
    pct_improved = (time / max(time)) - 1
  ) %>%
  dplyr::ungroup()

# Pull last record to see which tracks saw the most improvement
final_improvements <- plot_data %>%
  group_by(track) %>%
  dplyr::filter(date == max(date)) %>%
  ungroup() %>%
  dplyr::arrange(pct_improved)

View(final_improvements)

first <- unlist(final_improvements[1, "track"], use.names = FALSE)
second <- unlist(final_improvements[2, "track"], use.names = FALSE)
third <- unlist(final_improvements[3, "track"], use.names = FALSE)
last <- unlist(final_improvements[nrow(final_improvements), "track"],
               use.names = FALSE)

plot_data %<>%
  dplyr::mutate(
    track_rank = case_when(
      track == first ~ "first",
      track == second ~ "second",
      track == third ~ "third",
      track == last ~ "last",
      TRUE ~ "other"
    )) %>%
  dplyr::mutate(
    highlighted = ifelse(track_rank == "other", FALSE, TRUE)
  )

track_rank <- c("first", "second", "third", "last", "other")
line_colors <- c("#5EAAA8", "#A3D2CA", "#F7F3E9", "#F05945", "#ffffff")
highlighted <- c(TRUE, FALSE)
highlight_alpha <- c(0.7, 0.1)

p1 <- plot_data %>%
  ggplot(aes(x = date,
             y = pct_improved,
             group = track,
             color = track_rank,
             alpha = highlighted
             )) +
  geom_step(size = 1) +
  geom_point(data = plot_data %>%
               group_by(track) %>%
               dplyr::filter(date == max(date)) %>%
               ungroup(),
             aes(col = track_rank), size = 1.5) +
  geom_text(data = plot_data %>%
              group_by(track) %>%
              dplyr::filter(date == max(date)) %>%
              ungroup(),
             aes(label = ifelse(highlighted, track, "")),
                    hjust = -0.1, vjust = 0, size = 4.5, family = "cantarell") +
  scale_alpha_discrete(range = c(0.15, 0.7)) +
  scale_color_manual(values = setNames(line_colors, track_rank)) +
  scale_x_date(
    date_labels = "%Y",
    limits = as.Date(c("1997-01-01","2025-01-01"))
    ) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_minimal() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_text(color = "#ffffff",
                             size = 12,
                             family = "cantarell"),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#ffffff", size = 0.1),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  plot_annotation(
    title = "Which track saw the most improvement in three lap race times?",
    subtitle = "Mario Kart 64 World Records",
    caption = paste("#TidyTuesday Week 22",
                    "Data: Mario Kart World Records",
                    "Graphic: Chris Nemarich",
                    sep = " | "
    )) &
  theme(
    panel.background = element_rect(fill = "#232229", color = "#232229"),
    plot.background = element_rect(fill = "#232229", color = "#232229"),
    plot.title = element_text(size = 25,
                              face = "bold",
                              hjust = 0,
                              color = "white",
                              family = "cantarell"),
    plot.subtitle = element_text(size = 20,
                                 face = "italic",
                                 hjust = 0,
                                 color = "white",
                                 family = "cantarell"),
    plot.caption = element_text(size = 15,
                                face = "bold",
                                hjust = 0,
                                color = "white",
                                family = "cantarell")
  )

p1
