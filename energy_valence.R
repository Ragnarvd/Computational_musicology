pop <- get_playlist_audio_features("", "37i9dQZF1DWSoyxGghlqv5")
classic <- get_playlist_audio_features("", "37i9dQZF1EIgLoMVUd9oTU")
electronic <- get_playlist_audio_features("", "37i9dQZF1DX0wMD4IoQ5aJ")
studying <- get_playlist_audio_features("", "7ex5hFjCiAdaXy6FFoe2Vi")

study <-
  pop %>%
  mutate(country = "Pop study") %>%
  bind_rows(classic %>% mutate(country = "Classical study")) %>%
  bind_rows(electronic %>% mutate(country = "Electronic study")) %>%
  bind_rows(studying %>% mutate(country = "Stu(dying)"))%>%
  mutate(
    country = fct_relevel(country, "Pop study, Classical study, Electronical study", "Stu(dying)")
  )

february_dip <-
  study %>%
  ggplot(                          # Set up the plot.
    aes(
      x = tempo,
      y = instrumentalness,
      size = track.popularity,
      colour = energy,
      label = track.name           # Labels will be interactively visible.
    )
  ) +
  geom_point() +                   # Scatter plot.
  geom_rug(size = 0.1) +           # Add 'fringes' to show data distribution.
  facet_wrap(~country) +           # Separate charts per country.
  scale_x_continuous(              # Fine-tune the x axis. Outliers are not 
    limits = c(0, 200),
    breaks = c(0, 100, 200),        # Use grid-lines for quadrants only.
    minor_breaks = c(25, 50, 75, 125, 150, 175)            # Remove 'minor' grid-lines.
  ) +
  scale_y_continuous(              # Fine-tune the y axis in the same way.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),
    minor_breaks = NULL
  ) +
  scale_colour_viridis_c(          # Use the cividis palette
    option = "E",                  # Qualitative set.
    alpha = 0.8,                   # Include some transparency
    guide = "none"
  ) +
  scale_size_continuous(           # Fine-tune the sizes of each point.
    guide = "none"                 # Remove the legend for size.
  ) +
  theme_light() +                  # Use a simpler theme.
  labs(                            # Make the titles nice.
    x = "Tempo",
    y = "Instrumentalness"
  )

ggplotly(february_dip)