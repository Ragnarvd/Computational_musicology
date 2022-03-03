pop <- get_playlist_audio_features("", "37i9dQZF1DWSoyxGghlqv5")
classic <- get_playlist_audio_features("", "37i9dQZF1EIgLoMVUd9oTU")
electronic <- get_playlist_audio_features("", "37i9dQZF1DX0wMD4IoQ5aJ")
lofi <- get_playlist_audio_features("", "37i9dQZF1DX8Uebhn9wzrS")

study <-
  pop %>%
  mutate(country = "Pop study") %>%
  bind_rows(classic %>% mutate(country = "Classical study")) %>%
  bind_rows(electronic %>% mutate(country = "Electronic study")) %>%
  bind_rows(lofi %>% mutate(country = "Lofi study"))
mutate(
  country = fct_relevel(country, "Pop study, Classical study, Electronical study", "Lofi study")
)

february_dip <-
  study %>%
  ggplot(                          # Set up the plot.
    aes(
      x = valence,
      y = energy,
      size = track.popularity,
      colour = danceability,
      label = track.name           # Labels will be interactively visible.
    )
  ) +
  geom_point() +                   # Scatter plot.
  geom_rug(size = 0.1) +           # Add 'fringes' to show data distribution.
  facet_wrap(~country) +           # Separate charts per country.
  scale_x_continuous(              # Fine-tune the x axis.
    limits = c(0, 1),
    breaks = c(0, 0.50, 1),        # Use grid-lines for quadrants only.
    minor_breaks = NULL            # Remove 'minor' grid-lines.
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
    x = "Valence",
    y = "Energy"
  )

ggplotly(february_dip)
