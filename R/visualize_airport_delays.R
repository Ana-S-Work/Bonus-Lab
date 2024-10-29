library(dplyr)
library(ggplot2)
library(nycflights13)

visualize_airport_delays <- function() {
  # Ensure the necessary datasets from nycflights13 are available
  data("flights")
  data("airports")
  
  # Calculate the average delay for each destination airport
  avg_delays <- flights %>%
    filter(!is.na(dep_delay)) %>%               # Exclude rows with NA delays
    group_by(dest) %>%                          # Group by destination airport code
    summarise(mean_delay = mean(dep_delay, na.rm = TRUE))  # Calculate mean delay
  
  # Join the delays with airport information
  airport_delays <- avg_delays %>%
    inner_join(airports, by = c("dest" = "faa")) %>%  # Join on destination airport code
    filter(!is.na(lon), !is.na(lat))                 # Filter out rows without location data
  
  # Plot the data
  ggplot(airport_delays, aes(x = lon, y = lat)) +
    geom_point(aes(size = mean_delay, color = mean_delay), alpha = 0.7) +
    scale_color_viridis_c(option = "C", name = "Mean Delay (min)") + # Viridis color scale for visibility
    labs(
      title = "Average Flight Delays by Airport",
      subtitle = "Mean departure delay in minutes for each destination airport",
      x = "Longitude",
      y = "Latitude"
    ) +
    theme_minimal() +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      legend.position = "right"
    )
}

# Run the function to display the plot
visualize_airport_delays()
