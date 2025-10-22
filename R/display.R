library(nycflights13)
library(ggplot2)
library(dplyr)
library(maps)
# Problem 1.1.5

#' Visualize mean arrival delays by destination
#'
#' @return A \code{ggplot} object (map) showing mean arrival delays by destination.
#' @details The function uses \code{nycflights13::flights} and
#'   \code{nycflights13::airports}. Airports with missing coordinates are
#'   omitted from the plotted points and listed in the plot subtitle.
#' @examples
#' \dontrun{
#' visualize_airport_delays()
#' }
#' @import ggplot2 dplyr maps nycflights13
#' @export
visualize_airport_delays <- function() {
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  flights_delay <- flights |>
    dplyr::summarise(
      mean_arr_delay = mean(arr_delay, na.rm = TRUE),
      n = dplyr::n(),
      .by = dest
    ) |>
    dplyr::left_join(
      airports |> dplyr::select(faa, lon, lat),
      by = c("dest" = "faa")
    )
  missing_airports <- flights_delay |> dplyr::filter(is.na(lon) | is.na(lat))
  flights_delay <- flights_delay |> dplyr::filter(!is.na(lon) & !is.na(lat))

  ggplot2::ggplot(flights_delay, ggplot2::aes(x = lon, y = lat)) +
    ggplot2::borders("state") +
    ggplot2::geom_point(aes(size = n, color = mean_arr_delay), alpha = 0.7) +
    ggplot2::scale_color_viridis_c(option = "inferno") +
    ggplot2::guides(size = ggplot2::guide_legend(title = "Num flights"),
                    color = ggplot2::guide_colorbar(title = "Mean arrival delay (min)")) +
    ggplot2::labs(
      x = "Longitude", y = "Latitude",
      title = "Mean Arrival Delay by Destination (2013 NYC Flights)",
      subtitle = sprintf("Bubble size: number of flights; Missing Airports: %s",
                         paste(missing_airports$dest, collapse = ", "))
    ) +
    ggplot2::coord_fixed(1.3) +
    ggplot2::theme_minimal()
}