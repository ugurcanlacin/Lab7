
#'@title Plot mean delay
#'@description visualizes the mean delay of flights for different airports by longitude and latitude.
#'@field no argument
#'@examples
#' visualize_airport_delays()
#'@export 
visualize_airport_delays = function(){
  requireNamespace("nycflights13")
  requireNamespace("dplyr")
  flights <- nycflights13::flights
  airports <- nycflights13::airports
  
}

# visualize_airport_delays()