library(dplyr)
#getting weather count 
weather_count <- crashdata |> count(`Weather Condition Text Format`)
weather_count