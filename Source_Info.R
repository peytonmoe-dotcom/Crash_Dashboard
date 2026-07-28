library(dplyr)



#OVERALL TRENDS TAB 

#getting weather count 
weather_count <- crashdata |> count(`Weather Condition Text Format`)
weather_count

#light count
light_count <- crashdata |> count(`Light Condition Text Format`)
light_count

#road surface count 
roadsurface_count <- crashdata |> count(`Road Surface Condition Text Format`)
roadsurface_count 

#day of the week count

#changing to factor variable
crashdata$`Day of the Week Text Format` <- factor(
  crashdata$`Day of the Week Text Format`,
  levels=c(
    "Sunday",
    "Monday",
    "Tuesday",
    "Wednesday",
    "Thursday",
    "Friday",
    "Saturday"
  )
)
day_count <- crashdata |> count(`Day of the Week Text Format`)
day_count

#time of day count
#creating time bins

time_count <- crashdata %>%
        mutate(timebins=case_when(
        `Hour of the Day` >=0 & `Hour of the Day` < 3 ~ "12-2:59 am",
        `Hour of the Day` >=3 & `Hour of the Day` < 6 ~ "3-5:59 am",
        `Hour of the Day` >=6 & `Hour of the Day` < 9  ~ "6-8:59 am",
        `Hour of the Day` >=9 & `Hour of the Day` < 12 ~ "9-11:59 am",
        `Hour of the Day` >=12 & `Hour of the Day` < 15 ~ "12-2:59 pm",
        `Hour of the Day` >=15 & `Hour of the Day` < 18 ~ "3-5:59 pm",
        `Hour of the Day` >=18 & `Hour of the Day` < 21 ~ "6-8:59 pm",
        `Hour of the Day` >=21 & `Hour of the Day` < 24 ~ "9-11:59 pm",
        TRUE ~ NA_character_
)
  ) %>%
  count(timebins)
  time_count$timebins <- factor(
  time_count$timebins,
  levels = c(
    "12-2:59 am",
    "3-5:59 am",
    "6-8:59 am",
    "9-11:59 am",
    "12-2:59 pm",
    "3-5:59 pm",
    "6-8:59 pm",
    "9-11:59 pm"
  )
)

#fatal time count
  fataltime_count <- individualdata %>%
    mutate(fataltimebins=case_when(
      `Hour of the Day` >=0 & `Hour of the Day` < 3 ~ "12-2:59 am",
      `Hour of the Day` >=3 & `Hour of the Day` < 6 ~ "3-5:59 am",
      `Hour of the Day` >=6 & `Hour of the Day` < 9  ~ "6-8:59 am",
      `Hour of the Day` >=9 & `Hour of the Day` < 12 ~ "9-11:59 am",
      `Hour of the Day` >=12 & `Hour of the Day` < 15 ~ "12-2:59 pm",
      `Hour of the Day` >=15 & `Hour of the Day` < 18 ~ "3-5:59 pm",
      `Hour of the Day` >=18 & `Hour of the Day` < 21 ~ "6-8:59 pm",
      `Hour of the Day` >=21 & `Hour of the Day` < 24 ~ "9-11:59 pm",
      TRUE ~ NA_character_
    )
    ) %>%
    count(fataltimebins)
  fataltime_count$fataltimebins <- factor(
    fataltime_count$fataltimebins,
    levels = c(
      "12-2:59 am",
      "3-5:59 am",
      "6-8:59 am",
      "9-11:59 am",
      "12-2:59 pm",
      "3-5:59 pm",
      "6-8:59 pm",
      "9-11:59 pm"
    )
  )
  

#most harmful event

#creating other harmful event category 
crashdata <- crashdata %>%
  mutate(
    HarmfulEventGroup = case_when(
      `Most harmful event` %in% c(
        "Ran Off Roadway Right",
        "Other Post, Pole, or Support",
        "Curb",
        "Other Non-motorist",
        "Not Applicable",
        "Separation of Units",
        "Ran Off Roadway Left",
        "Fence",
        "Fell/Jumped From Vehicle",
        "Other Non-Collision",
        "Reentering Roadway",
        "Other Non-Fixed Object",
        "Embankment",
        "Guardrail Face",
        "Traffic Sign Support",
        "Unknown",
        "Other Traffic Barrier",
        "Concrete Traffic Barrier",
        "Traffic Signal Support",
        "Cross Centerline",
        "Struck By Falling, Shifting Cargo or Anything Set in Motion By Motor Vehicle",
        "Light Support",
        "Guardrail End",
        "Jackknife",
        "Work Zone/Maintenance Equipment",
        "Cargo/Equipment Loss or Shift",
        "Immersion, Full or Partial",
        "Fire / Explosion",
        "Animal (live)",
        "Ditch",
        "Bridge Pier or Support",
        "Bridge Rail",
        "Equipment Failure (blown tire, brake failure, etc.)",
        "Cross Median"
      ) ~ "Other Harmful Event",
      #all else 
      TRUE ~ `Most harmful event`
    )
  )
  
harmful_count <- crashdata |> count(`HarmfulEventGroup`)
harmful_count


#contributing action by driver 

#creating other contributing action category

individualdata <- individualdata %>%
  mutate(
    `Driver Actions 1 Text Format` =
      na_if(trimws(`Driver Actions 1 Text Format`), ""),
    
    contributing_group = case_when(
      `Driver Actions 1 Text Format` %in% c(
        "Wrong Side or Wrong Way",
        "Operated Motor Vehicle in Reckless or Aggressive Manner",
        "Improper Backing",
        "Operated Motor Vehicle in Inattentive, Careless, Negligent, or Erratic Manner",
        "Swerved or Avoided Due to Wind, Slippery Surface, Motor Vehicle, Object, Non-Motorist in Roadway, etc.",
        "Disregarded Other Traffic Sign",
        "Disregarded Other Road Markings",
        "Over-Correcting/Over-Steering",
        "Not Applicable"
      ) ~ "Other Contributing Action",
      
      TRUE ~ `Driver Actions 1 Text Format`
    )
  )

contributing_action_count <- individualdata %>%
  filter(!is.na(contributing_group)) %>%
  count(contributing_group, sort = TRUE)

table(individualdata$`contributing_group`)


#crashes per year 
table(crashdata$Year)

#neighborhood 
neighborhood_crash_count <- neighborhood$MVA
neighborhood_crash_count



