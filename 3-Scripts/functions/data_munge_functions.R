sampling_filter_24hr <- function(df){
  return(df %>% 
    mutate(sampling = case_when(
      (Date_Time >= ymd_hms("2022-09-06 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-07 08:00:00", tz = "America/New_York") & site == "Catfish") ~ "Catfish 06-07 Sept",
      (Date_Time >= ymd_hms("2022-09-18 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-19 08:00:00", tz = "America/New_York") & site == "Catfish") ~ "Catfish 18-19 Sept",
      (Date_Time >= ymd_hms("2022-08-16 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-17 08:00:00", tz = "America/New_York") & site == "Deans") ~ "Deans 16-17 Aug",
      (Date_Time >= ymd_hms("2022-08-30 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-31 08:00:00", tz = "America/New_York") & site == "Deans") ~ "Deans 30-31 Aug",
      (Date_Time >= ymd_hms("2022-09-13 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-14 08:00:00", tz = "America/New_York") & site == "Blue Herron") ~ "Blue Herron 13-14 Sept",
      (Date_Time >= ymd_hms("2022-08-22 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-23 08:00:00", tz = "America/New_York") & site == "Sister") ~ "Sister 22-23 Aug",
      TRUE ~ NA_character_
    )))
}

sampling_filter_24hr_other <- function(df){
  return(df %>% 
           mutate(sampling = case_when(
             (Date_Time >= ymd_hms("2022-09-06 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-07 08:00:00", tz = "America/New_York")) ~ "Catfish 06-07 Sept",
             (Date_Time >= ymd_hms("2022-09-18 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-19 08:00:00", tz = "America/New_York")) ~ "Catfish 18-19 Sept",
             (Date_Time >= ymd_hms("2022-08-16 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-17 08:00:00", tz = "America/New_York")) ~ "Deans 16-17 Aug",
             (Date_Time >= ymd_hms("2022-08-30 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-31 08:00:00", tz = "America/New_York")) ~ "Deans 30-31 Aug",
             (Date_Time >= ymd_hms("2022-09-13 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-09-14 08:00:00", tz = "America/New_York")) ~ "Blue Herron 13-14 Sept",
             (Date_Time >= ymd_hms("2022-08-22 08:00:00", tz = "America/New_York") & Date_Time <= ymd_hms("2022-08-23 08:00:00", tz = "America/New_York")) ~ "Sister 22-23 Aug",
             TRUE ~ NA_character_
           )))
}

sampling_filter_28hr <- function(df){
  return(df %>% 
  mutate(sampling = case_when(
    (date >= ymd_hms("2022-09-06 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-09-07 12:00:00", tz = "America/New_York") & site == "Catfish") ~ "Catfish 06-07 Sept",
    (date >= ymd_hms("2022-09-18 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-09-19 12:00:00", tz = "America/New_York") & site == "Catfish") ~ "Catfish 18-19 Sept",
    (date >= ymd_hms("2022-08-16 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-08-17 12:00:00", tz = "America/New_York") & site == "Deans") ~ "Deans 16-17 Aug",
    (date >= ymd_hms("2022-08-30 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-08-31 12:00:00", tz = "America/New_York") & site == "Deans") ~ "Deans 30-31 Aug",
    (date >= ymd_hms("2022-09-13 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-09-14 12:00:00", tz = "America/New_York") & site == "Blue Herron") ~ "Blue Herron 13-14 Sept",
    (date >= ymd_hms("2022-08-22 08:00:00", tz = "America/New_York") & date <= ymd_hms("2022-08-23 12:00:00", tz = "America/New_York") & site == "Sister") ~ "Sister 22-23 Aug",
    TRUE ~ NA_character_
  )))
}

site_rename <- function(df){
  return(df %>% mutate(site = case_when(
    site == "Catfish" ~ "Catfish",
    site == "Deans" ~ "Deans", 
    site == "Pick" ~ "Blue Herron",
    site == "Picks" ~ "Blue Herron",
    site == "Sister3" ~ "Sister",
    site == "Sisters3" ~ "Sister"
  )))
}

daily_trace_aes <- function(x, date_time_vec= date_time_vec){
  return(x +theme_bw()+
           theme(axis.title.x = element_blank(), axis.text.x = element_text(angle = 90, hjust = 1, size = 14, color = "black"), 
                 axis.text.y = element_text(hjust = 1, size = 14, color = "black"),
                 axis.title = element_text(size = 16, color = "black"), strip.text = element_text(size = 14,  color = "black"))+
           scale_x_datetime(date_labels = "%H:%M", breaks = date_time_vec)+
           facet_wrap(~sampling, scales = "free_x", labeller = label_wrap_gen(width=12)))
}

date_to_sampling <- function(df){
  return(df %>% 
           mutate(sampling = case_when(
    date == "2022-08-16" ~ "Deans 8/16 - 8/17",
    date == "2022-08-17" ~ "Deans 8/16 - 8/17",
    date == "2022-08-30" ~ "Deans 8/30 - 8/31",
    date == "2022-08-31" ~ "Deans 8/30 - 8/31",
    date == "2022-09-06" ~ "Catfish 9/6 - 9/7",
    date == "2022-09-07" ~ "Catfish 9/6 - 9/7",
    date == "2022-09-18" ~ "Catfish 9/18 - 9/19",
    date == "2022-09-19" ~ "Catfish 9/18 - 9/19",
    date == "2022-09-13" ~ "Blue Herron 9/13 - 9/14",
    date == "2022-09-14" ~ "Blue Herron 9/13 - 9/14",
    date == "2022-08-22" ~ "Sister 8/22 - 8/23",
    date == "2022-08-23" ~ "Sister 8/22 - 8/23"
  )))
}

date_from_sampling <- function(df){
  return(df %>% 
           mutate(date = case_when(sampling == "Catfish1" ~ "09/06/22", 
                                   sampling == "Catfish2" ~ "09/18/22",
                                   sampling == "Deans1" ~ "08/16/22",
                                   sampling == "Deans2" ~ "08/30/22",
                                   sampling == "Pick1" ~ "09/13/22",
                                   sampling == "Sister31" ~ "08/22/22"))
           )
}

add_sampling_period <- function(df){
  return(df %>% 
           mutate(period = as.numeric(period), trip_wr =factor(case_when(period == 1 ~ "10AM", 
                                                                         period == 2 ~ "1PM", 
                                                                         period == 3 ~ "4PM", 
                                                                         period == 4 ~ "7PM", 
                                                                         period == 5 ~ "10PM", 
                                                                         period == 6 ~ "1AM", 
                                                                         period == 7 ~ "4AM", 
                                                                         period == 8 ~ "7AM"), levels = c("10AM", "1PM", "4PM", "7PM", "10PM", "1AM", "4AM", "7AM"))) %>% 
           mutate(sampling = case_when(sampling == "Catfish1" ~"Catfish 06-07 Sept",
                                       sampling == "Catfish2" ~ "Catfish 18-19 Sept",
                                       sampling == "Deans1" ~ "Deans 16-17 Aug",
                                       sampling == "Deans2" ~ "Deans 30-31 Aug", 
                                       sampling == "Pick1" ~ "Blue Herron 13-14 Sept",
                                       sampling == "Sister31" ~ "Sister 22-23 Aug")) )
}

time_elapsed <- function(df){
  return(df %>% group_by(location) %>%
           mutate(time_elapsed = (if_else(trip != 8,
                                          (lead(start_time, 1) - start_time) %>% as.period() %>% time_length(unit = "hours"),
                                          ((lag(start_time, 7) + days(1)) - start_time)%>% as.period() %>% time_length(unit = "hours")))) %>% 
           ungroup() %>%
           group_by(trip) %>%
           summarize(avg_time = mean(time_elapsed, na.rm = T)) %>%
           mutate(period = as.character(trip)) %>%
           select(-trip))
}
