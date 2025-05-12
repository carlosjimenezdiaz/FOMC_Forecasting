# Interesting websites
# https://www.economics-finance.org/jefe/fin/KeaslerGoffpaper.pdf
# https://www.cboe.com/us/options/trading/codes/ 

# Libraries that we need
library(tidyverse)
library(tidyquant)
library(gt)
library(readxl)

# Local Variables
Future_Id <- c("K","M","N","Q","U","V","X","Z") 

# Reading the data related to next FOMC meetings
db_FOMC <- read_excel("DB_FOMC_Meetings.xlsx") %>% 
  purrr::set_names(c("Next_Meetings", "Low", "High", "Increase")) %>% 
  dplyr::mutate(Next_Meetings = Next_Meetings %>% as.Date(format = "%m/%d/%Y"),
                Month         = as.Date(Next_Meetings, format = "%m/%d/%Y") %>% lubridate::month(),
                Meeting_Day   = as.Date(Next_Meetings, format = "%m/%d/%Y") %>% lubridate::day()) %>%
  janitor::clean_names()

# Download current price of the FF futures
Current_Year <- Sys.Date() %>% lubridate::year() %>% substr(3, 4) %>% as.numeric()
data_FFER    <- NULL
for(year_ID in seq(Current_Year, Current_Year + 2, 1)){
  for(month_ID in Future_Id){
    tryCatch(
      expr = {
        # Printing status
        print(str_glue("Getting info from ZQ{month_ID}{year_ID}.CBT"))
        
        # Data of FFER
        FFER <- getQuote(str_glue("ZQ{month_ID}{year_ID}.CBT")) %>%
          dplyr::mutate(FFER_Month = month_ID,
                        FFER_Year  = year_ID)
        
        if(is.null(data_FFER)){
          data_FFER <- FFER
        }else{
          data_FFER <- bind_rows(data_FFER, FFER)    
        }
        
      },
      error = function(e){ 
        print("ERROR")
      }
    )
  }
}

# Feature Eng
data_FFER_Enhanced <- data_FFER %>%
  dplyr::arrange(!desc(FFER_Year)) %>%
  dplyr::mutate(month = case_when(FFER_Month == "F" ~ 1,
                                  FFER_Month == "G" ~ 2,
                                  FFER_Month == "H" ~ 3,
                                  FFER_Month == "J" ~ 4,
                                  FFER_Month == "K" ~ 5,
                                  FFER_Month == "M" ~ 6,
                                  FFER_Month == "N" ~ 7,
                                  FFER_Month == "Q" ~ 8,
                                  FFER_Month == "U" ~ 9,
                                  FFER_Month == "V" ~ 10,
                                  FFER_Month == "X" ~ 11,
                                  FFER_Month == "Z" ~ 12),
                DaysInMonth    = lubridate::days_in_month(str_glue("01-{month}-{FFER_Year}") %>% as.Date(format = "%d-%m-%y")),
                ExpirationDate = str_glue("{month}-20{FFER_Year}"))  %>%
  left_join(db_FOMC %>%
              dplyr::mutate(ExpirationDate = str_glue("{month}-20{next_meetings %>% lubridate::year() %>% substr(3, 4) %>% as.numeric()}")), by = ("ExpirationDate"))

# Plotting the FFR curve
data_FFER_Enhanced[order(data_FFER_Enhanced$FFER_Year),] %>%
  dplyr::mutate(ExpirationDate = factor(ExpirationDate, levels=unique(ExpirationDate))) %>%
  ggplot() +
  theme_tq() +
  geom_line(aes(x = ExpirationDate, y = (100 - Last)/100, group = 1)) +
  labs(title    = "Fed Funds Rate",
       subtitle = "Expected by the Fed Funds Futures",
       caption  = "By: Carlos Jimenez",
       x = "Expiration Month",
       y = "FFR Mid Target") +
  scale_y_continuous(labels = scales::percent, limits = c(0, max(100 - data_FFER_Enhanced$Last)/100*1.2)) +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5, hjust=1))

# Showing the date of the next meeting
db_FOMC %>%
  dplyr::mutate() %>%
  dplyr::filter(next_meetings > Sys.Date()) %>%
  dplyr::select(next_meetings) %>%
  pull()

meeting_to_analyze <- "2025-05-07" # enter this value manually

# Odds Calculation
data_FFER_Enhanced_Odds_Calculations <- data_FFER_Enhanced %>%
  dplyr::select(month.x, DaysInMonth, meeting_day, next_meetings, Last) %>%
  janitor::clean_names()

db_Odds <- NULL
for(i in 1:nrow(data_FFER_Enhanced_Odds_Calculations)){ # i <- 1
  for(LLTR in seq(0, 500, 25)){ # LLTR <- 0 (Lower Limit Target Rate)
    
    # Hypotetic Current FED Target
    Current_Target <- (2*LLTR + 25)/2
    
    for(rate_adjust in seq(-100, 100, 25)){ # Sequence of Decrease and Increases in Rates - TEST: rate_adjust <- -50
      # Extracting information from the Futures
      FFER_Implied_by_Futures     <- 100 - data_FFER_Enhanced_Odds_Calculations$last[i]
      FED_Current_FFER_unadjusted <- Current_Target/100
      FED_Current_FFER_adjusted   <- (Current_Target/100*data_FFER_Enhanced_Odds_Calculations$meeting_day[i] + (Current_Target + rate_adjust)/100*(data_FFER_Enhanced_Odds_Calculations$days_in_month[i] - data_FFER_Enhanced_Odds_Calculations$meeting_day[i]))/data_FFER_Enhanced_Odds_Calculations$days_in_month[i]
      
      Odds <- (FFER_Implied_by_Futures - FED_Current_FFER_unadjusted)/(FED_Current_FFER_adjusted - FED_Current_FFER_unadjusted) %>% as.numeric()
      
      if(is.null(db_Odds)){
        db_Odds <- data.frame(low_target_rate      = LLTR,
                              high_target_rate     = LLTR + 25,
                              rate_adjust          = rate_adjust,
                              new_low_target_rate  = LLTR + rate_adjust,
                              new_high_target_rate = LLTR + rate_adjust + 25,
                              odds                 = ifelse(Odds < 0, 0, Odds),
                              ffer_month           = data_FFER_Enhanced_Odds_Calculations$next_meetings[i])
      }else{
        db_Odds <- db_Odds %>% bind_rows(data.frame(low_target_rate      = LLTR,
                                                    high_target_rate     = LLTR + 25,
                                                    rate_adjust          = rate_adjust,
                                                    new_low_target_rate  = LLTR + rate_adjust,
                                                    new_high_target_rate = LLTR + rate_adjust + 25,
                                                    odds                 = ifelse(Odds < 0, 0, Odds),
                                                    ffer_month           = data_FFER_Enhanced_Odds_Calculations$next_meetings[i]))
      }
    }
  }
}

# Formatting and cleaning the DB
db_Odds_normalized <- db_Odds %>%
  mutate_all(function(x) ifelse(is.infinite(x), NA, x)) %>%
  dplyr::mutate(current_target_label = str_glue("{low_target_rate} - {high_target_rate}"),
                type                 = ifelse(rate_adjust < 0, "Cut", "Hike"),
                type_weighted_odds   = case_when(type == "Cut" ~ -1*odds,
                                                 TRUE ~ +1*odds),
                odds                 = ifelse(odds > 1, 1, odds),
                ffer_month           = ffer_month %>% as.Date()) %>%
  dplyr::select(ffer_month, current_target_label, rate_adjust, type, odds, type_weighted_odds) %>%
  na.omit()

# Extracting some statistics
gt_odds <- db_Odds_normalized %>%
  dplyr::filter(ffer_month == meeting_to_analyze) %>%
  dplyr::select(current_target_label, rate_adjust, odds) %>%
  pivot_wider(names_from = rate_adjust, values_from = odds)

  # Locating the Row for Current Rate
  data_csv_file <- db_FOMC %>%
    dplyr::filter(next_meetings == meeting_to_analyze)
  
  row_ID <- which(gt_odds$current_target_label == str_glue("{data_csv_file$low[1]} - {data_csv_file$high[1]}"))
  
gt_odds %>%
  dplyr::slice(1:(row_ID + 3)) %>%
  gt() %>%
  tab_header(
    title    = md(str_glue("**FOMC Meeting - {meeting_to_analyze}**")),
    subtitle = "Rate Hike / Cut probability calculator"
  ) %>%
  fmt_percent(
    columns  = -current_target_label,
    decimals = 1
  ) %>%
  cols_hide(
    columns = "0"
  ) %>% 
  tab_spanner(
    label = "Rate cut / hikes",
    columns = c("-100", "-75", "-50", "-25", "25", "50", "75", "100")
  ) %>%
  cols_label(
    current_target_label  = "FED Current Target"
  ) %>% 
  tab_style(
    style = list(
      cell_fill(color = "green")
    ),
    locations = cells_body(rows = row_ID)
  ) %>%
  tab_source_note(
    source_note = str_glue("The FED Current Target is the one expected to have at the FOMC meeting.")
  ) %>%
  tab_source_note(
    source_note = str_glue("By: Carlos Jimenez - {Sys.Date()}")
  )
