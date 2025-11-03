file.choose()
dat<-read_csv(file.choose())

# Step 0: Parse the date column 

dat$Date <- as.Date(dat$Date, format = "%d %b %Y (%a)")
class(dat$Date) 
head(dat$Date)

# Step 1: Prepare the data

fb_1617 <- subset(dat, Date >= as.Date("2016-10-12") & Date <= as.Date("2017-05-21"))
nrow(fb_1617)
head(fb_1617)

# Step 2a: Clean percentage columns

library(dplyr)
library(readr)
fb_1617_clean <- fb_1617 %>%
  mutate(
    `Two-point%`   = parse_number(`Two-point%`),   
    `Three-point%` = parse_number(`Three-point%`), 
    `Free-throw%`  = parse_number(`Free-throw%`),  
    IsHome = ifelse(IsHome == 1, "Home", "Away")   
  )

# Step 2b: Home vs Away summary

home_away_summary <- fb_1617_clean %>%
  group_by(IsHome) %>%
  summarise(
    Games      = n(),                                         
    Avg_Points = mean(Points, na.rm = TRUE),                  
    Avg_PIR    = mean(Performance_Index_Rating, na.rm = TRUE),
    Avg_2P     = mean(`Two-point%`, na.rm = TRUE),            
    Avg_3P     = mean(`Three-point%`, na.rm = TRUE),        
    Avg_FT     = mean(`Free-throw%`, na.rm = TRUE),           
    Avg_OffReb = mean(Offensive_rebounds, na.rm = TRUE),      
    Avg_DefReb = mean(Defensive_rebounds, na.rm = TRUE),    
    Avg_TotReb = mean(Total_rebounds, na.rm = TRUE),          
    Avg_AST    = mean(Assists, na.rm = TRUE),                 
    Avg_TOV    = mean(Turnovers, na.rm = TRUE)                
  )

home_away_summary

# Step 2c: Quick visuals

library(ggplot2)

# Points comparison 
ggplot(home_away_summary, aes(x = IsHome, y = Avg_Points, fill = IsHome)) +
  geom_col() +
  labs(title = "Fenerbah??e 2016???2017: Home vs Away (Avg Points)",
       x = "Venue (Saha)", y = "Average Points (Ortalama Say??)") +
  theme_minimal()


# PIR comparison 
ggplot(home_away_summary, aes(x = IsHome, y = Avg_PIR, fill = IsHome)) +
  geom_col() +
  labs(title = "Fenerbah??e 2016???2017: Home vs Away (Avg PIR)",
       x = "Venue (Saha)", y = "Average PIR (Ortalama PIR)") +
  theme_minimal()

# 3PT% comparison 
ggplot(home_away_summary, aes(x = IsHome, y = Avg_3P, fill = IsHome)) +
  geom_col() +
  labs(title = "Fenerbah??e 2016???2017: Home vs Away (Avg 3PT%)",
       x = "Venue (Saha)", y = "Average 3PT% (Ort. 3S%)") +
  theme_minimal()



# step 3: T-tests for key metrics

t.test(Points ~ IsHome, data = fb_1617_clean)                   
t.test(Performance_Index_Rating ~ IsHome, data = fb_1617_clean) 
t.test(`Three-point%` ~ IsHome, data = fb_1617_clean) 


# Step 4: Trend over the Season 
ggplot(fb_1617, aes(Date, Performance_Index_Rating, color = IsHome)) +
  geom_line() + geom_point() +
  labs(title = "Fenerbah??e 2016???2017: PIR Trend Over the Season",
       x = "Date", y = "Performance Index Rating (PIR)") +
  theme_minimal()
# Step 4: PIR Trend Over the Season ??? Comment

# The graph shows how Fenerbah??e???s performance changed during the 2016???2017 season.
# PIR values go up and down across the months, which means the team had both strong and weak games.
# However, the trend rises again near the end of the season, showing better form before the Final Four.
# This stable finish explains why Fenerbah??e won the EuroLeague title that year.

# Step 5: Highlight Final and Semifinal Games 

finals <- data.frame(
  Date = as.Date(c("2017-05-19", "2017-05-21")),
  Label = c("Semifinal (19 May 2017)", "Final (21 May 2017)")
)

ggplot(fb_1617, aes(Date, Points)) +
  geom_line() +
  geom_point() +
  geom_vline(data = finals, aes(xintercept = Date), linetype = "dashed", color = "red") +
  geom_text(data = finals, aes(x = Date, y = max(fb_1617$Points, na.rm = TRUE),
                               label = Label), angle = 90, vjust = -0.5, hjust = 0, size = 3) +
  labs(title = "Fenerbah??e 2016???2017: Points Over Time (Finals Marked)",
       x = "Date", y = "Points") +
  theme_minimal()

# Step 5: Highlight Final and Semifinal Games ??? Comment

# The red dashed lines show the semifinal and final games in May 2017.
# These lines help us see how the team's scoring changed before and during the finals.
# The graph shows that Fenerbah??e stayed strong and stable, finishing the season at a high level.



# Title: Home vs Away Performance in EuroLeague 2016???2017

# In the 2016???2017 EuroLeague season, Fenerbah??e showed a stable and balanced performance.
# The team scored an average of 77.9 points at home and 75.2 points away.
# The Performance Index Rating (PIR) was higher at home (92.9) than away (83.4),
# but this difference was not statistically significant (p = 0.1477).
# The three-point percentage was almost the same: 40.8% at home and 39.7% away (p = 0.7774).
# These results show that Fenerbah??e played with strong discipline and consistency
# in both home and away games during their championship season.

