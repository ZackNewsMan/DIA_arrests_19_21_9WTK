# Author's note: This code has been partially redacted to remove names.
  # This is because: 9NEWS has chosen not to fully identify the people experiencing homelessness in this story because advocates say they are in a constant state of crisis and doing so would perpetuate a cycle preventing them from seeking gainful employment.
  # Removed columns: "arrestee_name" and "dob."

library(tidyverse)

DIA_Arrests_19_21_2 %>% 
  group_by(clean_violation) %>% 
  summarize(count = n()) %>% 
  View()

# Most common type of arrests were for warrants
  # It looks like people were arrested for a variety of outstanding warrants at the airport (assault, larceny, etc.)
    # See full viol description for more
  # WARRANT: 339
  # VIOLATION OF AIRPORT RULES: 301
  # TRESPASS: 212

  arrest_type_count <- DIA_Arrests_19_21_2 %>% 
    group_by(clean_violation) %>% 
    summarize(count = n())
  
  # To export for gfx:
  
  arrest_type_count %>% write_csv("arrest_type_count.csv", na = "")
  
# Let's do it by year
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrest_year) %>% 
    summarize(count = n()) %>% 
    View()
  
   arrest_year <- DIA_Arrests_19_21 %>% 
    group_by(arrest_year) %>% 
    summarize(count = n())
  

    # There have been 456 arrests so far this year at DIA, up from 377 last year. 
      # The number of arrests at DIA at starting to get back to pre-pandemic levels. I would like more data before COVID in a perfect world. 
      # But - arrests at DIA are still down from a high of 476 in 2019. 
   
   
    # Make it into a basic line graph so I can see what's going on 
    # Ideas: https://www.r-bloggers.com/2020/09/create-a-line-graph-with-ggplot/ 
    
  ggplot(arrest_year) +
    geom_line(mapping = aes(x = arrest_year, y = count))
    
# And by person 
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n()) %>% 
    View()
  
  name_repeat <- DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n())
  
  name_repeat %>% write_csv("name_repeat.csv", na = "")
  
  
  # Above shows who keeps getting arrested for the same offense repeatedly. 
  
  #  I wonder how many repeat people there are. 
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n()) %>% 
    filter(count >= 2) %>% 
    View()
  
  # 69 people arrested at least twice from 2019 - 2021.
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n()) %>% 
    filter(count >= 4)
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n()) %>% 
    filter(count >= 3)
  
  
  # 22 ppl arrested at least 4 times. 
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name) %>% 
    summarize(count = n()) %>% 
    filter(count >= 10)
  
  # 5 people were arrested at least 10 times. 
  
  # To see who keeps getting arrested for the same thing: 
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name, clean_violation) %>% 
    summarize(count = n()) %>% 
    View()
  
  name_offense_repeat <- DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name, clean_violation) %>% 
    summarize(count = n())
  
  # It is clear from the data that if you keep getting arrested for the same thing at DIA, it is usually for trespassing accusations. 
    # Eight of the ten folks who were re-arrested most often for the same charge were originally arrested for trespassing.
  
  name_offense_repeat %>% write_csv("name_offense_repeat.csv", na = "")
  
  # People are commonly arrested for trespassing repeatedly. 
    # [REDACTED] was arrested for trespassing 15 times at DIA. 
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name, clean_violation) %>% 
    summarize(count = n()) %>% 
    filter(count >= 2)
  
  DIA_Arrests_19_21_2 %>% 
    group_by(arrestee_name, clean_violation) %>% 
    summarize(count = n()) %>% 
  filter(count >= 10)
  
    # Three people were arrested for trespassing at DIA at least 10 times between 2019 and 2021.
  
  # I want to see if trespassing arrests are on the rise specifically. 
    # mERP. I misspelled "trespass" as "tresspass" in the formula. That's embarrassing. 
    # Fixed it and re-imported as DIA_Arrests_19_21_2
    
  DIA_Arrests_19_21 %>% 
    filter(clean_violation == "TRESSPASS")
    group_by(arrest_year) %>% 
    summarize(count = n()) %>% 
    View()
  
    # Doing it all at once doesn't work, so I'll break it into 2 queries.
    
     trespass <- DIA_Arrests_19_21_2 %>% 
      filter(clean_violation == "TRESPASS")
    
     trespass %>% 
     group_by(arrest_year) %>% 
       summarize(count = n()) %>% 
       View()
  
     # No clear trend with trespassing. There's been at least 63 trespassing arrests each year. Peaked with 77 in 2020 and is at 72 in 2021. 
     
     # How many trespass repeats per year?
     
       trespass %>% 
         group_by(arrest_year, arrestee_name) %>% 
         summarize(count = n()) %>% 
         filter(count >= 2) %>% 
         View()
      
      trespass_repeat_19_21 <- trespass %>% 
         group_by(arrest_year, arrestee_name) %>% 
         summarize(count = n()) %>% 
         filter(count >= 2)
       
       # WORKED! Manually checked [REDACTED] (9 trespass in 2021), [REDACTED] (8 trespass in 2021) and [REDACTED] (8 trespass in 2021).
     
      # Repeats in just 2021
         trespass %>% 
           group_by(arrest_year, arrestee_name) %>% 
           summarize(count = n()) %>% 
           filter(arrest_year == "2021") %>% 
           filter(count >= 2) %>% 
           View()
      
         # And to publish the CSV:
     
         trespass_repeat_2021 <- trespass %>% 
           group_by(arrest_year, arrestee_name) %>% 
           summarize(count = n()) %>% 
           filter(arrest_year == "2021") %>% 
           filter(count >= 2)
         
         trespass_repeat_2021 %>% write_csv("trespass_repeat_2021.csv", na = "")
         
         # 2020
         
         trespass_repeat_2020 <- trespass %>% 
           group_by(arrest_year, arrestee_name) %>% 
           summarize(count = n()) %>% 
           filter(arrest_year == "2020") %>% 
           filter(count >= 2)
         
         # 2019
         
         trespass_repeat_2019 <- trespass %>% 
           group_by(arrest_year, arrestee_name) %>% 
           summarize(count = n()) %>% 
           filter(arrest_year == "2019") %>% 
           filter(count >= 2)
         
         
         trespass_repeat_2020 %>% write_csv("trespass_repeat_2020.csv", na = "")
         
         trespass_repeat_2019 %>% write_csv("trespass_repeat_2019.csv", na = "")
         
  # Is there a trend with airport rule violations?
     
     airport_viol <- DIA_Arrests_19_21_2 %>% 
       filter(clean_violation == "VIOLATION OF AIRPORT RULES")
     
     airport_viol %>% 
       group_by(arrestee_name) %>% 
       summarize(count = n()) %>% 
       filter(count >= 2)

     # One person, [REDACTED], was arrested twice for breaking airport rules/regs. Unsure of what happened here, but he's the only one. 

# Now let's look at this by race to see if there's a trend.
     
     DIA_Arrests_19_21_2 %>% 
       group_by(race) %>% 
       summarize(count = n())

     DIA_Arrests_19_21_2 %>% 
       group_by(race) %>% 
       summarize(count = n()) %>% 
       View()

    # 1,309 total rows.
     # 758 W arrests. 
        
        ((758/1309)*100)
     
        # 58% of arrests.
          # Remember - Denver is estimated to be 81% White
     
      # 353 B arrests
     
     ((353/1309)*100)
     
         # 27% of arrests.
          # Remember - Denver is estimated to be 10% Black.
          # Census: https://www.census.gov/quickfacts/denvercountycolorado  
     
      # 145 H arrests
     
     ((145/1309)*100)
     
     # 11% of arrests.
     # Remember - Denver is estimated to be 29% Hispanic.
     
     # White vs POC:
     
       1309 - 758
    
       551-15
       
        # 15 comes from 12 "U" (assuming that's unknown) and 3 NA. 
       
        536/1309        
        
        # 41% of arrests were from POC, which is way higher than it should be. 