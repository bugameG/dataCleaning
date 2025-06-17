library(tidyverse)
library(readxl)
library(reshape2)

### Cleaning Sales Data 

sls = read_excel("sales.xlsx", sheet = 1)

# RENAMING OF COLUMN NAMES
sls = rename(sls,
      "order.date"="Ship Mode",       
       "fc.consumer"="First Class",
       "fc.corporate"=`...3`,
       "fc.homeoff"=`...4`,
        "sd.consumer"="Same Day",
        "sd.corporate"=`...6`,
        "sd.homeoff"=`...7`,
         "sc.consumer"="Second Class",
         "sc.corporate"=`...9`,
         "sc.homeoff"=`...10`,
          "stdc.consumer"="Standard Class",
          "stdc.corporate"=`...12`,
          "stdc.homeoff"=`...13`) 

# Delete the first 2 rows
sls = sls[-c(1,2),] 

# CONVERTING DATA TO LONG FORMAT 
# and removing missing values 
sls = melt(data = sls,
     measure.vars = c("fc.consumer","fc.corporate","fc.homeoff",
                      "sd.consumer","sd.corporate","sd.homeoff",
                      "sc.consumer","sc.corporate","sc.homeoff",
                      "stdc.consumer","stdc.corporate","stdc.homeoff"),
     variable.name = c("ship.segment"),
     value.name = c("sales")   
     ) %>% na.omit()


# SPLITTING SHIP.SEGMENT COLUMN TO TWO COLUMNS
# First class
fc = sls %>% 
  filter(ship.segment == "fc.consumer" | ship.segment =="fc.corporate" | ship.segment == "fc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "First Class"="fc.consumer",
                                  "First Class"="fc.corporate",
                                  "First Class"="fc.homeoff"))

 ## Consumer
fc.consumer = fc %>% 
  filter(ship.segment == "fc.consumer") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Consumer"="fc.consumer"))

 ## Corporate
fc.corporate = fc %>% 
  filter(ship.segment == "fc.corporate") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Corporate"="fc.corporate"))

 ## Home office
fc.homeoff = fc %>% 
  filter(ship.segment == "fc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Home Office"="fc.homeoff"))

# Same day
sd = sls %>%
  filter(ship.segment == "sd.consumer" | ship.segment =="sd.corporate" | ship.segment == "sd.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Same Day"="sd.consumer",
                                  "Same Day"="sd.corporate",
                                  "Same Day"="sd.homeoff"))

 ## Consumer
sd.consumer = sd %>% 
  filter(ship.segment == "sd.consumer") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Consumer"="sd.consumer"))

 ## Corporate
sd.corporate = sd %>% 
  filter(ship.segment == "sd.corporate") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Corporate"="sd.corporate"))

 ## Home office
sd.homeoff = sd %>% 
  filter(ship.segment == "sd.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Home Office"="sd.homeoff"))

# Second class
sc = sls %>% 
  filter(ship.segment == "sc.consumer" | ship.segment =="sc.corporate" | ship.segment == "sc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Second Class"="sc.consumer",
                                  "Second Class"="sc.corporate",
                                  "Second Class"="sc.homeoff"))

 ## Consumer
sc.consumer = sc %>% 
  filter(ship.segment == "sc.consumer") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Consumer"="sc.consumer"))

 ## Corporate
sc.corporate = sc %>% 
  filter(ship.segment == "sc.corporate") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Corporate"="sc.corporate"))

 ## Home office
sc.homeoff = sc %>% 
  filter(ship.segment == "sc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Home Office"="sc.homeoff"))

# Standard class
stdc = sls %>% 
  filter(ship.segment == "stdc.consumer" | ship.segment =="stdc.corporate" | ship.segment == "stdc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Standard Class"="stdc.consumer",
                                  "Standard Class"="stdc.corporate",
                                  "Standard Class"="stdc.homeoff"))

 ## Consumer
stdc.consumer = stdc %>% 
  filter(ship.segment == "stdc.consumer") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Consumer"="stdc.consumer"))

 ## Corporate
stdc.corporate = stdc %>% 
  filter(ship.segment == "stdc.corporate") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Corporate"="stdc.corporate"))

 ## Home office
stdc.homeoff = stdc %>% 
  filter(ship.segment == "stdc.homeoff") %>% 
  mutate(ship.segment, fct_recode(ship.segment,
                                  "Home Office"="stdc.homeoff"))


# MERGING ALL 12 DATAFRAMES
## First Class
fc1 = merge(fc.consumer, fc.corporate, all = TRUE) 
fcfinal = merge(fc1, fc.homeoff, all = TRUE)

# Merging the segment types
first.class.final=pivot_longer(fcfinal,
                               cols = c(`fct_recode(ship.segment, Consumer = "fc.consumer")`,
                                        `fct_recode(ship.segment, Corporate = "fc.corporate")`,
                                        `fct_recode(ship.segment, \`Home Office\` = "fc.homeoff")`),
                               values_to = "segment",
                               values_drop_na = TRUE) 

first.class.final = first.class.final[,-c(5)]
first.class.final = rename(first.class.final, "ship.mode"=`fct_recode(...)`)



## Same day
sd1 = merge(sd.consumer, sd.corporate, all = TRUE) 
sdfinal = merge(sd1, sd.homeoff, all = TRUE)

# Merging the segment types
same.day.final = pivot_longer(sdfinal,
                               cols = c(`fct_recode(ship.segment, Consumer = "sd.consumer")`,
                                        `fct_recode(ship.segment, Corporate = "sd.corporate")`,
                                        `fct_recode(ship.segment, \`Home Office\` = "sd.homeoff")`),
                               values_to = "segment",
                               values_drop_na = TRUE) 

same.day.final = same.day.final[,-c(5)] 
same.day.final = rename(same.day.final, "ship.mode"=`fct_recode(...)`)


## Second class
sc1 = merge(sc.consumer, sc.corporate, all = TRUE) 
scfinal = merge(sc1, sc.homeoff, all = TRUE)

# Merging the segment types
second.class.final = pivot_longer(scfinal,
                              cols = c(`fct_recode(ship.segment, Consumer = "sc.consumer")`,
                                       `fct_recode(ship.segment, Corporate = "sc.corporate")`,
                                       `fct_recode(ship.segment, \`Home Office\` = "sc.homeoff")`),
                              values_to = "segment",
                              values_drop_na = TRUE) 

second.class.final = second.class.final[,-c(5)] 
second.class.final = rename(second.class.final, "ship.mode"=`fct_recode(...)`)



## Standard class
stdc1 = merge(stdc.consumer, stdc.corporate, all = TRUE) 
stdcfinal = merge(stdc1, stdc.homeoff, all = TRUE)

# Merging the segment types
standard.class.final = pivot_longer(stdcfinal,
                                  cols = c(`fct_recode(ship.segment, Consumer = "stdc.consumer")`,
                                           `fct_recode(ship.segment, Corporate = "stdc.corporate")`,
                                           `fct_recode(ship.segment, \`Home Office\` = "stdc.homeoff")`),
                                  values_to = "segment",
                                  values_drop_na = TRUE) 

standard.class.final = standard.class.final[,-c(5)] 
standard.class.final = rename(standard.class.final, "ship.mode"=`fct_recode(...)`)


# MERGING THE FINAL 4 DATAFRAMES
fc.sd = merge(first.class.final, same.day.final, all=TRUE)
sc.stdc = merge(second.class.final, standard.class.final, all = TRUE)
last.merge = merge(fc.sd, sc.stdc, all = TRUE) %>% 
             select(-ship.segment) %>% 
             arrange(segment) %>% 
             arrange(ship.mode)


# VARIABLES
# Order date
last.merge$order.date = as.Date(as.numeric(last.merge$order.date),
                                origin = "1899-12-30")
# Ordering sales data by date
last.merge = last.merge %>% arrange(order.date)

# Sales
last.merge$sales = as.numeric(last.merge$sales)


# ship.mode
last.merge$ship.mode = factor(last.merge$ship.mode,
                          levels = c("First Class", "Same Day",
                                     "Second Class", "Standard Class"))
# segment
last.merge$segment = factor(last.merge$segment,
                              levels = c("Consumer", "Corporate",
                                         "Home Office"))

# NO DUPLICATES
# last.merge[duplicated(last.merge),]

# NO MISSING VALUES                                
# last.merge[is.na(last.merge$order.date),]
# last.merge[is.na(last.merge$sales),]
# last.merge[is.na(last.merge$ship.mode),]
# last.merge[is.na(last.merge$segment),]


### RENAMING THE CLEANED DATA
sales.clean = last.merge
sales.clean
sales.clean %>% glimpse()

### EXPORTING SALES DATA
# write_csv(sales.clean, "clean_sales.csv")


# sales.clean 
#      %>% mutate(Month = month(order.date),
#                   Ttl = sum(sales)) 
#      %>% group_by(Month)
#      %>% View()
# 
# sales.clean %>% 
#   filter(order.date <="2013-12-31") %>% 
#   group_by(order.date) %>% 
#   mutate(Month = month(order.date)) %>% 
#   group_by(Month) %>% 
#   summarise(Total = sum(sales)) %>% View()
