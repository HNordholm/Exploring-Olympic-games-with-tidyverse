
## ------------------- 120 Years of Olympic history ------------------- ##
##                     Data analysis with Tidyverse                    ##
##                       Made by Hampus Nordholm                      ##
##                          Date: 2024-09-07                         ##
## ---------------------------------------------------------------- ##

## Main library ## 

library(tidyverse)

## -- Read data into dataframes -- 

ae_tbl <- read_csv("athlete_events.csv")

nr_tbl <- read_csv("noc_regions.csv")


## Quick overview of ae_tbl ## 

ae_tbl %>% glimpse()
ae_tbl %>% head(n=10)
View(ae_tbl)

## Note:15 columns, 271.116 rows 

## Quick overview NOC region ## 

nr_tbl %>% glimpse()
View(nr_tbl)

# Note: -> From noc  

## NA count ## 

colSums(is.na(ae_tbl))

## -- Approx 60k missing values for height and 60k for weight, 9474 for age.
## -- Medal 231333 NA:s--

# ---- Intro data manipulation  -----

## -- Convert to Female/Male

ae_tbl_c <- ae_tbl %>% 
  mutate(Sex=ifelse(Sex=="M","Male","Female"))

#Join Countryname tbl -> NOC -> Nationality

ae_tbl_c <- ae_tbl_c %>% left_join(nr_tbl,by="NOC") %>% 
  select(-notes,-NOC)

ae_tbl_c <- ae_tbl_c %>% rename(Nationality=region)


## -- EXPLORATORY DATA ANALYSIS -- 

## -Number of distinct athletes competing in Olympics by gender- ##

ae_tbl_c %>% distinct(ID, .keep_all = TRUE) %>% 
  group_by(Sex) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))

## Note:101590 men, 33981 women 

## -- Results visualization -- 

ae_tbl_c %>% distinct(ID, .keep_all = TRUE) %>% 
  group_by(Sex) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count)) %>% 
  
  ggplot(aes(x=Sex,y=count,fill=Sex))+
  geom_bar(stat="identity")+
  scale_fill_manual(values=c("Male"="skyblue","Female"="pink"))+
  labs(title ="Number of athletes in the Olympics between 1896-2016",
       subtitle="By gender",x=NULL,y=NULL)+
  theme_minimal()
  
## -Top 10 medal-winning nationalities- ## 

ae_tbl_c %>% filter(!is.na(Medal)) %>% 
  group_by(Nationality) %>% 
  summarise(total_medals=n()) %>% 
  arrange(desc(total_medals)) %>% 
  top_n(10,total_medals)

## -- Results visualization -- 

ae_tbl_c %>% filter(!is.na(Medal)) %>% 
  group_by(Nationality) %>% 
  summarise(total_medals=n()) %>% 
  arrange(desc(total_medals)) %>% 
  slice_max(order_by=total_medals,n=10) %>% 
  
  ggplot(aes(x=total_medals,y=reorder(Nationality,total_medals),fill=total_medals))+
  geom_bar(stat="identity")+
  scale_fill_gradient(low="lightblue",high="darkblue")+
  labs(title="Top 10 nationalities",
         subtitle="With most won medals",x="Total medals",y=NULL)+
  theme_minimal()
 

## -- Finding and visualizing the most succesful athletes --


ae_tbl_c %>% filter(!is.na(Medal)) %>% 
  group_by(ID,Name) %>% 
  summarise(total_medals=n(),.groups='drop') %>% 
  arrange(desc(total_medals)) %>% 
  slice_max(order_by=total_medals,n=5) %>% 
  
  ## -- Results visualization --
  
  ggplot(aes(x=total_medals,y=reorder(Name,total_medals),fill=total_medals))+
  geom_col()+
  geom_text(aes(label=total_medals),hjust=-0.2,size=4)+
  scale_fill_gradient(low="lightblue",high="darkblue")+
  labs(title="Most successful athletes by total medals",y=NULL,
       x="Total medals")+
  theme_minimal()

## -- Which sports has the highest vs lowest avg. age? -- 

## Highest --
ae_tbl_c %>% filter(!is.na(Age)) %>% 
  group_by(Sport) %>% 
  summarise(avg_age=mean(Age)) %>% 
  arrange(desc(avg_age))

## Lowest --
ae_tbl_c %>% filter(!is.na(Age)) %>% 
  group_by(Sport) %>% 
  summarise(avg_age=mean(Age)) %>% 
  arrange((avg_age))


## Note: Rhytmic gymnastics,swimming and figure skating -> sports -> lowest avg.age 
## vs. Roque,art competition and alpinism -> sports -> highest avg.age.

## -- Countries with most gold medals --

ae_tbl_c %>% filter(!is.na(Medal)&Medal=="Gold") %>% 
  group_by(Nationality) %>% 
  summarise(total_gold_medals=n()) %>% 
  arrange(desc(total_gold_medals)) %>% 
  slice_max(order_by=total_gold_medals,n=10)


## ---- Analyzing avg.age of medalists over time ---


ae_tbl_c %>% filter(!is.na(Medal)) %>% 
  group_by(Year,Medal) %>% 
  summarise(avg_age=mean(Age,na.rm=TRUE), .groups = 'drop') %>% 
  
  
  ggplot(aes(x=Year,y=avg_age,color=Medal))+
  geom_line(size=1.7,aes(linetype=Medal))+
  scale_color_manual(values=c("Gold"="Gold","Silver"="#C0C0C0","Bronze"="#cd7f32"))+
  theme_minimal()+
  labs(title="Average age of medalists over time",
       subtitle="Olympic games 1896-2016",
       x="Year",y=NULL)


## END ## 








