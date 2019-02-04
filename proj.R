# rm(list=ls())
require(tidyverse)

# Get the data
## 500 Cities Data will be the main dataset
h1 <- read_csv('project/500_Cities__Local_Data_for_Better_Health__2018_release.csv') %>% 
  filter(StateAbbr=="FL" & GeographicLevel=="Census Tract"
         & DataValueTypeID=="CrdPrv") %>%  # Focus on Florida and Census Tract data, Crude Prevalence only
  mutate(Data_Value=as.numeric(Data_Value))

## Florida census tract income data from American Community Survey
i1 <- read_csv('project/ACS_16_5YR_S1901_with_ann.csv') %>% # 
  select(GEO.id:HC01_EST_VC01,
         HC01_EST_VC02, HC01_EST_VC03, HC01_EST_VC04, HC01_EST_VC05,
         HC01_EST_VC06, HC01_EST_VC07, HC01_EST_VC08, HC01_EST_VC09, 
         HC01_EST_VC10, HC01_EST_VC11, 
         HC01_EST_VC13, HC01_EST_VC15) %>% 
  mutate(GEO.id2=as.character(GEO.id2),
         HC01_EST_VC01=as.numeric(HC01_EST_VC01),
         HC01_EST_VC02=as.numeric(HC01_EST_VC02),
         HC01_EST_VC03=as.numeric(HC01_EST_VC03),
         HC01_EST_VC04=as.numeric(HC01_EST_VC04), 
         HC01_EST_VC05=as.numeric(HC01_EST_VC05),
         HC01_EST_VC06=as.numeric(HC01_EST_VC06), 
         HC01_EST_VC07=as.numeric(HC01_EST_VC07), 
         HC01_EST_VC08=as.numeric(HC01_EST_VC08), 
         HC01_EST_VC09=as.numeric(HC01_EST_VC09), 
         HC01_EST_VC10=as.numeric(HC01_EST_VC10), 
         HC01_EST_VC11=as.numeric(HC01_EST_VC11), 
         HC01_EST_VC13=as.numeric(HC01_EST_VC13), 
         HC01_EST_VC15=as.numeric(HC01_EST_VC15)
         ) %>% 
  rename(TractFIPS=GEO.id2,
         Tract_Cty_State="GEO.display-label",
         Nb_hh=HC01_EST_VC01,
         Nb_hh_below_10000=HC01_EST_VC02,
         Nb_hh_10000_14999=HC01_EST_VC03,
         Nb_hh_15000_24999=HC01_EST_VC04,
         Nb_hh_25000_34999=HC01_EST_VC05,
         Nb_hh_35000_49999=HC01_EST_VC06, 
         Nb_hh_50000_74999=HC01_EST_VC07, 
         Nb_hh_75000_99999=HC01_EST_VC08, 
         Nb_hh_100000_149999=HC01_EST_VC09, 
         Nb_hh_150000_199999=HC01_EST_VC10, 
         Nb_hh_200000_up=HC01_EST_VC11, 
         Median_income=HC01_EST_VC13, 
         Mean_income=HC01_EST_VC15
  )

# Calculate the proportion of the tract population that makes each these income levels:
# low (<25k), mid (25k-99999), high (>100k) 
i2 <- i1 %>% 
  # mutate(pc_hh_below_10000=Nb_hh_below_10000/Nb_hh,
  #        pc_hh_10000_14999=Nb_hh_10000_14999/Nb_hh,
  #        pc_hh_15000_24999=Nb_hh_15000_24999/Nb_hh,
  #        pc_hh_25000_34999=Nb_hh_25000_34999/Nb_hh,
  #        pc_hh_35000_49999=Nb_hh_35000_49999/Nb_hh,
  #        pc_hh_50000_74999=Nb_hh_50000_74999/Nb_hh,
  #        pc_hh_75000_99999=Nb_hh_75000_99999/Nb_hh,
  #        pc_hh_100000_149999=Nb_hh_100000_149999/Nb_hh,
  #        pc_hh_150000_199999=Nb_hh_150000_199999/Nb_hh,
  #        pc_hh_200000_up=Nb_hh_200000_up/Nb_hh
  #        )
mutate(Md_income_level=case_when(
  (Median_income<24000) ~ "Low",
  (Median_income>=24000) & (Median_income<100000) ~ "Mid",
  (Median_income>=100000) ~ "High",
  TRUE~NA_character_
  ))

counts_by_md_income_level <- i2 %>% 
  na.omit() %>% 
  group_by(Md_income_level) %>% 
  tally

## left_join the two based on TractFIPS
hi_df <- left_join(h1,i2,
                   by = c("TractFIPS")) 

# Rough descriptives

## For each income level and measure, calculate average response
health.inc <- hi_df %>% 
  group_by(MeasureId,Measure,Short_Question_Text, Md_income_level,Category) %>%
  summarize(ave_prop=mean(Data_Value,na.rm = T), sd_prop=sd(Data_Value,na.rm=T),  n = n()) %>% 
  na.omit()
  
health.inc.beh <- health.inc %>% 
  filter(Category=="Prevention" |
         Category=="Unhealthy Behaviors")


health.inc.out <- health.inc %>% 
  filter(Category=="Health Outcomes")


# ## Plot of all measures
# g <- ggplot(health.inc, 
#             aes(x=Md_income_level,
#                 y=ave_prop,
#                 fill=factor(Md_income_level)
#                 # group=Short_Question_Text
#                 )) +
#   geom_bar(stat="identity") +
#   scale_x_discrete(limits=c("Low","Mid","High")) +
#   scale_fill_discrete(limits=c("Low","Mid","High")) +
#   facet_grid(Category~Short_Question_Text) +
#   labs(x = NULL, y = "Proportion reporting"
#        ,fill="Income Level"
#        ,subtitle = "Low: median income < $24k. Mid: median income 24k-99k. High: median income 100k up") +
#   ggtitle("Health outcomes or behaviors according to census tract income level as of 2016") +
#   theme_minimal(base_size = 9)
# print(g)  

## Plot health outcomes
g2 <- ggplot(health.inc.out,
            aes(x=Md_income_level,
                y=ave_prop,
                colour=factor(Md_income_level),
                group=Short_Question_Text
                )) +
  geom_point(size=2) +
  geom_line(size=1.1) +
  scale_x_discrete(limits=c("Low","Mid","High"),labels=NULL) +
  scale_colour_discrete(limits=c("Low","Mid","High")) +
  facet_wrap(~Short_Question_Text) +
  labs(x = NULL, y = "Percent reporting"
       ,colour="Income level"
       ,subtitle = "Low: median income < $24k. Mid: median income $24k-99k. High: median income $100k up") +
  ggtitle("Health outcomes according to census tract income level as of 2016") +
  theme_bw(base_size = 11)

print(g2)

## Plot health and preventative behaviors
g3 <- ggplot(health.inc.beh,
             aes(x=Md_income_level,
                 y=ave_prop,
                 colour=factor(Md_income_level),
                 group=Short_Question_Text
             )) +
  geom_point(size=2) +
  geom_line(size=1.1) +
  scale_x_discrete(limits=c("Low","Mid","High"),labels=NULL) +
  scale_colour_discrete(limits=c("Low","Mid","High")) +
  facet_wrap(~Short_Question_Text) +
  labs(x = NULL, y = "Percent reporting"
       ,colour="Income level"
       ,subtitle = "Low: median income < $24k. Mid: median income $24k-99k. High: median income $100k up") +
  ggtitle("Health and prevention behaviors according to census tract income level as of 2016") +
  theme_bw(base_size = 11)

print(g3)


ggsave("outcomes.png"
       ,plot = g2
       ,width=9
       ,height=7)

ggsave("behaviors.png"
       ,plot = g3
       ,width=9
       ,height=7)
