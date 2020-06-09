library(essurvey) # install from CRAN first
library(labelled)
library(tidyverse)
library(srvyr)




set_email("privalki@tcd.ie") # set your registered email


irl8 <- import_country(country = "Ireland",
                      rounds = c(8),
                      format = 'spss')
irl7 <- import_country(country = "Ireland",
                       rounds = c(7),
                       format = 'spss')
irl6 <- import_country(country = "Ireland",
                       rounds = c(6),
                       format = 'spss')


# explore and label -------------------------------------------------------

look_for(irl7, "income")
look_for(irl8, "gender")
look_for(irl8, "marital")
look_for(irl8, "years")
look_for(irl8, "firm")
look_for(irl8, "partner")



df <- irl8 %>% 
  select(gndr, marsts, 
         maritalb, eduyrs,
         hincsrca, hinctnta,
         dweight, pspwght) %>% 
  filter(hincsrca==1)



df

df$wages <- to_factor(df$hinctnta, drop_unused_labels=TRUE, ordered=TRUE)
table(df$wages)


df$marr <- to_factor(df$marsts, drop_unused_labels=TRUE, ordered=TRUE)
table(df$marr)

df$gndr <- to_factor(df$gndr, drop_unused_labels=TRUE)
table(df$marr)

df <- df %>% 
  mutate(w1 = fct_recode(wages, 
                           "top 20" ="J - 1st decile", 
                           "top 20" = "R - 2nd decile",
                           "mid" = "C - 3rd decile",
                           "mid" = "M - 4th decile",
                           "mid" = "M - 4th decile",
                           "mid"="F - 5th decile",
                           "mid"="S - 6th decile",
                           "mid"="K - 7th decile",
                           "mid"="P - 8th decile",
                           "bottom 20"="D - 9th decile",
                           "bottom 20"="H - 10th decile")) 


df %>% count(w1)


df_srvyr <- df %>% 
  filter(!is.na(w1),
         !is.na(marr),
         !is.na(gndr)) %>% 
  as_survey_design(ids = 1,
                   weight = pspwght)


out <- df_srvyr %>%
  group_by(gndr, marr, w1) %>% 
  summarize(proportion = survey_mean(),
            total = survey_total())
out




out %>% 
  ggplot(aes(x=marr,
             y=proportion,
             fill=w1))+
  geom_col()+
  coord_flip()+
  theme(legend.position="bottom")+
  facet_wrap(~gndr)

#+
  labs(title = "When asked whether large differences \nin income are acceptable to reward talents and \nefforts, most disagree.",
       subtitle = "Even when we split the outcome by income decile, \nmost disagree.",
       y="Proportion of income decile",
       x="",
       fill="",
       caption = "Source: European Social Survey. \nPlot: @privlko")
