### NBA draft pick value trade chart

library(tidyverse)
library(readr)
library(dplyr)
library(splines)
library(ggplot2)
library(ggpubr)

theme_set(theme_bw())
theme_update(
  text = element_text(size=20),
  plot.title = element_text(hjust = 0.5),
  axis.title = element_text(size=20),
  axis.text = element_text(size=15),
  legend.text = element_text(size=20),
  legend.title = element_text(size=20)
)
install.packages("tidyverse")
install.packages("ggpubr")
install.packages("hoopR")
install.packages("readr")

nba_stats_raptor <- NBA_Stat_Sheet <- read_csv("Final Projec/NBA_Stat_Sheet.csv")
nba_stats_darko <- read_csv("Final Projec/DARKO.csv")
nba_stats_draft <- read_csv("Final Projec/Draft_Data.csv")
nba_stats_draft_2 <- read_csv("Final Projec/Draft_Data_2.csv")
nba_stats_draft_3 <- read_csv("Final Projec/Draft_Data_2.csv")


nba_stats_raptor_2 <- nba_stats_raptor %>% 
  filter(Year >= 2010) %>% 
  select(player_ID, Player, Year, Tm, G, MP, WAR, "WAR/82", PER, "WS/48", OBPM, DBPM, BPM, eRO, eRD, eRT, Type) %>%
  rename(season = Year)

nba_stats_darko <- nba_stats_darko %>% 
  filter(season >= 2010) %>% 
  rename(Player = player_name ) %>% 
  select(nba_id, Player, season, team_name, dpm, o_dpm, d_dpm)


nba_stats_draft <- nba_stats_draft %>% 
  filter(DraftYr >= 1980) %>% 
  select(Rk, Pk, Tm, Player, DraftYr)

nba_stats_draft <- nba_stats_draft_2 %>% 
  filter(DraftYr >= 1980) %>% 
  select(Rk, Pk, Tm, Player, DraftYr)

nba_stats_draft <- nba_stats_draft_3 %>% 
  filter(DraftYr >= 1980) %>% 
  select(Rk, Pk, Tm, Player, DraftYr)

draft_data <- nba_stats_draft %>% 
  filter(DraftYr >= 1980) %>% 
  select(Rk, Pk, Tm, Player, DraftYr)


merged_right <- right_join(nba_stats_draft, nba_stats_raptor, by = "Player")

merged_right_2 <- right_join(nba_stats_draft, nba_stats_raptor_2, by = "Player")

merged_right <- merged_right %>% 
  rename(season = Year)
merged_right_more <- right_join(merged_right, nba_stats_darko, by = c("Player", "season"))

merged_right_more_2 <- right_join(merged_right_2, nba_stats_darko, by = c("Player", "season"))

nba_stats_final <- merged_right_more_2 %>%
  select(Pk, DraftYr, Tm.x, Player, season, G, MP, "WS/48",  OBPM, DBPM, BPM, WAR, "WAR/82",
         dpm, o_dpm, d_dpm, eRO, eRD, eRT) %>% 
  rename(Team = Tm.x, Pick = Pk, DraftYear = DraftYr,
         Darko = dpm, O_Darko = o_dpm, D_Darko = d_dpm, Season = season,
         O_Raptor = eRO, D_Raptor = eRD, Raptor = eRT, O_BPM = OBPM, D_BPM = DBPM) %>% 
  filter(G > 20, MP > 300, Season <= 2021) %>% 
  replace_na(list(Pick = 0, Team = "Undrafted", DraftYear = 0))

player_career_stats <- nba_stats_final %>% 
  group_by(Player) %>% 
  summarize(
    Total_Games = sum(G),
    Total_MP = sum(MP),
    Total_O_BPM = sum(O_BPM),
    Avg_O_BPM = mean(O_BPM),
    `Total_WS/48` = sum(`WS/48`),
    ##Avg_O_WS/"48" = mean(WS/"48"),
    Total_BPM = sum(BPM),
    Avg_BPM = mean(BPM),
    Total_WAR = sum(WAR),
    Avg_WAR = mean(WAR),
    ##Total_WAR/82 = sum(WAR/82),
    ##Avg_WAR/82 = mean(WAR/82),
    Total_Darko = sum(Darko),
    Avg_Darko = mean(Darko),
    Total_O_Darko = sum(O_Darko),
    Avg_O_Darko = mean(O_Darko),
    Total_D_Darko = sum(D_Darko),
    Avg_D_Darko = mean(D_Darko),
    Total_Raptor = sum(Raptor),
    Avg_Raptor = mean(Raptor),
    Total_O_Raptor = sum(O_Raptor),
    Avg_Raptor = mean(O_Raptor),
    Total_D_Raptor = sum(D_Raptor),
    Avg_D_Raptor = mean(D_Raptor)
  )

Draft_Data <- nba_stats_draft %>% 
  select(Pk, Tm, Player, DraftYr)

player_career_stats <- right_join(Draft_Data, player_career_stats, by = c("Player"))


ggplot(data = player_career_stats) +
  geom_point(aes(x = Avg_Raptor, y = Player))
##---------------------------------------------

salary_cap_df = tibble(
  season = 2000:2020,
  cap = c(
    35500000,
    42500000,
    40271000,
    43840000,
    43870000,
    49500000,
    53135000,
    55630000,
    58680000,
    57700000,
    58040000,
    58044000,
    58044000,
    58679000,
    63065000,
    70000000,
    94134000,
    99093000,
    101869000,
    109140000,
    109140000
  )
)

super_max_df = tibble(
  season = 2000:2020,
  max = c(
    14000000,
    14000000,
    14875000,
    14094850,
    15344000,
    15355000,
    16800000,
    17437000,
    18257750,
    19261000,
    18928700,
    19045250,
    18091071,
    19136250,
    19181750,
    20644400,
    22970500,
    30963450,
    34682550,
    35654150,
    38199000
  )
)
nba_salaries_0 = read_csv("nba-salaries.csv")
nba_salaries = nba_salaries_0 %>% 
  left_join(salary_cap_df)

nba_salaries_3 = nba_salaries_0 %>% 
  left_join(super_max_df)

nba_salaries_career = nba_salaries %>% 
  mutate(salary_pct = salary/cap) %>% 
  group_by(name) %>% 
  reframe(
    total_career_salary_pct = sum(salary_pct),
    avg_career_salary = sum(salary_pct)/n()
  ) %>% 
  rename(Player = name)

nba_salaries_career_2 = nba_salaries_3 %>% 
  mutate(super_max_pct = salary/max) %>% 
  group_by(name) %>% 
  reframe(
    total_super_max_pct = sum(super_max_pct),
    avg_super_max_pct = sum(super_max_pct)/n()
  ) %>% 
  rename(Player = name)

player_career_stats_1 = player_career_stats %>% 
  left_join(nba_salaries_career) %>% 
  left_join(nba_salaries_career_2) %>% 
  mutate(
    Total_Raptor = Total_Raptor - mean(Total_Raptor, na.rm = TRUE),
    Avg_Raptor = Avg_Raptor - mean(Avg_Raptor, na.rm = TRUE),
    Total_BPM = Total_BPM - mean(Total_BPM, na.rm = TRUE),
    Avg_BPM = Avg_BPM - mean(Avg_BPM, na.rm = TRUE)
  )

##---------------------------------------------

basic_avg_raptor_2 <- player_career_stats %>% 
  ggplot(aes(x = Pk, y = Avg_Raptor)) +
  geom_point()

df_avgRaptor_meanGraph = player_career_stats_1 %>% 
  group_by(Pk) %>% 
  summarise(mean_avg_raptor = mean(Avg_Raptor)) %>% 
  drop_na()

avg_raptor_graph <- df_avgRaptor_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  geom_point()




#----------------------------------------

model_1_avg_raptor = lm(mean_avg_raptor ~ Pk, data = df_avgRaptor_meanGraph)
##model_2_avg_raptor = lm(mean_avg_raptor ~ bs(Pk, knots=c(6)), data = df_avgRaptor_meanGraph)

df_avg_raptor_meanGraph_1 = df_avgRaptor_meanGraph %>% drop_na()
smooth_spline_avg_raptor_meanGraph = smooth.spline(x = df_avg_raptor_meanGraph_1$Pk, y = df_avg_raptor_meanGraph_1$mean_avg_raptor, df=5)
plot(smooth_spline_avg_raptor_meanGraph$y)

#spline_model_version_2 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~Pk)

spline_model_version_2 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~bs(Pk, knots = c(7)))


avg_raptor_graph_spline <- df_avgRaptor_meanGraph %>% 
  mutate(smoothed_avg_raptor_spline = smooth_spline_avg_raptor_meanGraph$y) %>% 
  mutate(spline_version_2_avg_raptor = predict(spline_model_version_2, .)) %>% 
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_raptor_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_avg_raptor), color = 'blue')

avg_raptor_graph_spline

#----------------------------------------

avg_raptor_histogram <- player_career_stats %>% 
  mutate(Pk_str = paste0("Pick = ", Pk)) %>% 
  filter(Pk %in% c(1, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) %>%
  group_by(Pk) %>% 
  mutate(mean_value = mean(Avg_Raptor)) %>% 
  mutate(median_value = median(Avg_Raptor)) %>% 
  ggplot() +
  #facet_wrap(~ Pk) +
  facet_wrap(~ fct_reorder(Pk_str,Pk), nrow = 2) +
  geom_density(aes(x = Avg_Raptor)) +
  geom_vline(aes(xintercept = mean_value), color = 'red') +
  geom_vline(aes(xintercept = median_value), color = 'blue') +
  labs(x = "Average Raptor")

avg_BPM_histogram <- player_career_stats %>% 
  mutate(Pk_str = paste0("Pick = ", Pk)) %>% 
  filter(Pk %in% c(1, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) %>%
  group_by(Pk) %>% 
  mutate(mean_value = mean(Avg_BPM)) %>% 
  mutate(median_value = median(Avg_BPM)) %>% 
  ggplot() +
  #facet_wrap(~ Pk) +
  facet_wrap(~ fct_reorder(Pk_str,Pk), nrow = 2) +
  geom_density(aes(x = Avg_BPM)) +
  geom_vline(aes(xintercept = mean_value), color = 'red') +
  geom_vline(aes(xintercept = median_value), color = 'blue') +
  labs(x = "Average BPM")

#Make BPM version after making B-Spline

avg_raptor_histogram_new <- player_career_stats_pred %>% 
  mutate(Pk_str = paste0("Pick = ", Pk)) %>% 
  filter(Pk %in% c(1, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) %>%
  group_by(Pk) %>% 
  mutate(mean_value = mean(pred_avg_career_salary_avg_raptor)) %>% 
  mutate(median_value = median(pred_avg_career_salary_avg_raptor)) %>% 
  ggplot() +
  #facet_wrap(~ Pk) +
  facet_wrap(~ fct_reorder(Pk_str,Pk), nrow = 2) +
  geom_density(aes(x = pred_avg_career_salary_avg_raptor)) +
  geom_vline(aes(xintercept = mean_value), color = 'red') +
  geom_vline(aes(xintercept = median_value), color = 'blue') +
  labs(x = "Predicted Average Career Salary (From Average Raptor)")

avg_BPM_histogram_new <- player_career_stats_pred %>% 
  mutate(Pk_str = paste0("Pick = ", Pk)) %>% 
  filter(Pk %in% c(1, 3, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60)) %>%
  group_by(Pk) %>% 
  mutate(mean_value = mean(pred_avg_career_salary_avg_BPM)) %>% 
  mutate(median_value = median(pred_avg_career_salary_avg_BPM)) %>% 
  ggplot() +
  #facet_wrap(~ Pk) +
  facet_wrap(~ fct_reorder(Pk_str,Pk), nrow = 2) +
  geom_density(aes(x = pred_avg_career_salary_avg_BPM)) +
  geom_vline(aes(xintercept = mean_value), color = 'red') +
  geom_vline(aes(xintercept = median_value), color = 'blue') +
  labs(x = "Predicted Average Career Salary (From Average BPM)") 

library(patchwork)

avg_raptor_histogram + avg_raptor_histogram_new



#----------------------------------------
model_1_avg_raptor = lm(mean_avg_raptor ~ Pk, data = df_avgRaptor_meanGraph)
##model_2_avg_raptor = lm(mean_avg_raptor ~ bs(Pk, knots=c(6)), data = df_avgRaptor_meanGraph)

df_avg_raptor_meanGraph_1 = df_avgRaptor_meanGraph %>% drop_na()
smooth_spline_avg_raptor_meanGraph = smooth.spline(x = df_avg_raptor_meanGraph_1$Pk, y = df_avg_raptor_meanGraph_1$mean_avg_raptor, df=5)
plot(smooth_spline_avg_raptor_meanGraph$y)

#spline_model_version_2 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~Pk)

spline_model_version_2 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~bs(Pk, knots = c(7)))

k = 15
df_avg_raptor_meanGraph_1 = df_avg_raptor_meanGraph_1 %>% 
  mutate(new_x = ifelse(Pk <= k, -2*Pk+Pk^2, -2*k+k^2))
custom_model_version_3 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~1 + new_x)

avg_raptor_graph_spline <- df_avgRaptor_meanGraph %>% 
  mutate(smoothed_avg_raptor_spline = smooth_spline_avg_raptor_meanGraph$y) %>% 
  mutate(pred_model_avg_raptor = predict(spline_model_version_2, .)) %>%
  mutate(new_x = ifelse(Pk <= k, -2*Pk+Pk^2, -2*k+k^2)) %>% 
  mutate(custom_model_version_3_mean_avg_raptor = predict(custom_model_version_3, .)) %>% 
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_raptor_spline), color = 'red') +
  geom_line(aes(y = pred_model_avg_raptor), color = 'blue') +
  geom_line(aes(y = custom_model_version_3_mean_avg_raptor), color = 'green')

avg_raptor_graph_spline

df_avgRaptor_meanGraph <- df_avgRaptor_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_avg_raptor = predict(spline_model_version_2, .),
    #pred_model_2_avg_raptor_shifted_new = pred_wow - mean(pred_wow, na.rm=T),
    pred_avgRaptorMeanGraph_scaled_new = pred_model_avg_raptor/max(pred_model_avg_raptor, na.rm=T),
  )


avg_raptor_graph <- df_avgRaptor_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  #geom_point() +
  # geom_line(aes(y = pred_model_1_avg_raptor), color="magenta", size=1) +
  #geom_line(aes(y = pred_model_2_avg_raptor), color="cyan", size=1) +
  #geom_line(aes(y = pred_model_2_avg_raptor_shifted), color="green", size=1) +
  #geom_line(aes(y = pred_avgRaptorMeanGraph_scaled), color="red", size=1) +
  geom_line(aes(y = pred_model_avg_raptor), color = "gold", size = 1) +
  geom_line(aes(y = pred_avgRaptorMeanGraph_scaled_new), color = "blue", size = 1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  #scale_y_continuous(breaks=seq(3,3,by=0.5)) +
  geom_hline(yintercept=0)

avg_raptor_graph_2 <- df_avgRaptor_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_avgRaptorMeanGraph_scaled_new), color = "gold", size = 1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0) +
  labs(x = "Pick", y = "Mean Average Raptor") 

avg_raptor_graph

##----------------------------------------------
model_1_total_raptor = lm(mean_total_raptor ~ Pk, data = df_totalRaptor_meanGraph)
##model_2_avg_raptor = lm(mean_avg_raptor ~ bs(Pk, knots=c(6)), data = df_avgRaptor_meanGraph)

df_total_raptor_meanGraph_1 = df_totalRaptor_meanGraph %>% drop_na()
smooth_spline_total_raptor_meanGraph = smooth.spline(x = df_total_raptor_meanGraph_1$Pk, y = df_totalRaptor_meanGraph$mean_total_raptor, df=5)
plot(smooth_spline_total_raptor_meanGraph$y)

#spline_model_version_2 = lm(data = df_avg_raptor_meanGraph_1, mean_avg_raptor~Pk)

spline_model_version_2 = lm(data = df_total_raptor_meanGraph_1, mean_total_raptor~bs(Pk, knots = c(7)))

total_raptor_graph_spline <- df_total_raptor_meanGraph_1 %>% 
  #mutate(smoothed_avg_raptor_spline = smooth_spline_avg_raptor_meanGraph$y) %>% 
  mutate(pred_model = predict(spline_model_version_2, .)) %>%
  ggplot(aes(x = Pk, y = mean_avg_raptor)) +
  #geom_point() +
  geom_line(aes(y = spline_version_2_avg_raptor), color = 'blue')

total_raptor_graph_spline



df_totalRaptor_meanGraph <- df_totalRaptor_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model = predict(spline_model_version_2, .),
    #pred_model_2_avg_raptor_shifted_new = pred_wow - mean(pred_wow, na.rm=T),
    pred_totalRaptorMeanGraph_scaled_new = pred_model/max(pred_model, na.rm=T),
  )

##----------------------------------------------

df_totalRaptor_meanGraph = player_career_stats_1 %>%
  group_by(Pk) %>%
  summarise(mean_total_raptor = mean(Total_Raptor)) 

model_1 = lm(mean_total_raptor ~ Pk, data = df_totalRaptor_meanGraph)
model_1_total_raptor = lm(mean_total_raptor ~ Pk, data = df_totalRaptor_meanGraph)
model_2 = lm(mean_total_raptor ~ bs(Pk, knots=c(13)), data = df_totalRaptor_meanGraph)
df_total_raptor_meanGraph_1 = df_totalRaptor_meanGraph %>% drop_na()

#old
smooth_spline_total_raptor_meanGraph = smooth.spline(x = df_total_raptor_meanGraph_1$Pk, y = df_total_raptor_meanGraph_1$mean_total_raptor, df=3)
plot(smooth_spline_total_raptor_meanGraph$y)

#new
spline_model_version_2 = lm(data = df_total_raptor_meanGraph_1, mean_total_raptor~bs(Pk, knots = c(7)))

#combine
total_raptor_graph_spline <- df_totalRaptor_meanGraph %>% 
  mutate(smoothed_total_raptor_spline = smooth_spline_total_raptor_meanGraph$y) %>% 
  mutate(pred_model_total_raptor = predict(spline_model_version_2, .)) %>% 
  ggplot(aes(x = Pk, y = mean_total_raptor)) +
  geom_point() +
  geom_line(aes(y = smoothed_total_raptor_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_total_raptor), color = 'blue')

total_raptor_graph_spline

#combine
df_totalRaptor_meanGraph = df_totalRaptor_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_total_raptor = predict(spline_model_version_2, .),
    pred_model_1_new_scaled = pred_model_total_raptor/max(pred_model_total_raptor)
  )

total_raptor_graph <- df_totalRaptor_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_total_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_new), color="magenta", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_raptor_graph_2 <-df_totalRaptor_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_total_raptor)) +
  geom_line(aes(y = pred_model_1_new_scaled), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_raptor_graph

##----------------------------------------------

df_avg_BPM_meanGraph = player_career_stats %>% 
  group_by(Pk) %>% 
  summarise(mean_Avg_BPM = mean(Avg_BPM))

model_1_avg_BPM = lm(mean_Avg_BPM ~ Pk, data = df_avg_BPM_meanGraph)
##model_2_avg_BPM = lm(mean_Avg_BPM ~ bs(Pk, knots=c(13)), data = df_avg_BPM_meanGraph)

#old
df_avg_BPM_meanGraph_1 = df_avg_BPM_meanGraph %>% drop_na()
smooth_spline_avg_BPM_meanGraph = smooth.spline(x = df_avg_BPM_meanGraph_1$Pk, y = df_avg_BPM_meanGraph_1$mean_Avg_BPM, df=4)
plot(smooth_spline_avg_BPM_meanGraph$y)

#new
spline_model_version_2_avg_BPM = lm(data = df_avg_BPM_meanGraph_1, mean_Avg_BPM~bs(Pk, knots = c(4)))

#combine
avg_BPM_graph_spline <- df_avg_BPM_meanGraph %>% 
  #mutate(smoothed_avg_BPM_spline = smooth_spline_avg_BPM_meanGraph$y) %>% 
  mutate(pred_model_2_avg_BPM_final = predict(spline_model_version_2_avg_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = mean_Avg_BPM)) +
  geom_point() +
  #geom_line(aes(y = smoothed_avg_BPM_spline), color = 'red') +
  geom_line(aes(y = pred_model_2_avg_BPM_final), color = 'blue')

avg_BPM_graph_spline


df_avg_BPM_meanGraph <- df_avg_BPM_meanGraph %>%
  drop_na() %>%
  mutate(
    pred_model_1_avg_BPM = predict(model_1_avg_BPM, .),
    pred_model_2_avg_BPM_final = predict(spline_model_version_2_avg_BPM, .),
    #pred_model = smooth_spline_avg_BPM_meanGraph$y,
    #pred_model_2_avg_BPM_shifted = pred_model_2_avg_BPM - min(pred_model_2_avg_BPM, na.rm=T),
    pred_avgBPMMeanGraph_scaled = pred_model_2_avg_BPM_final/max(pred_model_2_avg_BPM_final, na.rm=T),
  )

avg_BPM_graph <- df_avg_BPM_meanGraph %>%  
  ggplot(aes(x = Pk, y = mean_Avg_BPM)) +
  geom_point() +
  #geom_line(aes(y = pred_model_1_avg_BPM), color="magenta", size=1) +
  geom_line(aes(y = pred_model_2_avg_BPM_final), color="gold", size=1) +
  labs(x = "Pick", y = "Mean Average BPM") 
#geom_line(aes(y = pred_model_2_avg_BPM_shifted), color="green", size=1) +
#geom_line(aes(y = pred_avgBPMMeanGraph_scaled), color="red", size=1)

avg_BPM_graph

##---------------------------------

df_total_BPM_meanGraph = player_career_stats %>% 
  group_by(Pk) %>% 
  summarise(mean_total_BPM = mean(Total_BPM))

model_1_total_BPM = lm(mean_total_BPM ~ Pk, data = df_total_BPM_meanGraph)
##model_2_total_BPM = lm(mean_total_BPM ~ bs(Pk, knots=c(14,45)), data = df_total_BPM_meanGraph)

#old
df_total_BPM_meanGraph_1 = df_total_BPM_meanGraph %>% drop_na()
smooth_spline_total_BPM_meanGraph = smooth.spline(x = df_total_BPM_meanGraph_1$Pk, y = df_total_BPM_meanGraph_1$mean_total_BPM, df=2.6)
plot(smooth_spline_total_BPM_meanGraph$y)

#new
spline_model_version_2_total_BPM = lm(data = df_total_BPM_meanGraph_1, mean_total_BPM~bs(Pk, knots = c(7)))

#combine
total_BPM_graph_spline <- df_total_BPM_meanGraph %>% 
  mutate(smoothed_total_BPM_spline = smooth_spline_total_BPM_meanGraph$y) %>% 
  mutate(pred_model_2_total_BPM = predict(spline_model_version_2_total_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = mean_total_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_total_BPM), color = 'blue')

total_BPM_graph_spline


df_total_BPM_meanGraph <- df_total_BPM_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_total_BPM = predict(model_1_total_BPM, .),
    pred_model_2_total_BPM = predict(spline_model_version_2_total_BPM, .),
    pred_model = smooth_spline_total_BPM_meanGraph$y,
    pred_totalBPMMeanGraph_scaled = pred_model_2_total_BPM/max(pred_model_2_total_BPM, na.rm=T),
  )

total_BPM_graph <- df_total_BPM_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_total_BPM)) +
  geom_point() +
  #geom_line(aes(y = pred_model_1_total_BPM), color="magenta", size=1) +
  geom_line(aes(y = pred_model_2_total_BPM), color="cyan", size=1) +
  #geom_line(aes(y = pred_model_2_total_BPM_shifted), color="green", size=1) +
  #geom_line(aes(y = pred_totalBPMMeanGraph_scaled), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  scale_y_continuous(breaks=seq(3,3,by=0.5)) +
  geom_hline(yintercept=0) +
  labs(x = "Pick", y = "Mean Total BPM") 



##-------------------------------------

df_total_WAR_meanGraph = player_career_stats %>% 
  group_by(Pk) %>% 
  summarise(mean_total_WAR = mean(Total_WAR))

model_1_total_WAR = lm(mean_total_WAR ~ Pk, data = df_total_WAR_meanGraph)
##model_2_total_WAR = lm(mean_total_WAR ~ bs(Pk, knots=c(12)), data = df_total_WAR_meanGraph)

df_total_WAR_meanGraph_1 = df_total_WAR_meanGraph %>% drop_na()
smooth_spline_total_WAR_meanGraph = smooth.spline(x = df_total_WAR_meanGraph_1$Pk, y = df_total_WAR_meanGraph_1$mean_total_WAR, df=2.7)
plot(smooth_spline_total_WAR_meanGraph$y)

#new
spline_model_version_2_total_WAR = lm(data = df_total_WAR_meanGraph_1, mean_total_WAR~bs(Pk, knots = c(10)))

#combine
total_WAR_graph_spline <- df_total_WAR_meanGraph %>% 
  mutate(smoothed_total_WAR_spline = smooth_spline_total_WAR_meanGraph$y) %>% 
  mutate(spline_version_2_total_WAR = predict(spline_model_version_2_total_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = mean_total_WAR)) +
  geom_point() +
  geom_line(aes(y = smoothed_total_WAR_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_total_WAR), color = 'blue')

total_WAR_graph_spline


df_total_WAR_meanGraph <- df_total_WAR_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_total_WAR = predict(model_1_total_WAR, .),
    pred_model_2_total_WAR = predict(spline_model_version_2_total_WAR, .),
    #pred_model_2 = smooth_spline_total_WAR_meanGraph$y,
    #pred_model_2_total_WAR_shifted = pred_model_2_total_WAR - min(pred_model_2_total_WAR, na.rm=T),
    pred_totalWARMeanGraph_scaled = pred_model_2_total_WAR/max(pred_model_2_total_WAR, na.rm=T),
    pred_totalWARMeanGraph_scaled_2 = pred_model_2_total_WAR/max(pred_model_2_total_WAR, na.rm=T),
  )

total_WAR_graph <- df_total_WAR_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_total_WAR)) +
  #geom_point() +
  #geom_line(aes(y = pred_model_1_total_WAR), color="magenta", size=1) +
  #geom_line(aes(y = pred_model_2_total_WAR), color="cyan", size=1) +
  #geom_line(aes(y = pred_model_2_total_WAR_shifted), color="green", size=1) +
  geom_line(aes(y = pred_totalWARMeanGraph_scaled), color="red", size=1) +
  geom_line(aes(y = pred_totalWARMeanGraph_scaled_2), color="gold", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  #scale_y_continuous(breaks=seq(3,3,by=0.5)) +
  geom_hline(yintercept=0)

total_WAR_graph

##----------------------------------------------

df_avg_WAR_meanGraph = player_career_stats %>% 
  group_by(Pk) %>% 
  summarise(mean_avg_WAR = mean(Avg_WAR))

model_1_avg_WAR = lm(mean_avg_WAR ~ Pk, data = df_avg_WAR_meanGraph)
##model_2_avg_WAR = lm(mean_avg_WAR ~ bs(Pk, knots=c(40)), data = df_avg_WAR_meanGraph)

df_avg_WAR_meanGraph_1 = df_avg_WAR_meanGraph %>% drop_na()
smooth_spline_avg_WAR_meanGraph = smooth.spline(x = df_avg_WAR_meanGraph_1$Pk, y = df_avg_WAR_meanGraph_1$mean_avg_WAR, df=3)
plot(smooth_spline_avg_WAR_meanGraph$y)

#new
spline_model_version_2_avg_WAR = lm(data = df_avg_WAR_meanGraph_1, mean_avg_WAR~bs(Pk, knots = c(7)))

#combine
avg_WAR_graph_spline <- df_avg_WAR_meanGraph %>% 
  mutate(smoothed_avg_WAR_spline = smooth_spline_avg_WAR_meanGraph$y) %>% 
  mutate(pred_model_2_avg_WAR = predict(spline_model_version_2_avg_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = mean_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_WAR_spline), color = 'red') +
  geom_line(aes(y = pred_model_2_avg_WAR), color = 'blue')

avg_WAR_graph_spline

df_avg_WAR_meanGraph <- df_avg_WAR_meanGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_avg_WAR = predict(model_1_avg_WAR, .),
    ##pred_model_2_avg_WAR = predict(model_2_avg_WAR, .),
    pred_model_2_avg_WAR = predict(spline_model_version_2_avg_WAR, .),
    pred_avgWARMeanGraph_scaled = pred_model_2_avg_WAR/max(pred_model_2_avg_WAR, na.rm=T),
  ) 

avg_WAR_graph <- df_avg_WAR_meanGraph %>% 
  ggplot(aes(x = Pk, y = mean_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_avg_WAR), color="magenta", size=1) +
  geom_line(aes(y = pred_model_2_avg_WAR), color="cyan", size=1) +
  geom_line(aes(y = pred_model_2_avg_WAR_shifted), color="green", size=1) +
  geom_line(aes(y = pred_avgWARMeanGraph_scaled), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  scale_y_continuous(breaks=seq(3,3,by=0.5)) +
  geom_hline(yintercept=0)

mean_based_chart <- ggplot() +
  geom_line(aes(x = 1:60,y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled[1:60], color="Avg War"), size=1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled[1:60], color="Total War"), size=1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled[1:60], color="Total BPM"), size=1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled[1:60], color="Avg BPM"), size=1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_totalRaptorMeanGraph_scaled[1:60], color="Total Raptor"), size=1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled[1:60], color="Avg Raptor"), size=1) +
  scale_x_continuous(breaks=c(0, 15, 30, 45, 60)) +
  labs(title = "Mean Based NBA Draft Value Chart",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Legend") +
  theme_minimal() +
  scale_color_manual(values = c("Avg War" = "red",
                                "Total War" = "cyan",
                                "Total BPM" = "gold",
                                "Avg BPM" = "blue",
                                "Total Raptor" = "green",
                                "Avg Raptor" = "pink"))

##---------------------------------------

df_avgRaptor_medianGraph = player_career_stats %>% 
  group_by(Pk) %>% 
  summarise(median_avg_raptor = median(Avg_Raptor))

model_1_avg_raptor = lm(median_avg_raptor ~ Pk, data = df_avgRaptor_medianGraph)
##model_2_avg_raptor = lm(median_avg_raptor ~ bs(Pk, knots=c(22)), data = df_avgRaptor_medianGraph)

#old
df_avg_raptor_medianGraph_1 = df_avgRaptor_medianGraph %>% drop_na()
smooth_spline_avg_raptor_medianGraph = smooth.spline(x = df_avg_raptor_medianGraph_1$Pk, y = df_avg_raptor_medianGraph_1$median_avg_raptor, df=3.5)
plot(smooth_spline_avg_raptor_medianGraph$y)

#new
spline_model_version_2_median_avg_raptor = lm(data = df_avg_raptor_medianGraph_1, median_avg_raptor~bs(Pk, knots = c(7)))

#combine
median_avg_raptor_graph_spline <- df_avgRaptor_medianGraph %>% 
  mutate(smoothed_avg_raptor_spline = smooth_spline_avg_raptor_medianGraph$y) %>% 
  mutate(pred_model = predict(spline_model_version_2_median_avg_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = median_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_raptor_spline), color = 'red') +
  geom_line(aes(y = pred_model), color = 'blue')

df_avgRaptor_medianGraph <- df_avgRaptor_medianGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_avg_raptor = predict(model_1_avg_raptor, .),
    pred_model_2_avg_raptor_2 = predict(spline_model_version_2_median_avg_raptor, .),
    pred_model_2_avg_raptor = smooth_spline_avg_raptor_medianGraph$y,
    pred_avgRaptorMedianGraph_scaled = pred_model_2_avg_raptor/max(pred_model_2_avg_raptor, na.rm=T)
  )

avg_raptor_graph_median <- df_avgRaptor_medianGraph %>% 
  ggplot(aes(x = Pk, y = median_avg_raptor)) +
  geom_point() +
  #geom_line(aes(y = pred_model_1_avg_raptor), color="magenta", size=1) +
  geom_line(aes(y = pred_model_2_avg_raptor), color="cyan", size=1) +
  #geom_line(aes(y = pred_model_2_avg_raptor_shifted), color="green", size=1) +
  #geom_line(aes(y = pred_avgRaptorMedianGraph_scaled), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  scale_y_continuous(breaks=seq(3,3,by=0.5)) +
  geom_hline(yintercept=0)

##--------------------------------------------------------

df_totalRaptor_medianGraph <- player_career_stats %>%
  group_by(Pk) %>%
  summarise(median_total_raptor = median(Total_Raptor))

# Fit linear models using median_total_raptor
model_1 <- lm(median_total_raptor ~ Pk, data = df_totalRaptor_medianGraph)
##model_2 <- lm(median_total_raptor ~ bs(Pk, knots=c(7,20)), data = df_totalRaptor_medianGraph)

#old
df_total_raptor_medianGraph_1 = df_totalRaptor_medianGraph %>% drop_na()
smooth_spline_total_raptor_medianGraph = smooth.spline(x = df_total_raptor_medianGraph_1$Pk, y = df_total_raptor_medianGraph_1$median_total_raptor, df=2.3)
plot(smooth_spline_total_raptor_medianGraph$y)

#new
spline_model_version_2_median_total_raptor = lm(data = df_total_raptor_medianGraph_1, median_total_raptor~bs(Pk, knots = c(7)))

#combine
median_total_raptor_graph_spline <- df_totalRaptor_medianGraph %>% 
  #mutate(smoothed_total_raptor_spline = smooth_spline_total_raptor_medianGraph$y) %>% 
  mutate(pred_model_2_total_raptor = predict(spline_model_version_2_median_total_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = median_total_raptor)) +
  geom_point() +
  #geom_line(aes(y = smoothed_total_raptor_spline), color = 'red') +
  geom_line(aes(y = pred_model_2_total_raptor), color = 'blue')

median_total_raptor_graph_spline

# Generate predictions
df_totalRaptor_medianGraph <- df_totalRaptor_medianGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1 = predict(model_1, .),
    pred_model_2_old = predict(spline_model_version_2_median_total_raptor, .),
    #pred_model_2_total_raptor = smooth_spline_total_raptor_medianGraph$y,
    pred_model_2_total_raptor = predict(spline_model_version_2_median_total_raptor, .),
    pred_totalRaptorMedianGraph_scaled = pred_model_2_total_raptor/max(pred_model_2_total_raptor, na.rm = TRUE)
  )

# Plotting with ggplot using median values
total_raptor_graph_2 <- df_totalRaptor_medianGraph %>% 
  ggplot(aes(x = Pk, y = median_total_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_1), color = "magenta", size = 1) +
  geom_line(aes(y = pred_model_2_total_raptor), color = "cyan", size = 1) +
  geom_line(aes(y = pred_model_2_shifted), color = "green", size = 1) +
  geom_line(aes(y = pred_totalRaptorMedianGraph_scaled), color = "red", size = 1) +
  scale_x_continuous(breaks = seq(1, 60, by = 5)) +
  geom_hline(yintercept = 0)

##--------------------------------

df_avg_BPM_medianGraph <- player_career_stats %>%
  group_by(Pk) %>%
  summarise(median_Avg_BPM = median(Avg_BPM))

model_1_avg_BPM <- lm(median_Avg_BPM ~ Pk, data = df_avg_BPM_medianGraph)
##model_2_avg_BPM <- lm(median_Avg_BPM ~ bs(Pk, knots=c(13)), data = df_avg_BPM_medianGraph)

df_avg_BPM_medianGraph_1 = df_avg_BPM_medianGraph %>% drop_na()
smooth_spline_avg_BPM_medianGraph = smooth.spline(x = df_avg_BPM_medianGraph_1$Pk, y = df_avg_BPM_medianGraph_1$median_Avg_BPM, df=4.8)
plot(smooth_spline_avg_BPM_medianGraph$y)

#new
spline_model_version_2_median_avg_BPM = lm(data = df_avg_BPM_medianGraph_1, median_Avg_BPM~bs(Pk, knots = c(7)))

#combine
median_avg_BPM_graph_spline <- df_avg_BPM_medianGraph %>% 
  mutate(smoothed_avg_BPM_spline = smooth_spline_avg_BPM_medianGraph$y) %>% 
  mutate(pred_model = predict(spline_model_version_2_median_avg_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = median_Avg_BPM)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_BPM_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_avg_BPM), color = 'blue')

median_avg_BPM_graph_spline

df_avg_BPM_medianGraph <- df_avg_BPM_medianGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_avg_BPM = predict(model_1_avg_BPM, .),
    pred_model_2_avg_BPM = predict(spline_model_version_2_median_avg_BPM, .),
    #pred_model_2_avg_BPM = smooth_spline_avg_BPM_medianGraph$y,
    #pred_model_2_avg_BPM_shifted = pred_model_2_avg_BPM - min(pred_model_2_avg_BPM, na.rm = TRUE),
    pred_avgBPMMedianGraph_scaled = pred_model_2_avg_BPM / max(pred_model_2_avg_BPM, na.rm = TRUE)
  )

avg_BPM_graph_2 <- df_avg_BPM_medianGraph %>%  
  ggplot(aes(x = Pk, y = median_Avg_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_avg_BPM), color = "magenta", size = 1) +
  geom_line(aes(y = pred_model_2_avg_BPM), color = "cyan", size = 1) +
  geom_line(aes(y = pred_model_2_avg_BPM_shifted), color = "green", size = 1) +
  geom_line(aes(y = pred_avgBPMMedianGraph_scaled), color = "red", size = 1) +
  scale_x_continuous(breaks = seq(1, 60, by = 5)) +
  scale_y_continuous(breaks = seq(3, 3, by = 0.5)) +
  geom_hline(yintercept = 0)

##-----------------------------------------

df_total_BPM_medianGraph <- player_career_stats %>%
  group_by(Pk) %>%
  summarise(median_total_BPM = median(Total_BPM))

model_1_total_BPM <- lm(median_total_BPM ~ Pk, data = df_total_BPM_medianGraph)
##model_2_total_BPM <- lm(median_total_BPM ~ bs(Pk, knots=c(14, 42)), data = df_total_BPM_medianGraph)

df_total_BPM_medianGraph_1 = df_total_BPM_medianGraph %>% drop_na()
smooth_spline_total_BPM_medianGraph = smooth.spline(x = df_total_BPM_medianGraph_1$Pk, y = df_total_BPM_medianGraph_1$median_total_BPM, df=2.25)
plot(smooth_spline_total_BPM_medianGraph$y)

#new
spline_model_version_2_median_total_BPM = lm(data = df_total_BPM_medianGraph_1, median_total_BPM~bs(Pk, knots = c(7)))

#combine
median_total_BPM_graph_spline <- df_total_BPM_medianGraph %>% 
  mutate(smoothed_total_BPM_spline = smooth_spline_total_BPM_medianGraph$y) %>% 
  mutate(spline_version_2_total_BPM = predict(spline_model_version_2_median_total_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = median_total_BPM)) +
  geom_point() +
  geom_line(aes(y = smoothed_total_BPM_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_total_BPM), color = 'blue')

median_total_BPM_graph_spline

df_total_BPM_medianGraph <- df_total_BPM_medianGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_total_BPM = predict(model_1_total_BPM, .),
    ## pred_model_2_total_BPM = predict(model_2_total_BPM, .),
    pred_model_2_total_BPM = predict(spline_model_version_2_median_total_BPM, .),
    pred_totalBPMMedianGraph_scaled = pred_model_2_total_BPM /max(pred_model_2_total_BPM, na.rm = TRUE)
  )

total_BPM_graph_2 <- df_total_BPM_medianGraph %>% 
  ggplot(aes(x = Pk, y = median_total_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_total_BPM), color = "magenta", size = 1) +
  geom_line(aes(y = pred_model_2_total_BPM), color = "cyan", size = 1) +
  geom_line(aes(y = pred_model_2_total_BPM_shifted), color = "green", size = 1) +
  geom_line(aes(y = pred_totalBPMMedianGraph_scaled), color = "red", size = 1) +
  scale_x_continuous(breaks = seq(1, 60, by = 5)) +
  scale_y_continuous(breaks = seq(3, 3, by = 0.5)) +
  geom_hline(yintercept = 0)

##---------------------------------------

df_total_WAR_medianGraph <- player_career_stats %>%
  group_by(Pk) %>%
  summarise(median_total_WAR = median(Total_WAR))

model_1_total_WAR <- lm(median_total_WAR ~ Pk, data = df_total_WAR_medianGraph)
model_2_total_WAR <- lm(median_total_WAR ~ bs(Pk, knots=c(30)), data = df_total_WAR_medianGraph)

df_total_WAR_medianGraph_1 = df_total_WAR_medianGraph %>% drop_na()
smooth_spline_total_WAR_medianGraph = smooth.spline(x = df_total_WAR_medianGraph_1$Pk, y = df_total_WAR_medianGraph_1$median_total_WAR, df=2.17)
plot(smooth_spline_total_WAR_medianGraph$y)

#new
spline_model_version_2_median_total_WAR = lm(data = df_total_WAR_medianGraph_1, median_total_WAR~bs(Pk, knots = c(7)))

#combine
median_total_WAR_graph_spline <- df_total_WAR_medianGraph %>% 
  #mutate(smoothed_total_WAR_spline = smooth_spline_total_WAR_medianGraph$y) %>% 
  mutate(spline_version_2_total_WAR = predict(spline_model_version_2_median_total_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = median_total_WAR)) +
  geom_point() +
  #geom_line(aes(y = smoothed_total_WAR_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_total_WAR), color = 'blue')

median_total_WAR_graph_spline

df_total_WAR_medianGraph <- df_total_WAR_medianGraph %>%
  drop_na() %>% 
  mutate(pred_model_1_total_WAR = predict(model_1_total_WAR, .),
    pred_model_total_WAR = predict(spline_model_version_2_median_total_WAR, .),
    pred_totalWARMedianGraph_scaled = pred_model_total_WAR / max(pred_model_total_WAR, na.rm = TRUE)
  )

total_WAR_graph_2 <- df_total_WAR_medianGraph %>% 
  ggplot(aes(x = Pk, y = median_total_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_total_WAR), color = "magenta", size = 1) +
  geom_line(aes(y = pred_model_2_total_WAR), color = "cyan", size = 1) +
  geom_line(aes(y = pred_model_2_total_WAR_shifted), color = "green", size = 1) +
  geom_line(aes(y = pred_totalWARMedianGraph_scaled), color = "red", size = 1) +
  scale_x_continuous(breaks = seq(1, 60, by = 5)) +
  scale_y_continuous(breaks = seq(3, 3, by = 0.5)) +
  geom_hline(yintercept = 0)

##-------------------------

df_avg_WAR_medianGraph <- player_career_stats %>%
  group_by(Pk) %>%
  summarise(median_avg_WAR = median(Avg_WAR))

model_1_avg_WAR <- lm(median_avg_WAR ~ Pk, data = df_avg_WAR_medianGraph)
##model_2_avg_WAR <- lm(median_avg_WAR ~ bs(Pk, knots=c(18)), data = df_avg_WAR_medianGraph)

df_avg_WAR_medianGraph_1 = df_avg_WAR_medianGraph %>% drop_na()
smooth_spline_avg_WAR_medianGraph = smooth.spline(x = df_avg_WAR_medianGraph_1$Pk, y = df_avg_WAR_medianGraph_1$median_avg_WAR, df=2.55)
plot(smooth_spline_avg_WAR_medianGraph$y)

#new
spline_model_version_2_median_avg_WAR = lm(data = df_avg_WAR_medianGraph_1, median_avg_WAR~bs(Pk, knots = c(7)))

#combine
median_avg_WAR_graph_spline <- df_avg_WAR_medianGraph %>% 
  mutate(smoothed_avg_WAR_spline = smooth_spline_avg_WAR_medianGraph$y) %>% 
  mutate(spline_version_2_avg_WAR = predict(spline_model_version_2_median_avg_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = median_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = smoothed_avg_WAR_spline), color = 'red') +
  geom_line(aes(y = spline_version_2_avg_WAR), color = 'blue')

median_avg_WAR_graph_spline

df_avg_WAR_medianGraph <- df_avg_WAR_medianGraph %>%
  drop_na() %>% 
  mutate(
    pred_model_1_avg_WAR = predict(model_1_avg_WAR, .),
    ##pred_model_2_avg_WAR = predict(model_2_avg_WAR, .),
    pred_model_2_avg_WAR = predict(spline_model_version_2_median_avg_WAR, .),
    pred_avgWARMedianGraph_scaled = pred_model_2_avg_WAR /max(pred_model_2_avg_WAR, na.rm = TRUE)
  )

avg_WAR_graph_2 <- df_avg_WAR_medianGraph %>% 
  ggplot(aes(x = Pk, y = median_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_1_avg_WAR), color = "magenta", size = 1) +
  geom_line(aes(y = pred_model_2_avg_WAR), color = "cyan", size = 1) +
  geom_line(aes(y = pred_model_2_avg_WAR_shifted), color = "green", size = 1) +
  geom_line(aes(y = pred_avgWARMedianGraph_scaled), color = "red", size = 1) +
  scale_x_continuous(breaks = seq(1, 60, by = 5)) +
  scale_y_continuous(breaks = seq(3, 3, by = 0.5)) +
  geom_hline(yintercept = 0) 




combined_based_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph$pred_avgWARMedianGraph_scaled[1:60], color = "Avg War"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph$pred_totalWARMedianGraph_scaled[1:60], color = "Total War"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph$pred_totalBPMMedianGraph_scaled[1:60], color = "Total BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph$pred_avgBPMMedianGraph_scaled[1:60], color = "Avg BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_medianGraph$pred_totalRaptorMedianGraph_scaled[1:60], color = "Total Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_medianGraph$pred_avgRaptorMedianGraph_scaled[1:60], color = "Avg Raptor"), size = 1) +
  geom_line(aes(x = 1:60,y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled[1:60], color="Avg War"), size=1, linetype = "dashed" ) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled[1:60], color="Total War"), size=1,  linetype = "dashed") +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled[1:60], color="Total BPM"), size=1,  linetype = "dashed") +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled[1:60], color="Avg BPM"), size=1,  linetype = "dashed") +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_totalRaptorMeanGraph_scaled[1:60], color="Total Raptor"), size=1,  linetype = "dashed") +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled[1:60], color="Avg Raptor"), size=1,  linetype = "dashed") +
  scale_x_continuous(breaks = c(0, 15, 30, 45, 60)) +
  labs(title = "Median + Mean Based NBA Draft Value Chart",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color",
       linetype = "Variable") +
  theme_minimal() +
  scale_color_manual(values = c("Avg War" = "red",
                                "Total War" = "cyan",
                                "Total BPM" = "gold",
                                "Avg BPM" = "blue",
                                "Total Raptor" = "green",
                                "Avg Raptor" = "pink")) +
  scale_linetype_manual(values = c("Avg War" = "dashed",
                                   "AVG War" = "solid"))

avg_bpm_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph$pred_avgBPMMedianGraph_scaled[1:60], color = "Meidan Avg BPM", ), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled[1:60], color="Mean Avg BPM"), size=1) +
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

total_bpm_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph$pred_totalBPMMedianGraph_scaled[1:60], color = "Median Total BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled[1:60], color="Mean Total BPM"), size=1)+
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

avg_raptor_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_totalRaptor_medianGraph$pred_totalRaptorMedianGraph_scaled[1:60], color = "Meidan Total Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_totalRaptorMeanGraph_scaled[1:60], color ="Mean Total Raptor"), size=1) +
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

total_raptor_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avgRaptor_medianGraph$pred_avgRaptorMedianGraph_scaled[1:60], color = "Meidan Avg Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled[1:60], color ="Mean Avg Raptor"), size=1) +
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

avg_WAR_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph$pred_avgWARMedianGraph_scaled[1:60], color = "Meidan Avg Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled[1:60], color ="Mean Avg Raptor"), size=1) +
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

total_WAR_final_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph$pred_totalWARMedianGraph_scaled[1:60], color = "Meidan Avg Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled[1:60], color ="Mean Avg Raptor"), size=1) +
  labs(title = "Avg Bpm Mean + Median",
       x = "Draft Pick (1-60)",
       y = "Relative Value",
       color = "Color") +
  theme_minimal()

median_based_chart
mean_based_chart

all_mean <- ggarrange(avg_raptor_graph, total_raptor_graph, avg_BPM_graph, total_BPM_graph,
                      avg_WAR_graph, total_WAR_graph, nrow = 3, ncol = 2)

all_median <- ggarrange(avg_raptor_graph_2, total_raptor_graph_2, avg_BPM_graph_2, total_BPM_graph_2,
                        avg_WAR_graph_2, total_WAR_graph_2, nrow = 3, ncol = 2)

all_together <- ggarrange(avg_bpm_final_chart, total_bpm_final_chart,avg_raptor_final_chart,
                          total_raptor_final_chart,avg_WAR_final_chart,total_WAR_final_chart, nrow = 3, ncol = 2 )


##---------------------------------------------


player_career_stats_new = player_career_stats_1 %>% 
  mutate(raptor_bin = cut(Avg_Raptor, breaks = quantile(Avg_Raptor, probs = seq(0, 1, by = 0.1)))) %>% 
  group_by(raptor_bin) %>% 
  mutate(bin_avg_career_salary = mean(avg_career_salary, na.rm = TRUE)) %>% 
  ggplot() +
  facet_wrap(~raptor_bin) +
  geom_density(aes(x = avg_career_salary)) +
  geom_vline(aes(xintercept = bin_avg_career_salary), color = 'red')

player_career_stats_new

##---------------------------------------------

gamma_reg_model = glm(formula = avg_career_salary ~ Avg_Raptor, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model)

tibble(Avg_Raptor = seq(-6, 6, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_avg_raptor = predict(gamma_reg_model, ., type = "response")) %>% 
  ggplot(aes(x = Avg_Raptor, y = pred_avg_career_salary_avg_raptor)) +
  geom_line()

player_career_stats_pred = player_career_stats_1 %>% 
  mutate(pred_avg_career_salary_avg_raptor = predict(gamma_reg_model, ., type = "response"))

##---------------------------------------------

gamma_reg_model_2 = glm(formula = avg_career_salary ~ Total_Raptor, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model_2)

tibble(Total_Raptor = seq(-150, 150, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_total_raptor = predict(gamma_reg_model_2, ., type = "response")) %>% 
  ggplot(aes(x = Total_Raptor, y = pred_avg_career_salary_total_raptor)) +
  geom_line()

player_career_stats_pred = player_career_stats_1 %>% 
  mutate(pred_avg_career_salary_total_raptor = predict(gamma_reg_model_2, ., type = "response"))

##---------------------------------------------
gamma_reg_model_3 = glm(formula = avg_career_salary ~ Avg_BPM, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model_3)

tibble(Avg_BPM = seq(-12, 12, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_avg_BPM = predict(gamma_reg_model_3, ., type = "response")) %>% 
  ggplot(aes(x = Avg_BPM, y = pred_avg_career_salary_avg_BPM)) +
  geom_line()

player_career_stats_pred = player_career_stats_1 %>% 
  mutate(pred_mean_career_salary_avg_BPM = predict(gamma_reg_model_3, ., type = "response"))

##---------------------------------------------
gamma_reg_model_4 = glm(formula = avg_career_salary ~ Total_BPM, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model_4)

tibble(Total_BPM = seq(-50, 50, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_total_BPM = predict(gamma_reg_model_4, ., type = "response")) %>% 
  ggplot(aes(x = Total_BPM, y = pred_avg_career_salary_total_BPM)) +
  geom_line()

player_career_stats_pred = player_career_stats_1 %>% 
  mutate(pred_avg_career_salary_total_BPM = predict(gamma_reg_model_4, ., type = "response"))

##---------------------------------------------
gamma_reg_model_5 = glm(formula = avg_career_salary ~ Avg_WAR, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model_5)

tibble(Avg_WAR= seq(-12, 12, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_Avg_WAR = predict(gamma_reg_model_5, ., type = "response")) %>% 
  ggplot(aes(x = Avg_WAR, y = pred_avg_career_salary_Avg_WAR)) +
  geom_line()

player_career_stats_pred = player_career_stats_1 %>% 
  mutate(pred_avg_career_salary_Avg_WAR = predict(gamma_reg_model_5, ., type = "response"))

##---------------------------------------------
gamma_reg_model_6 = glm(formula = avg_career_salary ~ Total_WAR, family = Gamma(link = "log"), data = player_career_stats_1)
summary(gamma_reg_model_6)

tibble(Total_WAR= seq(-6, 200, length.out = 50)) %>% 
  mutate(pred_avg_career_salary_Total_WAR = predict(gamma_reg_model_6, ., type = "response")) %>% 
  ggplot(aes(x = Total_WAR, y = pred_avg_career_salary_Total_WAR)) +
  geom_line()

#combine all
player_career_stats_pred = player_career_stats_1 %>%
  mutate(pred_avg_career_salary_avg_raptor = predict(gamma_reg_model, ., type = "response")) %>% 
  mutate(pred_avg_career_salary_total_raptor = predict(gamma_reg_model_2, ., type = "response")) %>%
  mutate(pred_avg_career_salary_avg_BPM = predict(gamma_reg_model_3, ., type = "response")) %>% 
  mutate(pred_avg_career_salary_total_BPM = predict(gamma_reg_model_4, ., type = "response")) %>% 
  mutate(pred_avg_career_salary_avg_WAR = predict(gamma_reg_model_5, ., type = "response")) %>%
  mutate(pred_avg_career_salary_total_WAR = predict(gamma_reg_model_6, ., type = "response"))

##----mean_avg_raptor-----------------------------------------

df_avg_raptor_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_avg_raptor = mean(pred_avg_career_salary_avg_raptor))

df_avg_raptor_meanGraph_new_1 = df_avg_raptor_meanGraph_new %>% drop_na()
spline_model_version_2_avg_raptor = lm(data = df_avg_raptor_meanGraph_new_1, mean_career_salary_avg_raptor~bs(Pk, knots = c(7)))


#combine
avg_raptor_salary_graph_spline <- df_avg_raptor_meanGraph_new_1 %>% 
  mutate(spline_version_2_avg_raptor_salary = predict(spline_model_version_2_avg_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_avg_raptor_salary), color = 'blue')

avg_raptor_salary_graph_spline

#combine
df_avg_raptor_meanGraph_new_1 = df_avg_raptor_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_avg_raptor, .),
    pred_avgRaptorMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_raptor_salary_graph_1 <- df_avg_raptor_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_raptor_salary_graph_2 <- df_avg_raptor_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_line(aes(y = pred_avgRaptorMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_avg_raptor---------

df_avg_raptor_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_avg_raptor = median(pred_avg_career_salary_avg_raptor))

df_avg_raptor_medianGraph_new_1 = df_avg_raptor_medianGraph_new %>% drop_na()
spline_model_version_median_2_avg_raptor = lm(data = df_avg_raptor_medianGraph_new_1, median_career_salary_avg_raptor~bs(Pk, knots = c(7)))


#combine
avg_raptor_median_salary_graph_spline <- df_avg_raptor_medianGraph_new_1 %>%
  mutate(spline_version_2_median_avg_raptor_salary = predict(spline_model_version_median_2_avg_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_median_avg_raptor_salary), color = 'blue')

avg_raptor_median_salary_graph_spline

#combine
df_avg_raptor_medianGraph_new_1 = df_avg_raptor_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_median_2_avg_raptor, .),
    pred_avgRaptorMedianSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_raptor_median_salary_graph_1 <- df_avg_raptor_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_raptor_median_salary_graph_2 <- df_avg_raptor_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_raptor)) +
  geom_line(aes(y = pred_avgRaptorMedianSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Mean_total_raptor---------

df_total_raptor_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_avg_raptor = mean(pred_avg_career_salary_total_raptor))

df_total_raptor_meanGraph_new_1 = df_total_raptor_meanGraph_new %>% drop_na()
spline_model_version_mean_2_total_raptor = lm(data = df_total_raptor_meanGraph_new_1, mean_career_salary_avg_raptor~bs(Pk, knots = c(7)))


#combine
total_raptor_mean_salary_graph_spline <- df_total_raptor_meanGraph_new_1 %>%
  mutate(spline_version_2_mean_total_raptor_salary = predict(spline_model_version_mean_2_total_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_mean_total_raptor_salary), color = 'blue')

total_raptor_mean_salary_graph_spline

#combine
df_total_raptor_meanGraph_new_1 = df_total_raptor_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_mean_2_total_raptor, .),
    pred_totalRaptorMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

total_raptor_mean_salary_graph_1 <- df_total_raptor_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_raptor_mean_salary_graph_2 <- df_total_raptor_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_raptor)) +
  geom_line(aes(y = pred_totalRaptorMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_total_raptor---------

df_total_raptor_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_total_raptor = median(pred_avg_career_salary_total_raptor))

df_total_raptor_medianGraph_new_1 = df_total_raptor_medianGraph_new %>% drop_na()
spline_model_version_median_2_total_raptor = lm(data = df_total_raptor_medianGraph_new_1, median_career_salary_total_raptor~bs(Pk, knots = c(7)))


#combine
total_raptor_median_salary_graph_spline <- df_total_raptor_medianGraph_new_1 %>%
  mutate(spline_version_2_median_total_raptor_salary = predict(spline_model_version_median_2_total_raptor, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_raptor)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_median_total_raptor_salary), color = 'blue')

total_raptor_median_salary_graph_spline

#combine
df_total_raptor_medianGraph_new_1 = df_total_raptor_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_median_2_total_raptor, .),
    pred_totalRaptorMedianSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

total_raptor_median_salary_graph_1 <- df_total_raptor_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_raptor)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_raptor_median_salary_graph_2 <- df_total_raptor_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_raptor)) +
  geom_line(aes(y = pred_totalRaptorMedianSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Mean_avg_BPM---------

df_avg_BPM_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_avg_BPM = mean(pred_avg_career_salary_avg_BPM))

df_avg_BPM_meanGraph_new_1 = df_avg_BPM_meanGraph_new %>% drop_na()
spline_model_version_2_avg_BPM = lm(data = df_avg_BPM_meanGraph_new_1, mean_career_salary_avg_BPM~bs(Pk, knots = c(7)))


#combine
avg_BPM_salary_graph_spline <- df_avg_BPM_meanGraph_new_1 %>% 
  mutate(spline_version_2_avg_BPM_salary = predict(spline_model_version_2_avg_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_BPM)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_avg_BPM_salary), color = 'blue')

avg_BPM_salary_graph_spline

#combine
df_avg_BPM_meanGraph_new_1 = df_avg_BPM_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_avg_BPM, .),
    pred_avgBPMMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_BPM_mean_salary_graph_1 <- df_avg_BPM_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_BPM_mean_salary_graph_2 <- df_avg_BPM_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_BPM)) +
  geom_line(aes(y = pred_avgBPMMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_avg_BPM---------

df_avg_BPM_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_avg_BPM = median(pred_avg_career_salary_avg_BPM))

df_avg_BPM_medianGraph_new_1 = df_avg_BPM_medianGraph_new %>% drop_na()
spline_model_version_2_avg_BPM_median = lm(data = df_avg_BPM_medianGraph_new_1, median_career_salary_avg_BPM~bs(Pk, knots = c(7)))


#combine
avg_BPM_median_salary_graph_spline <- df_avg_BPM_medianGraph_new_1 %>% 
  mutate(spline_version_2_avg_median_BPM_salary = predict(spline_model_version_2_avg_BPM_median, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_BPM)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_avg_median_BPM_salary), color = 'blue')

avg_BPM_median_salary_graph_spline

#combine
df_avg_BPM_medianGraph_new_1 = df_avg_BPM_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_avg_BPM_median, .),
    pred_avgBPMMedianSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_BPM_median_salary_graph_1 <- df_avg_BPM_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_BPM_median_salary_graph_2 <- df_avg_BPM_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_BPM)) +
  geom_line(aes(y = pred_avgBPMMedianSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_total_BPM---------

df_total_BPM_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_total_BPM = median(pred_avg_career_salary_total_BPM))

df_total_BPM_medianGraph_new_1 = df_total_BPM_medianGraph_new %>% drop_na()
spline_model_version_2_total_BPM_median = lm(data = df_total_BPM_medianGraph_new_1, median_career_salary_total_BPM~bs(Pk, knots = c(7)))


#combine
total_BPM_median_salary_graph_spline <- df_total_BPM_medianGraph_new_1 %>% 
  mutate(spline_version_2_total_median_BPM_salary = predict(spline_model_version_2_total_BPM_median, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_BPM)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_total_median_BPM_salary), color = 'blue')

total_BPM_median_salary_graph_spline

#combine
df_total_BPM_medianGraph_new_1 = df_total_BPM_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_total_BPM_median, .),
    pred_totalBPMMedianSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

total_BPM_median_salary_graph_1 <- df_total_BPM_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_BPM_median_salary_graph_2 <- df_total_BPM_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_BPM)) +
  geom_line(aes(y = pred_totalBPMMedianSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Mean_total_BPM---------

df_total_BPM_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_total_BPM = mean(pred_avg_career_salary_total_BPM))

df_total_BPM_meanGraph_new_1 = df_total_BPM_meanGraph_new %>% drop_na()
spline_model_version_mean_2_total_BPM = lm(data = df_total_BPM_meanGraph_new_1, mean_career_salary_total_BPM~bs(Pk, knots = c(7)))


#combine
total_BPM_mean_salary_graph_spline <- df_total_BPM_meanGraph_new_1 %>%
  mutate(spline_version_2_mean_total_BPM_salary = predict(spline_model_version_mean_2_total_BPM, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_total_BPM)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_mean_total_BPM_salary), color = 'blue')

total_BPM_mean_salary_graph_spline

#combine
df_total_BPM_meanGraph_new_1 = df_total_BPM_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_mean_2_total_BPM, .),
    pred_totalBPMMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

total_BPM_mean_salary_graph_1 <- df_total_BPM_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_total_BPM)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_BPM_mean_salary_graph_2 <- df_total_BPM_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_BPM)) +
  geom_line(aes(y = pred_totalBPMMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Mean_avg_WAR---------

df_avg_WAR_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_avg_WAR = mean(pred_avg_career_salary_avg_WAR))

df_avg_WAR_meanGraph_new_1 = df_avg_WAR_meanGraph_new %>% drop_na()
spline_model_version_2_avg_WAR = lm(data = df_avg_WAR_meanGraph_new_1, mean_career_salary_avg_WAR~bs(Pk, knots = c(7)))


#combine
avg_WAR_salary_graph_spline <- df_avg_WAR_meanGraph_new_1 %>% 
  mutate(spline_version_2_avg_WAR_salary = predict(spline_model_version_2_avg_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_avg_WAR_salary), color = 'blue')

avg_WAR_salary_graph_spline

#combine
df_avg_WAR_meanGraph_new_1 = df_avg_WAR_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_avg_WAR, .),
    pred_avgWARMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_WAR_mean_salary_graph_1 <- df_avg_WAR_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_WAR_mean_salary_graph_2 <- df_avg_WAR_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_line(aes(y = pred_avgWARMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_avg_WAR---------

df_avg_WAR_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_avg_WAR = median(pred_avg_career_salary_avg_WAR))

df_avg_WAR_medianGraph_new_1 = df_avg_WAR_medianGraph_new %>% drop_na()
spline_model_version_2_avg_WAR_median = lm(data = df_avg_WAR_medianGraph_new_1, median_career_salary_avg_WAR~bs(Pk, knots = c(7)))


#combine
avg_WAR_salary_graph_spline <- df_avg_WAR_medianGraph_new_1 %>% 
  mutate(spline_version_2_avg_median_WAR_salary = predict(spline_model_version_2_avg_WAR_median, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_avg_median_WAR_salary), color = 'blue')

avg_WAR_salary_graph_spline

#combine
df_avg_WAR_medianGraph_new_1 = df_avg_WAR_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_avg_WAR_median, .),
    pred_avgWARMedianSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

avg_WAR_median_salary_graph_1 <- df_avg_WAR_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

avg_WAR_median_salary_graph_2 <- df_avg_WAR_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_avg_WAR)) +
  geom_line(aes(y = pred_avgWARMedianSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Mean_total_WAR---------

df_total_WAR_meanGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(mean_career_salary_total_WAR = mean(pred_avg_career_salary_total_WAR))

df_total_WAR_meanGraph_new_1 = df_total_WAR_meanGraph_new %>% drop_na()
spline_model_version_mean_2_total_WAR = lm(data = df_total_WAR_meanGraph_new_1, mean_career_salary_total_WAR~bs(Pk, knots = c(7)))


#combine
total_WAR_mean_salary_graph_spline <- df_total_WAR_meanGraph_new_1 %>%
  mutate(spline_version_2_mean_total_WAR_salary = predict(spline_model_version_mean_2_total_WAR, .)) %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_mean_total_WAR_salary), color = 'blue')

total_WAR_mean_salary_graph_spline

#combine
df_total_WAR_meanGraph_new_1 = df_total_WAR_meanGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_mean_2_total_WAR, .),
    pred_totalWARMeanSalaryGraph_scaled_new = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T),
  )

total_WAR_mean_salary_graph_1 <- df_total_WAR_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_WAR_mean_salary_graph_2 <- df_total_WAR_meanGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = mean_career_salary_avg_WAR)) +
  geom_line(aes(y = pred_totalWARMeanSalaryGraph_scaled_new), color="red", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Median_total_WAR---------

df_total_WAR_medianGraph_new = player_career_stats_pred %>% 
  group_by(Pk) %>% 
  summarise(median_career_salary_total_WAR = median(pred_avg_career_salary_total_WAR))

df_total_WAR_medianGraph_new_1 <- df_total_WAR_medianGraph_new_1[-5,]

df_total_WAR_medianGraph_new_1 = df_total_WAR_medianGraph_new %>% drop_na()
spline_model_version_2_total_WAR_median = lm(data = df_total_WAR_medianGraph_new_1 %>% 
                                               filter(!(Pk %in% c(51, 56, 57))),
                                             median_career_salary_total_WAR~bs(Pk, knots = c(7)))


#combine
total_WAR_median_salary_graph_spline <- df_total_WAR_medianGraph_new_1 %>% 
  mutate(spline_version_2_total_median_WAR_salary = predict(spline_model_version_2_total_WAR_median, .)) %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_WAR)) +
  geom_point() +
  geom_line(aes(y = spline_version_2_total_median_WAR_salary), color = 'blue')

total_WAR_median_salary_graph_spline

#combine
df_total_WAR_medianGraph_new_1 = df_total_WAR_medianGraph_new_1 %>%
  drop_na() %>% 
  mutate(
    pred_model_2_salary_new = predict(spline_model_version_2_total_WAR_median, .),
    pred_model_2_salary_shifted_new = pred_model_2_salary_new - min(pred_model_2_salary_new, na.rm=T),
   # pred_totalWARMedianSalaryGraph_scaled_new = pred_model_2_salary_shifted_new/max(pred_model_2_salary_shifted_new, na.rm=T),
    pred_totalWARMedianSalaryGraph_scaled_new_2 = pred_model_2_salary_new/max(pred_model_2_salary_new, na.rm=T))


total_WAR_median_salary_graph_1 <- df_total_WAR_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_WAR)) +
  geom_point() +
  geom_line(aes(y = pred_model_2_salary_new), color="cyan", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

total_WAR_median_salary_graph_2 <- df_total_WAR_medianGraph_new_1 %>% 
  ggplot(aes(x = Pk, y = median_career_salary_total_WAR)) +
  geom_line(aes(y = pred_totalWARMedianSalaryGraph_scaled_new_2), color="blue", size=1) +
  scale_x_continuous(breaks=seq(1,60,by=5)) +
  geom_hline(yintercept=0)

##-------Final Visuals---------
df_plot_3 <- bind_rows(
  df_total_WAR_medianGraph %>% 
    rename(dot = median_total_WAR) %>%
    rename(pred_model_2 = pred_model_total_WAR) %>% 
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Total WAR"),
  df_avg_WAR_medianGraph %>% 
    rename(dot = median_avg_WAR) %>% 
    rename(pred_model_2 = pred_model_2_avg_WAR) %>% 
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Average WAR"),
  df_avg_WAR_meanGraph %>% 
    rename(dot = mean_avg_WAR) %>%
    rename(pred_model_2 = pred_model_2_avg_WAR) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Average WAR"),
  df_total_WAR_meanGraph %>% 
    rename(dot = mean_total_WAR) %>%
    rename(pred_model_2 = pred_model_2_total_WAR) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Total WAR"),
  df_totalRaptor_medianGraph %>% 
    rename(dot = median_total_raptor) %>%
    rename(pred_model_2 = pred_model_2_total_raptor) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Total Raptor"),
  df_totalRaptor_meanGraph %>% 
    rename(dot = mean_total_raptor) %>%
    rename(pred_model_2 = pred_model_total_raptor) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Total Raptor"),
  df_avgRaptor_meanGraph %>% 
    rename(dot = mean_avg_raptor) %>%
    rename(pred_model_2 = pred_model_avg_raptor) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Average Raptor"),
  df_avgRaptor_medianGraph %>% 
    rename(dot = median_avg_raptor) %>%
    rename(pred_model_2 = pred_model_2_avg_raptor_2) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Average Raptor"),
  df_avg_BPM_medianGraph %>% 
    rename(dot = median_Avg_BPM) %>%
    rename(pred_model_2 = pred_model_2_avg_BPM) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Average BPM"),
  df_total_BPM_medianGraph %>% 
    rename(dot = median_total_BPM) %>%
    rename(pred_model_2 = pred_model_2_total_BPM) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "median", metric = "Total BPM"),
  df_total_BPM_meanGraph %>% 
    rename(dot = mean_total_BPM) %>%
    rename(pred_model_2 = pred_model_2_total_BPM) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Total BPM"),
  df_avg_BPM_meanGraph %>% 
    rename(dot = mean_Avg_BPM) %>%
    rename(pred_model_2 = pred_model_2_avg_BPM_final) %>%
    select(Pk, dot, pred_model_2) %>% 
    mutate(m = "mean", metric = "Average BPM"),
)

df_plot_3 %>% 
  ggplot(aes(x = Pk, linetype = m)) +
  facet_wrap(~m+metric, scales = "free_y", nrow = 2) +
  geom_point(aes(y = dot)) +
  geom_line(aes(y = pred_model_2,  color = metric,), linewidth = 1.5) +
  labs(x = "Draft Position",
       color = "Player\nPerformance\nMeasure",
       linetype = "",
       y = "Performance Value") +
  scale_x_continuous(breaks = seq(0,60, by = 10))


df_plot_1 <- bind_rows(
  df_total_WAR_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_total_WAR) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Total WAR"),
  df_total_WAR_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_total_WAR) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Total WAR"),
  df_avg_WAR_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_avg_WAR) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Average WAR"),
  df_avg_WAR_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_avg_WAR) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Average WAR"),
  df_total_BPM_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_total_BPM) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Total BPM"),
  df_total_BPM_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_total_BPM) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Total BPM"),
  df_avg_BPM_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_avg_BPM) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Average BPM"),
  df_avg_BPM_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_avg_BPM) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Average BPM"),
  df_total_raptor_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_total_raptor) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Total Raptor"),
  df_total_raptor_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_avg_raptor) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Total Raptor"),
  df_avg_raptor_medianGraph_new_1 %>% 
    rename(dot = median_career_salary_avg_raptor) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "median", metric = "Average Raptor"),
  df_avg_raptor_meanGraph_new_1 %>% 
    rename(dot = mean_career_salary_avg_raptor) %>% 
    select(Pk, dot, pred_model_2_salary_new) %>% 
    mutate(m = "mean", metric = "Average Raptor")
)

df_plot_1 %>% 
  ggplot(aes(x = Pk, linetype = m)) +
  facet_wrap(~m+metric, scales = "free_y", nrow = 2) +
  geom_point(aes(y = dot)) +
  geom_line(aes(y = pred_model_2_salary_new, color = metric), linewidth = 1.5) +
  labs(
    x = "Draft Position",
    color = "Player\nPerformance\nMeasure",
    linetype = "",
    y = "Performance Value Mapped to Salary \n(as a proportion of the maximum contract)"
  ) +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  theme(
    axis.title.y = element_text(size = 12.5) # Adjust size as needed
  )



final_median_chart_scaled <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_raptor_medianGraph_new_1$pred_avgRaptorMedianSalaryGraph_scaled_new, color = "Median Average Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_raptor_medianGraph_new_1$pred_totalRaptorMedianSalaryGraph_scaled_new, color = "Median Total Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph_new_1$pred_avgBPMMedianSalaryGraph_scaled_new, color = "Median Average BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph_new_1$pred_totalBPMMedianSalaryGraph_scaled_new, color = "Median Total BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph_new_1$pred_avgWARMedianSalaryGraph_scaled_new, color = "Median Average WAR"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph_new_1$pred_totalWARMedianSalaryGraph_scaled_new_2, color = "Median Total WAR"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_raptor_meanGraph_new_1$pred_avgRaptorMeanSalaryGraph_scaled_new, color = "Mean Average Raptor"), linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_total_raptor_meanGraph_new_1$pred_totalRaptorMeanSalaryGraph_scaled_new, color = "Mean Total Raptor"), linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph_new_1$pred_avgBPMMeanSalaryGraph_scaled_new, color = "Mean Average BPM"),linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph_new_1$pred_totalBPMMeanSalaryGraph_scaled_new, color = "Mean Total BPM"),linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph_new_1$pred_avgWARMeanSalaryGraph_scaled_new, color = "Mean Average WAR"),linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph_new_1$pred_totalWARMeanSalaryGraph_scaled_new, color = "Mean Total WAR"),linetype = "dashed", size = 1) +
  labs(x = "Draft Position",
       y = "Value Relative To First Pick") +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = .1))
final_median_chart_scaled

final_mean_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_raptor_meanGraph_new_1$pred_avgRaptorMeanSalaryGraph_scaled_new, color = "mean_avg_raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_raptor_meanGraph_new_1$pred_totalRaptorMeanSalaryGraph_scaled_new, color = "mean_total_raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph_new_1$pred_avgBPMMeanSalaryGraph_scaled_new, color = "mean_avg_BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph_new_1$pred_totalBPMMeanSalaryGraph_scaled_new, color = "mean_total_BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph_new_1$pred_avgWARMeanSalaryGraph_scaled_new, color = "mean_avg_WAR"), size = 1) +
  #geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph_new_1$pred_totalWARMeanSalaryGraph_scaled_new, color = "mean_total_WAR"), size = 1) +
  labs(title = "Mean + Media for all metrics",
       x = "Pk (1:60)",
       y = "salary_pred_value_scaled")  

b_spline_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled_new, color = "Mean Average Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_model_1_new_scaled, color = "Mean Total Raptor"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled, color = "Mean Average BPM"), size = 1, linetype = "dashed") +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled, color = "Mean Total BPM"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled, color = "Mean Total WAR"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled, color = "Mean Average WAR"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph$pred_avgBPMMedianGraph_scaled, color = "Median Average BPM"), linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph$pred_totalBPMMedianGraph_scaled, color = "Median Total BPM"), linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph$pred_avgWARMedianGraph_scaled, color = "Median Average WAR"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_medianGraph$pred_avgRaptorMedianGraph_scaled, color = "Median Average Raptor"),linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph$pred_totalWARMedianGraph_scaled, color = "Median Total WAR"),linetype = "dashed", size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_medianGraph$pred_totalRaptorMedianGraph_scaled, color = "Median Total Raptor"),linetype = "dashed", size = 1) +
  labs(
    x = "Pick",
    y = "Metric Value Scaled")
b_spline_chart

df_plot <- bind_rows(
  tibble(
    x = 1:60,
    y = df_avg_raptor_medianGraph_new_1$pred_avgRaptorMedianSalaryGraph_scaled_new,
    m = "median",
    metric = "average Raptor"
  ),
  tibble(
    x = 1:60,
    y = df_total_raptor_medianGraph_new_1$pred_totalRaptorMedianSalaryGraph_scaled_new,
    m = "median",
    metric = "total Raptor"
  ),
  tibble(
    x = 1:60,
    y = df_avg_raptor_meanGraph_new_1$pred_avgRaptorMeanSalaryGraph_scaled_new,
    m = "mean",
    metric = "average Raptor"
  )
)
df_plot %>% 
  ggplot(aes(x = x, y = y, color = metric, linetype = m))+
  geom_line(linewidth = 1.5) +
  labs(x = "Draft Position",
       color = "Player\nPerformance\nMeasure",
       linetype = "",
       y = "Value Relative To First Pick") +
  scale_x_continuous(breaks = seq(0,60, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = .1))



##Testing------------------------

final_median_chart_scaled <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avg_raptor_medianGraph_new_1$pred_avgRaptorMedianSalaryGraph_scaled_new,
                color = "Average Raptor", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_raptor_medianGraph_new_1$pred_totalRaptorMedianSalaryGraph_scaled_new,
                color = "Total Raptor", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph_new_1$pred_avgBPMMedianSalaryGraph_scaled_new,
                color = "Average BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph_new_1$pred_totalBPMMedianSalaryGraph_scaled_new,
                color = "Total BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph_new_1$pred_avgWARMedianSalaryGraph_scaled_new,
                color = "Average WAR", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph_new_1$pred_totalWARMedianSalaryGraph_scaled_new_2,
                color = "Total WAR", linetype = "Median"), size = 1) +
  
  # Mean Lines
  geom_line(aes(x = 1:60, y = df_avg_raptor_meanGraph_new_1$pred_avgRaptorMeanSalaryGraph_scaled_new,
                color = "Average Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_raptor_meanGraph_new_1$pred_totalRaptorMeanSalaryGraph_scaled_new,
                color = "Total Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph_new_1$pred_avgBPMMeanSalaryGraph_scaled_new,
                color = "Average BPM", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph_new_1$pred_totalBPMMeanSalaryGraph_scaled_new,
                color = "Total BPM", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph_new_1$pred_avgWARMeanSalaryGraph_scaled_new,
                color = "Average WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph_new_1$pred_totalWARMeanSalaryGraph_scaled_new,
                color = "Total WAR", linetype = "Mean"), size = 1) +
    labs(x = "Draft Position", y = "Value Relative To First Pick",
       linetype = "", color = "Player Performance Measure") +
  scale_x_continuous(breaks = seq(0, 60, by = 10)) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  scale_linetype_manual(values = c("Mean" = "dashed", "Median" = "solid")) +
  guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))
print(final_median_chart_scaled)

b_spline_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled_new, 
                color = "Mean Average Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_model_1_new_scaled, 
                color = "Mean Total Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled, 
                color = "Mean Average BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled, 
                color = "Mean Total BPM", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled, 
                color = "Mean Total WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled, 
                color = "Mean Average WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph$pred_avgBPMMedianGraph_scaled, 
                color = "Median Average BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph$pred_totalBPMMedianGraph_scaled, 
                color = "Median Total BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph$pred_avgWARMedianGraph_scaled, 
                color = "Median Average WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_medianGraph$pred_avgRaptorMedianGraph_scaled, 
                color = "Median Average Raptor", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph$pred_totalWARMedianGraph_scaled, 
                color = "Median Total WAR", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_medianGraph$pred_totalRaptorMedianGraph_scaled, 
                color = "Median Total Raptor", linetype = "Median"), size = 1) +
    labs(x = "Pick", y = "Metric Value Scaled",
       linetype = "Type", color = "Player Performance Measure") +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed")) +
    guides(linetype = guide_legend(order = 1), color = guide_legend(order = 2))
print(b_spline_chart)


b_spline_chart <- ggplot() +
  geom_line(aes(x = 1:60, y = df_avgRaptor_meanGraph$pred_avgRaptorMeanGraph_scaled_new, 
                color = "Average Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_meanGraph$pred_model_1_new_scaled, 
                color = "Total Raptor", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_meanGraph$pred_avgBPMMeanGraph_scaled, 
                color = "Average WAR", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_meanGraph$pred_totalBPMMeanGraph_scaled, 
                color = "Total BPM", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_meanGraph$pred_totalWARMeanGraph_scaled, 
                color = "Total WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_meanGraph$pred_avgWARMeanGraph_scaled, 
                color = "Average WAR", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_BPM_medianGraph$pred_avgBPMMedianGraph_scaled, 
                color = "Average BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_BPM_medianGraph$pred_totalBPMMedianGraph_scaled, 
                color = "Total BPM", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avg_WAR_medianGraph$pred_avgWARMedianGraph_scaled, 
                color = "Average BPM", linetype = "Mean"), size = 1) +
  geom_line(aes(x = 1:60, y = df_avgRaptor_medianGraph$pred_avgRaptorMedianGraph_scaled, 
                color = "Average Raptor", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_total_WAR_medianGraph$pred_totalWARMedianGraph_scaled, 
                color = "Total WAR", linetype = "Median"), size = 1) +
  geom_line(aes(x = 1:60, y = df_totalRaptor_medianGraph$pred_totalRaptorMedianGraph_scaled, 
                color = "Total Raptor", linetype = "Median"), size = 1) +
  labs(x = "Pick", y = "Metric Value Scaled",
       color = "Player Performance Measure", linetype = "Type") +
  scale_linetype_manual(values = c("Mean" = "solid", "Median" = "dashed")) +
    guides(color = guide_legend(override.aes = list(linetype = "solid")),
         linetype = guide_legend(order = 2))
print(b_spline_chart)






