# 04_missingness


# Missing Data Analysis


df_wide <- read.csv(here::here("data", "raw", "data_2.csv"))

dir.create("figures/missingness", recursive = TRUE, showWarnings = FALSE)



## 1. Exploring missing data


prop_miss(df_wide)

print(miss_var_summary(df_wide), n=50) # Missingness per variable

df_wide |> miss_case_summary() # Missingness per case

aggr(df_wide, numbers = TRUE, prop = FALSE) # missing pattern



missing_1 <- vis_miss(df_wide,
                      sort_miss = TRUE)



ggsave("figures/missingness/missingness_plot.png", 
       missing_1, width = 7, height = 5, dpi = 300)

missing_2 <- gg_miss_var(df_wide)

ggsave("figures/missingness/missingness_plot2.png", 
       missing_2, width = 7, height = 5, dpi = 300)



df_nab <- df_wide |>  
  bind_shadow()


missing_3 <- df_nab |> 
  ggplot(aes(x = nom_like_NA, y = SUS_1)) +
  geom_boxplot()


ggsave("figures/missingness/missingness_plot3.png", 
       missing_3, width = 7, height = 5, dpi = 300)


# 2. Missing Data Mechanism


vis_miss(df_wide, sort_miss = TRUE)


gg_miss_var(df_wide)


df_nab |> 
  ggplot(aes(x = nom_like_NA, y = SUS_1)) +
  geom_boxplot()


# --- nom_like --- #

df_nab |> 
  ggplot(aes(x = nom_like_NA,
             y = class)) +
  geom_boxplot()



df_nab |> 
  ggplot(aes(x = nom_like_NA,
             y = group)) +
  geom_boxplot()


df_nab |> 
  ggplot(aes(x = nom_like_NA,
             y = school)) +
  geom_boxplot()


plot_sus_na <- df_nab |> 
  ggplot(aes(x = nom_like_NA,
             y = SUS_1)) +
  geom_boxplot()


ggsave("figures/missingness/missingness_plot4.png", 
       plot_sus_na, width = 7, height = 5, dpi = 300)


# --- SUS scores --- #


df_nab |> 
  ggplot(aes(x = SUS_1_NA,
             y = class)) +
  geom_boxplot()



df_nab |> 
  ggplot(aes(x = SUS_1_NA,
             y = group)) +
  geom_boxplot()


df_nab |> 
  ggplot(aes(x = SUS_1_NA,
             y = school)) +
  geom_boxplot()


# --- X2 --- #


df_nab |> 
  ggplot(aes(x = X2_CLIMATE_2hard90_NA,
             y = class)) +
  geom_boxplot()



df_nab |> 
  ggplot(aes(x = X2_CLIMATE_2hard90_NA,
             y = group)) +
  geom_boxplot()


df_nab |> 
  ggplot(aes(x = X2_CLIMATE_2hard90_NA,
             y = school)) +
  geom_boxplot()



message("Missingness diagnostics and plots created.")