# Define function to compute SAC by group
sp_accum_group <- function(x, groups){
  
  data_clean <- {{ x }} %>%
  # Which groups do you want different SAC's for? 
  dplyr::group_by( {{ groups }} ) %>%
  # Splits the data into the different groups 
  tidyr::nest() %>%
  # Run the species accumulation curves by group
  dplyr::mutate(data = purrr::map(data, vegan::poolaccum)) %>%
  # Extract the observed species richness estimator (denoted by S)
  dplyr::mutate(data_df = purrr::map(data,
                                     ~ data.frame(summary(.)$S,
                                                  check.names = FALSE))) %>%
  # Drop unnecessary columns
  dplyr::select(-c(data)) %>%
  # Convert the lists back into a data frame
  unnest(cols = c(data_df)) %>%
  # Rename the columns
  dplyr::rename(group = {{ groups }},
                N = N,
                S = S,
                lower_ci = `2.5%`,
                upper_ci = `97.5%`,
                std_dev = Std.Dev) %>%
  # Make the groups into a factor
  dplyr::mutate(group = as.factor(group))
  
}

###############################################################################
# Test SAC function 
test_sac <- sp_accum_group(x = data_clean, 
                          groups = season)
test_sac

# Define function to plot the SAC by groups above 
plot_sp_accum_group <- function(x){
  
  {{ x }} %>%
  ggplot(data = ., aes(x = N,
                       y = S,
                       group = group)) +
  geom_ribbon(aes(ymin = lower_ci,
                  ymax = upper_ci,
                  fill = group),
              alpha = 0.3) +
  geom_line(aes(colour = group)) +
  labs(x = "No. of surveys",
       y = "Species richness") +
  guides(colour = FALSE) + 
  facet_wrap(~ group, ncol = 2) + 
  theme(legend.position = "right")

} 

# Test plotting function
test_plot_fun <- plot_sp_accum_group(x = test_sac) 
test_plot_fun 




# Define function to compute all richness estimators (no groups)
sp_est_all <- function(x){
  
  {{ x }} %>%
    tidyr::nest(data = everything()) %>%
    dplyr::mutate(data = purrr::map(data, vegan::poolaccum)) %>%
    # Extract the observed species richness estimator (denoted by S)
    dplyr::mutate(data_s    = purrr::map(data,
                                       ~ data.frame(summary(.)$S,
                                                    check.names = FALSE))) %>%
    # Extract the chao richness estimator (denoted by chao)
    dplyr::mutate(data_chao = purrr::map(data,
                                       ~ data.frame(summary(.)$chao,
                                                    check.names = FALSE))) %>%
  # Extract the jack1 richness estimator (denoted by chao)
  dplyr::mutate(data_jack = purrr::map(data,
                                       ~ data.frame(summary(.)$jack1,
                                                    check.names = FALSE))) %>%
  # Extract the boot richness estimator (denoted by chao)
  dplyr::mutate(data_boot = purrr::map(data,
                                       ~ data.frame(summary(.)$boot,
                                                    check.names = FALSE))) %>%
  # Drop unnecessary columns
  dplyr::select(-c(data)) %>%
  # Make longer
  tidyr::pivot_longer(
    cols = data_s:data_boot,
    names_to = "estimator"
  ) %>%
  tidyr::unnest(cols = c(value)) %>%
  tidyr::unite(col = "value", 
               c(S, Chao), 
               sep = " ", 
               na.rm = TRUE, 
               remove = FALSE) %>%
  dplyr::rename(jack1 = 9) %>%
  tidyr::unite(col = "value", 
               c(value, jack1), 
               sep = " ", 
               na.rm = TRUE, 
               remove = FALSE) %>%
  tidyr::unite(col = "value", 
               c(value, Bootstrap), 
               sep = " ", 
               na.rm = TRUE, 
               remove = FALSE) %>%
  dplyr::mutate(value = as.numeric(value)) %>%
  # Rename the columns
  dplyr::rename(N = N,
                S = S,
                lower_ci = `2.5%`,
                upper_ci = `97.5%`,
                std_dev = Std.Dev) %>%
  # Drop some rubbish columns
  dplyr::select(-c(Chao, jack1, Bootstrap, std_dev, S)) %>%
  dplyr::mutate(estimator = dplyr::case_when(
    estimator == "data_s" ~ "S",
    estimator == "data_chao" ~ "Chao",
    estimator == "data_jack" ~ "Jacknife",
    estimator == "data_boot" ~ "Bootstrap"
  )) %>%
  dplyr::mutate(estimator = as.factor(estimator)) %>%
  dplyr::mutate(estimator = fct_relevel(estimator, 
                                        "S",
                                        "Chao",
                                        "Jacknife",
                                        "Bootstrap"))
  
}

# Define function to plot the species richness estimators (no groups )
plot_sp_est_all <- function(x){
  
  {{ x }} %>%
    ggplot(data = ., aes(x = N,
                         y = value)) +
    geom_ribbon(aes(ymin = lower_ci,
                    ymax = upper_ci),
                alpha = 0.3) +
    geom_line() +
    labs(x = "No. of surveys",
         y = "Estimated species richness") +
    # Different panels for the different estimators
    facet_wrap(~ estimator, ncol = 4)

}

# Define function to compute all richness estimators (no groups)

sp_est_group <- function(x, groups){
  
  {{ x }} %>%
    dplyr::group_by({{ groups }}) %>%
    tidyr::nest() %>%
    dplyr::mutate(data = purrr::map(data, vegan::poolaccum)) %>%
    # Extract the observed species richness estimator (denoted by S)
    dplyr::mutate(data_s    = purrr::map(data,
                                         ~ data.frame(summary(.)$S,
                                                      check.names = FALSE))) %>%
    # Extract the chao richness estimator (denoted by chao)
    dplyr::mutate(data_chao = purrr::map(data,
                                         ~ data.frame(summary(.)$chao,
                                                      check.names = FALSE))) %>%
    # Extract the jack1 richness estimator (denoted by chao)
    dplyr::mutate(data_jack = purrr::map(data,
                                         ~ data.frame(summary(.)$jack1,
                                                      check.names = FALSE))) %>%
    # Extract the boot richness estimator (denoted by chao)
    dplyr::mutate(data_boot = purrr::map(data,
                                         ~ data.frame(summary(.)$boot,
                                                      check.names = FALSE))) %>%
    # Drop unnecessary columns
    dplyr::select(-c(data)) %>%
    # Make longer
    tidyr::pivot_longer(
      cols = data_s:data_boot,
      names_to = "estimator"
    ) %>%
    tidyr::unnest(cols = c(value)) %>%
    tidyr::unite(col = "value", 
                 c(S, Chao), 
                 sep = " ", 
                 na.rm = TRUE, 
                 remove = FALSE) %>%
    dplyr::rename(jack1 = 10) %>%
    tidyr::unite(col = "value", 
                 c(value, jack1), 
                 sep = " ", 
                 na.rm = TRUE, 
                 remove = FALSE) %>%
    tidyr::unite(col = "value", 
                 c(value, Bootstrap), 
                 sep = " ", 
                 na.rm = TRUE, 
                 remove = FALSE) %>%
    dplyr::mutate(value = stringr::str_extract(value, "[^ ]+")) %>%
    dplyr::mutate(value = as.numeric(value)) %>%
    # Rename the columns
    dplyr::rename(group = {{ groups }},
                  N = N,
                  S = S,
                  lower_ci = `2.5%`,
                  upper_ci = `97.5%`,
                  std_dev = Std.Dev) %>%
    # Drop some rubbish columns
    dplyr::select(-c(Chao, jack1, Bootstrap, std_dev, S)) %>%
    dplyr::mutate(estimator = dplyr::case_when(
      estimator == "data_s" ~ "S",
      estimator == "data_chao" ~ "Chao",
      estimator == "data_jack" ~ "Jacknife",
      estimator == "data_boot" ~ "Bootstrap"
    )) %>%
    dplyr::mutate(estimator = as.factor(estimator),
                  group = as.factor(group)) %>%
    dplyr::mutate(estimator = fct_relevel(estimator, 
                                          "S",
                                          "Chao",
                                          "Jacknife",
                                          "Bootstrap"))
}

# Define function to plot the species richness estimators (no groups )
plot_sp_est_group <- function(x){
  
  {{ x }} %>%
    ggplot(data = ., aes(x = N,
                         y = value,
                         group = group)) +
    geom_ribbon(aes(ymin = lower_ci,
                    ymax = upper_ci,
                    fill = group),
                alpha = 0.3) +
    geom_line(aes(colour = group)) +
    labs(x = "No. of surveys",
         y = "Estimated species richness",
         fill = "group") +
    guides(colour = FALSE) + 
    # Different panels for the different estimators
    #facet_wrap(~ group + estimator, ncol = 4)
    facet_grid(vars(group), vars(estimator)) +
    theme(strip.text.y = element_blank(),
          legend.position = "right")
  
}
