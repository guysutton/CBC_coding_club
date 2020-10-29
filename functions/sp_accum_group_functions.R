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
