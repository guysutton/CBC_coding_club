library(tidyverse)

data <- readxl::read_xlsx("./data_raw/Book1.xlsx")

head(data)

# Get mean and sd for each water chemistry variable between sites
data %>%
  dplyr::mutate(site = as.factor(site)) %>%
  dplyr::group_by(site) %>%
  dplyr::summarise(across(K:O, list(mean = mean, max = max)))

data %>%
  group_by(site) %>%
  dplyr::summarise(mean_K = mean(K),
                   mean_N = mean(N),
                   mean_O = mean(O),
                   sd_K = mean(K),
                   sd_N = mean(N),
                   sd_O = mean(O))
