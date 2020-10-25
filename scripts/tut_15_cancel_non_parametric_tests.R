


# Simulate some data
library(tidyverse)

###########################################################################
# Mann-Whitney U-Test -----------------------------------------------------
###########################################################################

# What is a Mann-Whitney U-test?
# - Usually, the first thing we say is 'non-parametric t-test'. 
# - (1) We often say that it is a comparison of medians.
#   - i.e. we are testing the null hypothesis that two group medians
#     are the same. 
# - (2) The original derivation was a test of stochastic equality:
#   - Used to test whether two samples come from the same distribution/population
#     (i.e. they have the same shape). 
#   - In other words, the probability that a random sample from
#     one group is larger than a random sample from the other group. 
#     - I think it makes more sense to think of this as numerical dominance. 

# Depending on the structure of our data, the M-W test could be 
# either (1) a test of difference in medians (as we traditionally believe),
# or (2) a test of stochastic equality (much more likely and common). 
# - For the M-W to be a test of medians (1), the shape must be identical between
#   the two groups.
#   - I.e. We must satisfy the equal variances assumption of traditional 
#     parametric analyses. 
#   - The M-W test is extremely sensitive to unequal variance.
#   - The only thing that can differ between groups is the 'location' 
#     of the median/centre of each group.
# - When the equal variances/shape assumption is not met, the M-W then becomes
#   a test of stochastic equality.

# Let's take a look at an example 

# Simulate some data 
group1 <- c(9,9,9,9,9,9,9,9,9,
            10,11,12,13,14,15,16,17,18,19)
group2 <- c(1,2,3,4,5,6,7,8,9,
            10,11,11,11,11,11,11,11,11,11)
group1 <- as.data.frame(group1)
group2 <- as.data.frame(group2)
data <- bind_cols(group1, group2)
head(data)
data <- data %>%
  tidyr::pivot_longer(
    cols = c(group1, group2),
    names_to = "groups",
    values_to = "values") %>%
  dplyr::mutate(groups = as.factor(groups))
head(data)

# Visualise 
data %>%
  ggplot(data = ., aes(x = groups,
                       y = values)) +
  geom_boxplot() +
  labs(subtitle = "Are group medians the same?")

# What can we tell about our data:
# - Are the medians the same? YES! 
# - Is there unequal variances? Well...
# - Is the shape of the distribution different? 

# Mann-Whitney U-test
wilcox.test(values ~ groups, 
            data = data)

# P-value < 0.05, therefore we would typically infer the the median value across
# groups is different.
# - But clearly that is not the case. 
# - Clearly, the data dictates that this M-W test is a test of stochastic dominance.
#   - There is a statistically higher probability that a random sample 
#     from group1 is larger than group2... 
# - This data didn't even fail the unequal variances assumption, the shape 
#   of the data distribution was just different... 

# Sucks, right? 

