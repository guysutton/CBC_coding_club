###########################################################
###########################################################
# - Linear modelling in R 
# - CBC Coding Club
# - Tutorial #1: Linear models in R 
# - Rhodes Universtiy
# - Script written: 01/05/2020
# - By: Guy F. Sutton
###########################################################
###########################################################

# Load required libraries to run script 
# You may need to install the package first
# install.packages("tidyverse") # - remove the # from this row and click run. 
library(tidyverse)

# I like my plots to look a certain way
theme_set(theme_classic() +
            theme(panel.border = element_rect(colour = "black", fill = NA),
                  axis.text = element_text(colour = "black"),
                  axis.title.x = element_text(margin = unit(c(2, 0, 0, 0), "mm")),
                  axis.title.y = element_text(margin = unit(c(0, 4, 0, 0), "mm")),
                  legend.position = "none"))

# Import data
data <- readxl::read_excel("./data_raw/poisson_data.xlsx")

# Check data imported properly
str(data)
head(data)

###
# - Experimental design 
###

# Ran an experiment to determine
# - (1) Does temperature influence insect reproduction?
# - (2) Does insect body mass influence insect reproduction? 

# Temperature = temp - 4 levels:
# - 15 degrees, 20 degrees, 25 degrees, 30 degrees
# - factor variable 

# Insect body mass = adult_mass
# - numeric variable

# No of larvae produced by insects = larvae 
# - numeric variable

###
# Research question #1 -  Do larger insects produce more larvae? 
###

###
# Step 1 - Explore the data (visualise)
###

# Firstly, visualise relationship between predictor and response
ggplot(data = data, aes(x = adult_mass,
                        y = larvae)) +
  geom_point()

# Secondly, is your predictor linear-ish?
hist(data$adult_mass)

# Thirdly, are there outliers in the response variable?
raw_data <- data %>%
  mutate(adult_mass = as.factor(adult_mass))

ggplot(data = raw_data, aes(x = adult_mass,
                            y = larvae)) +
  geom_boxplot()

###
# - Step 2 - Run a linear model
###

# Specify a linear model 
# - We use the 'lm' function to tell R to run a linear model
# - We then specificy our response var (larvae),
# - We then specificy our predictor var (adult_mass),
#   (predictors go to the right of ~ sign). 
# - We also have to specify where R must look for this data
#   using the 'data = ...' argument 
mod1 <- lm(larvae ~ adult_mass,
           data = data)

# Print the summary of the model
summary(mod1)

###
# Step 3 - Model diagnostics 
###

# Before looking at the outcome of our model,
# we should first check whether the model we specified is 
# a good fit to the data or not. 
# We do this by performing model diagnostics 

# Basically, here we are going to check whether our 
# data and model meet the assumptions required to run a linear model.

# 1. Independence of data points
#    - This cannot be checked per se. 
#    - This is a property of your experimental design
#    - Make sure you design your experiment properly. 
#    - If you have non-independence, there are ways to deal 
#      it, will deal with this in a latter video (GLMM's)!!! Fun!!!

# 2. Are the residuals normally distributed?
#    - Often this assumption is applied to the data,
#      i.e. are your data / response normally distributed?
#    - This is not correct.
#    - The assumption is actually that your residuals are normally 
#      distributed. 
#      - Residuals are how far each data point deviates 
#        from the expected value / trend. 
#    - Residuals represent variation in the data that your model can't account for. 
#    - We use residual plots to discover patterns (QQ plot).
#    - We don't want to see any systematic patterns in residuals,
#      should be a straight 1:1 line. 
plot(mod1, which = 2)

# *** Alternative approach, probably more interpretable
resids <- resid(mod1) # extract model residuals
hist(resids) # plot a histogram

# Here, we can see that the points in the tails of the line deviate from the dashed line
# - This is an indication that the residuals are not normally distributed.
# - As such, the model does not capture the variation in our data well
#   at the extremities of the dataset 
#   (i.e. predictions of how many larvae are expected from adults at the min and max 
#   adult mass values is going to be misleading). 
# - The histogram definitively shows that our residuals are not normally distributed. 
# - TAKE HOME: Linear model is probably not a good model to use for this analysis. 

# 3. Homogeneity of variance
#   - Testing the assumption of equality of variances. 
#   - Residuals vs fitted - we don't want to see any pattern.
#   - Here, we want to see that red line lie on the y = 0 line.
#   - You really don't want to see your white circles have any pattern (i.e. funnel, U-shape)
plot(mod1, which = 1)

# Here, the red line varies a bit, indicating that the variance varies slightly, 
# but there is nothing for concern. 
# You would be happy with this as your model satifies the homogeneity of variance assumption. 

# LINEAR MODEL IS NOT A GREAT CHOICE. YOU WOULD LOOK AT AN ALTERNATIVE MODEL TYPE.
# CODE BELOW ASSUMES THAT A LM WAS APPROPRIATE - use on your own data. 

###
# Step 4 - Interpreting model output
###

# Print summary of results 
summary(mod1)

# Get Type II sum of squares 
car::Anova(mod1, type="II")

# - (1) Intercept estimate - value of y-value (larvae) where X (adult_mass) = 0
coef(mod1)[[1]]

#       Intercept = 2.39,
#       In this example, adult mass cannot = 0,  
#       Intercept has no meaning in this example.
#       It often can have a meaning (whenever x = 0 is reasonable, e.g. temperature), 
#       and be very important. 

# - (2) Is my treatment/factor significant?
#       Look at the row for your predictor variable in SOS table (here: adult_mass)
#       Look at the P-value: P <0.05 means this variable is significant 
#       - Here, adult mass P-val < 0.05, 
#         so we conclude there is an association between adult mass and larvae produced. 

# - (3) How does my treatment effect response?
#       Look at the estimate value in your predictor variable row
#       > 0 estimate = response increases with greater values of predictor
#       < 0 estimate = response decreases with greater values of predictor
coef(mod1)[2]

#       Here, our estimate = 2.6066 (> 0, positive relationship).
#       This means that as adult mass increases, the number of larvae they produce increases.
#       - The actual value (2.6066) means that for every 1 unit increase in adult body mass
#         (i.e. if adult mass increases by 1g), fecundity increases by 2.6 larvae. 

# - (4) Extract parameter uncertainty 
#       It is always good to report the uncertainty of your model estimates.
#       To do this, we report a 95% confidence interval.
#       - The interval DOES NOT say that we can be 95% certain that 
#         our estimate falls somewhere within this range (lots of papers do this 
#         incorrectly.)
#       - The interval shows that if we had to repeat this experiment 100 times,
#         we would expect our parameter to fall within the 95% CI, 95 times.
#       - A subtle but very inportant distinction - outside the scope of this course. 
#       - We will now extract the 95% CI for our adult_mass estimate
confint(mod1)

# Here, our 95% CI for adult body mass is 1.52 - 3.68
# This means that there is some uncertaintly in our parameter estimate from (3).
# Interpretation: For every 1 unit increase in adult mass (i.e. as adults 
# become 1g heavier), we can be quite confident that they will produce 
# 1.52 - 3.68 more larvae than an adult 1g lighter in mass.

# - (4) How much variation is accounted for?
#       - We must extract the adjusted R-squared value
#         - This shows how much variation in your response var (e.g. larvae)
#           is explained by your predictors (e.g. adult_mass)
summary(mod1)$adj.r.squared

#       Here, an R-squared = 0.21
#       Therefore, 21% of the variation in larvae produced 
#       is explained by female body mass. 

###
# Step 5 - Plot your model predictions 
###

# (1) Define the range of x-values you want to predict over 
#     - Here, we predict over body mass of 1 - 15g
adult_mass <- seq(1, 15, 1)

# (2) Predict the number of larvae from our model
mod_predict <- predict(mod1, 
                       list(temp = adult_mass), 
                       interval = "confidence")

# (3) Convert (2) into a data frame 
mod_predict <- as.data.frame(mod_predict)

# (4) Add the range of x values back to the model predictions
mod_predict <- bind_cols(mod_predict, as.data.frame(adult_mass))

# Plot your model predictions
ggplot() + 
  # Add a ribbon of 95% confidence intervals
  geom_ribbon(data = mod_predict, aes(x = adult_mass, 
                                      ymin = lwr,
                                      ymax = upr),
              fill = "gray80") + 
  # Add line of model prediction
  geom_line(data = mod_predict, aes(x = adult_mass, 
                                    y = fit)) +
  # Define y-axis limits
  scale_y_continuous(breaks = seq(0, 60, 10),
                     limits = c(0, 60)) +
  # Define x-axis limits
  scale_x_continuous(breaks = seq(0, 15, 3),
                     limits = c(0, 15)) + 
  # Write x and y axis labels
  labs(x = "Adult body mass (g)",
       y = "No. of larvae produced",
       subtitle = "(a)")

###
# Step 6 - Write-up your results  
###

# Everyone has a different style of writing. 
# Below I give some examples. 
# Any text within [] is me explaining, and not actually included in the text. 
# This is my approach, it does not have to be yours. 

# (1) The standard example is:
# Adult body mass had a significant influence on the number of larvae produced 
# (P < 0.05)

# - In my opinion, this is pretty poor. 
# - It really tells us nothing. 

# (2) Improvement on (1):
# Adult body mass had a significant influence on the number of larvae produced 
# (beta = 2.61; P < 0.05). [where beta is estimate from 3]. 

# - Adding the parameter estimate at least tells the reader the sign
#   and magnitude of the effect adult_mass has on larvae. 

# (3) More improvements:
# Adult body mass had a significant influence on the number of larvae produced 
# (beta = 2.61; 95% CI = 1.52 - 3.68; P < 0.05).

# - Adding the CI shows the uncertainty in our estimate.
# - Much better. 
# - We can still do better. 

# (4) Actually explain your results.
#     - This is my prefered approach. 

# Larger females produced more larvae than smaller adults (P < 0.05). 
# Approximately 21% of the variation in fecundity was explained
# by female body mass (Adj. R-squared = 0.21). 
# For every 1g increase in adult body mass, adults produce approximately
# 2.61 more larvae (95% CI: 1.52 - 3.68) (Fig. x) [refer the reader to your plot
# of the model prediction]. 

# - We have told the reader exactly how the predictor effects response,
#   we have reported uncertainty, and referred the reader to our awesome graph!!! 

# Your results for your paper are now ready to go!!!
