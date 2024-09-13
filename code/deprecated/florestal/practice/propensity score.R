# Purpose: Propensity score matching example
# Author: Henry Chen
# Date: 6/29/2023

# Consider this scenario: two units, A and B, are assigned to treatment and control, 
# respectively. But their propensity score is 0.6. Thus, they had the same 60% conditional 
# probability of being assigned to treatment, but by random chance, A was assigned to 
# treatment and B was assigned to control. The idea with propensity score methods 
# is to compare units who, based on observables, had very similar probabilities of 
# being placed into the treatment group even though those units differed with regard 
# to actual treatment assignment.

# requires conditional independence assumption (CIA) as well as common support assumption
# which says that there be units in the treatment and control group across the 
# estimated propensity score.


# calculating the ATE

library(tidyverse)
library(haven)

read_data <- function(df)
{
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/",
                     df, sep = "")
  df <- read_dta(full_path)
  return(df)
}

nsw_dw <- read_data("nsw_mixtape.dta")

nsw_dw %>%
  filter(treat == 1) %>%
  summary(re78)

mean1 <- nsw_dw %>%
  filter(treat == 1) %>%
  pull(re78) %>%
  mean()

nsw_dw$y1 <- mean1

nsw_dw %>%
  filter(treat == 0) %>%
  summary(re78)

mean0 <- nsw_dw %>%
  filter(treat == 0) %>%
  pull(re78) %>%
  mean()

nsw_dw$y0 <- mean0

ate <- unique(nsw_dw$y1 - nsw_dw$y0)

nsw_dw <- nsw_dw %>%
  filter(treat == 1) %>%
  select(-y1, -y0)

# Now calculate propensity score
nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>%
  bind_rows(nsw_dw) %>%
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ * educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)

logit_nsw = glm(treat ~ age + agesq + agecube + educ + educsq +
                  marr + nodegree + black + hisp + re74 + re75 + 
                  u74 + u75 + interaction1, family = binomial(link = "logit"),
                data = nsw_dw_cpscontrol)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>%
  mutate(pscore = logit_nsw$fitted.values)

pscore_control <- nsw_dw_cpscontrol %>%
  filter(treat == 0) %>%
  pull(pscore) %>%
  mean()

pscore_treated <- nsw_dw_cpscontrol %>%
  filter(treat == 1) %>%
  pull(pscore) %>%
  mean()

nsw_dw_cpscontrol %>%
  filter(treat == 0) %>%
  ggplot() + 
  geom_histogram(aes(x = pscore))

nsw_dw_cpscontrol %>%
  filter(treat == 1) %>%
  ggplot() + 
  geom_histogram(aes(x = pscore))

# The probability of treatment is spread out across the units in the treatment group, 
# but there is a very large mass of nearly zero propensity scores in the CPS. How do 
# we interpret this? What this means is that the characteristics of individuals in the 
# treatment group are rare in the CPS sample. This is not surprising given the strong 
# negative selection into treatment. These individuals are younger, less likely to be 
# married, and more likely to be uneducated and a minority. The lesson is, if the two 
# groups are significantly different on background characteristics, then the propensity 
# scores will have grossly different distributions by treatment status.


# inverse propensity weighting


