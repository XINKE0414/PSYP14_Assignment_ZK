#PART 1

#1. loading packages and data

library(tidyverse)
library(psych)
library(gridExtra)
library(dplyr)
library(lmtest)
library(lm.beta)
library(car)

data_pain = read.csv("https://tinyurl.com/yxm5rd89")

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

#2. checking coding errors

summary(data_pain) 
describe(data_pain)

data_pain_new = data_pain %>% 
  filter(!(pain == 55.0|STAI_trait == 4.20))

summary(data_pain_new)
describe(data_pain_new)

#3. buiding models

mod_1 = lm(pain ~ age + sex, data = data_pain_new)
summary(mod_1)

mod_2 = lm(pain ~ age 
                      + sex 
                      + STAI_trait 
                      + pain_cat
                      + mindfulness
                       + cortisol_serum 
                       + cortisol_saliva,
           data = data_pain_new)
summary(mod_2)

#4. checking influencial outliers
#scatterplots
data_pain_new %>%
  ggplot() + aes(x = age, y = pain, label = ID, color = sex) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

data_pain_new %>%
  ggplot() + aes(x = STAI_trait, y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

data_pain_new %>%
  ggplot() + aes(x = pain_cat, y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

data_pain_new %>%
  ggplot() + aes(x = mindfulness, y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

data_pain_new %>%
  ggplot() + aes(x = cortisol_serum , y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

data_pain_new %>%
  ggplot() + aes(x = cortisol_saliva , y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")

#Cook's distance > 4/158=0.025
mod_1 %>%
  plot(which = 4)
 
mod_2 %>%
  plot(which = 4)

data_pain_new %>%
  slice(c(8, 23, 46, 73, 85))

#5. Model Diagnosis

#5.1 Multicollinearity VIF>3
mod_2 %>%
  vif()

data_pain_new %>%
  select(pain, cortisol_saliva, cortisol_serum) %>%
  pairs.panels(col = "red", lm = T)

#remove cortisol_saliva because serum is considered more reliable

mod_3  = lm(pain ~ age +
                        sex +
                        STAI_trait +
                        pain_cat +
                        mindfulness +
                        cortisol_serum, 
                   data = data_pain_new)
summary(mod_3)

mod_3 %>%
  vif()


#5.2 Normality

# QQ plot
mod_3 %>%
  plot(which = 2)

# histogram
residuals_mod_3 = enframe(residuals(mod_3))
residuals_mod_3 %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(mod_3))

#5.3 Linearity
mod_3 %>%
  residualPlots()

data_pain_new %>%
  ggplot() + aes(x = age, y = pain, label = ID) +
  geom_point() + geom_text() +
  geom_smooth(method = "lm")



# 5.4 Homoscedasticty
mod_3 %>%
  plot(which = 3)

mod_3 %>%
  ncvTest()

mod_3 %>%
  bptest()


#6. Model Comparison
mod_1 = lm(pain ~ age + sex, data = data_pain_new)
summary(mod_1)

mod_2  = lm(pain ~ age +
              sex +
              STAI_trait +
              pain_cat +
              mindfulness +
              cortisol_serum, 
            data = data_pain_new)
summary(mod_2)

confint(mod_1)
confint(mod_2)

lm.beta(mod_1)
lm.beta(mod_2)

coef_table(mod_1)
coef_table(mod_2)

AIC(mod_1)
AIC(mod_2)

anova(mod_1, mod_2)

#PART 2

#1. loading packages and data

library(tidyverse)
library(psych)
library(gridExtra)
library(dplyr)
library(lmtest)
library(lm.beta)
library(car)

data_train = read.csv("https://tinyurl.com/yxm5rd89")
data_train = data_train %>% 
  filter(!(pain == 55.0|STAI_trait == 4.20))
data_test = read.csv("https://tinyurl.com/87v6emky")

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}

#2. buiding model

mod_original  = lm(pain ~ age +
                          sex +
                          STAI_trait +
                          pain_cat +
                          mindfulness +
                          cortisol_serum +
                          weight +
                          IQ +
                          household_income, 
            data = data_train)
summary(mod_original)

#3. checking influencial outliers

#Cook's distance > 4/158=0.025
mod_original %>%
  plot(which = 4)

data_train %>%
  slice(c(46, 84, 85))

#4. Model Diagnosis

#4.1 Normality

# QQ plot
mod_original %>%
  plot(which = 2)

# histogram
residuals_mod_original = enframe(residuals(mod_original))
residuals_mod_original %>%
  ggplot() + aes(x = value) + geom_histogram()

# skew and kurtosis
describe(residuals(mod_original))

#4.2 Linearity
mod_original %>%
  residualPlots()

#4.3 Homoscedasticty
mod_original %>%
  plot(which = 3)

mod_original %>%
  ncvTest()

mod_original %>%
  bptest()

#4.4 Multicollinearity VIF<3
mod_original %>%
  vif()

#5. Backward Regression
mod_backward = step(mod_original, direction = "backward")
summary(mod_backward)

# re-examining the model
mod_backward %>% 
  plot(which = 2)

describe(residuals(mod_backward))

mod_backward %>%
  residualPlots()

mod_backward %>%
  plot(which = 3)

mod_backward %>% 
  ncvTest()

mod_backward %>%
  bptest()

mod_backward %>%
  vif()


#6. Model Comparison
mod_theorybased = lm(pain ~ age +
                            sex +
                            STAI_trait +
                             pain_cat +
                             mindfulness +
                             cortisol_serum, 
                     data = data_train)
summary(mod_theorybased)
summary(mod_backward)
summary(mod_original)

AIC(mod_theorybased)
AIC(mod_backward)
AIC(mod_original)

anova(mod_original, mod_backward)
anova(mod_theorybased, mod_backward)

coef_table(mod_backward)

#7. Model Test
summary(data_test)

pred_theorybased = predict(mod_theorybased, data_test)
pred_backward = predict(mod_backward, data_test)
	
RSS_theorybased = sum((data_test[, "pain"] - pred_theorybased)^2)
RSS_backward = sum((data_test[, "pain"] - pred_backward)^2)

RSS_theorybased
RSS_backward

#Part 3
#1. loading packages and data

library(tidyverse)
library(cAIC4)
library(r2glmm) 
library(lme4) 
library(lmerTest)
library(MuMIn)
library(psych)
library(gridExtra)
library(dplyr)
library(lmtest)
library(lm.beta)
library(car)

data_train = read.csv("https://tinyurl.com/yxm5rd89")
data_train = data_train %>% 
  filter(!(pain == 55.0|STAI_trait == 4.20))
data_3 = read.csv("https://tinyurl.com/b385chpu")
data_4 = read.csv("https://tinyurl.com/4f8thztv")

data_3 = data_3 %>%
  mutate(hospital = factor(hospital))


stdCoef.merMod <- function(object) {
  sdy <- sd(getME(object, "y"))
  sdx <- apply(getME(object, "X"), 2, sd)
  sc <- fixef(object) * sdx/sdy
  se.fixef <- coef(summary(object))[, "Std. Error"]
  se <- se.fixef * sdx/sdy
  return(data.frame(stdcoef = sc, stdse = se))
}

coef_table = function(model) {
  require(lm.beta)
  mod_sum = summary(model)
  mod_sum_p_values = as.character(round(mod_sum$coefficients[,
                                                             4], 3))
  mod_sum_p_values[mod_sum_p_values != "0" & mod_sum_p_values !=
                     "1"] = substr(mod_sum_p_values[mod_sum_p_values != "0" &
                                                      mod_sum_p_values != "1"], 2, nchar(mod_sum_p_values[mod_sum_p_values !=
                                                                                                            "0" & mod_sum_p_values != "1"]))
  mod_sum_p_values[mod_sum_p_values == "0"] = "<.001"
  mod_sum_table = cbind(as.data.frame(round(cbind(coef(model),
                                                  confint(model), c(0, lm.beta(model)$standardized.coefficients[c(2:length(model$coefficients))])),
                                            2)), mod_sum_p_values)
  names(mod_sum_table) = c("b", "95%CI lb", "95%CI ub", "Std.Beta",
                           "p-value")
  mod_sum_table["(Intercept)", "Std.Beta"] = "0"
  return(mod_sum_table)
}
#2. checking coding errors

summary(data_3) 
describe(data_3)

data_3 = data_3 %>% 
  filter(!(household_income == -7884))

summary(data_3)

#3. building model

mod_rnd_int = lmer(pain ~ sex +
                          age +
                          STAI_trait +
                          pain_cat +
                          cortisol_serum +
                          mindfulness +
                          (1|hospital), 
                   data = data_3)
summary(mod_rnd_int)

confint(mod_rnd_int)

stdCoef.merMod(mod_rnd_int)

mod_theorybased = lm(pain ~ age +
                       sex +
                       STAI_trait +
                       pain_cat +
                       mindfulness +
                       cortisol_serum, 
                     data = data_train)
summary(mod_theorybased)
confint(mod_theorybased)
coef_table(mod_theorybased)

r2beta(mod_rnd_int, method = "nsj", data = data_3)
r.squaredGLMM(mod_rnd_int)

#4. testing model
pred_rnd_int = predict(mod_rnd_int, data_4, allow.new.levels = TRUE)

RSS_pred_rnd_int = sum((data_4[, "pain"] - pred_rnd_int)^2)
TSS_pred_rnd_int = sum((data_4[, "pain"] - mean(data_4[, "pain"]))^2)
R_squared = 1 - (RSS_pred_rnd_int/TSS_pred_rnd_int)
R_squared

# random slope model
mod_rnd_slope = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), data = data_3)

mod_rnd_slope_opt = lmer(pain ~ cortisol_serum + (cortisol_serum | hospital), control = lmerControl(optimizer = "Nelder_Mead"),
                         data = data_3)

slope_plot = data_3 %>%
  ggplot() + aes(y = pain, x = cortisol_serum, color = hospital) +
  geom_point(size = 4) + geom_smooth(method = "lm", se = F,
                                     fullrange = TRUE) + xlim(1, 9) + geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)
slope_plot

data_3 = data_3 %>%
  mutate(pred_slope = predict(mod_rnd_slope_opt))

data_3 %>%
  ggplot() + 
  aes(y = pain, x = cortisol_serum, group = hospital) +
  geom_point(aes(color = hospital), size = 2) + 
  geom_line(color = "red", aes(y = pred_slope, x = cortisol_serum)) + 
  facet_wrap(~hospital, ncol = 2)

