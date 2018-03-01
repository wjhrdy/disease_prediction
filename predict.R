library(tidyverse)
library(readxl)
library(ggplot2)

df <- read_excel(path = "s5820 Ex 3 Workbook 2007.xlsm", 
                 sheet = "Health Data", 
                 col_names = c("year", "deaths", "a_cancer", "b_heart", "c_flu"),
                 skip = 1)

model <- lm(deaths ~ a_cancer + b_heart + c_flu, data = df)

broom::tidy(model)

df_predict <- data_frame(a_cancer = 1000,
                         b_heart = 600,
                         c_flu = 900)


prediction <- predict(model, newdata = df_predict)

prediction

to_plot <- df %>% gather(key = "disease", 
                         value = "cases", 
                         a_cancer, 
                         b_heart, 
                         c_flu)

vlines <- rownames_to_column(as.data.frame(t(df_predict)))
names(vlines) <- c("disease", "prediction")

ggplot(to_plot, aes(cases, deaths)) + 
  geom_point(aes(color = disease)) + 
  geom_smooth(aes(color = disease), method='lm', formula = y ~ x, se = F) +
  geom_vline(data = vlines, aes(xintercept = prediction, color = disease)) +
  geom_hline(aes(yintercept = prediction))


  