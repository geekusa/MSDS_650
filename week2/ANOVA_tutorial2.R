setwd("/Users/worshamn/Dropbox/Documents/Regis/MSDS650/week2/")
plant.df = PlantGrowth
plant.df$group = factor(plant.df$group, labels = c("Control", "Treatment 1", "Treatment 2"))
require(ggplot2)
ggplot(plant.df, aes(x = group, y = weight)) +
  geom_boxplot(fill = "grey80", colour = "blue") +
  scale_x_discrete() + xlab("Treatment Group") +
  ylab("Dried weight of plants")
plant.mod1 = lm(weight ~ group, data = plant.df)
summary(plant.mod1)
anova(plant.mod1)
confint(plant.mod1)
plant.mod = data.frame(Fitted = fitted(plant.mod1), Residuals = resid(plant.mod1), Treatment = plant.df$group)
ggplot(plant.mod, aes(Fitted, Residuals, colour = Treatment)) + geom_point()



#2-way

delivery.res = delivery.df
delivery.res$M1.Fit = fitted(delivery.mod1)
delivery.res$M1.Resid = resid(delivery.mod1)
ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) + geom_point() + 
  xlab("Fitted Values") + ylab("Residuals")
ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Service)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Destination)
ggplot(delivery.res, aes(M1.Fit, M1.Resid, colour = Destination)) +
  geom_point() + xlab("Fitted Values") + ylab("Residuals") +
  facet_wrap( ~ Service)
ggplot(delivery.res, aes(sample = M1.Resid)) + stat_qq()
TukeyHSD(delivery.mod1, which = "Service")
delivery.hsd = data.frame(TukeyHSD(delivery.mod1, which = "Service")$Service)
delivery.hsd$Comparison = row.names(delivery.hsd)
ggplot(delivery.hsd, aes(Comparison, y = diff, ymin = lwr, ymax = upr)) +
  geom_pointrange() + ylab("Difference in Mean Delivery Time by Service") +
  coord_flip()