#FCE LTER Dissolved Organic Matter 
# John Kominoski
# June 7, 2026

library(rstudioapi)
versionInfo()$citation

getwd()
library(tidyverse)
library(tidyr)
library(dplyr)
library(patchwork)
library(scales)
library(ggplot2)
library(gridExtra)
library(stats)
library(trend)
library(purrr)
library(lme4)
library(mgcv)
library(DescTools)
library(marginaleffects)
library(data.table)
library(zoo)
library(gratia)

dom <- read.csv("FCE_DOM_Compiled_060726.csv")
dom <-na.omit(dom)
View(dom)
dom$Site_ID=as.factor(dom$Site_ID)
dom$Ecosystem=as.factor(dom$Ecosystem)
dom$WaterLevel=as.numeric(dom$WaterLevel)
dom$Year=as.factor(dom$Year)
library(lubridate)
dom$Date <- ymd(dom$Date)# Use mdy() if Month-Day-Year, or dmy() if Day-Month-Year
dom$Date <- as.Date(dom$Date)

level<-read.csv("monthly_fce_level.csv")
level <-na.omit(level)
View(level)
level_mean<-tapply(level$WaterLevel, level$Site_ID, mean)
level_sd<-tapply(level$WaterLevel, level$Site_ID, sd)

# 1. Install and load required libraries
install.packages(c("ggplot2", "ggfortify"))
library(ggplot2)
library(ggfortify)

# 2. Perform PCA (only on the numeric columns, scaling is highly recommended)
pca_result <- prcomp(dom[, 10:23], scale. = TRUE)

# 3. Plot automatically using autoplot
autoplot(pca_result, data = dom, size = 3, shape = 'Ecosystem', colour = 'Ecosystem', frame = TRUE, frame.type = 'norm',
  loadings = TRUE, loadings.label.repel = TRUE,
  loadings.colour = 'blue', loadings.label.colour = 'blue', label.fontface = 'bold') +
  #scale_shape_manual(values = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) +
  scale_shape_manual(values = c(21, 19, 15, 22, 17)) +
  #scale_color_manual(values = c("springgreen", "springgreen2", "springgreen4", "springgreen3", "seagreen1"))+
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  theme_minimal()+
  theme(text = element_text(face = "bold"))

#Figure  Boxplots showing the median, upper and lower quartiles of water level from 2012 to 2025.
ggplot(dom, aes(x = factor(Ecosystem), y = WaterLevel), group = Year, colour = Year) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  xlab("") +
  ylab("Water Level (cm)") +
  theme_bw() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

ggplot(dom, aes(x = factor(Year), y = WaterLevel), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(fill = "deepskyblue", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  xlab("") +
  ylab("Water Level (cm)") +
  theme_bw() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )


#Figure 9. "Scatterplot and spatial (loess) trends in hydroperiod among Near-Canal Transect locations downstream of
#the L-29 Canal in Northeast Shark River Slough across Water Years (lines) and surface water TOC and surface water 
#TP concentrations along hydroperiods across transects from East (T1-T4) and West (T5-T8) locations.

a<- ggplot(dom, aes(x=Date, y=DOC_mg_L))+
  geom_point()+
  theme_classic()+
  facet_wrap(~ Site_ID)+
  xlab("Date")+
  ylab(expression("DOC" ~ (mg ~ L^-1)))+
  ylim(0,30)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

b<- ggplot(dom, aes(x=Date, y=WaterLevel))+
  geom_point()+
  theme_classic()+
  facet_wrap(~ Site_ID)+
  xlab("Date")+
  ylab("Water Level (cm)")+
  ylim(-100,300)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

c<- ggplot(data=arrange(dom, DOC), aes(x=HIX, y=BIX))+
  geom_point(aes(shape = Ecosystem, color = DOC), size = 3, alpha = 1) +
  scale_shape_manual(values = c(21, 19, 15, 22, 17)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_minimal() +
  theme(legend.position = "top")+
  #facet_wrap("Ecosystem")+
  theme_classic()+
  xlab("HIX")+
  ylab("BIX")+
  ylim(0.5,1.2)+
  xlim(0,25)+
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

####DOM Components vs Ecosystem
a<- ggplot(dom, aes(x = factor(Ecosystem), y = C1), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C1") +
  ylim(0,50)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

b<- ggplot(dom, aes(x = factor(Ecosystem), y = C2), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C2") +
  ylim(0,30)+
  theme_bw() +
  theme(legend.position = c(0.13, 0.3))+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

c<- ggplot(dom, aes(x = factor(Ecosystem), y = C3), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C3") +
  ylim(0,20)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

d<- ggplot(dom, aes(x = factor(Ecosystem), y = C4), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C4") +
  ylim(0,20)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

e<- ggplot(dom, aes(x = factor(Ecosystem), y = C5), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C5") +
  ylim(0,20)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

f<- ggplot(dom, aes(x = factor(Ecosystem), y = C6), group = Ecosystem, colour = Year) +
  geom_boxplot(fill = "tan", color = "black", outlier.shape = NA, outlier.size = 2) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Year)) +
  scale_color_gradient(low = "lightblue", high = "darkblue")+
  theme_classic()+
  xlab("") +
  ylab("% C6") +
  ylim(0,20)+
  theme_bw() +
  theme(legend.position = "none")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  theme(
    plot.title = element_text(hjust = 0.5, size = 14),
    axis.title.y = element_text(vjust = 3),
    axis.title.x = element_text(vjust = -1),
    axis.text = element_text(size = 10)
  )

grid.arrange(a,b,c,d,e,f,nrow=2,ncol=3)



####DOM Components vs Year
df_dom.long <- dom %>% 
  select("Year", "Ecosystem", "C1","C2", "C3", "C4", "C5", "C6") %>% 
  pivot_longer(c(-Ecosystem, -Year), names_to = "variable", values_to = "value")
head(df_dom.long)
View(df_dom.long)

a<- ggplot(df_dom.long, aes(factor(Year), value, group = variable, color=variable)) + 
  geom_point(alpha = 0.3) +
  #geom_line(linewidth=1)+
  stat_summary(aes(shape = Ecosystem),fun = max, geom = "point", size = 3) +
  scale_shape_manual(values = c(21, 19, 15, 22, 17)) +
  stat_summary(fun = max, geom = "line")+
  scale_color_manual(values =c("springgreen4", "springgreen3","springgreen2","springgreen1","turquoise","turquoise2"))+
  facet_wrap(~ Ecosystem)+
  xlab("Year")+
  ylab("% C1-C6")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

a<- ggplot(dom, aes(x = factor(Year), y = C1), group = Ecosystem, colour = Ecosystem) +  
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  xlab("")+
  ylab("% C1")+
  ylim(0,50)+
  theme(legend.position = "none")


b<- ggplot(dom, aes(x = factor(Year), y = C2), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  xlab("") +
  ylab("% C2") +
  ylim(10,30)+
  theme(legend.position = c(0.25, 0.3))+
  theme(legend.background = element_rect(fill = "transparent", color = NA))

c<- ggplot(dom, aes(x = factor(Year), y = C3), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+xlab("") +
  ylab("% C3") +
  ylim(0,20)+
  theme(legend.position = "none")

d<- ggplot(dom, aes(x = factor(Year), y = C4), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  xlab("Year") +
  ylab("% C4") +
  ylim(0,20)+
  theme(legend.position = "none")

e<- ggplot(dom, aes(x = factor(Year), y = C5), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  xlab("Year") +
  ylab("% C5") +
  ylim(0,20)+
  theme(legend.position = "none")

f<- ggplot(dom, aes(x = factor(Year), y = C6), group = Ecosystem, colour = Ecosystem) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.2, size = 2, alpha = 0.8, aes(color = Ecosystem)) +
  scale_color_manual(values =c("springgreen", "springgreen3","springgreen4","seagreen3","turquoise"))+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))+
  xlab("Year") +
  ylab("% C6") +
  ylim(0,20)+
  theme(legend.position = "none")

grid.arrange(a,b,c,d,e,f,nrow=2,ncol=3)

####DOM Components vs Water Level
a<-plot(dom$WaterLevel,dom$C1)
b<-plot(dom$WaterLevel,dom$C2)
c<-plot(dom$WaterLevel,dom$C3)
d<-plot(dom$WaterLevel,dom$C4)
e<-plot(dom$WaterLevel,dom$C5)
f<-plot(dom$WaterLevel,dom$C6)

a<-plot(dom$WaterLevel,dom$DOC)
b<-plot(dom$WaterLevel,dom$SUVA254)
c<-plot(dom$WaterLevel,dom$SR)
d<-plot(dom$WaterLevel,dom$ES_E3)
e<-plot(dom$WaterLevel,dom$slp350_400)
f<-plot(dom$WaterLevel,dom$slp274_295)
g<-plot(dom$WaterLevel,dom$HIX)
h<-plot(dom$WaterLevel,dom$BIX)
i<-plot(dom$WaterLevel,dom$FI)


####DATA ANALYSIS
##DOC
qqnorm(dom$DOC)
gam_formula <- DOC ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + Date
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$DOC_lag1 <- c(NA, head(dom$DOC, -1))
lag_gam_formula <- DOC ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + DOC_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(DOC ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
a<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "DOC",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")


##HIX
qqnorm(dom$HIX)
#p1<- ggplot(data = dom, aes(x = WaterLevel, y = HIX))+
#p1
gam_formula <- HIX ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year 
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
#q1<- p1 + stat_smooth(method = "gam", formula = y ~ s(x, k = 12), linewidth = 1.5, color="indianred1") + theme(axis.text.x = element_blank()) 
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)
library(gratia)
draw(gam_model)

##Lag 1 Model
dom$HIX_lag1 <- c(NA, head(dom$HIX, -1))
lag_gam_formula <- HIX ~ s(as.numeric(WaterLevel), by=Ecosystem) + Year + HIX_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(HIX ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
  data = dom, method = "REML", random = list(Ecosystem = ~1), 
  correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
b<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "HIX",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##BIX
qqnorm(dom$BIX)
gam_formula <- BIX ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem 
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML",
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$BIX_lag1 <- c(NA, head(dom$BIX, -1))
lag_gam_formula <- BIX ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + BIX_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(BIX ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
c<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "BIX",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##FI
qqnorm(dom$FI)
gam_formula <- FI ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$FI_lag1 <- c(NA, head(dom$FI, -1))
lag_gam_formula <- FI ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + FI_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)


##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(FI ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
d<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "FI",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##SUVA254
qqnorm(dom$SUVA254)
gam_formula <- SUVA254 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = quasi(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

plot_predictions(gam_model, condition = "WaterLevel", points =.5)+
  theme_classic(base_size = 12)+
  xlab("Water Level (cm)")

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(SUVA254 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
e<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "SUVA254",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")


##E2:E3
qqnorm(dom$E2_E3)
gam_formula <- E2_E3 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = quasi(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$E2_E3_lag1 <- c(NA, head(dom$E2_E3, -1))
lag_gam_formula <- E2_E3 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + E2_E3_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(E2_E3 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
f<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "E2:E3",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##SR
qqnorm(dom$SR)
gam_formula <- SR ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem 
gam_model <- gam(
  gam_formula,
  data = dom,
  family = quasi(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$SR_lag1 <- c(NA, head(dom$SR, -1))
lag_gam_formula <- SR ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + SR_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(SR ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
g<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "SR",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")


##C1
qqnorm(dom$C1)
gam_formula <- C1 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C1_lag1 <- c(NA, head(dom$C1, -1))
lag_gam_formula <- C1 ~s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C1_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C1 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
h<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C1",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##C2
qqnorm(dom$C2)
gam_formula <- C2 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C2_lag1 <- c(NA, head(dom$C2, -1))
lag_gam_formula <- C2 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C2_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C2 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
i<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C2",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##C3
qqnorm(dom$C3)
gam_formula <- C3 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C3_lag1 <- c(NA, head(dom$C3, -1))
lag_gam_formula <- C3 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C3_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C3 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
j<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C3",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##C4
qqnorm(dom$C4)
gam_formula <- C4 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C4_lag1 <- c(NA, head(dom$C4, -1))
lag_gam_formula <- C4 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C4_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C4 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
k<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C4",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##C5
qqnorm(dom$C5)
gam_formula <- C5 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = quasi(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C5_lag1 <- c(NA, head(dom$C5, -1))
lag_gam_formula <- C5 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C5_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C5 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
l<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C5",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

##C6
qqnorm(dom$C6)
gam_formula <- C6 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem
gam_model <- gam(
  gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(gam_model)
gam.check(gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(gam_model)
p1<-plot(gam_model, scheme = 2, pages = 1, shade = TRUE)

##Lag 1 Model
dom$C6_lag1 <- c(NA, head(dom$C6, -1))
lag_gam_formula <- C6 ~ s(as.numeric(WaterLevel)) +s(as.numeric(Year)) + Ecosystem + C6_lag1
lag_gam_model <- gam(
  lag_gam_formula,
  data = dom,
  family = gaussian(),
  method = "REML"
)
summary(lag_gam_model)
gam.check(lag_gam_model, pch = 19, cex = 0.3)
DurbinWatsonTest(lag_gam_model)
p1<-plot(lag_gam_model, scheme = 2, pages = 1, shade = TRUE)
#p1<-plot_predictions(lag_gam_model, condition = "Year", 
#                    points = 0.5)
#p1<-plot_predictions(lag_gam_model, condition = "WaterLevel", 
#                     points = 0.5)

##GAMM - Generalized Additive Mixed Model
gamm_model <- gamm(C6 ~ s(as.numeric(WaterLevel), by = Ecosystem) + Year,
                   data = dom, method = "REML", random = list(Ecosystem = ~1), 
                   correlation = corAR1(form = ~ Date()))

summary(gamm_model$lme)
acf(residuals(gamm_model$lme, type = "normalized"))
pacf(residuals(gamm_model$lme, type = "normalized"))
summary(gamm_model$gam)
gam.check(gamm_model$gam, rep = 100)

# Generate conditional predictions and draw in one step
new_data <- with(dom, expand.grid(
  WaterLevel = seq(min(WaterLevel), max(WaterLevel), length.out = 100),
  Ecosystem = levels(Ecosystem),
  Year = levels(Year)
))

# Calculate predictions (on the scale of the response)
# Include the standard error (se.fit = TRUE)
preds <- predict(gamm_model$gam, newdata = new_data, type = "response", se.fit = TRUE)
new_data$fit <- preds$fit
new_data$se.fit <- preds$se.fit

# Plot the overlaid smooths with ggplot2
m<-ggplot(new_data, aes(x = WaterLevel, y = fit, color = Ecosystem, fill = Ecosystem)) +
  # Add confidence intervals
  geom_ribbon(aes(ymin = fit - 2*se.fit, ymax = fit + 2*se.fit), 
              alpha = 0.2, color = NA) +
  scale_color_manual(values =c("lightgreen", "green2","brown","brown2","turquoise2"))+
  # Add the smooth lines
  geom_line(aes(linetype = Ecosystem), linewidth = 1.5) +
  scale_linetype_manual(values = c("solid", "solid", "solid", "solid", "solid"))+
  labs(
    x = "Water Level (cm)",
    y = "% C6",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal()+
  theme(legend.position = "none")

grid.arrange(a,b,c,d,e,f,g,h,i,j,k,l,m, nrow=4,ncol=4)
#------------------------------------------------------------------------






