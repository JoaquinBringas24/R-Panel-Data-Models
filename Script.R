rm(list = ls())

# Librerias que se van a utilizar

library(plm)
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(tidyr)
library(coefplot)
library(pastecs)
library(writexl)
library(GGally)
library(reshape2)
library(stargazer)

# Cargamos las bases de datos tal cual las obtuvimos de las bases de datos.

setwd("F:/R/proyectos/segundo parcial/Econometrics Project")

raw_guns <- read_excel("F:/R/database/Segundo parcial/Guns_panel.xlsx")
raw_crimes <- read.csv("F:/R/database/Segundo parcial/estimated_crimes_1979_2020.csv")
raw_politics <- read_excel("F:/R/database/Segundo parcial/Parties.xlsx")
raw_pib <- read.csv("F:/R/database/Segundo parcial/gross domestic product (gdp) by state (dollars).csv")
raw_pib <- raw_pib[1:26,]
raw_income <- read_excel("F:/R/database/Segundo parcial/Personal income per state 2.xlsx", range = "A6:Q186")

# Alabama estaba en formato string, pasamos a float.
raw_pib$Alabama <- as.numeric(raw_pib$Alabama) 

# Pasamos de wide a long los datos del pib.
pib_long<- pivot_longer(raw_pib,
                        cols = 2:52,
                        names_to = "State",
                        values_to = "PIB")

# Depuramos los nombres para poder hacer un merge; requerimos de exactamente los
#   nombres.
pib_long$State <- sub(pattern = "[.]", replacement = " ", x = pib_long$State)
pib_long$State <- sub(pattern = "[.]", replacement = " ", x = pib_long$State)

# La base de datos de crimen cuenta con espacios a la izquierda. Los quitamos para el merge.
raw_crimes$state_name <- str_trim(raw_crimes$state_name, "left")


# Reacomodamos la base de datos de income.
income_long<- pivot_longer(raw_income,
                        cols = 5:17,
                        names_to = "year",
                        values_to = "income")


income_wide <- pivot_wider(income_long,
                           id_cols = c( "year", "GeoName"),
                           names_from = LineCode,
                           values_from = income)

income_wide <- subset(income_wide, select = -4)

income_wide$GeoName <- sub(pattern = "Hawaii [*]", replacement = "Hawaii", x = income_wide$GeoName)
income_wide$GeoName <- sub(pattern = "Alaska [*]", replacement = "Alaska", x = income_wide$GeoName)

names(income_wide)[3:4] <- c("personal_income", "per_capita_personal_income") 

# Concatenamos todas las bases de datos para crear el df final.

merged <- merge(raw_crimes, raw_guns, by.x=c("year", "state_name"), by.y=c("year", "State"))

merged <- merge(merged, raw_politics, by.x=c("year", "state_name"), by.y=c("year", "State"))

merged <- merge(merged, pib_long, by.x=c("year", "state_name"), by.y=c("Years", "State"))

merged <- merge(merged, income_wide, by.x=c("year", "state_name"), by.y=c("year", "GeoName"))

# Eliminamos las columnas que no nos sirven

df <- subset(merged, select=-c(rape_revised, caveats, rape_legacy, PIB))

# Pasamos a factores las variables dummy para que no aparezcan el el corr

df$Governor <- as.factor(df$Governor); df$House <- as.factor(df$House)

df$Senate <- as.factor(df$Senate); df$year <- as.factor(df$year)

colnames(df) <- sub(pattern = " ", replacement = "_", x = colnames(df))
colnames(df) <- sub(pattern = " ", replacement = "_", x = colnames(df))

# Correlograma y exploracion grafica

df_melt <- melt(df)

kdensity <- ggplot(df_melt, aes(x = value)) +
stat_density() +
facet_wrap(~variable, scales = "free") + 
scale_x_continuous(n.breaks = 3)

ggsave(kdensity, filename="Kdensity.png", dpi=200, units="px", width=1980, height=1200)

df_log <- select_if(df, is.numeric)

df_log <- log(df_log)

df_log <- select(df_log, -c(Total, personal_income, population))

lowerFn <- function(data, mapping, method = "lm") {
	p <- ggplot(data = data, mapping = mapping) +
		geom_point(colour = "black", size = 0.01, alpha=0.5) +
		geom_smooth(method = method, colour = "red", size=0.2)
	p
}

Scatter_Matrix <- ggpairs(df_log,columns = c(1:15), 
                         title = "Scatter Plot Matrix in logs", 
                         axisLabels = "show",
				 upper = list(continuous = wrap("cor", size=1, colour="black")),
                         lower = list(continuous = wrap(lowerFn, method="lm"))) + 
                         theme(axis.line=element_blank(),
                               axis.text=element_blank(),
                               axis.ticks=element_blank(),
                               strip.text.x = element_text(size = 3, angle=90),
                               strip.text.y = element_text(size = 3, angle=20))

ggsave(Scatter_Matrix, filename="Scatter_Matrix.png", dpi=400, units="px", width=1980, height=1800)

riflesxhomicide <- ggplot(df, aes(x=log(Short_Barreled_Rifle), y=log(homicide))) +
  geom_point(aes(col=state_abbr), size=3, alpha=0.8) +
  theme(legend.position = "none")

ggsave(riflesxhomicide, filename="riflesxhomicide.png", dpi=200, units="px", width=1980, height=1200)

# Estadistica descriptiva.

stargazer(df, type="html", title="Descriptive Statistics",
digits=1, out="desc_sta.html", flip=FALSE)

stargazer(df_log, type="html", title="Descriptive Statistics in Log",
digits=1, out="desc_sta_log.html", flip=FALSE)

# Regresiones.

fe_regression <- plm(data = df, log(homicide) ~ 
                       log(Any_Other_Weapon) + log(Destructive_Device) + 
                       log(Machinegun) + log(Silencer) + log(Short_Barreled_Rifle) +
                       log(Short_Barreled_Shotgun) + 
                       log(per_capita_personal_income) + 
                       log(violent_crime) + log(robbery) + 
                       log(aggravated_assault) +
                       log(property_crime) + 
                       log(burglary), model = "within", index=c("state_abbr", "year"))

POLS_regression <- plm(data = df, log(homicide) ~ 
                       log(Any_Other_Weapon) + log(Destructive_Device) + 
                       log(Machinegun) + log(Silencer) + log(Short_Barreled_Rifle) +
                       log(Short_Barreled_Shotgun) + 
                       Governor*Senate*House + 
                       log(per_capita_personal_income) + 
                       log(violent_crime) + log(robbery) + 
                       log(aggravated_assault) +
                       log(property_crime) + 
                       log(burglary), model = "pooling", index=c("state_abbr", "year"))

RE_regression <- plm(data = df, log(homicide) ~ 
                         log(Any_Other_Weapon) + log(Destructive_Device) + 
                         log(Machinegun) + log(Silencer) + log(Short_Barreled_Rifle) +
                         log(Short_Barreled_Shotgun) + 
                         Governor*Senate*House + 
                         log(per_capita_personal_income) + 
                       log(violent_crime) + log(robbery) + 
                       log(aggravated_assault) +
                       log(property_crime) + 
                       log(burglary), model = "random", index=c("state_abbr", "year"))

stargazer(POLS_regression, type= "html", star.char = c("+", "*", "**", "***"), 
	star.cuttofs = c(0.1, 0.05, 0.01, 0.001), 
	notes = c("+ p<0.1; * p<0.05; **p<0.01; ***p<0.001"),
	out= "POLS_reg.html")

stargazer(fe_regression, type= "html", star.char = c("+", "*", "**", "***"), 
	star.cuttofs = c(0.1, 0.05, 0.01, 0.001), 
	notes = c("+ p<0.1; * p<0.05; **p<0.01; ***p<0.001"),
	out= "fe_reg.html")

stargazer(RE_regression, type= "html", star.char = c("+", "*", "**", "***"), 
	star.cuttofs = c(0.1, 0.05, 0.01, 0.001), 
	notes = c("+ p<0.1; * p<0.05; **p<0.01; ***p<0.001"),
	out= "RE_reg.html")

phtest(fe_regression, RE_regression)

fe_coefplot <- coefplot(fe_regression) +
  theme_bw() 

ggsave(fe_coefplot, filename="fe_coefplot.png", dpi=200, units="px", width=1980, height=1200)

#Residuales

fe_re <- resid(fe_regression)

png("resid_plot.png", height=800, width=800, units="px")

resid_plot <- plot(log(df$homicide), fe_re)
abline(0,0)

dev.off()

png("qqnorm_plot.png", height=800, width=800, units="px")

qqnorm_fe <- qqnorm(fe_re)
qqline(fe_re)

dev.off()

#Fitted Values

df_pred <- pdata.frame(df, index = c("state_abbr", "year"))

df_pred[4:12] <- 1

df_pred[14:28] <- 1

df_pred$fe_regression_pred <- predict(fe_regression, df_pred)

fitted_fe_plot <- ggplot(data=df_pred, aes(x=log(Any_Other_Weapon), y=fe_regression_pred, group=state_abbr)) +
  geom_line(aes(color=state_abbr)) +
  geom_point(aes(color=state_abbr, alpha=0.8)) +
  theme(legend.position = "none")

ggsave(fitted_fe_plot, filename="fitted_fe_plot.png", dpi=200, units="px", width=1980, height=1200)

#Analisis de vif 

df_log$year <- df$year

df_log$state_abbr <- df$state_abbr


vif_var_short <- c("Short_Barreled_Rifle", "+ violent_crime", "+ robbery", "+ aggravated_assault",
                      "+ property_crime", "+ burglary", "+ larceny", "+ motor_vehicle_theft",
                      "+ Silencer", "+ Destructive_Device", "+ Machinegun", "+ Short_Barreled_Shotgun", 
                      "+  per_capita_personal_income", "+ Any_Other_Weapon")


vifs <- seq(1, 14, 1)

for(i in 1:14) {             
  predictors_i <- vif_var_short[1:i] 
  vifs[i] <- 1 / (1- summary(plm(data= df_log, index=c("state_abbr", "year"), model = "within",
                                sprintf("homicide ~  %s ", (paste(predictors_i, collapse = " ")))))$r.squared[1])
}

regression_num <- seq(1, 14, 1)

df_vifs <- data.frame(vifs, regression_num)

vifs_rifle_plot <- ggplot(df_vifs, aes(x=regression_num, y=vifs)) + scale_x_continuous(breaks=regression_num) +
  scale_y_continuous(breaks=seq(1,2, 0.03)) +
  geom_line() + geom_point() + annotate("text", x=12.8, y=1.89, label= "Personal_Income") +
  annotate("text", x=2, y=1.54, label= "violent_crime") + annotate("text", x=3, y=1.55, label= "robbery") +
  annotate("text", x=4, y=1.56, label= "aggravated_assault") + annotate("text", x=5, y=1.61, label= "property_crime") +
  annotate("text", x=6, y=1.57, label= "burglary") + annotate("text", x=7, y=1.655, label= "larceny") +
  annotate("text", x=8, y=1.68, label= "motor_vehicle_theft") + annotate("text", x=9, y=1.66, label= "Silencer") +
  annotate("text", x=10, y=1.75, label= "Destructive_Device") + annotate("text", x=11, y=1.72, label= "Machinegun") +
  annotate("text", x=12, y=1.77, label= "Short_Barreled_Shotgun") + annotate("text", x=1.7, y=1.33, label= "Short_Barreled_Rifle") + 
  annotate("text", x=13.7, y=1.85, label= "Any_Other_Weapon")

ggsave(vifs_rifle_plot, filename="vifs_rifle_plot.png", dpi=200, units="px", width=1980, height=1400)

