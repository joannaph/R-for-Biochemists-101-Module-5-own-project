## Data imported allows for the deduction of the optimum temperature for beta-galatosidase, taking into account protein denaturation over time.
library(readxl)
library(ggplot2)
library(ggthemes)


## import data -- anomalous data removed when processed by lab supervisors
data <- read_excel(file.choose())
data <- as.data.frame(data)
colnames(data) <- c("Session", "Temp_C", "Reaction_1", "Control_1", "Reaction_2", "Control_2", "Reaction_5", "Control_5", "Reaction_10", "Control_10")

## separate data by temperature 
{
# 22˚C
data_22 <- subset(data, Temp_C == "22")
data_22$change_1min <- c(data_22$Reaction_1 - data_22$Control_1)
data_22$change_2min <- c(data_22$Reaction_2 - data_22$Control_2)
data_22$change_5min <- c(data_22$Reaction_5 - data_22$Control_5)
data_22$change_10min <- c(data_22$Reaction_10 - data_22$Control_10)

# 35˚C
data_35 <- subset(data, Temp_C == "35")
data_35$change_1min <- c(data_35$Reaction_1 - data_35$Control_1)
data_35$change_2min <- c(data_35$Reaction_2 - data_35$Control_2)
data_35$change_5min <- c(data_35$Reaction_5 - data_35$Control_5)
data_35$change_10min <- c(data_35$Reaction_10 - data_35$Control_10)

# 45˚C
data_45 <- subset(data, Temp_C == "45")
data_45$change_1min <- c(data_45$Reaction_1 - data_45$Control_1)
data_45$change_2min <- c(data_45$Reaction_2 - data_45$Control_2)
data_45$change_5min <- c(data_45$Reaction_5 - data_45$Control_5)
data_45$change_10min <- c(data_45$Reaction_10 - data_45$Control_10)

# 55˚C
data_55 <- subset(data, Temp_C == "55")
data_55$change_1min <- c(data_55$Reaction_1 - data_55$Control_1)
data_55$change_2min <- c(data_55$Reaction_2 - data_55$Control_2)
data_55$change_5min <- c(data_55$Reaction_5 - data_55$Control_5)
data_55$change_10min <- c(data_55$Reaction_10 - data_55$Control_10)

# 65˚C
data_65 <- subset(data, Temp_C == "65")
data_65$change_1min <- c(data_65$Reaction_1 - data_65$Control_1)
data_65$change_2min <- c(data_65$Reaction_2 - data_65$Control_2)
data_65$change_5min <- c(data_65$Reaction_5 - data_65$Control_5)
data_65$change_10min <- c(data_65$Reaction_10 - data_65$Control_10)

# 75˚C
data_75 <- subset(data, Temp_C == "75")
data_75$change_1min <- c(data_75$Reaction_1 - data_75$Control_1)
data_75$change_2min <- c(data_75$Reaction_2 - data_75$Control_2)
data_75$change_5min <- c(data_75$Reaction_5 - data_75$Control_5)
data_75$change_10min <- c(data_75$Reaction_10 - data_75$Control_10)


# make data.frame of useful data
useful_data_22 <- select(data_22, "change_1min", "change_2min", "change_5min", "change_10min")
useful_data_35 <- select(data_35, "change_1min", "change_2min", "change_5min", "change_10min")
useful_data_45 <- select(data_45, "change_1min", "change_2min", "change_5min", "change_10min")
useful_data_55 <- select(data_55, "change_1min", "change_2min", "change_5min", "change_10min")
useful_data_65 <- select(data_65, "change_1min", "change_2min", "change_5min", "change_10min")
useful_data_75 <- select(data_75, "change_1min", "change_2min", "change_5min", "change_10min")
}

## create data.frame of each temperature with time vs mean ∆abs
sem <- function(x) 
  return(sd(x) / sqrt(length(x)))

{
#22˚C
means = c(mean(useful_data_22$change_1min), mean(useful_data_22$change_2min), mean(useful_data_22$change_5min), mean(useful_data_22$change_10min))
stan_err_mean = c(sem(useful_data_22$change_1min), sem(useful_data_22$change_2min), sem(useful_data_22$change_5min), sem(useful_data_22$change_10min))

mean_data_22 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)

#35˚C
means = c(mean(useful_data_35$change_1min), mean(useful_data_35$change_2min), mean(useful_data_35$change_5min), mean(useful_data_35$change_10min))
stan_err_mean = c(sem(useful_data_35$change_1min), sem(useful_data_35$change_2min), sem(useful_data_35$change_5min), sem(useful_data_35$change_10min))

mean_data_35 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)

#45˚C
means = c(mean(useful_data_45$change_1min), mean(useful_data_45$change_2min), mean(useful_data_45$change_5min), mean(useful_data_45$change_10min))
stan_err_mean = c(sem(useful_data_45$change_1min), sem(useful_data_45$change_2min), sem(useful_data_45$change_5min), sem(useful_data_45$change_10min))

mean_data_45 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)

#55˚C
means = c(mean(useful_data_55$change_1min), mean(useful_data_55$change_2min), mean(useful_data_55$change_5min), mean(useful_data_55$change_10min))
stan_err_mean = c(sem(useful_data_55$change_1min), sem(useful_data_55$change_2min), sem(useful_data_55$change_5min), sem(useful_data_55$change_10min))

mean_data_55 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)

#65˚C
means = c(mean(useful_data_65$change_1min), mean(useful_data_65$change_2min), mean(useful_data_65$change_5min), mean(useful_data_65$change_10min))
stan_err_mean = c(sem(useful_data_65$change_1min), sem(useful_data_65$change_2min), sem(useful_data_65$change_5min), sem(useful_data_65$change_10min))

mean_data_65 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)

#75˚C 
means = c(mean(useful_data_75$change_1min), mean(useful_data_75$change_2min), mean(useful_data_75$change_5min), mean(useful_data_75$change_10min))
stan_err_mean = c(sem(useful_data_75$change_1min), sem(useful_data_75$change_2min), sem(useful_data_75$change_5min), sem(useful_data_75$change_10min))

mean_data_75 <- data.frame(
  time = c(1, 2, 5, 10),
  mean = means,
  SEM = stan_err_mean)
}

# combine all data frames into one data frame
{
mean_data_22$group <- "data_22"
mean_data_35$group <- "data_35"
mean_data_45$group <- "data_45"
mean_data_55$group <- "data_55"
mean_data_65$group <- "data_65"
mean_data_75$group <- "data_75"


combined_data <- rbind(mean_data_22, mean_data_35, mean_data_45, mean_data_55, mean_data_65, mean_data_75)
combined_data$time <- as.numeric(gsub("_min", "", combined_data$time))
} 
## plot Reaction Progress Chart, plot linear regression and SEM
# plot Reaction Progress Chart
plot <- ggplot(data = combined_data, aes(x = time, y = mean, colour = group)) +
  geom_point() +
  geom_errorbar(aes(ymin = mean - SEM, ymax = mean + SEM), width = 0.2) +
  stat_smooth(data = filter(combined_data, group != "data_65" & group != "data_75"),
              method = "lm",
              formula = y ~ 0 + x,
              se = FALSE) +
  xlab("Time (min)") + ylab("∆Abs 420nm") + ggtitle("Reaction Progress Chart",
                                                    subtitle = "beta-galactosidase")


# extract coefficients to give rate of reaction
{
coef_data_22 <- lm(mean ~ 0 + time, data = mean_data_22) 
coef_data_35 <- lm(mean ~ 0 + time, data = mean_data_35) 
coef_data_45 <- lm(mean ~ 0 + time, data = mean_data_45) 
coef_data_55 <- lm(mean ~ 0 + time, data = mean_data_55) 
coef_data_65 <- lm(mean ~ 0 + time, data = mean_data_65)
coef_data_75 <- lm(mean ~ 0 + time, data = mean_data_75) 
}
rate <- c(coef_data_22$coefficients[1], coef_data_35$coefficients[1], coef_data_45$coefficients[1], coef_data_55$coefficients[1])

# Extract coefficients and create equation labels
equations <- as.data.frame(rate)
equations$label <- paste0("y = ", round(rate, 6), "x")

# Add the label positions to the equations dataframe

y_position <- c(0.025, 0.038, 0.05, 0.045)
rp_chart <- plot + annotate("text", 
                            x = 10.7 , y = y_position,
                            label = equations$label)

rp_chart

## calculate Arrhenius parameters 
Temp_K <- c(22 + 273.15, 35 + 273.15, 45 + 273.15, 55 + 273.15)

# 0.0213 ml enzyme used -- from lab protocol
arrhenius <- as.data.frame(Temp_K)
arrhenius$rate_per_ml <- rate/0.0213
arrhenius$inverse_T <- 1/Temp_K
arrhenius$ln_V <- log(arrhenius$rate_per_ml)

## plot Arrhenius graph
arr_plot <- ggplot(data = arrhenius,
                   aes(x = inverse_T, y = ln_V)) +
            geom_point() +
            stat_smooth(data = arrhenius, 
                        method = "lm",
                        formula = y ~ x, 
                        se = FALSE) +
            xlab("1/T") + ylab("ln(V)") +
            ggtitle("Arrhenius Plot",
                    subtitle = "beta-galactosidase") +
          theme_bw()

arr_plot

## deduce Ea
#ln(k) = -(Ea/R)(1/T) + lnA <- gradient = -(Ea/R) ; y-intercept = lnA

#extract graph data
arr_plot_data <- lm(ln_V ~ inverse_T, data = arrhenius)
arr_slope <- arr_plot_data$coefficients[2]

# arr_slope = -(Ea/8.314)
Ea <- (-arr_slope * 8.314) / 1000

paste("Ea = ", Ea, "kJ/mol")
