# Load required libraries
library(ggplot2)
library(gridExtra)
library(psych)
library(nortest)
library(sjPlot)
library(corrplot)

# Read data from CSV
apples <- read.csv("10_appleq.csv", header = TRUE)

# Check type of data and remove NA rows
apples[which(is.na(apples)), ]
apples <- na.omit(apples)

# Structure of the data
str(apples)

# Change wrong data types
apples$Acidity <- as.numeric(apples$Acidity)
apples$Quality <- as.factor(apples$Quality)

# Split data into numeric and categorical
num <- which(sapply(apples, class) == "numeric")
apples_num <- apples[, num]

categ <- which(sapply(apples, class) == "factor")
apples_categ <- apples[, categ]

# Get separate quality apples
good_apples <- apples[apples$Quality == "good", ]
bad_apples <- apples[apples$Quality == "bad", ]

# See the distribution of quality
perc <- round(100 * prop.table(table(apples$Quality)), 1)
perc <- paste('(', perc, '%)', sep = '')
pielabels <- paste(levels(apples$Quality), perc)

pie(table(apples$Quality), labels = pielabels, border = 0, col = c("steelblue", "purple"), main = "Distribution of quality between apples")

# Plot functions
# BoxPlots of categorical and numeric
categ_num_bar <- function(data, numeric, categ, title, colors) {
  qplot(categ, numeric, data, geom = c("boxplot"), main = title, col = colors)
}

# Simple boxplot
simple_boxplot <- function(data, title, color) {
  boxplot(data, main = title, col = color)
}

# Density plot
density_plot <- function(data, var, list = NULL) {
  df <- data.frame(value = data[[var]])
  p <- ggplot(df, aes(x = value)) +
    geom_density(fill = "purple", alpha = 0.5) +
    labs(title = paste(var, "density plot"), y = "Density", x = var)
  if (is.null(list)) {
    list <- list()
  }
  list[[var]] <- p
  return(list)
}

# Two numeric scatterplot
num_scatter <- function(var1, var1_name, var2, var2_name) {
  var1 = var1
  var1.name = var1_name
  
  var2 = var2
  var2.name = var2_name
  
  cor(var1, var2)
  
  par(mfrow = c(1, 1))
  
  plot(var1, var2, main = paste(var1.name, " vs. ", var2.name), pch = 15, xlab = var1.name, ylab = var2.name)
  abline(lm(var2 ~ var1), col = "blue", lty = 2, lwd = 2)
}

# Summary for each variable
descr <- list()
for (i in 1:ncol(apples_num)) {
  descr[[colnames(apples_num)[i]]] <- describe(apples_num[, i])
}
descr

# QQ plots for numeric variables
par(mfrow = c(3, 3))
qqnorm(apples_num$Size, main = "QQ Plot of Size")
qqline(apples_num$Size)

qqnorm(apples_num$Weight, main = "QQ Plot of Weight")
qqline(apples_num$Weight)

qqnorm(apples_num$Sweetness, main = "QQ Plot of Sweetness")
qqline(apples_num$Sweetness)

qqnorm(apples_num$Crunchiness, main = "QQ Plot of Crunchiness")
qqline(apples_num$Crunchiness)

qqnorm(apples_num$Juiciness, main = "QQ Plot of Juiciness")
qqline(apples_num$Juiciness)

qqnorm(apples_num$Ripeness, main = "QQ Plot of Ripeness")
qqline(apples_num$Ripeness)

qqnorm(apples_num$Acidity, main = "QQ Plot of Acidity")
qqline(apples_num$Acidity)

# Density plots for all apples
plot_list <- list()
plot_list <- density_plot(apples, "Size", plot_list)
plot_list <- density_plot(apples, "Weight", plot_list)
plot_list <- density_plot(apples, "Juiciness", plot_list)
plot_list <- density_plot(apples, "Crunchiness", plot_list)
plot_list <- density_plot(apples, "Ripeness", plot_list)
plot_list <- density_plot(apples, "Sweetness", plot_list)
plot_list <- density_plot(apples, "Acidity", plot_list)

grid.arrange(grobs = plot_list, ncol = 3)

# Density plots for good apples
plot_list <- list()
plot_list <- density_plot(good_apples, "Size", plot_list)
plot_list <- density_plot(good_apples, "Weight", plot_list)
plot_list <- density_plot(good_apples, "Juiciness", plot_list)
plot_list <- density_plot(good_apples, "Crunchiness", plot_list)
plot_list <- density_plot(good_apples, "Ripeness", plot_list)
plot_list <- density_plot(good_apples, "Sweetness", plot_list)
plot_list <- density_plot(good_apples, "Acidity", plot_list)

grid.arrange(grobs = plot_list, ncol = 3)

# SW and KS tests function
sw_ks <- function(x) {
  shapiro_result <- shapiro.test(x)
  ks_result <- lillie.test(x)
  list(shapiro_p = shapiro_result$p.value, ks_p = ks_result$p.value)
}

results <- lapply(apples_num, sw_ks)

variables <- colnames(apples_num) # Get all variables
variables <- variables[variables != "A_id"] # Remove apple ID

all_combinations <- combn(variables, 2) # Get all combinations of 2 variables

# Calculate Pearson's correlation for each combination
cor_results <- data.frame(combination_name = character(), correlation_value = numeric(), stringsAsFactors = FALSE)
for (i in 1:ncol(all_combinations)) {
  r <- cor(apples_num[all_combinations[, i]])
  name <- paste(all_combinations[, i][1], all_combinations[, i][2], sep = "~")
  cor_results <- rbind(cor_results, data.frame(combination_name = name, correlation_value = r[1, 2]))
}

# Print results as a table
tab_df(cor_results[1:11, ], title = "Συσχέτιση μεταβλητών Ανά 2", file = NULL, col.header = c("Συνδυασμός", "Συντελεστής Συσχέτισης"))
tab_df(cor_results[12:21, ], title = "", file = NULL, col.header = c("Συνδυασμός", "Συντελεστής Συσχέτισης"))

correlation_matrix <- cor(apples_num)
corrplot(correlation_matrix, type = "upper", order = "hclust", tl.col = "black", tl.srt = 45)

# Boxplots of each numeric per quality category
boxplot(apples$Juiciness ~ apples$Quality, ylab = "Juiciness",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Juiciness per apple quality",
        names = c("bad", "good"))

boxplot(apples$Size ~ apples$Quality,
        ylab = "Size",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Size per apple quality",
        names = c("bad", "good"))

boxplot(apples$Weight ~ apples$Quality,
        ylab = "Weight",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Weight per apple quality",
        names = c("bad", "good"))

boxplot(apples$Crunchiness ~ apples$Quality,
        ylab = "Crunchiness",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Crunchiness per apple quality",
        names = c("bad", "good"))

boxplot(apples$Acidity ~ apples$Quality,
        ylab = "Acidity",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Acidity per apple quality",
        names = c("bad", "good"))

boxplot(apples$Sweetness ~ apples$Quality,
        ylab = "Sweetness",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Sweetness per apple quality",
        names = c("bad", "good"))

boxplot(apples$Ripeness ~ apples$Quality,
        ylab = "Ripeness",
        xlab = "Quality",
        col = c("red", "green"),
        main = "Ripeness per apple quality",
        names = c("bad", "good"))

# Test sweetness and quality
var.test(Sweetness ~ Quality, data = apples)
t.test(Sweetness ~ Quality, data = apples)

# Test size and quality
var.test(Size ~ Quality, data = apples)
t.test(Size ~ Quality, data = apples)
# Test weight and quality
var.test(Weight ~ Quality, data = apples)
t.test(Weight ~ Quality, data = apples)

# Test Acidity and quality


var.test(Acidity ~ Quality, data = apples)
t.test(Acidity ~ Quality, data = apples)
# Test Ripeness and quality

var.test(Ripeness ~ Quality, data = apples)
t.test(Ripeness ~ Quality, data = apples)
# Test Juiciness and quality

var.test(Juiciness ~ Quality, data = apples)
t.test(Juiciness ~ Quality, data = apples)
# Test Crunchiness and quality

var.test(Crunchiness ~ Quality, data = apples)
t.test(Crunchiness ~ Quality, data = apples)


#Simple linear models with one variable Sweetness~
model_size <- lm(Sweetness ~ Size, data = apples)
model_weight <- lm(Sweetness ~ Weight, data = apples)
model_ripeness <- lm(Sweetness ~ Ripeness, data = apples)
model_acidity <- lm(Sweetness ~ Acidity, data = apples)
model_crunchiness <- lm(Sweetness ~ Crunchiness, data = apples)
model_juiciness <- lm(Sweetness ~ Juiciness, data = apples)

par(mfrow = c(1, 3))


# Scatterplots for Sweetness vs Size, Weight, Ripeness
plot(apples$Sweetness ~ apples$Size, col = apples$Quality, pch = 19,
     xlab = "Size", ylab = "Sweetness", main = "Sweetness vs Size")
abline(model_size, col = "red")

plot(apples$Sweetness ~ apples$Weight, col = apples$Quality, pch = 19,
     xlab = "Weight", ylab = "Sweetness", main = "Sweetness vs Weight")
abline(model_weight, col = "red")

plot(apples$Sweetness ~ apples$Ripeness, col = apples$Quality, pch = 19,
     xlab = "Ripeness", ylab = "Sweetness", main = "Sweetness vs Ripeness")
abline(model_ripeness, col = "red")

mtext("Sweetness scatterplot vs Size, Weight and Ripeness", outer = TRUE, cex = 1.5, line = -1.5)


summary(model_size)
summary(model_weight)
summary(model_ripeness)
summary(model_acidity)
summary(model_crunchiness)
summary(model_juiciness)

tab_model(model_size)
tab_model(model_weight)
tab_model(model_ripeness)
tab_model(model_acidity)
tab_model(model_crunchiness)
tab_model(model_juiciness)

# Full model to estimate Sweetness
vars <- c("Sweetness", "Size", "Weight", "Acidity", "Ripeness", "Juiciness", "Crunchiness", "Quality")
df <- apples[, vars]
model_full <- lm(Sweetness ~ ., data = df)
m_step <- step(model_full, direction = "both", k = log(nrow(df))) #Stepwise with BIC

model <- m_step

#Test residual normality
res <- rstandard(model)
fit <- fitted(model)
shapiro.test(res)
lillie.test(res)

vars <- c("Sweetness", "Size", "Weight", "Acidity", "Ripeness", "Juiciness", "Crunchiness")
df_good <- good_apples[, vars]

#FUll model to estimate Sweetness using good apples only
model_full2 <- lm(Sweetness ~ ., data = df_good)
m_step2 <- step(model_full, direction = "both", k = log(nrow(df)))#Stepwise with BIC

model2 <- m_step2

#Test residual normality
res2 <- rstandard(model2)
fit2 <- fitted(model2)
shapiro.test(res2)
lillie.test(res2)
qqnorm(res2, main = "Residuals Q-Q plot model2")
