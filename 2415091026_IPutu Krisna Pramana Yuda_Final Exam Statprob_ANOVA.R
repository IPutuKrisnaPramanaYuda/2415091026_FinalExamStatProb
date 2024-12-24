
install.packages(c("readxl", "ggplot2", "car", "dplyr", "lmtest"))
library(readxl)
library(ggplot2)
library(car)
library(dplyr)
library(lmtest)


file_path <- "C:/Users/KRISNA/Documents/Product_data.xlsx"
data <- read_excel(file_path)
str(data)
data$product_type <- as.factor(data$product_type)

#Asumption Tets
#1. (Shapiro-Wilk Test) Normality test
normality_test <- data %>%
  group_by(product_type) %>%
  summarise(p_value = shapiro.test(score)$p.value)
print("Hasil Uji Normalitas:")
print(normality_test)

#2. (Levene's Test)
levene_test <- leveneTest(score ~ product_type, data = data)
print("Hasil Uji Homogenitas Varians:")
print(levene_test)

#3. (Scatter plot + Regression Line)
ggplot(data, aes(x = as.numeric(product_type), y = score)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Uji Linearitas", x = "Jenis Produk", y = "Skor") +
  theme_minimal()

#Analys - Perform ANOVA
anova_result <- aov(score ~ product_type, data = data)
print("Hasil Analisis ANOVA:")
summary(anova_result)

#Visualisation
ggplot(data, aes(x = product_type, y = score, fill = product_type)) +
  geom_boxplot() +
  labs(title = "Perbandingan Skor Berdasarkan Jenis Produk", 
       x = "Jenis Produk", y = "Skor") +
  theme_minimal()

# Interpretation of Normality Test
print("Interpretasi Hasil Uji Normalitas:")
normality_interpretation <- normality_test %>%
  mutate(interpretation = ifelse(p_value > 0.05, "Data normal", "Data tidak normal"))
print(normality_interpretation)

# Interpretation of Levene's Test (Homogeneity of Variances)
print("Interpretasi Hasil Uji Homogenitas Varians:")
if (levene_test$p.value > 0.05) {
  print("Varians antar grup homogen (tidak ada perbedaan signifikan dalam varians).")
} else {
  print("Varians antar grup tidak homogen (ada perbedaan signifikan dalam varians).")
}

# Interpretation of ANOVA
print("Interpretasi Hasil ANOVA:")
anova_summary <- summary(anova_result)
if (anova_summary[[1]]$`Pr(>F)`[1] < 0.05) {
  print("Ada perbedaan signifikan antara grup berdasarkan jenis produk.")
} else {
  print("Tidak ada perbedaan signifikan antara grup berdasarkan jenis produk.")
}

# Interpretation of Tukey HSD Post-Hoc Test
posthoc <- TukeyHSD(anova_result)
print("Hasil Tukey HSD:")
print(posthoc)
