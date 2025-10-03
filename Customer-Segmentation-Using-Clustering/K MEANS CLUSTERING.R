# ========================
# 1. LOAD LIBRARIES
# ========================
install.packages(c("tidyverse", "lubridate", "cluster", "factoextra", "corrplot", "ggrepel"))
library(tidyverse)
library(lubridate)
library(cluster)
library(factoextra)
library(corrplot)
library(ggrepel)

# ========================
# 2. LOAD & CLEAN DATA
# ========================
marketing_data <- read_csv("marketing_campaign.csv")  # Update path if needed

# Remove missing income values and age outliers
marketing_data <- marketing_data %>%
  drop_na(Income) %>%
  mutate(Year_Birth = as.numeric(Year_Birth),
         Age = 2025 - Year_Birth) %>%
  filter(Age <= 100)

# Convert date
marketing_data$Dt_Customer <- dmy(marketing_data$Dt_Customer)

# Derived variables
marketing_data <- marketing_data %>%
  mutate(Customer_Since_Days = as.numeric(difftime(Sys.Date(), Dt_Customer, units = "days")),
         TotalChildren = Kidhome + Teenhome,
         TotalSpent = MntWines + MntFruits + MntMeatProducts + 
                      MntFishProducts + MntSweetProducts + MntGoldProds)

# ========================
# 3. FEATURE SELECTION FOR CLUSTERING (k = 2)
# ========================
cluster_data <- marketing_data %>%
  select(Income, Recency, MntWines, MntMeatProducts, 
         NumWebPurchases, NumCatalogPurchases, Customer_Since_Days)

# Scale the data
scaled_data <- scale(cluster_data)

# ========================
# 4. K-MEANS CLUSTERING (k = 2)
# ========================
set.seed(123)
kmeans_2 <- kmeans(scaled_data, centers = 2, nstart = 25)

# Silhouette Score
sil <- silhouette(kmeans_2$cluster, dist(scaled_data))
mean(sil[, 3])  # ~0.3013

# PCA-style plot
fviz_cluster(kmeans_2, data = scaled_data,
             geom = "point", ellipse.type = "norm",
             palette = "jco", ggtheme = theme_minimal())

# ========================
# 5. CLUSTER INTERPRETATION USING 2 VARIABLES
# ========================
# Cluster plot for MntWines vs NumWebPurchases
plot_data <- marketing_data %>%
  mutate(cluster = kmeans_2$cluster,
         ID = row_number())

ggplot(plot_data, aes(x = MntWines, y = NumWebPurchases,
                      color = factor(cluster), label = ID)) +
  geom_point(alpha = 0.6) +
  geom_text_repel(size = 2.5, max.overlaps = 20) +
  labs(title = "Cluster Plot: Wine Spending vs Web Purchases (k = 2)",
       x = "Wine Spending", y = "Web Purchases", color = "Cluster") +
  theme_minimal()

# ========================
# 6. BUYER PERSONAS
# ========================
# Use insights from means to write descriptive profiles
# Cluster 1: Olivia Sebastian (low engagement, low spend)
# Cluster 2: Ethan Jones (high engagement, high spend)

# ========================
# 7. EVALUATE k = 3, 4, 5 CLUSTERS
# ========================
fviz_cluster(kmeans_2, data = scaled_data, geom = "point", main = "k = 2")

for (k in 3:5) {
  set.seed(123)
  km_temp <- kmeans(scaled_data, centers = k, nstart = 25)
  print(paste("k =", k, "→ Between SS % =", round(km_temp$betweenss / km_temp$totss * 100, 2)))
  fviz_cluster(km_temp, data = scaled_data, geom = "point", main = paste("k =", k))
}

# ========================
# 8. DETERMINE OPTIMAL K (ELBOW METHOD)
# ========================
fviz_nbclust(scaled_data, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2, color = "red") +
  labs(title = "Elbow Method to Determine Optimal k",
       x = "Number of Clusters", y = "Within-Cluster Sum of Squares")

# ========================
# 9. OPTIONAL - CORRELATION HEATMAP
# ========================
corrplot(cor(cluster_data), method = "circle", type = "upper")
