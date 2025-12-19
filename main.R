# ==============================================================================
# DATA WAREHOUSE AND DATA MINING PROJECT
# Retail Sales Analysis System
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║          DATA WAREHOUSE AND DATA MINING SYSTEM                 ║\n")
cat("║                  Retail Sales Analysis                         ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

library(stats)
library(graphics)
library(utils)

# ==============================================================================
# PART 1: DATA WAREHOUSE CREATION
# ==============================================================================

cat("═══ PHASE 1: DATA WAREHOUSE CREATION ═══\n\n")

set.seed(2024)

n_clients <- 300
n_products <- 50
n_stores <- 5
n_transactions <- 2000

cat("→ Generating source data...\n")

# DIMENSION TABLE: CLIENTS
dim_clients <- data.frame(
  id_client = 1:n_clients,
  name = paste("Client", sprintf("%04d", 1:n_clients)),
  age = pmax(18, pmin(75, round(rnorm(n_clients, 40, 15)))),
  gender = sample(c("M", "F"), n_clients, replace = TRUE),
  city = sample(c("Alger", "Oran", "Constantine", "Annaba", "Blida"), 
                 n_clients, replace = TRUE),
  segment = sample(c("Premium", "Standard", "Economy"), 
                   n_clients, replace = TRUE, prob = c(0.15, 0.55, 0.3)),
  stringsAsFactors = FALSE
)

# DIMENSION TABLE: PRODUCTS
categories <- c("Electronics", "Clothing", "Food", "Home", "Sports")

dim_products <- data.frame(
  id_product = 1:n_products,
  product_name = paste("Product", sprintf("%03d", 1:n_products)),
  category = sample(categories, n_products, replace = TRUE),
  unit_price = round(runif(n_products, 1000, 30000), 2),
  production_cost = round(runif(n_products, 500, 20000), 2),
  stringsAsFactors = FALSE
)

dim_products$margin <- dim_products$unit_price - dim_products$production_cost
dim_products$margin_rate <- round((dim_products$margin / dim_products$unit_price) * 100, 2)

# DIMENSION TABLE: STORES
dim_stores <- data.frame(
  id_store = 1:n_stores,
  store_name = paste("Store", c("Center", "North", "South", "East", "West")),
  city = c("Alger", "Oran", "Constantine", "Annaba", "Blida"),
  surface_m2 = c(500, 800, 600, 750, 550),
  stringsAsFactors = FALSE
)

# DIMENSION TABLE: TIME
dates <- seq.Date(as.Date("2023-01-01"), as.Date("2024-12-31"), by = "day")

dim_time <- data.frame(
  id_date = 1:length(dates),
  date = dates,
  year = as.integer(format(dates, "%Y")),
  month = as.integer(format(dates, "%m")),
  month_name = format(dates, "%B"),
  day = as.integer(format(dates, "%d")),
  weekday = weekdays(dates),
  stringsAsFactors = FALSE
)

# FACT TABLE: SALES
fact_sales <- data.frame(
  id_sale = 1:n_transactions,
  id_client = sample(1:n_clients, n_transactions, replace = TRUE),
  id_product = sample(1:n_products, n_transactions, replace = TRUE),
  id_date = sample(1:nrow(dim_time), n_transactions, replace = TRUE),
  id_store = sample(1:n_stores, n_transactions, replace = TRUE),
  quantity = sample(1:5, n_transactions, replace = TRUE, prob = c(0.4, 0.3, 0.2, 0.07, 0.03)),
  discount_percent = sample(c(0, 5, 10, 15, 20), n_transactions, 
                           replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)),
  stringsAsFactors = FALSE
)

# DATA WAREHOUSE CONSOLIDATION
cat("→ Consolidating data warehouse...\n")

warehouse <- merge(fact_sales, dim_clients, by = "id_client")
warehouse <- merge(warehouse, dim_products, by = "id_product")
warehouse <- merge(warehouse, dim_time, by = "id_date")
warehouse <- merge(warehouse, dim_stores, by = "id_store")

warehouse$price_before_discount <- warehouse$quantity * warehouse$unit_price
warehouse$discount_amount <- warehouse$price_before_discount * (warehouse$discount_percent / 100)
warehouse$total_amount <- warehouse$price_before_discount - warehouse$discount_amount
warehouse$total_cost <- warehouse$quantity * warehouse$production_cost
warehouse$net_profit <- warehouse$total_amount - warehouse$total_cost

cat("\n✓ Data warehouse created successfully!\n")
cat("  • Transactions:", nrow(warehouse), "\n")
cat("  • Unique clients:", length(unique(warehouse$id_client)), "\n")
cat("  • Products sold:", length(unique(warehouse$id_product)), "\n\n")

# ==============================================================================
# PART 2: OLAP ANALYSIS
# ==============================================================================

cat("═══ PHASE 2: OLAP ANALYSIS ═══\n\n")

# ANALYSIS BY CATEGORY
sales_category <- aggregate(total_amount ~ category, data = warehouse, FUN = sum)
sales_category <- sales_category[order(-sales_category$total_amount), ]
sales_category$nb_transactions <- aggregate(id_sale ~ category, data = warehouse, FUN = length)$id_sale

cat("┌─── Sales by Category ───┐\n")
print(sales_category)

# TEMPORAL ANALYSIS
sales_month <- aggregate(total_amount ~ year + month + month_name, data = warehouse, FUN = sum)
sales_month <- sales_month[order(sales_month$year, sales_month$month), ]

cat("\n┌─── Monthly Sales (Last 12 months) ───┐\n")
print(tail(sales_month, 12))

# STORE PERFORMANCE
perf_stores <- aggregate(cbind(total_amount, net_profit) ~ id_store + store_name + surface_m2, 
                          data = warehouse, FUN = sum)
perf_stores$nb_transactions <- aggregate(id_sale ~ id_store, data = warehouse, FUN = length)$id_sale
perf_stores$sales_per_m2 <- round(perf_stores$total_amount / perf_stores$surface_m2, 2)
perf_stores <- perf_stores[order(-perf_stores$total_amount), ]

cat("\n┌─── Store Performance ───┐\n")
print(perf_stores)

# RFM ANALYSIS (Recency, Frequency, Monetary)
date_ref <- max(warehouse$date)

rfm_data <- aggregate(cbind(date, id_sale, total_amount) ~ id_client + name + segment, 
                     data = warehouse, 
                     FUN = function(x) if(is.numeric(x)) c(max(x), length(x), sum(x)) else max(x))

rfm_analysis <- data.frame(
  id_client = rfm_data$id_client,
  name = rfm_data$name,
  segment = rfm_data$segment,
  recency = as.numeric(date_ref - as.Date(rfm_data$date[,1], origin = "1970-01-01")),
  frequency = rfm_data$id_sale[,2],
  monetary = rfm_data$total_amount[,3]
)

rfm_analysis <- rfm_analysis[order(-rfm_analysis$monetary), ]

cat("\n┌─── TOP 10 Clients (RFM Analysis) ───┐\n")
print(head(rfm_analysis, 10))

# ==============================================================================
# PART 3: VISUALIZATIONS
# ==============================================================================

cat("\n═══ PHASE 3: VISUALIZATIONS ═══\n\n")

par(mfrow = c(2, 2), mar = c(5, 4, 4, 2))

# CHART 1: SALES BY CATEGORY
barplot(sales_category$total_amount / 1000000,
        names.arg = sales_category$category,
        col = rainbow(nrow(sales_category)),
        main = "Sales by Category",
        ylab = "Sales (Millions DA)",
        las = 2,
        border = "white")
grid()

# CHART 2: TEMPORAL EVOLUTION
plot(1:nrow(sales_month), sales_month$total_amount / 1000000,
     type = "b",
     col = "blue",
     lwd = 2,
     pch = 19,
     main = "Monthly Sales Evolution",
     xlab = "Month",
     ylab = "Sales (Millions DA)",
     xaxt = "n")
axis(1, at = seq(1, nrow(sales_month), by = 3), 
     labels = sales_month$month_name[seq(1, nrow(sales_month), by = 3)], las = 2)
grid()

# CHART 3: STORE PERFORMANCE
barplot(perf_stores$total_amount / 1000000,
        names.arg = perf_stores$store_name,
        col = c("#E74C3C", "#3498DB", "#2ECC71", "#F39C12", "#9B59B6"),
        main = "Store Performance",
        ylab = "Sales (Millions DA)",
        las = 2,
        border = "white")
grid()

# CHART 4: SEGMENT DISTRIBUTION
segments_count <- table(warehouse$segment)
pie(segments_count,
    col = c("#FF6B6B", "#4ECDC4", "#45B7D1"),
    main = "Client Segment Distribution",
    labels = paste(names(segments_count), "\n", segments_count))

par(mfrow = c(1, 1))

cat("✓ Charts generated\n")

# ==============================================================================
# PART 4: DATA MINING
# ==============================================================================

cat("\n═══ PHASE 4: DATA MINING ═══\n\n")

# PRODUCT ASSOCIATIONS
cat("→ Product association analysis...\n")

top_products <- aggregate(total_amount ~ category + product_name, data = warehouse, FUN = sum)
top_products <- top_products[order(top_products$category, -top_products$total_amount), ]

cat("\n┌─── Top 3 Products by Category ───┐\n")
for(cat in unique(top_products$category)) {
  cat("\n", cat, ":\n")
  subset_cat <- head(top_products[top_products$category == cat, ], 3)
  print(subset_cat)
}

# K-MEANS CLUSTERING
cat("\n→ K-means clustering segmentation...\n")

client_profile <- aggregate(cbind(total_amount, id_sale, quantity) ~ id_client + age + segment,
                           data = warehouse,
                           FUN = function(x) c(sum(x), length(unique(x)), mean(x)))

clustering_data <- data.frame(
  age = client_profile$age,
  total_purchases = client_profile$total_amount[,1],
  nb_transactions = client_profile$id_sale[,2],
  avg_basket = client_profile$total_amount[,3]
)

norm_data <- scale(clustering_data)

set.seed(2024)
kmeans_result <- kmeans(norm_data, centers = 3, nstart = 25)

client_profile$cluster <- kmeans_result$cluster

cat("\n┌─── Cluster Characteristics ───┐\n")
for(i in 1:3) {
  cat("\nCluster", i, ":\n")
  cluster_data <- clustering_data[kmeans_result$cluster == i, ]
  cat("  • Number of clients:", nrow(cluster_data), "\n")
  cat("  • Average age:", round(mean(cluster_data$age), 1), "years\n")
  cat("  • Average purchases:", round(mean(cluster_data$total_purchases), 0), "DA\n")
  cat("  • Average transactions:", round(mean(cluster_data$nb_transactions), 1), "\n")
}

plot(norm_data[, c(2, 3)],
     col = kmeans_result$cluster + 1,
     pch = 19,
     main = "Client Segmentation (K-means)",
     xlab = "Total Purchases (normalized)",
     ylab = "Nb Transactions (normalized)")
points(kmeans_result$centers[, c(2, 3)], col = 2:4, pch = 8, cex = 2, lwd = 2)
legend("topright", legend = paste("Cluster", 1:3), col = 2:4, pch = 19)
grid()

# ANOMALY DETECTION
cat("\n→ Anomaly detection...\n")

warehouse$z_score <- scale(warehouse$total_amount)[,1]
anomalies <- warehouse[abs(warehouse$z_score) > 3, ]

cat("\n┌─── Anomalous Transactions ───┐\n")
cat("Number of anomalies:", nrow(anomalies), "out of", nrow(warehouse), 
    "(", round(nrow(anomalies)/nrow(warehouse)*100, 2), "%)\n")

if(nrow(anomalies) > 0) {
  print(head(anomalies[, c("id_sale", "product_name", "quantity", "total_amount", "z_score")], 10))
}

# ==============================================================================
# PART 5: KEY PERFORMANCE INDICATORS (KPI)
# ==============================================================================

cat("\n═══ PHASE 5: KPI DASHBOARD ═══\n\n")

total_revenue <- sum(warehouse$total_amount)
total_profit <- sum(warehouse$net_profit)
avg_margin <- mean(warehouse$margin_rate)
nb_transactions <- nrow(warehouse)
nb_clients <- length(unique(warehouse$id_client))
avg_basket <- mean(warehouse$total_amount)

revenue_2023 <- sum(warehouse$total_amount[warehouse$year == 2023])
revenue_2024 <- sum(warehouse$total_amount[warehouse$year == 2024])
growth <- if(revenue_2023 > 0) ((revenue_2024 - revenue_2023) / revenue_2023) * 100 else 0

cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                  EXECUTIVE DASHBOARD                           ║\n")
cat("╠════════════════════════════════════════════════════════════════╣\n")
cat("║ FINANCIAL INDICATORS                                           ║\n")
cat("║────────────────────────────────────────────────────────────────║\n")
cat(sprintf("║ • Total Revenue               : %15.0f DA         ║\n", total_revenue))
cat(sprintf("║ • Total Net Profit            : %15.0f DA         ║\n", total_profit))
cat(sprintf("║ • Average Margin              : %15.2f %%           ║\n", avg_margin))
cat(sprintf("║ • Annual Growth               : %15.2f %%           ║\n", growth))
cat("║                                                                ║\n")
cat("║ COMMERCIAL INDICATORS                                          ║\n")
cat("║────────────────────────────────────────────────────────────────║\n")
cat(sprintf("║ • Number of Transactions      : %15d             ║\n", nb_transactions))
cat(sprintf("║ • Active Clients              : %15d             ║\n", nb_clients))
cat(sprintf("║ • Average Basket              : %15.0f DA         ║\n", avg_basket))
cat("╚════════════════════════════════════════════════════════════════╝\n\n")

# ==============================================================================
# PART 6: EXPORT RESULTS
# ==============================================================================

cat("═══ PHASE 6: EXPORT RESULTS ═══\n\n")

if(!dir.exists("exports")) {
  dir.create("exports")
  cat("✓ 'exports' folder created\n")
}

write.csv(warehouse, "exports/warehouse_data.csv", row.names = FALSE)
write.csv(rfm_analysis, "exports/rfm_analysis.csv", row.names = FALSE)
write.csv(sales_category, "exports/sales_category.csv", row.names = FALSE)
write.csv(perf_stores, "exports/store_performance.csv", row.names = FALSE)

cat("✓ CSV files exported to 'exports/'\n")
cat("  • warehouse_data.csv\n")
cat("  • rfm_analysis.csv\n")
cat("  • sales_category.csv\n")
cat("  • store_performance.csv\n")

# ==============================================================================
# FINAL REPORT
# ==============================================================================

cat("\n")
cat("╔════════════════════════════════════════════════════════════════╗\n")
cat("║                      FINAL REPORT                              ║\n")
cat("╠════════════════════════════════════════════════════════════════╣\n")
cat("║ ✓ Data warehouse created successfully                          ║\n")
cat(sprintf("║   • %d transactions                                        ║\n", nrow(warehouse)))
cat(sprintf("║   • %d unique clients                                      ║\n", nb_clients))
cat(sprintf("║   • %d different products                                  ║\n", length(unique(warehouse$id_product))))
cat("║                                                                ║\n")
cat("║ ✓ OLAP analysis completed                                      ║\n")
cat("║   • Category, store, and time analysis                         ║\n")
cat("║   • RFM client analysis                                        ║\n")
cat("║                                                                ║\n")
cat("║ ✓ Data mining performed                                        ║\n")
cat(sprintf("║   • %d client clusters identified                          ║\n", 3))
cat(sprintf("║   • %d anomalies detected                                  ║\n", nrow(anomalies)))
cat("║                                                                ║\n")
cat("║ ✓ Visualizations generated                                     ║\n")
cat("║   • 4 charts created                                           ║\n")
cat("║                                                                ║\n")
cat("║ ✓ Exports completed                                            ║\n")
cat("║   • 4 CSV files exported                                       ║\n")
cat("╚════════════════════════════════════════════════════════════════╝\n")

cat("\n🎯 PROJECT COMPLETED SUCCESSFULLY!\n")
cat("📁 Check 'exports/' folder for results\n\n")