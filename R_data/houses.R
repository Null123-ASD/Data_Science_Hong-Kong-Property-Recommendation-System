library(ggplot2)
library(dplyr)
library(tidyr)  # 确保加载tidyr用于 pivot_longer
library(tidyverse)
library(GGally)
library(readxl)

District_basic_statistics <- read_excel("C:/Users/26385/OneDrive/桌面/District_basic_statistics.xlsx")


ggplot(data = District_basic_statistics, aes(x = reorder(subregion.name, price_mean), group = 1)) +
  geom_line(aes(y = price_mean, color = "Mean Price"), size = 1) +
  geom_line(aes(y = price_median, color = "Median Price"), size = 1) +
  coord_flip() +
  labs(
    title = "Comparison of Mean and Median Prices by Subregion",
    x = "Subregion",
    y = "Price (HK$)",
    color = "Price Type"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1) # 調整 x 軸文字角度
  )




ggplot(data = District_basic_statistics, aes(x = reorder(subregion.name, price_mean), y = price_mean, fill = subregion.name)) +
  geom_bar(stat = "identity") +
  coord_flip() +  # Flip to horizontal for better readability
  labs(
    title = "Average House Prices by Subregion",
    x = "Subregion",
    y = "Average Price (HK$)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))


ggplot(data = District_basic_statistics, aes(x = reorder(subregion.name, price_over_net_area_mean), y = price_over_net_area_mean, color = subregion.name)) +
  geom_point(size = 4) +
  coord_flip() +
  labs(
    title = "Price per Net Area by Subregion",
    x = "Subregion",
    y = "Price per Net Area (HK$/sq.ft)"
  ) +
  theme_minimal() +
  theme(legend.position = "none", plot.title = element_text(hjust = 0.5))







# 1. 清理金額數據，移除非數字字符並轉換為數字
District_basic_statistics <- District_basic_statistics %>%
  mutate(
    price_mean = as.numeric(gsub("[^0-9.]", "", price_mean)),
    price_median = as.numeric(gsub("[^0-9.]", "", price_median)),
    price_over_net_area_mean = as.numeric(gsub("[^0-9.]", "", price_over_net_area_mean)),
    price_over_net_area_median = as.numeric(gsub("[^0-9.]", "", price_over_net_area_median))
  )

# 檢查清理後的數據
print(summary(District_basic_statistics))

# 2. 選擇非空行並提取數值列
numeric_data <- District_basic_statistics %>%
  filter(
    !is.na(price_mean),
    !is.na(price_median),
    !is.na(price_over_net_area_mean),
    !is.na(price_over_net_area_median)
  ) %>%
  select(Count, price_mean, price_median, net_area_mean, net_area_median, price_over_net_area_mean, price_over_net_area_median)

# 確保數據框中有數據
if (nrow(numeric_data) == 0) {
  stop("數據框在清理後為空，請檢查原始數據是否包含有效值。")
}

# 確保列名合法
colnames(numeric_data) <- make.names(colnames(numeric_data))

# 4. 繪製散佈矩陣
ggpairs(
  numeric_data,
  title = "散佈矩陣",
  lower = list(continuous = wrap("points", alpha = 0.5)),  # 散點圖
  diag = list(continuous = wrap("densityDiag", alpha = 0.5)),  # 密度圖
  upper = list(continuous = wrap("cor", size = 4, alignPercent = 0.8))  # 顯示關聯係數
)



















# 选择特定的列，使用反引号包裹有空格的列名
selected_data <- District_basic_statistics %>% 
  select(subregion.name, 
         Count, 
         `price_mean`, 
         `price_median`, 
         `net_area_mean`, 
         `net_area_median`, 
         `price_over_net_area_mean`, 
         `price_over_net_area_median`)

# 查看选择的数据

ggplot(selected_data, aes(x = reorder(subregion.name, `price_mean`), y = `price_mean`)) +
  geom_point(size = 5, color = "gray") +  # 使用点来表示价格
  geom_segment(aes(x = subregion.name, xend = subregion.name, y = `price_mean`, yend = 0), color = "lightgray") +  # 绘制竖线
  labs(title = "Price Distribution by District", 
       x = "District", 
       y = "Price (HKD)") + 
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#柱状图 展示每个子区域的平均价格。
ggplot(selected_data, aes(x = subregion.name, y = `price_mean`, fill = subregion.name)) +
  geom_bar(stat = "identity") +
  labs(title = "Mean Price by Subregion", 
       x = "Subregion", 
       y = "Mean Price (HKD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 x 轴文本

#绘制折线图
ggplot(selected_data, aes(x = subregion.name, y = `price_over_net_area_mean`, group = 1)) +
  geom_line() +
  geom_point() +
  labs(title = "Price Over Net Area Mean by Subregion", 
       x = "Subregion", 
       y = "Price Over Net Area Mean (HKD)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 x 轴文本

#用条形图展示子区域的数量。
ggplot(selected_data, aes(x = subregion.name, y = Count)) +
  geom_col(fill = "steelblue") +
  labs(title = "Count by Subregion", 
       x = "Subregion", 
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # 旋转 x 轴文本

#散点图 展示价格与每平方呎价格之间的关系：
ggplot(selected_data, aes(x = `price_mean`, y = `price_over_net_area_mean`, label = subregion.name)) +
  geom_point(color = "blue") +          # 设置点的颜色
  geom_text(vjust = -1) +                # 添加标签
  labs(title = "Price Mean vs Price Over Net Area Mean", 
       x = "Price Mean (HKD)", 
       y = "Price Over Net Area Mean (HKD/sq. ft.)") + 
  theme_minimal()

#price median 和 price_over_net_area median
ggplot(selected_data, aes(x = `price_median`, y = `price_over_net_area_median`, label = subregion.name)) +
  geom_point(color = "green") +          # 设置点的颜色
  geom_text(vjust = -1) +                # 添加标签
  labs(title = "Price Median vs Price Over Net Area Median", 
       x = "Price Median (HKD)", 
       y = "Price Over Net Area Median (HKD/sq. ft.)") + 
  theme_minimal()


