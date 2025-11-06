# 載入必要的套件
library(readxl)
library(ggplot2)
library(dplyr)

# 讀取 Excel 文件
Hospital <- read_excel("C:/Users/26385/OneDrive/桌面/Hospital.xlsx")

# 計算各地區的醫院數量時排除「統計」這一行
hospital_counts <- Hospital %>%
  filter(地區 != "統計") %>%  # 排除統計行
  group_by(地區) %>%
  summarise(醫院數量 = n())





# 繪製條形圖
ggplot(hospital_counts, aes(x = 地區, y = 醫院數量, fill = 地區)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = 醫院數量), vjust = -0.5) +
  labs(title = "各地區醫院數量", 
       x = "地區", 
       y = "醫院數量") +
  theme_minimal()


# 繪製曲線圖
ggplot(hospital_counts, aes(x = 地區, y = 醫院數量, group = 1)) +
  geom_line(color = "blue", size = 1) +  # 繪製曲線
  geom_point(size = 3, color = "red") +  # 繪製數據點
  geom_text(aes(label = 醫院數量), vjust = -0.5) +  # 標籤
  labs(title = "各地區醫院數量曲線圖", 
       x = "地區", 
       y = "醫院數量") +
  theme_minimal()

# 繪製點圖
ggplot(hospital_counts, aes(x = 地區, y = 醫院數量, color = 地區)) +
  geom_point(size = 4) +
  geom_line(aes(group = 1)) +
  labs(title = "各地區醫院數量點圖", 
       x = "地區", 
       y = "醫院數量") +
  theme_minimal()

# 繪製梯形圖
ggplot(hospital_counts, aes(x = 地區, y = 醫院數量, fill = 地區)) +
  geom_col(position = "dodge") +
  labs(title = "各地區醫院數量梯形圖", 
       x = "地區", 
       y = "醫院數量") +
  theme_minimal()




# 計算每個醫院的數量（一般為1）
hospital_counts <- Hospital %>%
  group_by(醫院名稱) %>%
  summarise(醫院數量 = n())

# 繪製條形圖，顯示每個醫院名稱
ggplot(hospital_counts, aes(x = reorder(醫院名稱, -醫院數量), y = 醫院數量, fill = 醫院名稱)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = 醫院數量), vjust = -0.5) +
  labs(title = "醫院名稱及其數量", 
       x = "醫院名稱", 
       y = "醫院數量") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # 旋轉x軸標籤以方便閱讀
  theme_minimal()
