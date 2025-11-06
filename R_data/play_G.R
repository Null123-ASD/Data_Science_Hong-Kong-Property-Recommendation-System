# 載入必要的套件
library(readxl)
library(ggplot2)

# 讀取 Excel 文件
Performance_Venues <- read_excel("C:/Users/26385/OneDrive/桌面/Performance Venues.xlsx")

# 繪製條形圖
ggplot(Performance_Venues[Performance_Venues$地區 != '總數', ], aes(x = 地區, y = 表演場地, fill = 地區)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = 表演場地), vjust = -0.5) +
  labs(title = "各地區表演場地數量", 
       x = "地區", 
       y = "表演場地數量") +
  theme_minimal()
# 繪製圓餅圖
# 計算每個地區的百分比
Performance_Venues <- Performance_Venues %>%
  mutate(百分比 = 表演場地 / sum(表演場地) * 100)

ggplot(Performance_Venues, aes(x = "", y = 表演場地, fill = 地區)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y") +
  geom_text(aes(label = paste0(地區, ": ", 表演場地, " (", round(百分比, 1), "%)")),
            position = position_stack(vjust = 0.5)) +
  labs(title = "各地區表演場地比例") +
  theme_void()


