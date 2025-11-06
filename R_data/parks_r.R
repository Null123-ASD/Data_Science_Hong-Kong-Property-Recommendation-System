# 載入必要的套件
library(readxl)
library(ggplot2)

# 讀取 Excel 文件
parks <- read_excel("C:/Users/26385/OneDrive/桌面/parks.xlsx")

# 繪製條形圖
ggplot(parks[parks$地區 != '總數', ], aes(x = 地區, y = 公園)) +
  geom_bar(stat = "identity", fill = "lightgreen") +
  geom_text(aes(label = 公園), vjust = -0.5) +
  labs(title = "各地區公園數量", 
       x = "地區", 
       y = "公園數量") +
  theme_minimal()
