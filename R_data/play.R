# 載入所需的套件
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)

# 讀取 Excel 文件
playground <- read_excel("C:/Users/26385/OneDrive/桌面/playground.xlsx")

# 轉換數據為長格式
playground_long <- playground %>%
  pivot_longer(cols = c(運動場, 體育館), 
               names_to = "類型", 
               values_to = "數量") %>%
  filter(地區 != "總數")  # 排除總數行

# 繪製條形圖
ggplot(playground_long, aes(x = 地區, y = 數量, fill = 類型)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = 數量), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "各地區運動場及體育館數量", 
       x = "地區", 
       y = "數量") +
  theme_minimal()
# 繪製箱線圖
ggplot(playground_long, aes(x = 類型, y = 數量, fill = 類型)) +
  geom_boxplot() +
  labs(title = "運動場及體育館數量分佈", x = "類型", y = "數量") +
  theme_minimal()

# 繪製點圖
ggplot(playground_long, aes(x = 地區, y = 數量, color = 類型)) +
  geom_point(size = 3) +
  geom_line(aes(group = 類型)) +
  labs(title = "各地區運動場及體育館數量", x = "地區", y = "數量") +
  theme_minimal()