library(ggplot2)
library(dplyr)
library(tidyr)  # 确保加载tidyr用于 pivot_longer
library(tidyverse)
library(readxl)

Primary_school_ranking <- read_excel("C:/Users/26385/OneDrive/桌面/Primary school ranking.xlsx")


# 按校網地區進行統計
selected_data <- Primary_school_ranking %>%
  group_by(校網地區) %>%
  summarise(學校數量 = n()) %>%
  arrange(desc(學校數量))

# 繪製圖表
ggplot(selected_data, aes(x = reorder(校網地區, -學校數量), y = 學校數量)) +
  geom_bar(stat = "identity", fill = "lightblue", width = 0.7) +
  geom_text(aes(label = 學校數量), vjust = -0.5, hjust = -0.2, size = 4) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 20)) +
  labs(title = "前100名個校網地區的學校數量", x = "校網地區", y = "學校數量") +
  theme_minimal(base_size = 15) +
  theme(panel.grid.major = element_line(color = "grey", linetype = "dashed"),
        plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12))

ggplot(selected_data, aes(x = "", y = 學校數量, fill = 校網地區)) +
  geom_bar(stat = "identity") +
  coord_polar(theta = "y") +  # 將條形圖轉換為圓形圖
  labs(title = "各校網地區的學校數量", x = "", y = "") +
  theme_minimal(base_size = 15) +
  theme(plot.title = element_text(hjust = 0.5, size = 18),
        axis.text.y = element_blank(),  # 隱藏y軸
        axis.text.x = element_blank())  # 隱藏x軸



