library(ggplot2)
library(dplyr)
library(tidyr)  # 确保加载tidyr用于 pivot_longer
library(tidyverse)
library(readxl)

Table_925_92023_tc <- read_excel("Table 925-92023_tc.xlsx")

# 数据整理：将数据转为长格式
long_data <- Table_925_92023_tc %>%
  pivot_longer(cols = c("2020", "2021", "2022", "2023"), 
               names_to = "Year", 
               values_to = "Value") %>%
  mutate(Year = as.numeric(Year)) %>% # 将Year转换为数字类型
  filter(Area != "Overall Total")  # 去掉“整體總數” 

# 绘制点图
ggplot(long_data, aes(x = Year, y = Value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Value), position = position_dodge(width = 0.9), vjust = 0.5) +  # 调整vjust
  labs(title = "Number of schools data from 2020-2023",
       x = "Year",
       y = "Value",
       fill = "Area") +
  scale_y_continuous(limits = c(0, 1200)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # 使用色板# 調整 X 軸標籤角度

# 绘图
ggplot(long_data, aes(x = Year, y = Value, fill = Area)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = Value), position = position_dodge(width = 0.9), vjust = -0.5) +
  labs(title = "Number of Schools Data from 2020-2023",
       x = "Year",
       y = "Value") +
  scale_y_continuous(limits = c(0, 700)) +
  theme_minimal() +
  facet_wrap(~ Area)  # 添加多面图

