# 加載必要的庫
library(readxl)
library(dplyr)
library(ggplot2)

# 讀取數據
# 新界東
school_banding_D <- read_excel("C:/Users/26385/OneDrive/桌面/school_banding_D.xlsx")
# 新界西
school_banding_X <- read_excel("C:/Users/26385/OneDrive/桌面/school_banding_X.xlsx")

# 添加地區標籤
school_banding_D <- school_banding_D %>% mutate(Region = "新界東")
school_banding_X <- school_banding_X %>% mutate(Region = "新界西")

# 合併兩個表格
school_data <- bind_rows(school_banding_D, school_banding_X)

# 提取並統計“Banding”數據
banding_summary <- school_data %>%
  select(Banding, Region) %>%
  group_by(Region, Banding) %>%
  summarise(Count = n(), .groups = "drop")

# 繪製分組柱狀圖
ggplot(banding_summary, aes(x = Region, y = Count, fill = Banding)) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +
  labs(
    title = "新界東與新界西的學校 Banding 分佈",
    x = "地區",
    y = "學校數量",
    fill = "Banding"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12)
  )

