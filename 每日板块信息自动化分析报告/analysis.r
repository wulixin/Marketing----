
# analysis.R
library(ggplot2)
library(scales)
library(stringr)
library(ggcorrplot)
library(corrplot)
library(showtext)
library(plotly)
library(stringi)
showtext_auto(enable=TRUE)
font_add('Songti','Songti.ttc')
font_families()

# 加载预处理好的数据
load("/Users/wulixin/Downloads/processed_data.RData")

# 1. 生成“指数表现”部分的图表和文字
plot_index_perf <- ggplot(df_index_perf, aes(x = reorder(name, -daily_pct), y = daily_pct)) +
  geom_col(fill = ifelse(df_index_perf$daily_pct > 0, "red", "green"), width = 0.7) +
  coord_flip() +
  labs(title = "主要指数日涨跌幅", x = "指数名称", y = "涨跌幅(%)") +
  theme_minimal()

# 自动生成文字描述
top_gainer <- df_index_perf %>% arrange(desc(daily_pct)) %>% slice(1) %>% pull(name)
top_loser <- df_index_perf %>% arrange(daily_pct) %>% slice(1) %>% pull(name)
avg_daily_pct <- mean(df_index_perf$daily_pct, na.rm = TRUE)
text_index_perf <- paste0(
  "今日市场整体表现：",
  ifelse(avg_daily_pct > 0, "上涨", "下跌"),
  "，平均涨跌幅为 ", percent(avg_daily_pct, accuracy = 0.1), "。\n",
  "表现最佳的指数是 ", top_gainer, "，涨幅达 ", percent(df_index_perf$daily_pct[df_index_perf$name == top_gainer], accuracy = 0.1), "。\n",
  "表现最差的指数是 ", top_loser, "，跌幅达 ", percent(df_index_perf$daily_pct[df_index_perf$name == top_loser], accuracy = 0.1), "。"
)

# 2. 生成“指数估值”部分的图表和文字
# 创建一个长格式数据用于绘图
df_val_long <- df_index_valuation %>%
  pivot_longer(
    cols = c(static_pe, rolling_pe, pb, dividend_yield),
    names_to = "metric",
    values_to = "value"
  ) %>%
  mutate(metric = factor(metric, levels = c("static_pe", "rolling_pe", "pb", "dividend_yield"),
                         labels = c("静态PE", "滚动PE", "市净率", "股息率")))

plot_index_valuation <- ggplot(df_val_long, aes(x = name, y = value, fill = metric)) +
  geom_col(position = "dodge") +
  scale_fill_brewer(palette = "Set1") +
  labs(title = "主要指数估值水平对比", x = "指数名称", y = "估值指标") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 估值文字描述
lowest_pe_index <- df_index_valuation %>% arrange(static_pe) %>% slice(1) %>% pull(name)
highest_pe_index <- df_index_valuation %>% arrange(desc(static_pe)) %>% slice(1) %>% pull(name)
text_index_valuation <- paste0(
  "从估值角度看，", lowest_pe_index, "的静态市盈率最低，为 ", round(df_index_valuation$static_pe[df_index_valuation$name == lowest_pe_index], 2), "倍；\n",
  "而", highest_pe_index, "的静态市盈率最高，为 ", round(df_index_valuation$static_pe[df_index_valuation$name == highest_pe_index], 2), "倍。"
)


# 3. 生成“指数贡献”部分的图表和文字
# 选取沪深300和中证500的贡献前十大
df_contrib_top <- df_index_contribution %>%
  group_by(index_name) %>%
  top_n(10, desc(contribution_points)) %>%
  ungroup()

plot_index_contribution <- ggplot(df_contrib_top, aes(x = reorder(stock_name, contribution_points), y = contribution_points, fill = index_name)) +
  geom_col(width = 0.7) +
  facet_wrap(~ index_name, scales = "free_x") +
  coord_flip() +
  labs(title = "主要指数成分股贡献度", x = "股票名称", y = "贡献点数") +
  scale_fill_brewer(palette = "Dark2")+
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, vjust = 0.5, size = 9)
  )

# 贡献文字描述
top_contributor_sh300 <- df_contrib_top %>%
  filter(index_name == "沪深300指数前十大") %>%
  arrange(desc(desc(contribution_points))) %>%
  top_n(5) %>%
  pull(stock_name)

top_contributor_zz500 <- df_contrib_top %>%
  filter(index_name == "中证500指数前十大") %>%
  arrange(desc(desc(contribution_points))) %>%
  top_n(5)%>%
  pull(stock_name)

top_contributor_sz50 <- df_contrib_top %>%
  filter(index_name == "上证50指数前十大") %>%
  arrange(desc(desc(contribution_points))) %>%
  top_n(5) %>%
  pull(stock_name)


text_index_contribution <- paste0(
  "在沪深300指数中，对指数贡献前五的个股是 ", top_contributor_sh300, "，贡献了 ", stri_sprintf(df_contrib_top$contribution_points[df_contrib_top$stock_name %in% top_contributor_sh300 & df_contrib_top$index_name == "沪深300指数前十大"], 2), " 点。\n",
  "在中证500指数中，对指数贡献前五的个股是 ", top_contributor_zz500, "，贡献了 ", stri_sprintf(df_contrib_top$contribution_points[df_contrib_top$stock_name %in% top_contributor_zz500 & df_contrib_top$index_name == "中证500指数前十大"], 2), " 点。",
  "在上证50指数中，对指数贡献前五的个股是 ", top_contributor_sz50, "，贡献了 ", stri_sprintf(df_contrib_top$contribution_points[df_contrib_top$stock_name %in% top_contributor_sz50 & df_contrib_top$index_name == "上证50指数前十大"], 2), " 点。\n"
)

# 4. 生成“每日板块信息”部分的图表和文字
# 分别绘制最强和最弱板块
df_strong <- df_sector_info %>% filter(performance_type == "最强")
df_weak <- df_sector_info %>% filter(performance_type == "最弱")

plot_sector_strong <- ggplot(df_strong, aes(x = reorder(sector_name, -daily_pct), y = daily_pct)) +
  geom_col(fill = "red", width = 0.7) +
  coord_flip() +
  labs(title = "当日最强板块表现", x = "板块名称", y = "涨跌幅(%)") +
  theme_minimal()

plot_sector_weak <- ggplot(df_weak, aes(x = reorder(sector_name, daily_pct), y = daily_pct)) +
  geom_col(fill = "green", width = 0.7) +
  coord_flip() +
  labs(title = "当日最弱板块表现", x = "板块名称", y = "涨跌幅(%)") +
  theme_minimal()

# 板块文字描述
strongest_sector <- df_strong %>% arrange(desc(daily_pct)) %>% slice(1) %>% pull(sector_name)
weakest_sector <- df_weak %>% arrange(daily_pct) %>% slice(1) %>% pull(sector_name)

text_sector_info <- paste0(
  "当日市场热点集中在 ", strongest_sector, " 板块，涨幅高达 ", percent(max(df_strong$daily_pct), accuracy = 0.1), "。\n",
  "而表现最差的是 ", weakest_sector, " 板块，跌幅达 ", percent(min(df_weak$daily_pct), accuracy = 0.1), "。"
)

theme_finance <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_line(color = "grey90"),
      text = element_text(family = "Arial, sans-serif")
    )
}

# 应用到所有图表
plot_index_perf <- plot_index_perf + theme_finance()
plot_sector_strong <- plot_sector_strong + theme_finance()
plot_sector_weak<- plot_sector_weak + theme_finance()
plot_index_valuation<-plot_index_valuation + theme_finance()
#plot_index_contribution<-plot_index_contribution + theme_finance()
# 将所有结果保存到全局环境或.RData文件
save(
  plot_index_perf, text_index_perf,
  plot_index_valuation, text_index_valuation,
  plot_index_contribution, text_index_contribution,
  plot_sector_strong, plot_sector_weak, #text_sector_info,
  file = "/Users/wulixin/Downloads/analysis_results.RData"
)

