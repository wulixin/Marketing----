################################################################################
#  analysis_mechanism_did.R
#  功能：机制DID分析 — 协议转让+表决权让渡+高溢价的差异化效应
#
#  研究问题：
#    1. "并购六条"是否推动了"协议转让+表决权让渡"组合模式的普及？
#    2. 科技并购中，这一组合模式与CAR之间的交互效应如何？
#    3. 高溢价（市场定价）与表决权让渡（控制权安排）的关系是否支持"双轨"假说？
#    4. （预留接口）贷款融资比例进入分析后的效应
#
#  输出：
#    - 三组DID回归表格（协议转让/表决权让渡/高溢价）
#    - 四张机制分析图（配合论文第五章、第六章补充）
#    - 数据输出：mechanism_did_results.csv
#
#  依赖：source("generate_tables.R") 已加载 df
#  更新日期：2026-06-12
################################################################################

setwd("/Users/wulixin/Documents/GitHub/Marketing----/控制权变更研究")

# ── 加载数据（先加载，再 library 各包，避免命名空间冲突）────────────────────

.skip_run_all <- TRUE
if (!exists("df_main")) {
  if (!exists("df") || !is.data.frame(df)) source("generate_tables.R")
  # 立刻在任何额外 library 之前保存，防止 stats::df 等遮盖
  df_main <- df
}
rm(.skip_run_all)

# generate_tables.R 内 pacman::p_load 已经安装/加载了大部分包，
# 此处只补充本脚本额外需要的包，并确保 dplyr 在搜索路径最前面
library(dplyr)      # 必须在 generate_tables.R 之后 library，覆盖到顶部
library(stargazer)
library(scales)
library(grid)
if (!requireNamespace("gridExtra", quietly = TRUE)) install.packages("gridExtra")

# 字体设置（与 generate_graphics.R 保持一致）
FONT_FAMILY <- "Songti SC"

# ── 1. 数据准备：构建机制分析变量 ────────────────────────────────────────────

df_mech <- df_main  # 从原始数据框开始，用 base R 赋值（避免 dplyr 命名空间冲突）
df_mech$treat    <- as.numeric(df_main$is_tech)
df_mech$post     <- as.numeric(df_main$is_post)
df_mech$agr      <- as.numeric(df_main$is_agreement)
df_mech$vow      <- as.numeric(df_main$has_vow)
df_mech$agr_vow  <- df_mech$agr * df_mech$vow
pm_vec           <- as.numeric(df_main$premium_mkt)
df_mech$high_prem <- as.integer(pm_vec > median(pm_vec, na.rm = TRUE))
df_mech$vow_x_highprem <- df_mech$vow * df_mech$high_prem
df_mech$loan_ratio_num  <- as.numeric(df_main$loan_ratio)
df_mech$has_loan        <- as.integer(!is.na(df_mech$loan_ratio_num) & df_mech$loan_ratio_num > 0)
df_mech$loan_ratio_fill <- ifelse(is.na(df_mech$loan_ratio_num), 0, df_mech$loan_ratio_num)
df_mech$did_agr     <- df_mech$treat * df_mech$post * df_mech$agr
df_mech$did_vow     <- df_mech$treat * df_mech$post * df_mech$vow
df_mech$did_prem    <- df_mech$treat * df_mech$post * df_mech$high_prem
df_mech$treat_agr   <- df_mech$treat * df_mech$agr
df_mech$treat_vow   <- df_mech$treat * df_mech$vow
df_mech$post_agr    <- df_mech$post  * df_mech$agr
df_mech$post_vow    <- df_mech$post  * df_mech$vow
df_mech$treat_post  <- df_mech$treat * df_mech$post
df_mech$lev_num     <- as.numeric(df_main$lev)
df_mech$roe_num     <- as.numeric(df_main$roe)
df_mech$tobin_q_num <- as.numeric(df_main$tobin_q)
df_mech$lgsize_num  <- as.numeric(df_main$lgsize)
df_mech$top1_num    <- as.numeric(df_main$top1)
df_mech$tpct_num    <- as.numeric(df_main$transfer_pct)
df_mech$prem_mkt_num <- pm_vec
df_mech$car7_num    <- as.numeric(df_main$car7)

cat(sprintf("机制分析样本量：N = %d\n", nrow(df_mech)))
cat(sprintf("协议转让频率：%.1f%%\n", 100 * mean(df_mech$agr, na.rm=TRUE)))
cat(sprintf("表决权让渡频率：%.1f%%\n", 100 * mean(df_mech$vow, na.rm=TRUE)))
cat(sprintf("高溢价频率：%.1f%%（中位数=%.2f%%）\n",
            100 * mean(df_mech$high_prem, na.rm=TRUE),
            median(df_mech$prem_mkt_num, na.rm=TRUE)))

# ── 2. 机制DID回归：以"是否协议转让"为被解释变量（模式选择方程）─────────────

cat("\n\n========== 模式选择方程：协议转让 ==========\n")

# (A) 协议转让 ~ Treat + Post + DID（Logit，因为协议转让为0/1）
mA1 <- glm(agr ~ treat + post + treat_post,
           data = df_mech, family = binomial(link = "logit"))

mA2 <- glm(agr ~ treat + post + treat_post +
             roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
           data = df_mech, family = binomial(link = "logit"))

mA3 <- glm(agr ~ treat + post + treat_post +
             roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num +
             top1_num + vow,
           data = df_mech, family = binomial(link = "logit"))

cat("\n--- Logit: 协议转让 ~ DID ---\n")
capture.output(
  stargazer::stargazer(
    mA1, mA2, mA3,
    type = "text",
    title = "表A1  协议转让选择方程（Logit，边际效应）",
    dep.var.labels = "协议转让（=1）",
    covariate.labels = c("Treat（科技并购）", "Post（政策后）", "DID（核心）",
                         "ROE", "资产负债率", "托宾Q值", "企业规模",
                         "转让股份比例", "第一大股东持股", "表决权让渡"),
    omit.stat = c("f", "ser", "ll"),
    star.cutoffs = c(0.1, 0.05, 0.01),
    digits = 3
  )
) %>% cat(sep = "\n")

# ── 3. 机制DID回归：以"表决权让渡"为被解释变量（控制权安排方程）──────────────

cat("\n\n========== 控制权安排方程：表决权让渡 ==========\n")

mB1 <- glm(vow ~ treat + post + treat_post,
           data = df_mech, family = binomial(link = "logit"))

mB2 <- glm(vow ~ treat + post + treat_post +
             roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
           data = df_mech, family = binomial(link = "logit"))

mB3 <- glm(vow ~ treat + post + treat_post +
             roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num +
             top1_num + agr,
           data = df_mech, family = binomial(link = "logit"))

cat("\n--- Logit: 表决权让渡 ~ DID ---\n")
capture.output(
  stargazer::stargazer(
    mB1, mB2, mB3,
    type = "text",
    title = "表A2  表决权让渡选择方程（Logit）",
    dep.var.labels = "表决权让渡（=1）",
    covariate.labels = c("Treat（科技并购）", "Post（政策后）", "DID（核心）",
                         "ROE", "资产负债率", "托宾Q值", "企业规模",
                         "转让股份比例", "第一大股东持股", "协议转让"),
    omit.stat = c("f", "ser", "ll"),
    star.cutoffs = c(0.1, 0.05, 0.01),
    digits = 3
  )
) %>% cat(sep = "\n")

# ── 4. CAR方程：检验机制变量对市场反应的交互效应 ─────────────────────────────

cat("\n\n========== 市场反应方程：CAR7 ~ 机制变量交互 ==========\n")

# 基准：标准DID
mC0 <- lm(car7_num ~ treat + post + treat_post + roe_num + lev_num +
            tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

# (C1) 加入协议转让及其与DID的交互
mC1 <- lm(car7_num ~ treat + post + treat_post +
            agr + treat_agr + post_agr + did_agr +
            roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

# (C2) 加入表决权让渡及其与DID的交互
mC2 <- lm(car7_num ~ treat + post + treat_post +
            vow + treat_vow + post_vow + did_vow +
            roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

# (C3) 表决权让渡 + 高溢价 + 二维交互（检验双轨假说）
mC3 <- lm(car7_num ~ treat + post + treat_post +
            vow + high_prem + vow_x_highprem +
            roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

# (C4) 完整机制：协议转让+表决权让渡联合+高溢价
mC4 <- lm(car7_num ~ treat + post + treat_post +
            agr + vow + agr_vow + high_prem +
            roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

cat("\n--- OLS: CAR7 ~ 机制变量交互 ---\n")
capture.output(
  stargazer::stargazer(
    mC0, mC1, mC2, mC3, mC4,
    type = "text",
    title = "表A3  市场反应方程：机制变量的调节效应",
    dep.var.labels = "CAR[-7,7](%)",
    covariate.labels = c(
      "Treat（科技并购）", "Post（政策后）", "DID（核心）",
      "协议转让", "Treat×协议转让", "Post×协议转让", "三重DID×协议转让",
      "表决权让渡", "Treat×表决权让渡", "Post×表决权让渡", "三重DID×表决权让渡",
      "高溢价", "表决权让渡×高溢价",
      "协议转让+表决权让渡联合",
      "ROE", "资产负债率", "托宾Q值", "企业规模", "转让股份比例"
    ),
    omit.stat = c("f", "ser"),
    star.cutoffs = c(0.1, 0.05, 0.01),
    digits = 3
  )
) %>% cat(sep = "\n")

# ── 5. 溢价方程：检验机制变量对定价的影响（支持双轨假说）───────────────────

cat("\n\n========== 定价方程：溢价率 ~ 机制变量 ==========\n")

mD1 <- lm(prem_mkt_num ~ treat + post + treat_post +
            vow + roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

mD2 <- lm(prem_mkt_num ~ treat + post + treat_post +
            agr + vow + agr_vow +
            roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
          data = df_mech)

# 关键检验：若表决权让渡不显著影响溢价率，但显著影响CAR → 双轨假说得证
cat("\n--- OLS: 市价溢价率 ~ 机制变量 (双轨假说检验) ---\n")
capture.output(
  stargazer::stargazer(
    mD1, mD2,
    type = "text",
    title = "表A4  定价方程：溢价率对机制变量的响应（双轨假说检验）",
    dep.var.labels = "市价溢价率(%)",
    covariate.labels = c(
      "Treat（科技并购）", "Post（政策后）", "DID（核心）",
      "表决权让渡", "协议转让", "协议转让×表决权让渡",
      "ROE", "资产负债率", "托宾Q值", "企业规模", "转让股份比例"
    ),
    omit.stat = c("f", "ser"),
    star.cutoffs = c(0.1, 0.05, 0.01),
    digits = 3
  )
) %>% cat(sep = "\n")

# 输出关键系数摘要
cat("\n\n========== 关键系数摘要 ==========\n")
coef_summary <- data.frame(
  方程 = c("协议转让(Logit)", "表决权让渡(Logit)",
           "CAR7~协议转让DID", "CAR7~表决权让渡DID",
           "溢价率~表决权让渡", "溢价率~协议转让"),
  变量 = c("DID(Treat×Post)", "DID(Treat×Post)",
           "三重DID×协议转让", "三重DID×表决权让渡",
           "表决权让渡", "协议转让"),
  系数 = c(
    round(coef(mA2)["treat_post"], 3),
    round(coef(mB2)["treat_post"], 3),
    round(coef(mC1)["did_agr"], 3),
    round(coef(mC2)["did_vow"], 3),
    round(coef(mD1)["vow"], 3),
    round(coef(mD2)["agr"], 3)
  ),
  p值 = c(
    round(summary(mA2)$coefficients["treat_post", 4], 3),
    round(summary(mB2)$coefficients["treat_post", 4], 3),
    round(summary(mC1)$coefficients["did_agr", 4], 3),
    round(summary(mC2)$coefficients["did_vow", 4], 3),
    round(summary(mD1)$coefficients["vow", 4], 3),
    round(summary(mD2)$coefficients["agr", 4], 3)
  )
)
print(coef_summary)

# ── 6. 导出CSV便于后续读取 ────────────────────────────────────────────────────

write.csv(coef_summary,
          file.path("figures", "mechanism_did_coef_summary.csv"),
          row.names = FALSE)
cat("\n系数摘要已保存至 figures/mechanism_did_coef_summary.csv\n")

# ── 7. 生成机制分析图 ─────────────────────────────────────────────────────────

save_fig <- function(filename, plot_obj, width = 7, height = 5, dpi = 300) {
  png(filename, width = width, height = height, units = "in", res = dpi, type = "cairo")
  on.exit(dev.off(), add = TRUE)
  grid::grid.newpage()
  grid::grid.draw(plot_obj)
}

# ── 图M.1  2×2图：Treat×Post×{协议转让/表决权让渡} 均值CAR7 ────────────────

fig_m1_mechanism_heatmap <- function() {
  df_plot <- df_mech %>%
    filter(!is.na(car7_num), !is.na(treat), !is.na(post),
           !is.na(vow), !is.na(agr)) %>%
    mutate(
      组别 = case_when(
        treat == 1 & post == 1 ~ "科技并购\n政策后",
        treat == 1 & post == 0 ~ "科技并购\n政策前",
        treat == 0 & post == 1 ~ "非科技并购\n政策后",
        TRUE                   ~ "非科技并购\n政策前"
      ),
      组别 = factor(组别, levels = c("非科技并购\n政策前","非科技并购\n政策后",
                                      "科技并购\n政策前","科技并购\n政策后"))
    )

  # 协议转让子图
  df_agr <- df_plot %>%
    group_by(组别, 协议转让 = factor(agr, labels = c("否","是"))) %>%
    summarise(均值CAR7 = mean(car7_num, na.rm = TRUE),
              n = n(), .groups = "drop")

  p1 <- ggplot(df_agr, aes(x = 组别, y = 均值CAR7, fill = 协议转让)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", 均值CAR7, n)),
              position = position_dodge(0.6), vjust = -0.3, size = 2.8,
              family = FONT_FAMILY) +
    scale_fill_manual(values = c("#B0BEC5", "#1976D2")) +
    labs(title = "（A）协议转让与CAR7", x = NULL,
         y = "均值 CAR[-7,7](%)", fill = "协议转让") +
    theme_bw(base_family = FONT_FAMILY, base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, face = "bold", hjust = 0),
          axis.text.x = element_text(size = 9))

  # 表决权让渡子图
  df_vow <- df_plot %>%
    group_by(组别, 表决权让渡 = factor(vow, labels = c("否","是"))) %>%
    summarise(均值CAR7 = mean(car7_num, na.rm = TRUE),
              n = n(), .groups = "drop")

  p2 <- ggplot(df_vow, aes(x = 组别, y = 均值CAR7, fill = 表决权让渡)) +
    geom_col(position = "dodge", width = 0.6, alpha = 0.85) +
    geom_text(aes(label = sprintf("%.1f%%\n(n=%d)", 均值CAR7, n)),
              position = position_dodge(0.6), vjust = -0.3, size = 2.8,
              family = FONT_FAMILY) +
    scale_fill_manual(values = c("#B0BEC5", "#E53935")) +
    labs(title = "（B）表决权让渡与CAR7", x = NULL,
         y = "均值 CAR[-7,7](%)", fill = "表决权让渡") +
    theme_bw(base_family = FONT_FAMILY, base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, face = "bold", hjust = 0),
          axis.text.x = element_text(size = 9))

  p <- gridExtra::arrangeGrob(
    p1, p2, ncol = 2,
    top = grid::textGrob(
      "图M.1  控制权获取模式 × 政策组别 → 市场反应（均值CAR7）",
      gp = grid::gpar(fontsize = 13, fontfamily = FONT_FAMILY, fontface = "bold")
    )
  )
  save_fig("figures/fig_m1_mechanism_heatmap.png", p, width = 12, height = 5.5)
  cat("  fig_m1_mechanism_heatmap.png 已生成\n")
}

# ── 图M.2  双轨假说验证：散点图 CAR7 vs 溢价率，按表决权让渡分组 ───────────

fig_m2_dual_track_vow <- function() {
  df_plot <- df_mech %>%
    filter(!is.na(car7_num), !is.na(prem_mkt_num), !is.na(vow)) %>%
    mutate(
      表决权让渡 = factor(vow, labels = c("无让渡","有让渡")),
      是否科技 = factor(treat, labels = c("非科技并购","科技并购"))
    )

  p <- ggplot(df_plot, aes(x = prem_mkt_num, y = car7_num,
                            color = 表决权让渡, shape = 是否科技)) +
    geom_point(alpha = 0.65, size = 2.5) +
    geom_smooth(aes(group = 表决权让渡), method = "lm", se = TRUE,
                linewidth = 0.9, alpha = 0.12) +
    geom_vline(xintercept = median(df_plot$prem_mkt_num, na.rm = TRUE),
               linetype = "dashed", color = "grey50", linewidth = 0.6) +
    annotate("text", x = median(df_plot$prem_mkt_num, na.rm = TRUE) + 2,
             y = max(df_plot$car7_num, na.rm = TRUE) * 0.92,
             label = sprintf("中位数=%.1f%%", median(df_plot$prem_mkt_num, na.rm=TRUE)),
             size = 3.2, family = FONT_FAMILY, color = "grey50", hjust = 0) +
    scale_color_manual(values = c("无让渡" = "#78909C", "有让渡" = "#E53935")) +
    scale_shape_manual(values = c(16, 17)) +
    labs(
      title = "图M.2  双轨假说检验：溢价率 vs CAR7（按表决权让渡分组）",
      subtitle = "若两组拟合线斜率相似 → 溢价率对CAR的影响与表决权安排无关 → 支持双轨假说",
      x = "相对市价溢价率（%）",
      y = "CAR[-7,7]（%）",
      color = NULL, shape = NULL
    ) +
    theme_bw(base_family = FONT_FAMILY, base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )

  save_fig("figures/fig_m2_dual_track_vow.png", ggplotGrob(p), width = 8, height = 5.5)
  cat("  fig_m2_dual_track_vow.png 已生成\n")
}

# ── 图M.3  政策前后：表决权让渡频率变化（Treat vs Control的DiD趋势）─────────

fig_m3_vow_trend <- function() {
  df_bar <- df_mech %>%
    filter(!is.na(vow), !is.na(treat), !is.na(post)) %>%
    group_by(
      组别 = factor(treat, labels = c("非科技并购（控制组）","科技并购（处理组）")),
      时期 = factor(post,  labels = c("政策前","政策后"))
    ) %>%
    summarise(
      让渡率   = mean(vow, na.rm = TRUE),
      协议转让率 = mean(agr, na.rm = TRUE),
      n        = n(),
      .groups  = "drop"
    ) %>%
    mutate(
      让渡率_label   = sprintf("%.0f%%\n(n=%d)", 让渡率*100, n),
      协议转让率_label = sprintf("%.0f%%\n(n=%d)", 协议转让率*100, n)
    )

  p1 <- ggplot(df_bar, aes(x = 时期, y = 让渡率, fill = 组别, group = 组别)) +
    geom_col(position = "dodge", width = 0.55, alpha = 0.85) +
    geom_line(aes(color = 组别), position = position_dodge(0.55),
              linewidth = 1, linetype = "solid") +
    geom_text(aes(label = 让渡率_label),
              position = position_dodge(0.55), vjust = -0.2,
              size = 3, family = FONT_FAMILY) +
    scale_fill_manual(values  = c("#90A4AE","#EF5350")) +
    scale_color_manual(values = c("#546E7A","#B71C1C")) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 0.85)) +
    labs(title = "（A）表决权让渡频率", x = NULL, y = "频率", fill = NULL, color = NULL) +
    theme_bw(base_family = FONT_FAMILY, base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, face = "bold"))

  p2 <- ggplot(df_bar, aes(x = 时期, y = 协议转让率, fill = 组别, group = 组别)) +
    geom_col(position = "dodge", width = 0.55, alpha = 0.85) +
    geom_line(aes(color = 组别), position = position_dodge(0.55),
              linewidth = 1, linetype = "solid") +
    geom_text(aes(label = 协议转让率_label),
              position = position_dodge(0.55), vjust = -0.2,
              size = 3, family = FONT_FAMILY) +
    scale_fill_manual(values  = c("#90A4AE","#1976D2")) +
    scale_color_manual(values = c("#546E7A","#0D47A1")) +
    scale_y_continuous(labels = scales::percent, limits = c(0, 1.15)) +
    labs(title = "（B）协议转让频率", x = NULL, y = "频率", fill = NULL, color = NULL) +
    theme_bw(base_family = FONT_FAMILY, base_size = 11) +
    theme(legend.position = "bottom",
          plot.title = element_text(size = 11, face = "bold"))

  p <- gridExtra::arrangeGrob(
    p1, p2, ncol = 2,
    top = grid::textGrob(
      "图M.3  政策前后各组的控制权获取模式变化（DID趋势图）",
      gp = grid::gpar(fontsize = 13, fontfamily = FONT_FAMILY, fontface = "bold")
    )
  )
  save_fig("figures/fig_m3_vow_trend.png", p, width = 10, height = 5.5)
  cat("  fig_m3_vow_trend.png 已生成\n")
}

# ── 图M.4  三重分组：协议转让×表决权让渡×科技并购 → CAR7箱线图 ──────────────

fig_m4_triple_boxplot <- function() {
  df_plot <- df_mech %>%
    filter(!is.na(car7_num), !is.na(agr), !is.na(vow)) %>%
    mutate(
      模式 = case_when(
        agr == 1 & vow == 1 ~ "协议+让渡\n（完整模式）",
        agr == 1 & vow == 0 ~ "协议+无让渡",
        agr == 0 & vow == 1 ~ "非协议+让渡",
        TRUE                ~ "非协议+无让渡"
      ),
      模式 = factor(模式, levels = c("非协议+无让渡","非协议+让渡",
                                      "协议+无让渡","协议+让渡\n（完整模式）")),
      是否科技 = factor(treat, labels = c("非科技并购","科技并购"))
    )

  # 各组样本量
  n_label <- df_plot %>%
    group_by(模式, 是否科技) %>%
    summarise(n = n(), med = median(car7_num, na.rm=TRUE), .groups = "drop") %>%
    mutate(label = paste0("n=", n))

  p <- ggplot(df_plot, aes(x = 模式, y = car7_num, fill = 是否科技)) +
    geom_boxplot(width = 0.5, outlier.size = 1.5, alpha = 0.75,
                 position = position_dodge(0.6)) +
    geom_text(data = n_label,
              aes(x = 模式, y = -30, label = label, group = 是否科技),
              position = position_dodge(0.6), size = 3,
              family = FONT_FAMILY, color = "grey40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("非科技并购" = "#B0BEC5", "科技并购" = "#EF5350")) +
    labs(
      title = "图M.4  四种控制权获取模式下的市场反应（CAR7）",
      subtitle = "横轴：协议转让×表决权让渡的四种组合模式",
      x = NULL, y = "CAR[-7,7]（%）", fill = NULL
    ) +
    theme_bw(base_family = FONT_FAMILY, base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40"),
      axis.text.x = element_text(size = 9)
    )

  save_fig("figures/fig_m4_triple_boxplot.png", ggplotGrob(p), width = 9, height = 5.5)
  cat("  fig_m4_triple_boxplot.png 已生成\n")
}

# ── 图M.5  贷款融资预留图（当前用资金来源分类替代，后期补充贷款数据后激活）───

fig_m5_loan_placeholder <- function() {
  # 当前用"资金来源"作为代理变量
  df_plot <- df_mech %>%
    filter(!is.na(car7_num), !is.na(fund_source)) %>%
    mutate(
      资金来源简 = case_when(
        grepl("银行贷款|配资", fund_source) ~ "杠杆融资\n（银行贷款/配资）",
        grepl("混合", fund_source)          ~ "混合融资",
        TRUE                                ~ "自有资金"
      ),
      资金来源简 = factor(资金来源简, levels = c("自有资金","混合融资","杠杆融资\n（银行贷款/配资）")),
      是否科技 = factor(treat, labels = c("非科技并购","科技并购"))
    )

  n_label <- df_plot %>%
    group_by(资金来源简, 是否科技) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(label = paste0("n=", n))

  p <- ggplot(df_plot, aes(x = 资金来源简, y = car7_num, fill = 是否科技)) +
    geom_boxplot(width = 0.5, alpha = 0.75, outlier.size = 1.5,
                 position = position_dodge(0.6)) +
    geom_text(data = n_label,
              aes(x = 资金来源简, y = -30, label = label, group = 是否科技),
              position = position_dodge(0.6), size = 3,
              family = FONT_FAMILY, color = "grey40") +
    geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +
    scale_fill_manual(values = c("非科技并购" = "#B0BEC5", "科技并购" = "#42A5F5")) +
    labs(
      title = "图M.5  融资结构与市场反应（预留：后期补充贷款数据后完善）",
      subtitle = "当前以资金来源分类作为代理；贷款比例数据补充后可替换为连续变量",
      x = "资金来源", y = "CAR[-7,7]（%）", fill = NULL
    ) +
    theme_bw(base_family = FONT_FAMILY, base_size = 12) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 12, face = "bold"),
      plot.subtitle = element_text(size = 9, color = "grey40")
    )

  save_fig("figures/fig_m5_loan_placeholder.png", ggplotGrob(p), width = 8, height = 5.5)
  cat("  fig_m5_loan_placeholder.png 已生成\n")
}

# ── 8. 一键生成全部机制图 ─────────────────────────────────────────────────────

generate_mechanism_figures <- function() {
  dir.create("figures", showWarnings = FALSE)
  cat("\n=== 生成机制分析图 ===\n")
  fig_m1_mechanism_heatmap()
  fig_m2_dual_track_vow()
  fig_m3_vow_trend()
  fig_m4_triple_boxplot()
  fig_m5_loan_placeholder()
  cat("\n全部5张机制图已生成至 figures/ 目录\n")
}

# ── 9. 贷款数据补充接口（后期使用）──────────────────────────────────────────

update_loan_data <- function(loan_df) {
  # 参数：loan_df 为补充的贷款数据框，需含 stock_code, loan_ratio_new 列
  # 用法：
  #   loan_data <- read.xlsx("贷款数据补充.xlsx")
  #   update_loan_data(loan_data)
  #
  # 激活后的分析：
  #   mE <- lm(car7_num ~ treat + post + treat_post +
  #              loan_ratio_new + treat*loan_ratio_new + post*loan_ratio_new +
  #              roe_num + lev_num + tobin_q_num + lgsize_num + tpct_num,
  #            data = df_mech_updated)
  cat("贷款数据接口已预留。补充数据后：\n")
  cat("  1. 用 left_join(df_mech, loan_df, by='stock_code') 合并\n")
  cat("  2. 激活 mE 贷款融资回归\n")
  cat("  3. 更新 fig_m5_loan_placeholder 为连续变量散点图\n")
}

# ── 运行 ─────────────────────────────────────────────────────────────────────

if (!interactive()) {
  generate_mechanism_figures()
} else {
  cat("\n✓ analysis_mechanism_did.R 已加载\n")
  cat("  运行 generate_mechanism_figures() 生成全部5张图\n")
  cat("  贷款数据接口：update_loan_data(loan_df)\n")
}
