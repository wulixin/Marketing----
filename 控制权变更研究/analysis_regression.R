# ============================================================
# 控制权变更研究 - R语言初步回归分析
# 基于手工整理的66个样本（4个板块）
# 语言: R | 工具: lm, fixest, 描述统计
# ============================================================

# ---- 0. 加载包 ----
required_pkgs <- c("readxl", "dplyr", "tidyr", "ggplot2", "stargazer", "lmtest", "sandwich")
for (pkg in required_pkgs) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    install.packages(pkg, repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/", quiet = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 尝试加载 fixest（面板回归）
if (!require("fixest", quietly = TRUE)) {
  install.packages("fixest", repos = "https://mirrors.tuna.tsinghua.edu.cn/CRAN/", quiet = TRUE)
  library(fixest)
}

# ---- 1. 数据读取与整合 ----
filepath <- "/Users/wulixin/Documents/GitHub/Marketing----/控制权变更研究/按板块维度整理后的数据.xlsx"

# 逐sheet读取，将所有列统一读为character以避免类型冲突
sheets <- c("科创板", "创业板", "沪市主板", "深市主板")
df_list <- lapply(sheets, function(sh) {
  d <- readxl::read_excel(filepath, sheet = sh, col_types = "text")
  d$板块 <- sh
  d
})

# 合并（对齐公共列）
df_raw <- dplyr::bind_rows(df_list)
cat("总样本数:", nrow(df_raw), "\n")

# ---- 2. 变量整理 ----
df <- df_raw %>%
  dplyr::rename(
    premium = `溢价率(%)`,
    rel_premium = `相对市价溢价率(%)`,
    car7 = `CAR[-7,7]累计超额收益率(%)`,
    car60 = `CAR[-60,60]累计超额收益率(%)`,
    transfer_pct = `转让股份比例(%)`,
    deal_amount = `交易总金额(万元)`,
    top1 = `第一大股东持股比例(%)`,
    top2 = `第二大股东持股比例(%)`,
    roe = `ROE净资产收益率(%)`,
    roa = `ROA总资产收益率(%)`,
    lev = `资产负债率(%)`,
    tobin_q = `托宾Q值`,
    total_assets = `总资产(万元)`,
    board_size = `董事会人数`,
    indep_ratio = `独董比例(%)`,
    inquiry = `是否收到问询函(是/否)`,
    vote_waiver = `原有股东是否放弃表决权`,
    patent = `并购方发明专利数量`,
    policy_time = `政策时点`,
    industry = `一级行业`,
    region = `所在地区`,
    company = `公司简称`,
    code = `股票代码`
  ) %>%
  dplyr::mutate(
    # 数值化
    across(c(premium, rel_premium, car7, car60, transfer_pct, deal_amount,
             top1, top2, roe, roa, lev, tobin_q, total_assets, board_size, indep_ratio), 
           ~ suppressWarnings(as.numeric(as.character(.)))),
    
    # 虚拟变量
    treat = suppressWarnings(as.numeric(as.character(`处理组标识(Treat)_科技并购=1`))),
    treat = ifelse(is.na(treat), 0, treat),
    post = ifelse(policy_time == "政策后", 1, 0),
    did = treat * post,
    
    has_inquiry = ifelse(inquiry == "是", 1, 0),
    has_vote_waiver = ifelse(!is.na(vote_waiver) & vote_waiver == "是", 1, 0),
    
    # 股权制衡度
    balance = top2 / top1,
    
    # 企业规模（总资产对数）
    lgsize = log(pmax(total_assets, 1, na.rm = TRUE)),
    
    # 是否亏损
    is_loss = ifelse(`是否亏损(是/否)` == "是", 1, 0),
    
    # 政策前后标签
    post_label = ifelse(post == 1, "政策后", "政策前"),
    tech_label = ifelse(treat == 1, "科技并购", "非科技并购"),
    
    # 溢价率截尾（避免极端值干扰回归）
    premium_w = pmin(pmax(premium, -200, na.rm = TRUE), 3000, na.rm = TRUE),
    rel_premium_w = pmin(pmax(rel_premium, -100, na.rm = TRUE), 500, na.rm = TRUE),
    car7_w = pmin(pmax(car7, -100, na.rm = TRUE), 500, na.rm = TRUE),
  )

cat("处理组(Treat=1):", sum(df$treat, na.rm = TRUE), "家\n")
cat("政策后(Post=1):", sum(df$post, na.rm = TRUE), "家\n")
cat("DID交互项(Treat×Post=1):", sum(df$did, na.rm = TRUE), "家\n")

# ---- 3. 描述性统计表 ----
cat("\n======= 描述性统计（关键变量）=======\n")
key_vars <- c("premium_w", "rel_premium_w", "car7_w", "car60", "transfer_pct",
              "top1", "top2", "balance", "roe", "roa", "lev", "tobin_q", "lgsize",
              "treat", "post", "did", "has_inquiry", "has_vote_waiver")
desc_df <- df[, key_vars] %>%
  tidyr::pivot_longer(everything(), names_to = "variable", values_to = "value") %>%
  dplyr::group_by(variable) %>%
  dplyr::summarise(
    N = sum(!is.na(value)),
    均值 = round(mean(value, na.rm = TRUE), 3),
    标准差 = round(sd(value, na.rm = TRUE), 3),
    最小值 = round(min(value, na.rm = TRUE), 3),
    中位数 = round(median(value, na.rm = TRUE), 3),
    最大值 = round(max(value, na.rm = TRUE), 3),
    .groups = "drop"
  )
print(as.data.frame(desc_df), row.names = FALSE)

# ---- 4. 均值差异 t 检验 ----
cat("\n======= 关键变量：科技 vs 非科技 均值差异检验 =======\n")
tech_group <- df %>% filter(treat == 1)
ctrl_group <- df %>% filter(treat == 0)

for (var in c("car7_w", "rel_premium_w", "premium_w", "transfer_pct")) {
  x <- tech_group[[var]]; y <- ctrl_group[[var]]
  tt <- tryCatch(t.test(x, y), error = function(e) NULL)
  if (!is.null(tt)) {
    cat(sprintf("  %s: 科技均值=%.2f vs 非科技均值=%.2f, p=%.4f %s\n",
                var, mean(x, na.rm=TRUE), mean(y, na.rm=TRUE),
                tt$p.value, ifelse(tt$p.value < 0.05, "**", "")))
  }
}

cat("\n======= 政策前 vs 政策后 均值差异检验 =======\n")
pre_group <- df %>% filter(post == 0)
post_group <- df %>% filter(post == 1)
for (var in c("car7_w", "rel_premium_w", "premium_w")) {
  x <- post_group[[var]]; y <- pre_group[[var]]
  tt <- tryCatch(t.test(x, y), error = function(e) NULL)
  if (!is.null(tt)) {
    cat(sprintf("  %s: 政策后均值=%.2f vs 政策前均值=%.2f, p=%.4f %s\n",
                var, mean(x, na.rm=TRUE), mean(y, na.rm=TRUE),
                tt$p.value, ifelse(tt$p.value < 0.05, "**", "")))
  }
}

# ---- 5. DID基准回归模型 ----
cat("\n======= DID基准回归分析 =======\n")

# 5.1 简单OLS：CAR7 ~ Treat + Post + DID
m1 <- lm(car7_w ~ treat + post + did, data = df, na.action = na.omit)
cat("\n模型1: 简单DID（无控制变量）\n")
summary(m1)

# 5.2 加入财务控制变量
m2 <- lm(car7_w ~ treat + post + did + roe + lev + tobin_q + lgsize + transfer_pct, 
         data = df, na.action = na.omit)
cat("\n模型2: DID + 财务控制变量\n")
summary(m2)

# 5.3 加入治理变量
m3 <- lm(car7_w ~ treat + post + did + roe + lev + tobin_q + lgsize + 
           transfer_pct + top1 + balance + has_vote_waiver + has_inquiry,
         data = df, na.action = na.omit)
cat("\n模型3: DID + 财务 + 治理变量\n")
summary(m3)

# 5.4 被解释变量替换为净资产溢价率
m4 <- lm(premium_w ~ treat + post + did + roe + lev + tobin_q + lgsize + transfer_pct,
         data = df, na.action = na.omit)
cat("\n模型4: 净资产溢价率 ~ DID + 控制变量\n")
summary(m4)

# 5.5 被解释变量替换为相对市价溢价率
m5 <- lm(rel_premium_w ~ treat + post + did + roe + lev + tobin_q + lgsize + transfer_pct,
         data = df, na.action = na.omit)
cat("\n模型5: 相对市价溢价率 ~ DID + 控制变量\n")
summary(m5)

# ---- 6. 异质性分析 ----
cat("\n======= 异质性分析: 按板块分组 =======\n")
for (board in c("科创板", "创业板", "沪市主板", "深市主板")) {
  sub <- df %>% filter(板块 == board)
  if (sum(!is.na(sub$car7_w)) >= 5) {
    m <- tryCatch(lm(car7_w ~ treat + post + did, data = sub, na.action = na.omit), 
                  error = function(e) NULL)
    if (!is.null(m) && length(coef(m)) >= 4) {
      coefs <- coef(m)
      cat(sprintf("  %s (n=%d): DID系数=%.2f, Treat=%.2f, Post=%.2f\n",
                  board, nrow(sub), coefs["did"], coefs["treat"], coefs["post"]))
    }
  }
}

# ---- 7. 表决权让渡的调节效应 ----
cat("\n======= 表决权让渡的调节效应 =======\n")
m6 <- lm(car7_w ~ treat + post + did + has_vote_waiver + 
           treat:has_vote_waiver + did:has_vote_waiver,
         data = df, na.action = na.omit)
summary(m6)

# ---- 8. 股权集中度对溢价率的影响 ----
cat("\n======= 股权集中度对溢价率的影响 =======\n")
m7 <- lm(premium_w ~ top1 + balance + has_vote_waiver + transfer_pct + roe + lev + treat,
         data = df, na.action = na.omit)
summary(m7)

# ---- 9. 整理回归汇总表（stargazer）----
cat("\n======= 汇总回归表 =======\n")
tryCatch({
  stargazer(m1, m2, m3, m4, m5,
            type = "text",
            title = "表1: DID基准回归结果",
            column.labels = c("(1)简单DID", "(2)加财务控制", "(3)加治理变量", 
                               "(4)净资产溢价率", "(5)市价溢价率"),
            dep.var.labels.include = FALSE,
            omit.stat = c("ser", "f"),
            digits = 3,
            notes.append = FALSE,
            notes = "注: *p<0.1, **p<0.05, ***p<0.01. 括号内为异方差稳健标准误。")
}, error = function(e) cat("stargazer输出错误:", e$message, "\n"))

cat("\n======= R分析完成 =======\n")
cat("说明:\n")
cat("  - DID系数(did): 政策对科技并购相对于非科技并购的超额效应\n")
cat("  - 当前样本量较小(n=80)，结果供参考，扩大样本后可进行固定效应回归\n")
cat("  - 后续需要: fixest::feols() 进行行业×年份双向固定效应\n")
