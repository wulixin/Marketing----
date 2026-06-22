# ============================================================================
# 控制权变更研究 - H2O AutoML 多模型建模与评估 (R版本) v2
# 目标变量:
#   (1) is_tech     — 二分类：是否科技并购
#   (2) car7        — 回归A: CAR[-7,7] 短窗口超额收益（公告效应）
#   (3) car60       — 回归B: CAR[-60,60] 长窗口超额收益(winsorize, 价值创造)
#   (4) premium_w   — 回归C: 相对市价溢价率%(winsorize, 控制权溢价)
# 特征变量: 财务指标 + 治理结构 + 交易特征（与ml_full_analysis.R保持一致）
# 模型清单: GBM / DRF / DeepLearning / GLM / Stacked Ensemble
# v2 变更: 用CAR[-60,60]和相对市价溢价率替代净资产溢价率，多目标回归
# ============================================================================

Sys.setenv(JAVA_HOME = "/Library/Java/JavaVirtualMachines/jdk-17.0.1.jdk/Contents/Home")

library(h2o)
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

# --------------------------------------------------------------------------
# 0. 中文字体（使用标准图形设备，避免showtext版本冲突）
# --------------------------------------------------------------------------
try({
  # macOS 系统中文字体
  font_family <- "STSong"
  # 检测可用字体
  if (require("showtext", quietly = TRUE)) {
    # showtext 可能导致 Graphics API mismatch，仅在需要时加载
  }
}, silent = TRUE)

setwd("/Users/wulixin/Documents/GitHub/Marketing----/控制权变更研究")

cat("============================================================\n")
cat("   控制权变更研究 - H2O 多模型建模与评估 (R版本)\n")
cat("============================================================\n\n")

# ============================================================================
# 1. H2O 初始化
# ============================================================================
cat("=== 1. 初始化 H2O 集群 ===\n")

# 关闭已有实例（如有），再重新启动（忽略无实例时的错误）
tryCatch(h2o.shutdown(prompt = FALSE), error = function(e) invisible(NULL))
Sys.sleep(2)

h2o_cluster <- h2o.init(
  nthreads  = -1,          # 使用全部CPU核心
  max_mem_size = "4G",     # 4GB内存上限
  port      = 54325,
  strict_version_check = FALSE
)

cat(sprintf("  H2O集群版本: %s\n", h2o.getVersion()))
# h2o.clusterInfo() 不同版本返回格式不同，用tryCatch避免崩溃
tryCatch({
  ci <- h2o.clusterInfo()
  cat(sprintf("  集群节点: %d\n", ci$total_nodes))
}, error = function(e) {
  cat("  集群信息: 已连接 (版本兼容性提示可忽略)\n")
})

# ============================================================================
# 2. 数据读取与预处理
# ============================================================================
cat("\n=== 2. 数据读取与预处理 ===\n")

filepath <- "按板块维度整理后的数据.xlsx"
sheets <- readxl::excel_sheets(filepath)

df_list <- lapply(sheets, function(sh) {
  readxl::read_excel(filepath, sheet = sh, col_types = "text")
})
df_all_raw <- bind_rows(df_list)
cat(sprintf("  合并后: %d行 × %d列\n", nrow(df_all_raw), ncol(df_all_raw)))

# 安全列名获取函数（与ml_full_analysis.R完全一致）
safe_get <- function(df, patterns, default = NA) {
  for (pat in patterns) {
    matches <- grep(pat, names(df), value = TRUE, ignore.case = TRUE, perl = FALSE)
    if (length(matches) > 0) return(df[[matches[1]]])
  }
  return(rep(default, nrow(df)))
}

# 构建分析用数据框
df <- data.frame(
  公司简称   = safe_get(df_all_raw, "公司简称", ""),
  市场类型   = safe_get(df_all_raw, "市场类型", ""),
  一级行业   = safe_get(df_all_raw, "一级行业", ""),
  # ---------- 目标变量 ----------
  is_tech     = as.numeric(safe_get(df_all_raw, c("处理组标识","Treat","科技并购=1"), 0)),
  is_post     = ifelse(as.character(safe_get(df_all_raw, c("Post","政策后","时间标识"), "")) == "1" |
                       as.character(safe_get(df_all_raw, c("政策时点"), "")) == "政策后", 1, 0),
  car7        = as.numeric(safe_get(df_all_raw, c("CAR.*-7","\\[-7,7\\]"))),
  car60       = as.numeric(safe_get(df_all_raw, c("CAR.*-60","\\[-60,60\\]"))),
  bh          = as.numeric(safe_get(df_all_raw, c("BHAR买入持有"))),
  premium_w   = as.numeric(safe_get(df_all_raw, c("相对市价溢价率"))),
  # ---------- 财务特征 ----------
  roe         = as.numeric(safe_get(df_all_raw, c("ROE净资产收益率"))),
  roa         = as.numeric(safe_get(df_all_raw, c("ROA总资产收益率"))),
  lev         = as.numeric(safe_get(df_all_raw, c("资产负债率"))),
  current_ratio = as.numeric(safe_get(df_all_raw, c("流动比率"))),
  tobin_q     = as.numeric(safe_get(df_all_raw, c("托宾Q"))),
  bm          = as.numeric(safe_get(df_all_raw, c("账面市值比"))),
  ocf_ratio   = as.numeric(safe_get(df_all_raw, c("经营现金流"))),
  rev_growth  = as.numeric(safe_get(df_all_raw, c("营收增长率"))),
  profit_growth = as.numeric(safe_get(df_all_raw, c("净利润增长率"))),
  is_loss     = ifelse(as.character(safe_get(df_all_raw, c("是否亏损"))) == "是", 1, 0),
  lgsize      = as.numeric(safe_get(df_all_raw, c("总资产对数"))),
  # ---------- 治理特征 ----------
  top1        = as.numeric(safe_get(df_all_raw, c("第一大股东持股"))),
  top2        = as.numeric(safe_get(df_all_raw, c("第二大股东持股"))),
  balance_ratio = as.numeric(safe_get(df_all_raw, c("股权制衡度"))),
  multi_large = ifelse(as.character(safe_get(df_all_raw, c("多个大股东"))) == "是", 1, 0),
  sep_two     = as.numeric(safe_get(df_all_raw, c("两权分离度"))),
  board_size  = as.numeric(safe_get(df_all_raw, c("董事会人数"))),
  indep_ratio = as.numeric(safe_get(df_all_raw, c("独董比例"))),
  duality     = ifelse(as.character(safe_get(df_all_raw, c("两职合一"))) == "是", 1, 0),
  mgmt_hold   = ifelse(as.character(safe_get(df_all_raw, c("管理层是否持股"))) == "是", 1, 0),
  nom_nonexec = as.numeric(safe_get(df_all_raw, c("提名非独立董事数"))),
  nom_indep   = as.numeric(safe_get(df_all_raw, c("提名独立董事数"))),
  # ---------- 交易特征 ----------
  transfer_pct  = as.numeric(safe_get(df_all_raw, c("转让股份比例"))),
  deal_value    = as.numeric(safe_get(df_all_raw, c("交易总金额"))),
  lock_period   = as.numeric(safe_get(df_all_raw, c("锁定期限"))),
  has_vow       = ifelse(as.character(safe_get(df_all_raw, c("业绩承诺"))) == "是", 1, 0),
  has_lock      = ifelse(as.character(safe_get(df_all_raw, c("股权锁定承诺"))) == "是", 1, 0),
  waive_vote    = ifelse(as.character(safe_get(df_all_raw, c("原有股东是否放弃表决权"))) %in% c("是","1"), 1, 0),
  waive_ratio   = as.numeric(safe_get(df_all_raw, c("放弃表达权比例"))),
  # ---------- 监管与市场 ----------
  inquiry       = ifelse(as.character(safe_get(df_all_raw, c("问询函"))) == "是", 1, 0),
  inst_hold     = as.numeric(safe_get(df_all_raw, c("机构持股比例"))),
  media_attn    = as.numeric(safe_get(df_all_raw, c("媒体关注度"))),
  analyst       = as.numeric(safe_get(df_all_raw, c("分析师跟踪"))),
  total_days    = as.numeric(safe_get(df_all_raw, c("总共用时"))),
  abnormal_mvmt = as.numeric(safe_get(df_all_raw, c("事件异常波动次数"))),
  stringsAsFactors = FALSE
)

# 清理无穷值
df[df == Inf | df == -Inf] <- NA

# DID 交互项
df$did <- df$is_tech * df$is_post

cat(sprintf("  有效样本: %d\n", nrow(df)))

# ============================================================================
# 3. 特征工程
# ============================================================================
cat("\n=== 3. 特征工程 ===\n")

# ---------- Winsorize 处理（1%/99% 去极端值）----------
winsorize_01 <- function(x, lo = 0.01, hi = 0.99) {
  qx <- quantile(x, probs = c(lo, hi), na.rm = TRUE)
  pmin(pmax(x, qx[1]), qx[2])
}

# 对长窗口变量和溢价率做 winsorize
cat("  [Winsorize 1%/99%] 对 CAR[-60,60], BHAR, 相对市价溢价率 去极值...\n")
df$car60_w     <- winsorize_01(df$car60)
df$bh_w        <- winsorize_01(df$bh)
df$premium_w_w <- winsorize_01(df$premium_w)

cat(sprintf("    car60:   raw range=[%.1f,%.1f] -> win=[%.1f,%.1f]\n",
            min(df$car60,na.rm=T), max(df$car60,na.rm=T),
            min(df$car60_w,na.rm=T), max(df$car60_w,na.rm=T)))
cat(sprintf("    bh:      raw range=[%.1f,%.1f] -> win=[%.1f,%.1f]\n",
            min(df$bh,na.rm=T), max(df$bh,na.rm=T),
            min(df$bh_w,na.rm=T), max(df$bh_w,na.rm=T)))
cat(sprintf("    prem_w:  raw range=[%.1f,%.1f] -> win=[%.1f,%.1f]\n",
            min(df$premium_w,na.rm=T), max(df$premium_w,na.rm=T),
            min(df$premium_w_w,na.rm=T), max(df$premium_w_w,na.rm=T)))

# 核心分类特征（预测 is_tech）
class_features <- c(
  "roe","roa","lev","current_ratio","tobin_q","bm",
  "ocf_ratio","rev_growth","profit_growth","is_loss","lgsize",
  "top1","top2","balance_ratio","multi_large","sep_two",
  "board_size","indep_ratio","duality","mgmt_hold",
  "nom_nonexec","nom_indep",
  "transfer_pct","lock_period","has_vow","has_lock",
  "waive_vote","waive_ratio","inst_hold","inquiry"
)

# 核心回归特征（预测 car7）
reg_features <- c(
  "is_tech","is_post","did",
  "roe","lev","tobin_q","lgsize",
  "top1","balance_ratio","transfer_pct",
  "waive_vote","lock_period","has_vow","inst_hold",
  "inquiry","media_attn"
)

# ---------- 任务A：分类 ----------
df_class <- df[, c("is_tech", class_features), drop = FALSE]
df_class <- df_class[complete.cases(df_class[, class_features[1:10]]), ]
df_class$is_tech <- as.factor(df_class$is_tech)

# 对NA列做中位数填补
for (v in class_features) {
  if (is.numeric(df_class[[v]])) {
    med_v <- median(df_class[[v]], na.rm = TRUE)
    df_class[[v]][is.na(df_class[[v]])] <- ifelse(is.na(med_v), 0, med_v)
  } else {
    df_class[[v]][is.na(df_class[[v]])] <- 0
  }
}

cat(sprintf("  [分类任务] 样本=%d, 科技并购=%d, 非科技=%d\n",
            nrow(df_class),
            sum(df_class$is_tech == 1),
            sum(df_class$is_tech == 0)))

# ---------- 任务B：多目标回归 ----------
# 定义4个回归目标（含原始和winsorized版本）
reg_targets <- c(
  car7        = "CAR[-7,7] 短窗口超额收益(%)",
  car60_w     = "CAR[-60,60] 长窗口超额收益(winsorize)",
  bh_w        = "BHAR买入持有超额收益(winsorize)",
  premium_w_w = "相对市价溢价率(winsorize)"
)

# 存储所有回归任务的H2O数据和结果
reg_h2o_list   <- list()
reg_split_list <- list()
reg_aml_list   <- list()
reg_perf_list  <- list()
reg_lb_list    <- list()

for (target_name in names(reg_targets)) {
  target_col <- reg_targets[[target_name]]

  # 构建该目标的回归数据集
  reg_cols <- c(target_name, reg_features)
  df_reg_i <- df[, reg_cols, drop = FALSE]
  df_reg_i <- df_reg_i[!is.na(df_reg_i[[target_name]]), ]

  # 中位数填补特征缺失
  for (v in reg_features) {
    if (v %in% names(df_reg_i) && is.numeric(df_reg_i[[v]])) {
      med_v <- median(df_reg_i[[v]], na.rm = TRUE)
      df_reg_i[[v]][is.na(df_reg_i[[v]])] <- ifelse(is.na(med_v), 0, med_v)
    }
  }

  # 上传至 H2O
  h_reg_i <- as.h2o(df_reg_i)

  # 训练/测试切割
  set.seed(42)
  split_ri <- h2o.splitFrame(h_reg_i, ratios = 0.8, seed = 42)
  train_ri <- split_ri[[1]]; test_ri <- split_ri[[2]]

  cat(sprintf("  [回归-%s] 样本=%d, 训练=%d, 测试=%d, 范围=[%.1f, %.1f]\n",
              target_name, nrow(df_reg_i), nrow(train_ri), nrow(test_ri),
              min(df_reg_i[[target_name]], na.rm=TRUE), max(df_reg_i[[target_name]], na.rm=TRUE)))

  # AutoML
  aml_i <- h2o.automl(
    x                   = reg_features,
    y                   = target_name,
    training_frame      = train_ri,
    validation_frame    = test_ri,
    max_models          = 10,
    seed                = 42,
    sort_metric         = "RMSE",
    stopping_metric     = "RMSE",
    stopping_rounds     = 3,
    stopping_tolerance  = 0.01,
    include_algos       = c("GBM","DRF","DeepLearning","GLM","StackedEnsemble"),
    nfolds              = 3,
    keep_cross_validation_predictions = TRUE,
    project_name        = paste0("ctrl_change_reg_", target_name)
  )

  lb_i <- as.data.frame(aml_i@leaderboard)
  cat(sprintf("    最佳模型: %s\n", lb_i$model_id[1]))

  # 存储
  reg_h2o_list[[target_name]]   <- h_reg_i
  reg_split_list[[target_name]] <- list(train = train_ri, test = test_ri)
  reg_aml_list[[target_name]]   <- aml_i
  reg_lb_list[[target_name]]    <- lb_i
}


# ============================================================================
# 4. 上传数据至 H2O
# ============================================================================
cat("\n=== 4. 上传数据至 H2O ===\n")

h_class <- as.h2o(df_class)

cat(sprintf("  H2O Frame [分类]: %d×%d\n", nrow(h_class), ncol(h_class)))

# 训练/测试集切割（80/20）— 分类任务
set.seed(42)
split_c <- h2o.splitFrame(h_class, ratios = 0.8, seed = 42)
train_c <- split_c[[1]]; test_c <- split_c[[2]]

cat(sprintf("  分类训练集: %d  测试集: %d\n", nrow(train_c), nrow(test_c)))
cat("  回归任务: 在第3步特征工程中已定义4个目标，将在AutoML阶段分别处理\n")


# ============================================================================
# 5. H2O AutoML 多模型训练
# ============================================================================
cat("\n=== 5. H2O AutoML 多模型训练 ===\n")

# ---------------------------- 5A. 分类 AutoML ---------------------------
cat("\n--- 5A. AutoML 分类（is_tech）---\n")
aml_class <- h2o.automl(
  x                   = class_features,
  y                   = "is_tech",
  training_frame      = train_c,
  validation_frame    = test_c,
  max_models          = 10,            # 最多10个候选模型（小样本降低）
  seed                = 42,
  sort_metric         = "AUC",
  stopping_metric     = "AUC",
  stopping_rounds     = 3,
  stopping_tolerance  = 0.01,          # 放宽早停容忍度
  include_algos       = c("GBM","DRF","DeepLearning","GLM","StackedEnsemble"),
  nfolds              = 3,              # 减少折数加速
  keep_cross_validation_predictions = TRUE,
  project_name        = "ctrl_change_class"
)

lb_class <- as.data.frame(aml_class@leaderboard)
cat("分类 AutoML 排行榜 (Top10):\n")
print(head(lb_class[, c("model_id","auc","logloss","mean_per_class_error")], 10),
      row.names = FALSE)

# ---------------------------- 5B. 回归 AutoML（多目标）---------------------------
cat("\n--- 5B. AutoML 回归（多目标: car7 / car60_w / bh_w / premium_w_w）---\n")
# 回归任务已在步骤3的特征工程阶段完成AutoML训练和存储


# ============================================================================
# 6. 模型评估与结果提取
# ============================================================================
cat("\n=== 6. 模型评估 ===\n")

# ---- 6A. 分类性能（在测试集上重新评估）----
best_class <- aml_class@leader

perf_c <- h2o.performance(best_class, newdata = test_c)
cat(sprintf("\n[最佳分类模型] %s\n", best_class@model_id))
cat(sprintf("  AUC:                %.4f\n", h2o.auc(perf_c)))
cat(sprintf("  对数损失 (LogLoss): %.4f\n", h2o.logloss(perf_c)))
cat(sprintf("  混淆矩阵:\n"))
print(h2o.confusionMatrix(perf_c))

# ---- 6B. 回归性能（多目标汇总）----
cat("\n[回归任务 — 多目标性能]\n")

for (target_name in names(reg_targets)) {
  aml_i   <- reg_aml_list[[target_name]]
  split_i <- reg_split_list[[target_name]]
  lb_i    <- reg_lb_list[[target_name]]

  if (is.null(aml_i)) next

  best_reg_i <- aml_i@leader
  test_i     <- split_i$test

  perf_ri <- tryCatch(h2o.performance(best_reg_i, newdata = test_i), error = function(e) NULL)
  if (is.null(perf_ri)) next

  cat(sprintf("\n  [目标: %s] 最佳模型: %s\n", target_name, best_reg_i@model_id))
  cat(sprintf("    RMSE  = %.4f\n", h2o.rmse(perf_ri)))
  cat(sprintf("    MAE   = %.4f\n", h2o.mae(perf_ri)))
  cat(sprintf("    R²    = %.4f\n", h2o.r2(perf_ri)))

  # 收集全部模型的测试集指标
  model_perf_ri <- lapply(head(lb_i$model_id, 10), function(mid) {
    m <- tryCatch(h2o.getModel(mid), error = function(e) NULL)
    if (is.null(m)) return(NULL)
    p <- tryCatch(h2o.performance(m, newdata = test_i), error = function(e) NULL)
    if (is.null(p)) return(NULL)
    data.frame(
      target     = target_name,
      model_id   = mid,
      model_type = strsplit(mid, "_")[[1]][1],
      rmse       = tryCatch(h2o.rmse(p), error = function(e) NA),
      mae        = tryCatch(h2o.mae(p),  error = function(e) NA),
      r2         = tryCatch(h2o.r2(p),   error = function(e) NA),
      stringsAsFactors = FALSE
    )
  })
  reg_perf_list[[target_name]] <- do.call(rbind, Filter(Negate(is.null), model_perf_ri))

  cat(sprintf("    模型数: %d\n", nrow(reg_perf_list[[target_name]])))
}

# ---- 6C. 分类模型性能提取（全部模型测试集指标）----
model_perf_class <- lapply(head(lb_class$model_id, 10), function(mid) {
  m <- tryCatch(h2o.getModel(mid), error = function(e) NULL)
  if (is.null(m)) return(NULL)
  p <- tryCatch(h2o.performance(m, newdata = test_c), error = function(e) NULL)
  if (is.null(p)) return(NULL)
  data.frame(
    model_id  = mid,
    model_type = strsplit(mid, "_")[[1]][1],
    auc       = tryCatch(h2o.auc(p),     error = function(e) NA),
    logloss   = tryCatch(h2o.logloss(p), error = function(e) NA),
    stringsAsFactors = FALSE
  )
})
df_perf_class <- do.call(rbind, Filter(Negate(is.null), model_perf_class))

# ---- 6D. 全部模型性能汇总与保存 ----
# 合并所有回归目标的性能表
df_perf_reg_all <- do.call(rbind, reg_perf_list)

cat("\n分类模型性能汇总 (测试集):\n")
print(df_perf_class[!is.na(df_perf_class$auc), ], row.names = FALSE)

cat("\n回归模型性能汇总 (测试集, 多目标):\n")
print(df_perf_reg_all[!is.na(df_perf_reg_all$rmse), c("target","model_id","model_type","rmse","mae","r2")], row.names = FALSE)

# 保存CSV
write.csv(df_perf_class,    "h2o_class_model_perf.csv", row.names = FALSE, fileEncoding = "UTF-8")
write.csv(df_perf_reg_all, "h2o_reg_model_perf.csv",   row.names = FALSE, fileEncoding = "UTF-8")
cat("  ✓ 性能表已保存\n")


# ============================================================================
# 7. 变量重要性
# ============================================================================
cat("\n=== 7. 变量重要性 ===\n")

extract_varimp <- function(model, top_n = 15) {
  vi <- tryCatch(h2o.varimp(model), error = function(e) NULL)
  if (!is.null(vi)) {
    df_vi <- as.data.frame(vi)
    if ("variable" %in% names(df_vi) && "relative_importance" %in% names(df_vi)) {
      return(head(df_vi[order(-df_vi$relative_importance), ], top_n))
    }
    if ("names" %in% names(df_vi)) {
      names(df_vi)[names(df_vi) == "names"] <- "variable"
      names(df_vi)[names(df_vi) == "coefficients"] <- "relative_importance"
      return(head(df_vi[order(-df_vi$relative_importance), ], top_n))
    }
  }
  return(NULL)
}

vi_class <- extract_varimp(best_class)

var_labels <- c(
  roe = "ROE净资产收益率", roa = "ROA总资产收益率",
  lev = "资产负债率", tobin_q = "托宾Q", lgsize = "公司规模(lnA)",
  top1 = "第一大股东持股", balance_ratio = "股权制衡度",
  transfer_pct = "转让股份比例", waive_vote = "表决权放弃",
  lock_period = "锁定期限", has_vow = "业绩承诺",
  inst_hold = "机构持股比例", inquiry = "问询函",
  did = "DID交互项", is_tech = "科技并购", is_post = "政策后",
  indep_ratio = "独董比例", board_size = "董事会规模",
  current_ratio = "流动比率", sep_two = "两权分离度",
  nom_nonexec = "提名非独董数", nom_indep = "提名独董数",
  media_attn = "媒体关注度", rev_growth = "营收增长率"
)

format_var_label <- function(vars) {
  ifelse(vars %in% names(var_labels), var_labels[vars], vars)
}
cat("分类任务 Top变量重要性:\n")
if (!is.null(vi_class)) print(vi_class[, c("variable","relative_importance","scaled_importance")],
                               row.names = FALSE)

# 对每个回归目标提取最佳模型的变量重要性
vi_reg_list <- list()
for (target_name in names(reg_targets)) {
  aml_i <- reg_aml_list[[target_name]]
  if (is.null(aml_i)) next
  best_ri <- aml_i@leader
  vi_ri <- extract_varimp(best_ri)
  vi_reg_list[[target_name]] <- vi_ri
  cat(sprintf("\n回归任务[%s] Top变量重要性:\n", target_name))
  if (!is.null(vi_ri)) print(vi_ri[, c("variable","relative_importance","scaled_importance")],
                                row.names = FALSE)
}


# ============================================================================
# 8. 可视化
# ============================================================================
cat("\n=== 8. 可视化 ===\n")

theme_h2o <- theme_minimal(base_size = 11) +
  theme(
    plot.title      = element_text(hjust = 0.5, face = "bold", size = 13),
    plot.subtitle   = element_text(hjust = 0.5, color = "gray50"),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    text            = element_text(family = "STSong")  # macOS 中文字体
  )


# 辅助函数：缩短模型ID显示名
shorten_model_id <- function(model_id_str) {
  parts <- strsplit(as.character(model_id_str), "_")[[1]]
  paste0(parts[1], "(", paste(parts[2:min(3,length(parts))], collapse="-"), ")")
}

# 辅助函数：用标准图形设备保存图片（避免ggsave的Graphics API版本问题）
save_plot <- function(plot_obj, filename, width = 10, height = 6, dpi = 150) {
  tryCatch({
    png(filename, width = width * dpi, height = height * dpi, res = dpi)
    print(plot_obj)
    dev.off()
    cat(sprintf("  ✓ %s\n", filename))
  }, error = function(e) {
    cat(sprintf("  ✗ %s 保存失败: %s\n", filename, e$message))
  })
}

# ---- 8.1 分类模型 AUC 对比 ----
if (!is.null(df_perf_class) && nrow(df_perf_class) > 0) {
  df_c_plot <- df_perf_class[!is.na(df_perf_class$auc), ]
  df_c_plot$short_name <- sub("_.*", "", df_c_plot$model_id)
  # 缩短模型ID显示
  df_c_plot$label <- paste0(df_c_plot$short_name, "\n", sprintf("%.3f", df_c_plot$auc))
  df_c_plot <- df_c_plot[order(-df_c_plot$auc), ]
  df_c_plot$model_id <- factor(df_c_plot$model_id, levels = df_c_plot$model_id)

  p_auc <- ggplot(df_c_plot, aes(x = model_id, y = auc, fill = model_type)) +
    geom_bar(stat = "identity", width = 0.7, alpha = 0.85) +
    geom_text(aes(label = sprintf("%.3f", auc)), vjust = -0.4, size = 3) +
    scale_fill_brewer(palette = "Dark2", name = "算法类型") +
    coord_cartesian(ylim = c(max(0, min(df_c_plot$auc) - 0.1), 1.05)) +
    scale_x_discrete(labels = function(x) sapply(x, shorten_model_id)) +
    labs(title = "H2O AutoML — 分类模型 AUC (测试集)",
         subtitle = "目标: 是否科技并购",
         x = "", y = "AUC") +
    theme_h2o +
    theme(axis.text.x = element_text(size = 7, angle = 20, hjust = 1))

  save_plot(p_auc, "h2o_class_auc.png", width = 10, height = 5.5)
}


# ---- 8.2 回归模型 RMSE / R² 对比（多目标分面图）----
if (!is.null(df_perf_reg_all) && nrow(df_perf_reg_all) > 0) {
  # 按目标变量分组绘制
  df_r_all <- df_perf_reg_all[!is.na(df_perf_reg_all$rmse), ]

  # RMSE 分面图（按回归目标分组）
  df_r_all$target_label <- factor(
    ifelse(df_r_all$target == "car7",        "CAR[-7,7]",
    ifelse(df_r_all$target == "car60_w",     "CAR[-60,60](w)",
    ifelse(df_r_all$target == "bh_w",        "BHAR(w)",
                                       "Premium(w)"))),
    levels = c("CAR[-7,7]", "CAR[-60,60](w)", "BHAR(w)", "Premium(w)")
  )
  df_r_all$model_id_short <- sapply(df_r_all$model_id, shorten_model_id)

  p_rmse <- ggplot(df_r_all, aes(x = reorder(model_id_short, rmse), y = rmse, fill = model_type)) +
    geom_bar(stat = "identity", width = 0.65, alpha = 0.85) +
    scale_fill_brewer(palette = "Set2", name = "算法类型") +
    facet_wrap(~ target_label, scales = "free_y", ncol = 2) +
    labs(title = "H2O AutoML — 多目标回归 RMSE (测试集)",
         x = "", y = "RMSE (越低越好)") +
    theme_h2o +
    theme(axis.text.x = element_text(size = 6, angle = 30, hjust = 1))

  save_plot(p_rmse, "h2o_reg_rmse.png", width = 14, height = 9)

  # R² 分面图（按回归目标分组）
  df_r2_all <- df_r_all[!is.na(df_r_all$r2), ]
  if (nrow(df_r2_all) > 0) {
    p_r2 <- ggplot(df_r2_all, aes(x = reorder(model_id_short, -r2), y = r2, fill = model_type)) +
      geom_bar(stat = "identity", width = 0.65, alpha = 0.85) +
      scale_fill_brewer(palette = "Set2", name = "算法类型") +
      coord_cartesian(ylim = c(min(-0.1, min(df_r2_all$r2) - 0.05), 1.05)) +
      facet_wrap(~ target_label, scales = "free_y", ncol = 2) +
      labs(title = "H2O AutoML — 多目标回归 R² (测试集)",
           x = "", y = "R² (越高越好)") +
      theme_h2o +
      theme(axis.text.x = element_text(size = 6, angle = 30, hjust = 1))

    save_plot(p_r2, "h2o_reg_r2.png", width = 14, height = 9)
  }
}


# ---- 8.3 变量重要性图（分类 + 4个回归目标）----
make_varimp_plot <- function(vi_df, title, palette = "Reds") {
  if (is.null(vi_df) || nrow(vi_df) == 0) return(NULL)
  vi_df <- vi_df[order(-vi_df$scaled_importance), ]
  vi_df$label <- format_var_label(vi_df$variable)
  vi_df$label <- factor(vi_df$label, levels = rev(vi_df$label))

  ggplot(vi_df, aes(x = label, y = scaled_importance * 100)) +
    geom_bar(stat = "identity", fill = "#E74C3C", alpha = 0.85, width = 0.7) +
    geom_text(aes(label = sprintf("%.1f", scaled_importance * 100)),
              hjust = -0.1, size = 3) +
    coord_flip(clip = "off") +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    labs(title = title, x = "", y = "归一化重要性 (%)") +
    theme_h2o
}

p_vi_c <- make_varimp_plot(vi_class, "变量重要性 — 分类(科技并购识别)")

# 取第一个回归目标的变量重要性作为主要展示（通常car7最标准）
reg_target_names <- names(reg_targets)
vi_reg_main <- if (length(reg_target_names) > 0 && !is.null(vi_reg_list[[reg_target_names[1]]])) {
  vi_reg_list[[reg_target_names[1]]]
} else { NULL }

p_vi_r <- make_varimp_plot(vi_reg_main,
  paste0("变量重要性 — 回归(", reg_targets[[reg_target_names[1]]], ")"))

if (!is.null(p_vi_c)) save_plot(p_vi_c, "h2o_varimp_class.png", width = 8, height = 6)

if (!is.null(p_vi_r)) {
  # 尝试生成多目标组合图：分类 + car7 回归
  tryCatch({
    png("h2o_varimp_combined.png", width = 14 * 150, height = 7 * 150, res = 150)
    gridExtra::grid.arrange(p_vi_c, p_vi_r, ncol = 2)
    dev.off()
    cat("  ✓ h2o_varimp_combined.png\n")
  }, error = function(e) {
    cat(sprintf("  ✗ 组合图保存失败: %s\n", e$message))
    save_plot(p_vi_r, "h2o_varimp_reg.png", width = 8, height = 6)
  })
} else if (!is.null(p_vi_c)) {
  save_plot(p_vi_c, "h2o_varimp_combined.png", width = 8, height = 6)
}


# ---- 8.4 预测值 vs 实际值（回归任务，以CAR7为主展示）----
main_reg_target <- names(reg_targets)[1]  # car7
if (main_reg_target %in% names(reg_split_list)) {
  split_main <- reg_split_list[[main_reg_target]]
  aml_main   <- reg_aml_list[[main_reg_target]]
  if (!is.null(aml_main) && !is.null(split_main)) {
    test_main <- split_main$test
    best_r_main <- aml_main@leader
    pred_r <- as.data.frame(h2o.predict(best_r_main, newdata = test_main))
    actual_r <- as.data.frame(test_main[, main_reg_target])
    pred_actual <- data.frame(
      actual = actual_r[[1]],
      pred   = pred_r[[1]]
    )
    r2_test <- cor(pred_actual$actual, pred_actual$pred, use = "complete.obs")^2

    p_pred_vs_actual <- ggplot(pred_actual, aes(x = actual, y = pred)) +
      geom_point(alpha = 0.6, color = "#2980B9", size = 2.5) +
      geom_abline(slope = 1, intercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      geom_smooth(method = "lm", se = TRUE, color = "#E74C3C", fill = "#FADBD8", alpha = 0.3) +
      labs(title = "H2O 最佳回归模型: 预测值 vs 实际值",
           subtitle = sprintf("测试集 R²=%.3f | 目标: %s | 模型: %s",
                              r2_test, reg_targets[[main_reg_target]],
                              strsplit(best_r_main@model_id, "_")[[1]][1]),
           x = sprintf("实际 %s (%)", reg_targets[[main_reg_target]]),
           y = sprintf("预测 %s (%)", reg_targets[[main_reg_target]])) +
      theme_h2o

    save_plot(p_pred_vs_actual, "h2o_pred_vs_actual.png", width = 7, height = 6)
  }
}


# ---- 8.5 ROC曲线（分类任务）----
roc_data <- tryCatch({
  perf_roc <- h2o.performance(best_class, newdata = test_c)
  fp_tp    <- as.data.frame(h2o.fpr(perf_roc))[, 1:2]
  tpr_data <- as.data.frame(h2o.tpr(perf_roc))[, 1:2]
  data.frame(fpr = fp_tp[[1]], tpr = tpr_data[[1]])
}, error = function(e) NULL)

if (!is.null(roc_data)) {
  auc_val <- h2o.auc(h2o.performance(best_class, newdata = test_c))
  p_roc <- ggplot(roc_data, aes(x = fpr, y = tpr)) +
    geom_line(color = "#E74C3C", linewidth = 1.2) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray60") +
    geom_area(fill = "#FADBD8", alpha = 0.25) +
    annotate("text", x = 0.65, y = 0.15,
             label = sprintf("AUC = %.3f", auc_val),
             size = 5, color = "#E74C3C", fontface = "bold") +
    labs(title = "ROC 曲线 — 科技并购分类",
         subtitle = sprintf("最佳模型: %s", strsplit(best_class@model_id, "_")[[1]][1]),
         x = "假阳性率 (FPR)", y = "真阳性率 (TPR)") +
    theme_h2o

  save_plot(p_roc, "h2o_roc_curve.png", width = 6.5, height = 6)
}


# ---- 8.6 H2O AutoML 排行榜气泡图（综合视图）----
if (!is.null(df_perf_class) && nrow(df_perf_class) >= 3) {
  df_bubble <- df_perf_class[!is.na(df_perf_class$auc) & !is.na(df_perf_class$logloss), ]
  df_bubble$label <- sub("_AutoML.*", "", df_bubble$model_id)

  p_bubble <- ggplot(df_bubble, aes(x = logloss, y = auc,
                                     color = model_type, size = auc,
                                     label = sub("_.*","",label))) +
    geom_point(alpha = 0.75) +
    geom_text(nudge_y = 0.008, size = 3) +
    scale_color_brewer(palette = "Dark2", name = "算法") +
    scale_size_continuous(range = c(4, 10), guide = "none") +
    labs(title = "H2O AutoML 模型全景 (分类)",
         x = "LogLoss (越低越好)", y = "AUC (越高越好)",
         subtitle = "气泡大小 = AUC") +
    theme_h2o

  save_plot(p_bubble, "h2o_bubble_leaderboard.png", width = 8, height = 6)
}


# ============================================================================
# 9. 保存最佳模型 (MOJO格式，可部署)
# ============================================================================
cat("\n=== 9. 保存最佳模型 ===\n")

mojo_dir <- "h2o_models"
dir.create(mojo_dir, showWarnings = FALSE)

# 保存分类最佳模型
mojo_path_c <- tryCatch(
  h2o.save_mojo(best_class, path = mojo_dir, force = TRUE),
  error = function(e) {
    cat("  MOJO不支持该模型类型, 使用binary保存...\n")
    h2o.saveModel(best_class, path = mojo_dir, force = TRUE)
  }
)
cat(sprintf("  ✓ 分类最佳模型保存: %s\n", mojo_dir))

# 保存每个回归目标的最佳模型
for (target_name in names(reg_targets)) {
  aml_i <- reg_aml_list[[target_name]]
  if (is.null(aml_i)) next
  best_ri <- aml_i@leader
  tryCatch({
    h2o.save_mojo(best_ri, path = mojo_dir, force = TRUE)
  }, error = function(e) {
    h2o.saveModel(best_ri, path = mojo_dir, force = TRUE)
  })
  cat(sprintf("  ✓ 回归[%s]最佳模型保存: %s\n", target_name, mojo_dir))
}


# ============================================================================
# 10. 综合摘要
# ============================================================================
cat("\n")
cat("=============================================================\n")
cat("          H2O AutoML 分析结果摘要\n")
cat("=============================================================\n")

cat(sprintf("\n[分类任务] 最佳模型: %s\n", best_class@model_id))
cat(sprintf("  测试集 AUC   = %.4f\n", h2o.auc(perf_c)))
cat(sprintf("  测试集 LogLoss = %.4f\n", h2o.logloss(perf_c)))

cat("\n[回归任务 — 多目标汇总]\n")
for (target_name in names(reg_targets)) {
  aml_i   <- reg_aml_list[[target_name]]
  split_i <- reg_split_list[[target_name]]
  if (is.null(aml_i) || is.null(split_i)) next
  best_ri <- aml_i@leader
  test_i  <- split_i$test
  perf_ri <- tryCatch(h2o.performance(best_ri, newdata = test_i), error = function(e) NULL)
  if (is.null(perf_ri)) next
  cat(sprintf("  [%s] 最佳=%s | RMSE=%.2f, R²=%.3f, MAE=%.2f\n",
              target_name, best_ri@model_id,
              h2o.rmse(perf_ri), h2o.r2(perf_ri), h2o.mae(perf_ri)))
}

cat("\n[变量重要性 Top3]\n")
if (!is.null(vi_class) && nrow(vi_class) >= 3) {
  cat("  分类任务:", paste(vi_class$variable[1:3], collapse=" > "), "\n")
}
# 显示第一个回归目标的变量重要性
first_reg_vi <- vi_reg_list[[names(reg_targets)[1]]]
if (!is.null(first_reg_vi) && nrow(first_reg_vi) >= 3) {
  cat(sprintf("  回归任务(car7): %s\n", paste(first_reg_vi$variable[1:3], collapse=" > ")))
}

cat("\n[已生成图表]\n")
cat("  h2o_class_auc.png       — 分类模型AUC对比\n")
cat("  h2o_reg_rmse.png        — 回归模型RMSE对比\n")
cat("  h2o_reg_r2.png          — 回归模型R²对比\n")
cat("  h2o_varimp_combined.png — 变量重要性\n")
cat("  h2o_pred_vs_actual.png  — 预测值vs实际值\n")
cat("  h2o_roc_curve.png       — ROC曲线\n")
cat("  h2o_bubble_leaderboard.png — 模型全景\n")

cat("\n=============================================================\n")

# 关闭 H2O 集群
h2o.shutdown(prompt = FALSE)
cat("\n✓ H2O分析完成！集群已关闭。\n")
