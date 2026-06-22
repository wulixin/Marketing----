# ============================================================================
# 控制权变更研究 - ML综合分析脚本
# 聚类(K-means/H-Clust/GMM) → 高级回归(Lasso/Ridge/GLM/lme4) → 
# 集成树(RF/GBM/AdaBoost/XGBoost) → DALEX解释
# ============================================================================

library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(cluster)
library(factoextra)
library(mclust)
library(glmnet)
library(caret)
library(lme4)
library(rpart)
library(randomForest)
library(gbm)
library(xgboost)
library(DALEX)
library(ingredients)
library(stargazer)
library(gridExtra)
library(RColorBrewer)
library(scales)
library(patchwork)
library(corrplot)
library(showtext)

# ============================================================================
# 中文字体设置 (showtext)
# ============================================================================
showtext_auto(enable = TRUE)
# 尝试加载系统宋体，失败则回退到默认中文字体
try(font_add("Songti", "Songti.ttc"), silent = TRUE)
cat("  showtext中文字体已启用\n")

setwd("/Users/wulixin/Documents/GitHub/Marketing----/控制权变更研究")

# ============================================================================
# 1. 数据读取与预处理
# ============================================================================
cat("=== 阶段1：数据读取与预处理 ===\n")

filepath <- "按板块维度整理后的数据.xlsx"
sheets <- readxl::excel_sheets(filepath)

df_list <- lapply(sheets, function(sh) {
  d <- readxl::read_excel(filepath, sheet = sh, col_types = "text")
  d
})
names(df_list) <- sheets

# 合并所有sheet
df_all <- bind_rows(df_list)
cat(sprintf("合并后样本量: %d 行, %d 列\n", nrow(df_all), ncol(df_all)))

# 提取关键字段并数值化
# 使用位置无关的模糊列名匹配（处理不同sheet列顺序不同的问题）
safe_get <- function(df, patterns, default=NA) {
  # patterns可以是字符串或字符串向量
  for(pat in patterns) {
    matches <- grep(pat, names(df), value=TRUE, ignore.case=TRUE, perl=FALSE)
    if(length(matches) > 0) return(df[[matches[1]]])
  }
  return(rep(default, nrow(df)))
}

build_df <- function(df) {
  res <- data.frame(
    # === 标识字段 ===
    股票代码 = safe_get(df, "股票代码", ""),
    公司简称 = safe_get(df, "公司简称", ""),
    市场类型 = safe_get(df, "市场类型", ""),
    一级行业 = safe_get(df, "一级行业", ""),
    所在地区 = safe_get(df, "所在地区", ""),

    # === DID变量 ===
    is_tech = as.numeric(safe_get(df, c("Treat", "科技并购", "处理组标识"), 0)),
    is_post = ifelse(as.character(safe_get(df, c("Post", "政策后", "时间标识"), "")) == "1" | 
                     as.character(safe_get(df, c("政策时点"), "")) == "政策后", 1, 0),
    did = NA_real_,  # 后面填充

    # === 目标变量 ===
    car7 = as.numeric(safe_get(df, c("CAR.*-7", "\\[-7,7\\]"))),
    car60 = as.numeric(safe_get(df, c("CAR.*-60", "\\[-60,60\\]"))),
    premium_w = as.numeric(safe_get(df, c("溢价率"))),
    premium_m = as.numeric(safe_get(df, c("相对市价溢价率"))),

    # === 交易特征 ===
    transfer_pct = as.numeric(safe_get(df, c("转让股份比例"))),
    transfer_price = as.numeric(safe_get(df, c("转让价格"))),
    deal_value = as.numeric(safe_get(df, c("交易总金额"))),
    lock_period = as.numeric(safe_get(df, c("锁定期限"))),
    has_vow = ifelse(as.character(safe_get(df, c("业绩承诺"))) == "是", 1, 0),
    has_lock = ifelse(as.character(safe_get(df, c("股权锁定承诺"))) == "是", 1, 0),
    vow_amount = as.numeric(safe_get(df, c("业绩承诺金额"))),
    loan_ratio = as.numeric(safe_get(df, c("贷款比例"))),

    # === 表决权让渡 ===
    waive_vote = ifelse(as.character(safe_get(df, c("原有股东是否放弃表决权"))) == "是" | 
                         as.character(safe_get(df, c("原有股东是否放弃表决权"))) == "1", 1, 0),
    waive_ratio = as.numeric(safe_get(df, c("放弃表达权比例"))),

    # === 财务指标 (核心聚类特征) ===
    roe = as.numeric(safe_get(df, c("ROE净资产收益率"))),
    roa = as.numeric(safe_get(df, c("ROA总资产收益率"))),
    lev = as.numeric(safe_get(df, c("资产负债率"))),
    current_ratio = as.numeric(safe_get(df, c("流动比率"))),
    tobin_q = as.numeric(safe_get(df, c("托宾Q"))),
    bm = as.numeric(safe_get(df, c("账面市值比"))),
    ocf_ratio = as.numeric(safe_get(df, c("经营现金流"))),
    rev_growth = as.numeric(safe_get(df, c("营收增长率"))),
    profit_growth = as.numeric(safe_get(df, c("净利润增长率"))),
    is_loss = ifelse(as.character(safe_get(df, c("是否亏损"))) == "是", 1, 0),
    lgsize = as.numeric(safe_get(df, c("总资产对数"))),
    total_assets = as.numeric(safe_get(df, c("总资产"))),
    net_assets = as.numeric(safe_get(df, c("净资产"))),
    revenue = as.numeric(safe_get(df, c("营业收入"))),
    profit = as.numeric(safe_get(df, c("净利润"))),

    # === 治理指标 (核心聚类特征) ===
    top1 = as.numeric(safe_get(df, c("第一大股东持股"))),
    top2 = as.numeric(safe_get(df, c("第二大股东持股"))),
    balance_ratio = as.numeric(safe_get(df, c("股权制衡度"))),
    multi_large = ifelse(as.character(safe_get(df, c("多个大股东"))) == "是", 1, 0),
    sep_two = as.numeric(safe_get(df, c("两权分离度"))),
    board_size = as.numeric(safe_get(df, c("董事会人数"))),
    indep_ratio = as.numeric(safe_get(df, c("独董比例"))),
    duality = ifelse(as.character(safe_get(df, c("两职合一"))) == "是", 1, 0),
    mgmt_hold = ifelse(as.character(safe_get(df, c("管理层是否持股"))) == "是", 1, 0),
    nom_nonexec = as.numeric(safe_get(df, c("提名非独立董事数"))),
    nom_indep = as.numeric(safe_get(df, c("提名独立董事数"))),

    # === 监管与市场 ===
    inquiry = ifelse(as.character(safe_get(df, c("问询函"))) == "是", 1, 0),
    inquiry_severity = as.numeric(safe_get(df, c("严厉度", "LLM评估"))),
    media_attn = as.numeric(safe_get(df, c("媒体关注度"))),
    analyst = as.numeric(safe_get(df, c("分析师跟踪"))),
    inst_hold = as.numeric(safe_get(df, c("机构持股比例"))),
    geo_dist = as.numeric(safe_get(df, c("地理距离"))),
    same_province = ifelse(as.character(safe_get(df, c("同省"))) == "是", 1, 0),
    core_econ = ifelse(as.character(safe_get(df, c("核心经济区"))) == "是", 1, 0),

    # === 时间效率 ===
    total_days = as.numeric(safe_get(df, c("总共用时"))),
    abnormal_mvmt = as.numeric(safe_get(df, c("事件异常波动次数"))),

    stringsAsFactors = FALSE
  )

  # 填充DID交互项
  res$did <- as.numeric(res$is_tech) * as.numeric(res$is_post)

  # 清理无穷值
  res[res == Inf] <- NA
  res[res == -Inf] <- NA

  return(res)
}

df <- build_df(df_all)
rownames(df) <- df$公司简称
cat(sprintf("有效数据集: %d 样本\n", nrow(df)))

# 检查目标变量完整率
cat("\n--- 目标变量完整性 ---\n")
targets <- c("car7","premium_w","is_tech")
for(v in targets) {
  complete <- sum(!is.na(df[[v]]))
  cat(sprintf("  %s: %d/%d (%.1f%%)\n", v, complete, nrow(df), 100*complete/nrow(df)))
}

# ============================================================================
# 2. 聚类特征选择与数据准备
# ============================================================================
cat("\n=== 阶段2：被收购方聚类分析 ===\n")

# 选择用于聚类的特征（财务+治理，排除标识变量和目标变量）
cluster_vars <- c(
  # 财务健康度
  "roe","roa","lev","current_ratio","tobin_q",
  "ocf_ratio","rev_growth","profit_growth","is_loss","lgsize",
  # 治理结构
  "top1","balance_ratio","sep_two","board_size","indep_ratio",
  "duality","multi_large",
  # 交易特征
  "transfer_pct","waive_vote","lock_period"
)

# 构建聚类数据集 - 完整案例分析
cluster_data <- df[, cluster_vars, drop=FALSE]
complete_cases <- complete.cases(cluster_data)
cat(sprintf("聚类特征完整案例: %d/%d (%.1f%%)\n", 
            sum(complete_cases), nrow(df), 100*sum(complete_cases)/nrow(df)))

clust_df <- cluster_data[complete_cases, ]
clust_names <- df$公司简称[complete_cases]
clust_market <- df$市场类型[complete_cases]
clust_tech <- df$is_tech[complete_cases]

# 标准化
clust_scaled <- scale(clust_df)

# 移除NA列（如果有常数列导致sd=0）
valid_cols <- apply(clust_scaled, 2, function(x) all(is.finite(x)))
clust_scaled <- clust_scaled[, valid_cols, drop=FALSE]


# ============================================================================
# 2.1 K-means聚类 (肘部法则 + 轮廓系数)
# ============================================================================
cat("\n--- 2.1 K-means聚类 ---\n")

# 肘部法则
wss <- sapply(1:10, function(k) {
  kmeans(clust_scaled, centers=k, nstart=25, iter.max=100)$tot.withinss
})

# 轮廓系数（直接用循环，避免sapply返回列表导致索引问题）
sil_means <- numeric(9)
names(sil_means) <- as.character(2:10)
for(k in 2:10) {
  km_tmp <- kmeans(clust_scaled, centers=k, nstart=25, iter.max=100)
  sil_obj <- silhouette(km_tmp$cluster, dist(clust_scaled))
  sil_means[as.character(k)] <- mean(sil_obj[, "sil_width"])
}

# 肘部法则：找WSS下降最快的点（肘点）
# 方法：|WSS[k-1] - WSS[k]| / WSS[k] 最大的k
elbow_ratio <- -diff(wss) / wss[-1]   # 下降幅度/当前WSS（越大说明下降越显著）
best_k_elbow <- which.max(elbow_ratio)     # 肘点K

# 轮廓系数：直接取均值最大的K
best_k_sil <- which.max(sil_means) + 1   # sil_means索引从2开始，需+1

cat(sprintf("  肘部法则推荐K=%d, 轮廓系数推荐K=%d\n", best_k_elbow, best_k_sil))

# 可视化最优K
png("ml_cluster_optimal_k.png", width=900, height=400, res=150)
par(mfrow=c(1,2))
plot(1:10, wss, type="b", xlab="聚类数 K", ylab="簇内平方和 (WSS)",
     main="肘部法则确定最优K")
abline(v=best_k_elbow, col="red", lty=2, lwd=2)

plot(2:10, sil_means, type="b", xlab="聚类数 K", ylab="平均轮廓系数",
     main="轮廓系数确定最优K")
abline(v=best_k_sil, col="blue", lty=2, lwd=2)
par(mfrow=c(1,1))
dev.off()
cat("  ✓ 已保存: ml_cluster_optimal_k.png\n")

# 限制K最大值（每簇至少3个样本，最多5个簇）
k_max <- min(5, floor(sum(complete_cases) / 3))
k_best_raw <- best_k_sil
k_best <- min(k_best_raw, k_max)
if(k_best < 2) k_best <- 2  # 至少2个簇
cat(sprintf("  原始推荐K=%d, 样本量限制后K=%d (每簇≥3样本)\n", k_best_raw, k_best))

# 执行K-means
set.seed(123)
km_res <- kmeans(clust_scaled, centers=k_best, nstart=50, iter.max=200)
df$km_cluster[complete_cases] <- km_res$cluster
df$km_cluster[!complete_cases] <- NA

# PCA可视化K-means结果
p_km <- fviz_cluster(km_res, data=clust_scaled,
                      geom="point", ellipse.type="norm",
                      palette="Dark2", repel=TRUE,
                      title=sprintf("K-means聚类 (K=%d)", k_best),
                      subtitle="PCA投影 + 正态椭圆")
png("ml_cluster_kmeans_pca.png", width=750, height=600, res=150)
print(p_km)
dev.off()
cat("  ✓ 已保存: ml_cluster_kmeans_pca.png\n")

# 保存聚类中心（逆标准化：对每个变量 中心*原sd + 原mean）
# km_res$centers: K行 x P列 (标准化后)
raw_means <- apply(clust_df, 2, mean, na.rm=TRUE)
raw_sds   <- apply(clust_df, 2, sd,   na.rm=TRUE)
km_centers_raw <- t(apply(km_res$centers, 1, function(r) r * raw_sds + raw_means))
colnames(km_centers_raw) <- colnames(clust_df)
km_centers <- as.data.frame(km_centers_raw)
write.csv(km_centers, "ml_cluster_kmeans_centers.csv", fileEncoding="UTF-8")
cat("  ✓ 已保存: ml_cluster_kmeans_centers.csv\n")


# ============================================================================
# 2.2 层次聚类 (H-Clust, Ward.D2)
# ============================================================================
cat("\n--- 2.2 层次聚类 (Ward.D2) ---\n")

hc_res <- hclust(dist(clust_scaled), method="ward.D2")
df$hc_cluster[complete_cases] <- cutree(hc_res, k=k_best)
df$hc_cluster[!complete_cases] <- NA

# 树状图
png("ml_cluster_dendrogram.png", width=900, height=500, res=150)
plot(hc_res, hang=-1, cex=0.6, labels=substr(clust_names,1,6),
     main=sprintf("层次聚类树状图 (Ward.D2, 虚线=K=%d)", k_best))
abline(h=hc_res$height[length(hc_res$height)-k_best+1], col="red", lty=2, lwd=2)
dev.off()
cat("  ✓ 已保存: ml_cluster_dendrogram.png\n")

# 层次聚类PCA可视化
hc_cut <- cutree(hc_res, k=k_best)
p_hc <- fviz_cluster(list(cluster=hc_cut, data=clust_scaled),
                     geom="point", ellipse.type="norm",
                     palette="Set2", repel=TRUE,
                     title=sprintf("层次聚类 (Ward.D2, K=%d)", k_best))
png("ml_cluster_hc_pca.png", width=750, height=600, res=150)
print(p_hc)
dev.off()
cat("  ✓ 已保存: ml_cluster_hc_pca.png\n")


# ============================================================================
# 2.3 GMM高斯混合模型 (mclust)
# ============================================================================
cat("\n--- 2.3 GMM高斯混合模型 (mclust) ---\n")

# mclust自动选择最优模型和K
set.seed(123)
gmm_fit <- Mclust(clust_scaled, G=1:8, modelNames=c("EII","VII","EEI","VEI","EVI","VVI"))
df$gmm_cluster[complete_cases] <- gmm_fit$classification
df$gmm_cluster[!complete_cases] <- NA

cat(sprintf("  GMM最优模型: %s, K=%d, BIC=%.1f\n",
            gmm_fit$modelName, gmm_fit$G, gmm_fit$bic))
# 显示前几个样本的分类概率（最多3列）
gmm_z_disp <- if(gmm_fit$G <= 3) gmm_fit$G else 3
cat(sprintf("  分类概率 (前5个样本): %s\n",
            paste(sprintf("%.3f", gmm_fit$z[1,1:gmm_z_disp]), collapse=", ")))

# GMM可视化（K>1时才用fviz_mclust，否则用基础散点图）
if(gmm_fit$G > 1) {
  p_gmm <- fviz_mclust(gmm_fit, geom="point", ellipse.type="norm",
                         palette="Set1", repel=TRUE,
                         title=sprintf("GMM聚类 (%s, K=%d)", gmm_fit$modelName, gmm_fit$G))
  png("ml_cluster_gmm_pca.png", width=750, height=600, res=150)
  print(p_gmm)
  dev.off()
  cat("  ✓ 已保存: ml_cluster_gmm_pca.png\n")
} else {
  cat("  GMM K=1 (单簇), 跳过GMM可视化\n")
  # 可选：保存单簇散点图
  png("ml_cluster_gmm_pca.png", width=750, height=600, res=150)
  plot(clust_scaled[,1], clust_scaled[,2], main="GMM聚类 (K=1, 单簇)",
       xlab="Var1 (标准化)", ylab="Var2 (标准化)", pch=19, col="steelblue")
  dev.off()
  cat("  ✓ 已保存: ml_cluster_gmm_pca.png (单簇版)\n")
}


# ============================================================================
# 2.4 聚类结果对比与剖面分析
# ============================================================================
cat("\n--- 2.4 聚类剖面分析 ---\n")

# 使用K-means结果做剖面（最易解释）
cluster_profile <- data.frame(
  Cluster = as.factor(1:k_best),
  Size = as.numeric(table(km_res$cluster))
)
for(v in colnames(clust_df)) {
  # 对每个聚类计算均值（逆标准化到原始量纲）
  means <- tapply(clust_df[, v], km_res$cluster, mean, na.rm=TRUE)
  cluster_profile[[v]] <- means
}

# 剖面雷达图 (使用fmsb)
# fmsb要求：第1行=各变量上限，第2行=各变量下限，第3行起=各簇数据
library(fmsb)
radar_data <- cluster_profile[, !(names(cluster_profile) %in% c("Cluster","Size"))]
radar_max <- apply(radar_data, 2, max, na.rm=TRUE)
radar_min <- apply(radar_data, 2, min, na.rm=TRUE)
radar_plot_data <- rbind(radar_max, radar_min, radar_data)

# 每个簇单独画一张雷达图，避免mfrow边距问题
png("ml_cluster_profile.png", width=250*k_best, height=600, res=150)
par(mfrow=c(1, k_best), mar=c(1,1,3,1))
for(i in 1:k_best) {
  radarchart(radar_plot_data[, c(1,2,i+2)],
              title=sprintf("簇%d (n=%d)", i, cluster_profile$Size[i]),
              pcol="steelblue", plwd=2, pdensity=NULL)
}
par(mfrow=c(1,1), mar=c(5.1,4.1,4.1,2.1))  # 恢复默认边距
dev.off()
cat("  ✓ 已保存: ml_cluster_profile.png\n")

# 聚类交叉表
cat("\nK-means vs 层次聚类 交叉表:\n")
print(table(Kmeans=df$km_cluster[complete_cases], HClust=df$hc_cluster[complete_cases]))

cat("\nK-means vs GMM 交叉表:\n")
print(table(Kmeans=df$km_cluster[complete_cases], GMM=df$gmm_cluster[complete_cases]))


# ============================================================================
# 3. 高级回归分析
# ============================================================================
cat("\n=== 阶段3：高级回归分析 ===\n")

# 构建回归数据集（CAR7完整案例）
reg_vars <- c(
  "is_tech","is_post","did",
  "car7","premium_w",
  "roe","lev","tobin_q","lgsize",
  "transfer_pct","top1","balance_ratio",
  "waive_vote","lock_period","has_vow","inst_hold"
)

reg_data <- df[, reg_vars, drop=FALSE]
reg_complete <- complete.cases(reg_data$car7, reg_data$did)
reg_df <- reg_data[reg_complete, ]
cat(sprintf("回归分析完整案例 (CAR7): %d 样本\n", nrow(reg_df)))

# 修剪premium_w的极端值 (Winsorize 1%)
if(sum(!is.na(reg_df$premium_w)) > 0) {
  p01 <- quantile(reg_df$premium_w, 0.01, na.rm=TRUE)
  p99 <- quantile(reg_df$premium_w, 0.99, na.rm=TRUE)
  reg_df$premium_w_trim <- pmin(pmax(reg_df$premium_w, p01), p99)
} else {
  reg_df$premium_w_trim <- NA
}


# ----------------------------------------------------------------------------
# 3.1 OLS基准回归 (DID)
# ----------------------------------------------------------------------------
cat("\n--- 3.1 OLS基准回归 (DID) ---\n")

m_ols_car7 <- lm(car7 ~ is_tech + is_post + did + roe + lev + tobin_q + lgsize +
                   transfer_pct + top1 + balance_ratio + waive_vote + lock_period + has_vow + inst_hold,
                 data=reg_df)
summary(m_ols_car7)

m_ols_prem <- lm(premium_w_trim ~ is_tech + is_post + did + roe + lev + tobin_q + lgsize +
                   transfer_pct + top1 + balance_ratio,
                 data=reg_df)
summary(m_ols_prem)

# 打印DID系数（如果存在，否则提示共线性）
if("did" %in% names(coef(m_ols_car7)) && !is.na(coef(m_ols_car7)["did"])) {
  cat(sprintf("  CAR7-DID系数: %.4f (p=%.4f)\n",
              coef(m_ols_car7)["did"],
              summary(m_ols_car7)$coef["did","Pr(>|t|)"]))
} else {
  cat("  CAR7-DID系数: NA (共线性导致did被剔除)\n")
}


# ----------------------------------------------------------------------------
# 3.2 Lasso & Ridge回归 (glmnet)
# ----------------------------------------------------------------------------
cat("\n--- 3.2 Lasso & Ridge回归 ---\n")

# 构建设计矩阵（先na.omit确保X和y行数一致）
reg_clean <- na.omit(reg_df[, c("is_tech","is_post","did","roe","lev","tobin_q","lgsize",
                                  "transfer_pct","top1","balance_ratio",
                                  "waive_vote","lock_period","has_vow","inst_hold","car7")])
cat(sprintf("  glmnet有效样本: %d (na.omit后)\n", nrow(reg_clean)))

X_reg <- model.matrix(~ is_tech + is_post + did + roe + lev + tobin_q + lgsize +
                        transfer_pct + top1 + balance_ratio + waive_vote + lock_period + has_vow + inst_hold - 1,
                      data=reg_clean)
y_car7 <- reg_clean$car7

# 如果样本量太小（<5），跳过glmnet
if(nrow(X_reg) < 5 || ncol(X_reg) < 2) {
  cat("  样本量或变量数不足，跳过Lasso/Ridge\n")
} else {
  # Lasso (alpha=1)
  set.seed(123)
cv_lasso_car7 <- cv.glmnet(X_reg, y_car7, alpha=1, nfolds=5, type.measure="mse")
m_lasso_car7 <- glmnet(X_reg, y_car7, alpha=1, lambda=cv_lasso_car7$lambda.min)

# Ridge (alpha=0)
cv_ridge_car7 <- cv.glmnet(X_reg, y_car7, alpha=0, nfolds=5, type.measure="mse")
m_ridge_car7 <- glmnet(X_reg, y_car7, alpha=0, lambda=cv_ridge_car7$lambda.min)

cat(sprintf("  Lasso最优λ: %.4f (非零系数%d个)\n",
            cv_lasso_car7$lambda.min,
            sum(coef(cv_lasso_car7, s="lambda.min") != 0) - 1))  # -1 for intercept
cat(sprintf("  Ridge最优λ: %.4f\n", cv_ridge_car7$lambda.min))

# 正则化路径图
png("ml_regularization_path.png", width=900, height=400, res=150)
par(mfrow=c(1,2))
plot(cv_lasso_car7$glmnet.fit, xvar="lambda", main="Lasso路径 (CAR7)", label=TRUE)
abline(v=log(cv_lasso_car7$lambda.min), col="red", lty=2)
plot(cv_ridge_car7$glmnet.fit, xvar="lambda", main="Ridge路径 (CAR7)", label=TRUE)
abline(v=log(cv_ridge_car7$lambda.min), col="red", lty=2)
par(mfrow=c(1,1))
dev.off()
cat("  ✓ 已保存: ml_regularization_path.png\n")

# Lasso非零系数
lasso_coefs <- coef(cv_lasso_car7, s="lambda.min")
lasso_nz <- as.matrix(lasso_coefs)[which(as.matrix(lasso_coefs) != 0), , drop=FALSE]
cat("\nLasso非零系数 (CAR7):\n")
print(lasso_nz)
}  # end of glmnet if-else


# ----------------------------------------------------------------------------
# 3.3 GLM (Gamma/逆高斯/泊松)
# ----------------------------------------------------------------------------
cat("\n--- 3.3 GLM高阶回归 ---\n")

# Gamma GLM (适用正偏态溢价率, 要求y>0)
gamma_ok <- sum(!is.na(reg_df$premium_w_trim) & reg_df$premium_w_trim > 0)
if(gamma_ok > 10) {
  # 只用正值样本
  gamma_data <- reg_df[!is.na(reg_df$premium_w_trim) & reg_df$premium_w_trim > 0, ]
  m_gamma <- try(glm(premium_w_trim ~ is_tech + is_post + did + roe + lev,
                      data=gamma_data, family=Gamma(link="log")), silent=FALSE)
  if(!inherits(m_gamma, "try-error")) {
    cat("  Gamma GLM (溢价率) 收敛\n")
    cat(sprintf("    DID系数: %.4f\n", coef(m_gamma)["did"]))
  }
} else {
  cat(sprintf("  Gamma GLM: 正值样本不足(%d个), 跳过\n", gamma_ok))
}

# 泊松GLM (异常波动次数)
abnormal_counts <- df$abnormal_mvmt
abnormal_counts[is.na(abnormal_counts)] <- 0
if(length(unique(abnormal_counts)) > 2) {
  m_poisson <- try(glm(abnormal_counts ~ is_tech + is_post + did + inquiry + media_attn,
                        data=df, family=poisson(link="log")), silent=FALSE)
  if(!inherits(m_poisson, "try-error")) {
    cat("  泊松GLM (异常波动次数) 收敛\n")
  }
}


# ----------------------------------------------------------------------------
# 3.4 lme4混合效应模型 (板块随机效应)
# ----------------------------------------------------------------------------
cat("\n--- 3.4 lme4混合效应模型 ---\n")

# 使用df（包含市场类型列）构建混合效应模型
reg_lme_vars <- c("car7","premium_w_trim","is_tech","is_post","did",
                   "roe","lev","tobin_q","top1","balance_ratio","inst_hold",
                   "市场类型")
reg_lme_df <- df[, reg_lme_vars[reg_lme_vars %in% names(df)], drop=FALSE]
reg_lme_complete <- complete.cases(reg_lme_df)
reg_lme_df <- reg_lme_df[reg_lme_complete, ]

# 如果有市场类型作为随机效应
if("市场类型" %in% names(reg_lme_df) && length(unique(reg_lme_df$市场类型)) > 1) {
  m_lmer <- try(lmer(car7 ~ is_tech + is_post + did + roe + lev + tobin_q +
                       (1 | 市场类型), data=reg_lme_df), silent=FALSE)
  if(!inherits(m_lmer, "try-error")) {
    cat("  混合效应模型 (lmer) 收敛\n")
    vc <- VarCorr(m_lmer)
    var_group <- as.numeric(vc$`市场类型`[1])
    var_resid  <- attr(vc, "sc")^2
    icc <- var_group / (var_group + var_resid)
    cat(sprintf("    ICC (板块间方差占比): %.3f\n", icc))
  }
}


# ----------------------------------------------------------------------------
# 3.5 回归系数对比图
# ----------------------------------------------------------------------------
cat("\n--- 3.5 回归系数对比图 ---\n")

# 提取各模型DID系数
coef_ols_car7 <- try(coef(m_ols_car7)["did"], silent=TRUE)
if(!inherits(coef_ols_car7,"try-error") && !is.na(coef_ols_car7)) {
  se_ols_car7 <- try(summary(m_ols_car7)$coef["did","Std. Error"], silent=TRUE)
} else {
  se_ols_car7 <- NA
}
coef_ols_prem <- try(coef(m_ols_prem)["did"], silent=TRUE)
if(!inherits(coef_ols_prem,"try-error") && !is.na(coef_ols_prem)) {
  se_ols_prem <- try(summary(m_ols_prem)$coef["did","Std. Error"], silent=TRUE)
} else {
  se_ols_prem <- NA
}
coef_lasso <- try(as.numeric(coef(cv_lasso_car7, s="lambda.min")["did",]), silent=TRUE)

coef_df <- data.frame(
  Model = c("OLS-CAR7","OLS-Premium","Lasso-CAR7"),
  Coef = c(
    if(!inherits(coef_ols_car7,"try-error")) as.numeric(coef_ols_car7) else NA,
    if(!inherits(coef_ols_prem,"try-error")) as.numeric(coef_ols_prem) else NA,
    if(!inherits(coef_lasso,"try-error")) coef_lasso else NA
  ),
  SE = c(
    se_ols_car7, se_ols_prem, NA  # Lasso没有SE
  )
)

png("ml_regression_coefs.png", width=750, height=500, res=150)
p_coef <- ggplot(coef_df[!is.na(coef_df$Coef), ], aes(x=reorder(Model, Coef), y=Coef, fill=Model)) +
  geom_bar(stat="identity", width=0.6, alpha=0.8) +
  geom_errorbar(aes(ymin=Coef-1.96*SE, ymax=Coef+1.96*SE), width=0.2) +
  coord_flip() +
  scale_fill_brewer(type="qual", palette="Set2") +
  labs(title="DID系数对比 (多模型)", x="", y="DID系数估计值",
       subtitle="误差棒=95% CI (仅OLS)") +
  theme_minimal(base_size=12) +
  theme(plot.title=element_text(hjust=0.5, face="bold"),
        legend.position="none")
print(p_coef)
dev.off()
cat("  ✓ 已保存: ml_regression_coefs.png\n")


# ============================================================================
# 4. 集成树模型
# ============================================================================
cat("\n=== 阶段4：集成树模型 ===\n")

# 准备分类任务数据：预测是否为科技并购
# 使用完整is_tech标签的样本
class_data <- df[, c("is_tech","roe","lev","tobin_q","lgsize",
                      "top1","balance_ratio","transfer_pct",
                      "waive_vote","lock_period","inst_hold"), drop=FALSE]
class_complete <- complete.cases(class_data)
X_class <- class_data[class_complete, -1, drop=FALSE]
y_class <- as.factor(class_data$is_tech[class_complete])

cat(sprintf("分类模型样本: %d (科技=%d, 传统=%d)\n",
            length(y_class), sum(y_class==1), sum(y_class==0)))

# 变量标签 (用于DALEX可视化)
var_labels <- list(
  roe="ROE净资产收益率", lev="资产负债率", tobin_q="托宾Q",
  lgsize="公司规模(对数)", top1="第一大股东持股", 
  balance_ratio="股权制衡度", transfer_pct="转让股份比例",
  waive_vote="表决权放弃", lock_period="锁定期限", inst_hold="机构持股"
)


# ----------------------------------------------------------------------------
# 4.1 决策树
# ----------------------------------------------------------------------------
cat("\n--- 4.1 决策树 ---\n")

m_tree <- rpart(y_class ~ ., data=X_class, method="class",
                control=rpart.control(minsplit=5, cp=0.01))
cat(sprintf("  决策树复杂度参数CP: %.4f\n", m_tree$cptable[which.min(m_tree$cptable[,"xerror"]),"CP"]))


# ----------------------------------------------------------------------------
# 4.2 随机森林
# ----------------------------------------------------------------------------
cat("\n--- 4.2 随机森林 ---\n")

set.seed(123)
m_rf <- randomForest(y_class ~ ., data=X_class, ntree=500, importance=TRUE, proximity=TRUE)
cat(sprintf("  OOB误差: %.2f%%\n", 100*mean(m_rf$predicted != y_class)))

# 变量重要性图 (RF内置)
imp_rf <- importance(m_rf, type=1)
png("ml_rf_varimp.png", width=750, height=500, res=150)
barplot(sort(imp_rf[,1], decreasing=TRUE), horiz=TRUE,
        main="随机森林变量重要性 (MeanDecreaseAccuracy)",
        xlab="Mean Decrease Accuracy", col="steelblue")
dev.off()
cat("  ✓ 已保存: ml_rf_varimp.png\n")


# ----------------------------------------------------------------------------
# 4.3 GBM梯度提升
# ----------------------------------------------------------------------------
cat("\n--- 4.3 GBM梯度提升 ---\n")

# 准备GBM格式数据
gbm_data <- data.frame(y=as.numeric(y_class)-1, X_class)
gbm_best_iter <- NULL

set.seed(123)
# 小样本下禁用CV (n<50时CV会报错)
if(nrow(X_class) >= 50) {
  m_gbm <- try(gbm(y ~ ., data=gbm_data, distribution="bernoulli",
                    n.trees=500, interaction.depth=3, shrinkage=0.01,
                    cv.folds=5, n.cores=1), silent=FALSE)
  if(!inherits(m_gbm, "try-error")) {
    gbm_best_iter <- gbm.perf(m_gbm, method="cv")
    cat(sprintf("  GBM最优树数量: %d\n", gbm_best_iter))
  } else {
    m_gbm <- NULL
  }
} else {
  cat("  样本量<50, 禁用CV，使用固定树数量\n")
  m_gbm <- try(gbm(y ~ ., data=gbm_data, distribution="bernoulli",
                    n.trees=100, interaction.depth=3, shrinkage=0.01,
                    bag.fraction=0.5, n.minobsinnode=2), silent=FALSE)
  if(inherits(m_gbm, "try-error")) {
    m_gbm <- NULL
    cat("  GBM训练失败 (样本量过小)\n")
  }
}


# ----------------------------------------------------------------------------
# 4.4 XGBoost
# ----------------------------------------------------------------------------
cat("\n--- 4.4 XGBoost ---\n")

xgb_matrix <- xgb.DMatrix(data=as.matrix(X_class), label=as.numeric(y_class)-1)
set.seed(123)
m_xgb <- try(xgboost(data=xgb_matrix, nrounds=200, objective="binary:logistic",
                      eta=0.01, max_depth=3, verbose=0), silent=TRUE)
if(!inherits(m_xgb, "try-error")) {
  cat("  XGBoost训练完成\n")
} else {
  m_xgb <- NULL
  cat("  XGBoost训练失败\n")
}


# ----------------------------------------------------------------------------
# 4.5 AdaBoost (gbm包 adaboost分布)
# ----------------------------------------------------------------------------
cat("\n--- 4.5 AdaBoost ---\n")

ada_best <- NULL  # 初始化，防止后续引用时未定义

if(nrow(X_class) >= 50) {
  m_adaboost <- try(gbm(y ~ ., data=gbm_data, distribution="adaboost",
                         n.trees=200, interaction.depth=3, shrinkage=0.01,
                         cv.folds=5, n.cores=1), silent=FALSE)
  if(!inherits(m_adaboost, "try-error")) {
    ada_best <- gbm.perf(m_adaboost, method="cv")
    cat(sprintf("  AdaBoost最优树数量: %d\n", ada_best))
  } else {
    m_adaboost <- NULL
  }
} else {
  cat("  样本量<50, 禁用CV\n")
  m_adaboost <- try(gbm(y ~ ., data=gbm_data, distribution="adaboost",
                         n.trees=50, interaction.depth=3, shrinkage=0.01,
                         bag.fraction=0.5, n.minobsinnode=2), silent=FALSE)
  if(inherits(m_adaboost, "try-error")) {
    m_adaboost <- NULL
  }
}


# ----------------------------------------------------------------------------
# 4.6 h2o深度学习 (MXNet后端) — 跳过(h2o需要Java服务)
# ----------------------------------------------------------------------------
cat("\n--- 4.6 h2o深度学习 (跳过: 需要Java) ---\n")
cat("  提示: 如需启用h2o，请手动运行 h2o.init() 启动Java服务\n")

# 创建占位explainer（用于后续统一接口）
exp_h2o_rf <- NULL
exp_h2o_dl <- NULL
m_dl <- NULL  # 确保m_dl存在（为NULL）


# ----------------------------------------------------------------------------
# 4.7 模型性能对比  ← ← ← 修复版
# ----------------------------------------------------------------------------
cat("\n--- 4.7 模型性能对比 ---\n")

# 分步计算各模型准确率，避免c()内嵌套if/else的语法问题
acc_tree <- mean(predict(m_tree, type="class") == y_class)

acc_rf <- 1 - mean(m_rf$err.rate[nrow(m_rf$err.rate),"OOB"])

acc_gbm <- if(!is.null(m_gbm)) {
  gbm_pred <- predict(m_gbm, n.trees=ifelse(is.null(gbm_best_iter),50,gbm_best_iter),
                      type="response", newdata=gbm_data)
  mean((gbm_pred > 0.5) == gbm_data$y)
} else { NA }

acc_xgb <- if(!is.null(m_xgb)) {
  xgb_pred <- predict(m_xgb, newdata=xgb_matrix)
  mean((xgb_pred > 0.5) == (as.numeric(y_class)-1))
} else { NA }

acc_adaboost <- if(!is.null(m_adaboost)) {
  ada_pred <- predict(m_adaboost, n.trees=ifelse(is.null(ada_best),30,ada_best),
                      type="response", newdata=gbm_data)
  mean((ada_pred > 0.5) == gbm_data$y)
} else { NA }

# 构建performance summary数据框（所有值预先计算好）
perf_summary <- data.frame(
  Model = c("决策树", "随机森林", "GBM", "XGBoost", "AdaBoost", "h2o-DL", "h2o-RF"),
  Type  = c("Tree", "Ensemble", "Ensemble", "Ensemble", "Ensemble", "NN", "Ensemble"),
  Metric = c("Acc","Acc","Acc","Acc","Acc","AUC","AUC"),
  Score = c(acc_tree, acc_rf, acc_gbm, acc_xgb, acc_adaboost, NA, NA)
)
# 注意：不要在data.frame()构造后立即跟无关联的names()<-调用
# 这里Score向量的长度必须等于nrow，上面已经保证

cat("\n模型性能总览:\n")
print(perf_summary[!is.na(perf_summary$Score), ])

# 性能条形图
p_perf <- ggplot(perf_summary[!is.na(perf_summary$Score), ], 
                 aes(x=reorder(Model, Score), y=Score, fill=Type)) +
  geom_bar(stat="identity", width=0.65, alpha=0.85) +
  geom_text(aes(label=sprintf("%.3f", Score)), vjust=-0.5, size=3.5) +
  scale_fill_brewer(type="qual", palette="Dark2") +
  coord_cartesian(ylim=c(0, 1.15)) +
  labs(title="集成模型性能对比", x="", y="分数 (Acc/AUC)",
       subtitle="目标: 是否科技并购分类") +
  theme_minimal(base_size=12) +
  theme(plot.title = element_text(hjust=0.5, face="bold"),
        axis.text.x = element_text(angle=30, hjust=1),
        legend.position="bottom")

png("ml_model_performance.png", width=850, height=550, res=150)
print(p_perf)
dev.off()
cat("  ✓ 已保存: ml_model_performance.png\n")


# ============================================================================
# 5. DALEX模型解释
# ============================================================================
cat("\n=== 阶段5：DALEX模型解释与评估 ===\n")

# 为所有分类模型创建explainer
explainers <- list()

# OLS explainer (CAR7回归)
exp_ols <- explain(
  model = m_ols_car7,
  data = reg_df[, c("is_tech","is_post","did","roe","lev","tobin_q",
                     "lgsize","transfer_pct","top1","balance_ratio",
                     "waive_vote","lock_period","has_vow","inst_hold")],
  y = reg_df$car7,
  label = "OLS",
  verbose = FALSE
)
explainers[['OLS']] <- exp_ols

# Random Forest explainer
exp_rf <- explain(
  model = m_rf,
  data = X_class,
  y = as.numeric(y_class)-1,
  label = "RandomForest",
  verbose = FALSE
)
explainers[['RF']] <- exp_rf

# XGBoost explainer
if(!is.null(m_xgb)) {
  exp_xgb <- explain(
    model = m_xgb,
    data = as.matrix(X_class),
    y = as.numeric(y_class)-1,
    label = "XGBoost",
    verbose = FALSE,
    predict_function = function(model, newdata) {
      # xgboost需要matrix输入
      if(!is.matrix(newdata)) newdata <- as.matrix(newdata)
      as.vector(predict(model, newdata=newdata))
    }
  )
  explainers[['XGB']] <- exp_xgb
}

# GBM explainer
if(!is.null(m_gbm)) {
  exp_gbm_da <- explain(
    model = m_gbm,
    data = data.frame(X_class),
    y = as.numeric(y_class)-1,
    label = "GBM",
    verbose = FALSE,
    predict_function = function(model, newdata) {
      # gbm predict返回n.trees列的矩阵
      p <- predict(model, newdata=newdata, n.trees=ifelse(is.null(gbm_best_iter),50,gbm_best_iter),
                   type="response")
      as.vector(p)
    }
  )
  explainers[['GBM']] <- exp_gbm_da
}


# ----------------------------------------------------------------------------
# 5.1 变量重要性 (DALEX model_parts / feature_importance)
# ----------------------------------------------------------------------------
cat("\n--- 5.1 DALEX 变量重要性 ---\n")

imp_results <- list()

for(nm in c("RF","XGB","GBM")) {
  if(nm %in% names(explainers) && !is.null(explainers[[nm]])) {
    # 尝试多种方式: DALEX::model_parts (新API用loss_function参数)
    # 新版DALEX(>=2.0) loss_function接受字符串或函数
    imp <- tryCatch({
      # 方式1: 不指定loss_function，让DALEX自动选择
      model_parts(explainers[[nm]], B=15, n_features=NULL)
    }, error = function(e) {
      cat(sprintf("  model_parts方式1失败(%s): %s\n", nm, e$message))
      # 方式2: 用feature_importance (ingredients)
      tryCatch({
        feature_importance(explainers[[nm]], B=15, n_features=NULL)
      }, error = function(e2) {
        cat(sprintf("  feature_importance也失败(%s): %s\n", nm, e2$message))
        NULL
      })
    })
    if(!is.null(imp) && !all(is.na(imp$dropout_loss))) {
      # 过滤掉_baseline_行，只保留真实变量
      imp_vars <- imp[imp$variable != "_baseline_", ]
      if(nrow(imp_vars) > 0) {
        imp_results[[nm]] <- imp
        cat(sprintf("  ✓ %s 变量重要性计算成功 (%d变量)\n", nm, nrow(imp_vars)))
      } else {
        cat(sprintf("  %s 只有baseline无有效变量\n", nm))
      }
    } else {
      cat(sprintf("  %s 变量重要性结果无效\n", nm))
    }
  }
}

if(length(imp_results) > 0) {
  top_n <- 10
  # 提取每个模型的Top变量（排除_baseline_）
  imp_plot_data <- do.call(rbind, lapply(names(imp_results), function(nm) {
    imp <- imp_results[[nm]]
    imp_vars <- imp[imp$variable != "_baseline_" & !is.na(imp$dropout_loss), ]
    if(nrow(imp_vars) == 0) return(NULL)
    top_vars <- head(imp_vars[order(-imp_vars$dropout_loss), ], top_n)
    data.frame(Variable=top_vars$variable, Dropout_Loss=top_vars$dropout_loss, Model=nm)
  }))
  
  if(!is.null(imp_plot_data) && nrow(imp_plot_data) > 0) {
    p_imp <- ggplot(imp_plot_data, aes(x=reorder(Variable, Dropout_Loss),
                                        y=Dropout_Loss, fill=Model)) +
      geom_bar(stat="identity", position=position_dodge(width=0.85),
               width=0.75, alpha=0.85) +
      coord_flip() +
      scale_fill_manual(values=c("RF"="#E74C3C", "XGB"="#27AE60", "GBM"="#3498DB")) +
      labs(title="DALEX变量重要性 (跨模型一致性)",
           subtitle="Permutation-based dropout loss (越高越重要)",
           x="", y="Dropout Loss") +
      theme_minimal(base_size=11) +
      theme(plot.title = element_text(hjust=0.5, face="bold"),
            legend.position="bottom")

    png("ml_dalex_variable_importance.png", width=900, height=650, res=150)
    print(p_imp)
    dev.off()
    cat("  ✓ 已保存: ml_dalex_variable_importance.png\n")

    # 打印一致性排名表
    cat("\n变量重要性排名一致性 (Top-10):\n")
    for(nm in names(imp_results)) {
      imp_vars <- imp_results[[nm]][imp_results[[nm]]$variable != "_baseline_" &
                                     !is.na(imp_results[[nm]]$dropout_loss), ]
      if(nrow(imp_vars) > 0) {
        top_v <- head(imp_vars$variable[order(-imp_vars$dropout_loss)], top_n)
        cat(sprintf("  [%s] %s\n", nm, paste(top_v, collapse=", ")))
      }
    }
  } else {
    cat("  所有模型变量重要性只有baseline，使用RF内置重要性替代\n")
    # 回退到randomForest内置importance
    if(!is.null(m_rf)) {
      rf_imp_df <- data.frame(
        Variable = rownames(importance(m_rf)),
        Dropout_Loss = importance(m_rf)[,1],
        Model = "RF",
        stringsAsFactors = FALSE
      )
      rf_imp_df <- rf_imp_df[rf_imp_df$Variable != "%IncMSE" & 
                              rf_imp_df$Variable != "IncNodePurity" , ]
      if(nrow(rf_imp_df) > 0) {
        # 实际上rf importance返回的是矩阵
        rf_imp_raw <- importance(m_rf, type=1)
        rf_imp_df <- data.frame(
          Variable = rownames(rf_imp_raw),
          Dropout_Loss = rf_imp_raw[,1],
          Model = "RF(In-built)"
        )
        rf_imp_df <- head(rf_imp_df[order(-rf_imp_df$Dropout_Loss), ], top_n)

        p_imp2 <- ggplot(rf_imp_df, aes(x=reorder(Variable, Dropout_Loss), 
                                         y=Dropout_Loss, fill=Model)) +
          geom_bar(stat="identity", width=0.7, alpha=0.85, fill="#E74C3C") +
          coord_flip() +
          labs(title="随机森林内置变量重要性 (MeanDecreaseAccuracy)",
               x="", y="Importance") +
          theme_minimal(base_size=11) +
          theme(plot.title = element_text(hjust=0.5, face="bold"))

        png("ml_dalex_variable_importance.png", width=800, height=550, res=150)
        print(p_imp2)
        dev.off()
        cat("  ✓ 已保存(回退版): ml_dalex_variable_importance.png\n")
      }
    }
  }
} else {
  cat("  警告: 无有效explainer，跳过变量重要性图\n")
}


# ----------------------------------------------------------------------------
# 5.2 残差诊断
# ----------------------------------------------------------------------------
cat("\n--- 5.2 残差诊断 ---\n")

png("ml_dalex_residuals.png", width=1000, height=450, res=150)
par(mfrow=c(1,2))

# OLS残差
if(!is.null(exp_ols)) {
  res_ols <- try(compute_residuals(exp_ols), silent=TRUE)
  if(!inherits(res_ols, "try-error")) {
    plot(res_ols, main="OLS残差分布 (CAR7)")
  } else {
    plot(1, type="n", main="OLS残差: 计算失败")
  }
} else {
  plot(1, type="n", main="OLS explainer 不可用")
}

# RF残差
if(!is.null(exp_rf)) {
  res_rf <- try(compute_residuals(exp_rf), silent=TRUE)
  if(!inherits(res_rf, "try-error")) {
    plot(res_rf, main="RF残差分布 (科技并购)")
  } else {
    plot(1, type="n", main="RF残差: 计算失败")
  }
} else {
  plot(1, type="n", main="RF explainer 不可用")
}

par(mfrow=c(1,1))
dev.off()
cat("  ✓ 已保存: ml_dalex_residuals.png\n")


# ----------------------------------------------------------------------------
# 5.3 部分依赖图 (PDP) - 使用DALEX/ingredients + 手动ggplot回退
# ----------------------------------------------------------------------------
cat("\n--- 5.3 部分依赖图 (PDP) ---\n")

key_vars <- c("tobin_q","top1","lev","waive_vote","transfer_pct")

pdps <- list()
pdp_data_list <- list()  # 存储提取的原始数据用于手动绘图
for(v in key_vars) {
  if(v %in% colnames(X_class)) {
    pdp <- try(partial_dependence(exp_rf, variables=v, N=100), silent=TRUE)
    if(!inherits(pdp, "try-error") && !is.null(pdp)) {
      pdps[[v]] <- pdp
      # 尝试提取数据: ingredients返回的对象中result字段
      pdp_extract <- try({
        if(is.list(pdp) && !is.null(pdp$result)) {
          as.data.frame(pdp$result, stringsAsFactors=FALSE)
        } else if(is.data.frame(pdp)) {
          pdp
        } else {
          NULL
        }
      }, silent=TRUE)
      if(!inherits(pdp_extract, "try-error") && !is.null(pdp_extract) && nrow(pdp_extract)>0) {
        pdp_data_list[[v]] <- pdp_extract
      }
      cat(sprintf("  PDP %s 计算成功\n", v))
    } else {
      cat(sprintf("  PDP %s 计算失败\n", v))
    }
  }
}

if(length(pdps) > 0 && length(pdp_data_list) > 0) {
  n_pdp <- length(pdps)

  # 构建ggplot版本PDP（更可靠）
  pdp_gg_list <- list()
  for(v in names(pdp_data_list)) {
    dd <- pdp_data_list[[v]]
    # 找到x轴列(通常是变量名本身)和y轴列(prediction/预测值)
    x_col <- NULL
    y_col <- NULL
    for(cn in colnames(dd)) {
      if(tolower(cn) == tolower(v) || cn == "x" || cn == v) x_col <- cn
      if(tolower(cn) %in% c("prediction","predictions","yhat","response","value",
                            "_yhat_","_label_")) y_col <- cn
    }
    # 回退: 第一列为x，最后一数值列为y
    if(is.null(x_col)) x_col <- colnames(dd)[1]
    if(is.null(y_col)) {
      num_cols <- sapply(dd, is.numeric)
      y_candidates <- colnames(dd)[num_cols & colnames(dd) != x_col]
      if(length(y_candidates) > 0) y_col <- y_candidates[1] else y_col <- colnames(dd)[ncol(dd)]
    }

    main_title <- ifelse(is.null(var_labels[[v]]), v, var_labels[[v]])
    p_pdp_v <- ggplot(dd, aes(x=.data[[x_col]], y=.data[[y_col]])) +
      geom_line(color="steelblue", linewidth=1.2, alpha=0.85) +
      geom_ribbon(aes(ymin=min(.data[[y_col]], na.rm=TRUE),
                       ymax=max(.data[[y_col]], na.rm=TRUE)),
                  fill="steelblue", alpha=0.08) +
      labs(title=paste("PDP:", main_title),
           x=x_col, y="部分预测值") +
      theme_minimal(base_size=10) +
      theme(plot.title = element_text(hjust=0.5, face="bold"))
    pdp_gg_list[[v]] <- p_pdp_v
  }

  if(length(pdp_gg_list) > 0) {
    n_col <- ceiling(sqrt(length(pdp_gg_list)))
    # 使用gridExtra::grid.arrange（更稳定，不依赖patchwork内部布局）
    png("ml_dalex_pdp.png", width=1200, height=280*ceiling(length(pdp_gg_list)/n_col)+80, res=150)
    do.call(gridExtra::grid.arrange, c(pdp_gg_list, list(ncol=n_col, top="部分依赖图 (Partial Dependence Plot)")))
    dev.off()
    cat("  ✓ 已保存: ml_dalex_pdp.png (ggplot+gridExtra版)\n")
  }
} else if(length(pdps) > 0) {
  # ingredients计算成功但数据提取失败，尝试base plot
  n_pdp <- length(pdps)
  n_col <- ceiling(sqrt(n_pdp))
  n_row <- ceiling(n_pdp / n_col)

  png("ml_dalex_pdp.png", width=1200, height=900, res=150)
  par(mfrow=c(n_row, n_col))
  for(i in seq_along(pdps)) {
    v_name <- names(pdps)[i]
    main_title <- ifelse(is.null(var_labels[[v_name]]), v_name, var_labels[[v_name]])
    p_result <- try(plot(pdps[[i]], main=main_title), silent=TRUE)
    if(inherits(p_result, "try-error")) {
      plot(1, type="n", main=paste("PDP:", v_name))
    }
  }
  for(i in (length(pdps)+1):(n_row*n_col)) {
    plot(1, type="n", axes=FALSE, xlab="", ylab="")
  }
  par(mfrow=c(1,1))
  dev.off()
  cat("  ✓ 已保存: ml_dalex_pdp.png (base plot版)\n")
}


# ----------------------------------------------------------------------------
# 5.4 断点分析 (Break-down) - 单样本预测解释
# ----------------------------------------------------------------------------
cat("\n--- 5.4 断点分析 (单例预测解释) ---\n")

if(length(which(y_class == 1)) > 0) {
  # 选一个有代表性的样本（科技并购公司）
  sample_idx <- which(y_class == 1)[1]
  
  bd_rf <- try(break_down(exp_rf, 
                           new_observation = X_class[sample_idx, , drop=FALSE],
                           keep_distribution=TRUE, B=15), silent=TRUE)
  
  if(!inherits(bd_rf, "try-error") && !is.null(bd_rf)) {
    cat(sprintf("断点分析: 样本%d (真实标签=科技并购)\n", sample_idx))
    print(bd_rf)
    
    png("ml_dalex_breakdown.png", width=750, height=550, res=150)
    plot(bd_rf, main=sprintf("RF预测断点: 样本%d", sample_idx))
    dev.off()
    cat("  ✓ 已保存: ml_dalex_breakdown.png\n")
  } else {
    cat("  断点分析计算失败，跳过\n")
  }
} else {
  cat("  无科技并购样本，跳过断点分析\n")
}


# ----------------------------------------------------------------------------
# 5.5 Cumulative Local Effects (ALE) - 使用ingredients::accumulated_dependence
# ----------------------------------------------------------------------------
cat("\n--- 5.5 ALE效应 (DALEX/ingredients) ---\n")

# ingredients包中用accumulated_dependence计算ALE
ale_results <- list()
for(v in key_vars[1:min(3,length(key_vars))]) {
  if(v %in% colnames(X_class)) {
    ale <- try(accumulated_dependence(exp_rf, variables=v, N=100), silent=TRUE)
    if(!inherits(ale, "try-error") && !is.null(ale)) {
      ale_results[[v]] <- ale
      cat(sprintf("  ALE %s 计算成功\n", v))
    } else {
      cat(sprintf("  ALE %s 计算失败: %s\n", v, 
                   ifelse(inherits(ale,"try-error"), ale$message, "NULL result")))
    }
  }
}

if(length(ale_results) > 0) {
  n_ale <- length(ale_results)
  png("ml_dalex_ale.png", width=1000, height=350*n_ale, res=150)
  par(mfrow=c(n_ale, 1))
  for(v in names(ale_results)) {
    main_title <- ifelse(is.null(var_labels[[v]]), v, var_labels[[v]])
    ale_result <- try(plot(ale_results[[v]], main=paste("ALE:", main_title)), silent=TRUE)
    if(inherits(ale_result, "try-error")) {
      # 回退
      plot(1, type="n", main=paste("ALE:", main_title))
    }
  }
  par(mfrow=c(1,1))
  dev.off()
  cat("  ✓ 已保存: ml_dalex_ale.png\n")
}


# ============================================================================
# 6. 综合报告输出
# ============================================================================
cat("\n=== 阶段6：综合输出 ===\n")

# 保存聚类结果到CSV
cluster_output <- data.frame(
  公司简称 = df$公司简称,
  市场类型 = df$市场类型,
  科技并购 = df$is_tech,
  Kmeans_Cluster = df$km_cluster,
  HClust_Cluster = df$hc_cluster,
  GMM_Cluster = df$gmm_cluster
)
write.csv(cluster_output, "ml_cluster_results.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("  ✓ 已保存: ml_cluster_results.csv\n")

# 回归系数表
capture.output(
  stargazer(m_ols_car7, m_ols_prem, type="text",
            title="OLS回归结果对比",
            column.labels=c("CAR7","净资产溢价率"),
            dep.var.labels.include=FALSE),
  file="ml_regression_table.txt"
)
cat("  ✓ 已保存: ml_regression_table.txt\n")

# 模型性能表
write.csv(perf_summary, "ml_model_performance.csv", row.names=FALSE, fileEncoding="UTF-8")
cat("  ✓ 已保存: ml_model_performance.csv\n")

# 关键发现总结
cat("\n")
cat("=============================================================\n")
cat("         控制权变更研究 - ML分析核心发现总结\n")
cat("=============================================================\n")

cat(sprintf("\n【聚类】K-means最优K=%d, 平均轮廓系数=%.3f\n", k_best, max(sil_means, na.rm=TRUE)))
cat(sprintf("  GMM推荐模型=%s, K=%d\n", gmm_fit$modelName, gmm_fit$G))

cat("\n【回归-Lasso筛选】(CAR7目标):\n")
lasso_coefs_final <- coef(cv_lasso_car7, s="lambda.min")
nonzero_final <- lasso_coefs_final[lasso_coefs_final != 0]
for(nm in names(nonzero_final)[-1]) {  # skip intercept
  cat(sprintf("  %-20s %.4f\n", nm, as.numeric(nonzero_final[nm])))
}

cat("\n【集成模型-变量重要性TOP5 (RF MeanDecreaseAccuracy)】:\n")
if(!is.null(m_rf)) {
  rf_imp <- importance(m_rf, type=1)
  rf_top5 <- head(sort(rf_imp[,1], decreasing=TRUE), 5)
  for(nm in names(rf_top5)) {
    cat(sprintf("  %-20s %.2f\n", nm, rf_top5[nm]))
  }
}

cat("\n=============================================================\n")
cat("  全部图表已生成，论文初稿v2将整合上述ML结果\n")
cat("=============================================================\n")

cat("\n✓ 分析全部完成!\n")
