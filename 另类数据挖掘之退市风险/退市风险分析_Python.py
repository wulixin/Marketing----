# ==============================================================================
# 退市风险机会挖掘系统 — Python分析代码
# 数据来源：另类数据挖掘之退市风险.xlsx + 辅助画像标签数据
# ==============================================================================

import pandas as pd
import numpy as np
import json
import warnings
warnings.filterwarnings('ignore')

# 可视化库
import matplotlib
matplotlib.use('Agg')
import matplotlib.pyplot as plt
import matplotlib.font_manager as fm

# 设置中文字体
plt.rcParams['font.sans-serif'] = ['Arial Unicode MS', 'SimHei', 'STHeiti']
plt.rcParams['axes.unicode_minus'] = False

# 1. 数据读取 -------------------------------------------------------------------
data_dir = "."

# 1.1 退市风险公告数据（5个Sheet）
xls = pd.ExcelFile(f"{data_dir}/另类数据挖掘之退市风险.xlsx")
sheets_map = {'沪市A股':'沪市A股','深市A股主板':'深市A股主板','创业板':'创业板','北交所':'北交所','科创板':'科创板'}
delisting_all = []
for sname, label in sheets_map.items():
    df = pd.read_excel(xls, sheet_name=sname)
    df['板块'] = label
    delisting_all.append(df)
delisting_df = pd.concat(delisting_all, ignore_index=True)

# 1.2 其他数据源
name_change = pd.read_excel(f"{data_dir}/简称变更.xlsx")
pe_data = pd.read_csv(f"{data_dir}/2026年PE市盈率PB市净率ROE投资风格.txt", sep='\t')
prices = pd.read_csv(f"{data_dir}/dataprices_20260515.csv")
stock_label = pd.read_csv(f"{data_dir}/df_all_type.csv")
abnormal = pd.read_csv(f"{data_dir}/2027年个股异常波动标签画像.txt", sep='\t')
short_guide = pd.read_csv(f"{data_dir}/2026年短线动态操作指南.txt", sep='\t')

# 2. 数据预处理 -----------------------------------------------------------------

# 2.1 公告类型分类标注
delisting_df['is_zhaimao'] = delisting_df['公告类型'].str.contains('撤销|摘帽', na=False)
delisting_df['is_shishi'] = delisting_df['公告类型'].str.contains('实施', na=False)
delisting_df['is_fengxian'] = delisting_df['公告类型'].str.contains('风险提示|风险警示', na=False)
delisting_df['is_ST'] = delisting_df['名称'].str.contains(r'\*?ST', regex=True, na=False)
delisting_df['代码'] = delisting_df['代码'].astype(str).str.zfill(6)

# 2.2 按代码聚合
code_agg = delisting_df.groupby(['代码','板块']).agg(
    名称=('名称','last'),
    公告数=('序','count'),
    摘帽次数=('is_zhaimao','sum'),
    实施次数=('is_shishi','sum'),
    风险提示次数=('is_fengxian','sum'),
    最新公告日期=('公告日期','max'),
    最新公告类型=('公告类型','last'),
    最新公告标题=('公告标题','last'),
    最早公告日期=('公告日期','min')
).reset_index()

# 2.3 风险状态分类
def classify_status(row):
    if row['摘帽次数'] > 0 and row['实施次数'] == 0:
        return '摘帽成功'
    elif row['摘帽次数'] > 0 and row['实施次数'] > 0:
        return '摘帽申请中'
    elif row['实施次数'] > 0:
        return '退市风险实施中'
    elif row['风险提示次数'] > 0:
        return '风险提示中'
    else:
        return '其他公告'

code_agg['风险状态'] = code_agg.apply(classify_status, axis=1)

# 2.4 关联行情数据
prices['symbol'] = prices['ts_code'].str.replace(r'\.(SZ|SH|BJ)$','',regex=True)
price_map = prices.set_index('symbol')[['close','pct_chg','vol','amount']].to_dict('index')

# 2.5 关联PE/PB/ROE
pe_data['代码'] = pe_data['证券代码'].astype(str).str.zfill(6)
pe_map = pe_data.set_index('代码')[['个股股息率','个股市净率','个股滚动市盈率','个股静态市盈率','投资风格']].to_dict('index')

# 2.6 关联股票画像
stock_label['sym'] = stock_label['symbol'].astype(str).str.zfill(6)
label_map = stock_label.set_index('sym')[['ShiZhi','industry_1','industry_3','industry_4','market','act_name','act_ent_type','NTpye1','NTpye9','list_date']].to_dict('index')

# 2.7 关联异常波动标签
abnormal['sym'] = abnormal['ts_code'].str.replace(r'\.(SZ|SH|BJ)$','',regex=True)
abnormal_map = abnormal.set_index('sym')[['游资偏好度','Pianhao']].to_dict('index')

# 2.8 合并所有数据
results = []
for _, row in code_agg.iterrows():
    code = row['代码']
    rec = row.to_dict()

    p = price_map.get(code, {})
    rec['收盘价'] = p.get('close', None)
    rec['涨跌幅'] = p.get('pct_chg', None)
    rec['成交量'] = p.get('vol', None)
    rec['成交额'] = p.get('amount', None)

    pe = pe_map.get(code, {})
    rec['股息率'] = pe.get('个股股息率', None)
    rec['市净率'] = pe.get('个股市净率', None)
    rec['滚动PE'] = pe.get('个股滚动市盈率', None)
    rec['静态PE'] = pe.get('个股静态市盈率', None)
    rec['投资风格'] = pe.get('投资风格', None)

    lb = label_map.get(code, {})
    rec['市值'] = lb.get('ShiZhi', None)
    rec['行业'] = lb.get('industry_1', None)
    rec['细分行业'] = lb.get('industry_4', None)
    rec['市场'] = lb.get('market', None)
    rec['实控人'] = lb.get('act_name', None)
    rec['企业类型'] = lb.get('act_ent_type', None)
    rec['投资标签'] = lb.get('NTpye1', None)
    rec['动态标签'] = lb.get('NTpye9', None)
    rec['上市日期'] = str(lb.get('list_date', ''))

    ab = abnormal_map.get(code, {})
    rec['游资偏好度'] = ab.get('游资偏好度', None)
    rec['波动标签'] = ab.get('Pianhao', None)

    results.append(rec)

result_df = pd.DataFrame(results)

# 3. 三大板块分析 ----------------------------------------------------------------
focus_boards = ['科创板', '北交所', '创业板']
focus_df = result_df[result_df['板块'].isin(focus_boards)].copy()

# 3.1 摘帽机会识别
zhaimao_df = focus_df[focus_df['风险状态'].isin(['摘帽成功','摘帽申请中'])].copy()
print("=== 摘帽机会公司 ===")
print(zhaimao_df[['代码','名称','板块','风险状态','收盘价','涨跌幅']].to_string(index=False))

# 3.2 退市风险高危识别
risk_df = focus_df[focus_df['风险状态'].isin(['退市风险实施中','风险提示中'])].copy()
print("\n=== 退市风险高危公司 ===")
print(risk_df[['代码','名称','板块','风险状态','收盘价','涨跌幅']].to_string(index=False))

# 3.3 摘帽机会评分模型
def opportunity_score(row):
    """综合评分：摘帽概率 + 基本面改善 + 市场情绪"""
    score = 0
    # 摘帽相关公告越多越好
    score += min(row['摘帽次数'] * 15, 45)
    # 风险提示次数少说明问题不严重
    if row['风险提示次数'] <= 2:
        score += 15
    # 退市实施次数少
    if row['实施次数'] <= 1:
        score += 10
    # 市净率>0（未资不抵债）
    try:
        if row['市净率'] and float(row['市净率']) > 0:
            score += 10
    except:
        pass
    # 企业类型为国企加分
    if row.get('企业类型') in ['中央企业', '地方国有企业']:
        score += 10
    # 有实控人加分
    if row.get('实控人') and row['实控人'] not in ['无实际控制人', 'NA', None]:
        score += 10
    return min(score, 100)

focus_df['机会评分'] = focus_df.apply(opportunity_score, axis=1)
result_df['机会评分'] = result_df.apply(opportunity_score, axis=1)

# 4. 可视化 ---------------------------------------------------------------------

# 4.1 各板块风险状态分布
fig, ax = plt.subplots(figsize=(12, 6))
status_pivot = code_agg.groupby(['板块','风险状态']).size().unstack(fill_value=0)
colors = {'摘帽成功':'#27ae60','摘帽申请中':'#f39c12','退市风险实施中':'#e74c3c','风险提示中':'#e67e22','其他公告':'#95a5a6'}
status_pivot.plot(kind='bar', stacked=True, ax=ax, color=[colors.get(c,'#999') for c in status_pivot.columns])
ax.set_title('各板块退市风险状态分布', fontsize=16)
ax.set_xlabel('板块', fontsize=12)
ax.set_ylabel('公司数', fontsize=12)
ax.legend(title='风险状态', fontsize=10)
plt.xticks(rotation=30, ha='right')
plt.tight_layout()
plt.savefig('plot_board_status.png', dpi=150)

# 4.2 三大板块摘帽率对比
zhaimao_rate = code_agg.groupby('板块').apply(
    lambda x: pd.Series({
        '总数': len(x),
        '摘帽数': x['风险状态'].isin(['摘帽成功','摘帽申请中']).sum(),
        '摘帽率': round(x['风险状态'].isin(['摘帽成功','摘帽申请中']).sum() / len(x) * 100, 1)
    })
).reset_index()

fig, ax = plt.subplots(figsize=(8, 5))
bars = ax.bar(zhaimao_rate['板块'], zhaimao_rate['摘帽率'],
              color=['#3498db','#e74c3c','#2ecc71','#f39c12','#9b59b6'][:len(zhaimao_rate)])
for bar, val in zip(bars, zhaimao_rate['摘帽率']):
    ax.text(bar.get_x() + bar.get_width()/2., bar.get_height() + 1,
            f'{val}%', ha='center', va='bottom', fontsize=12)
ax.set_title('各板块摘帽成功率', fontsize=16)
ax.set_ylabel('摘帽率(%)', fontsize=12)
plt.tight_layout()
plt.savefig('plot_zhaimao_rate.png', dpi=150)

# 4.3 三大板块行业分布
fig, ax = plt.subplots(figsize=(12, 6))
focus_industry = focus_df[focus_df['行业'].notna()].groupby(['行业','风险状态']).size().unstack(fill_value=0)
focus_industry.plot(kind='barh', stacked=True, ax=ax,
                     color=[colors.get(c,'#999') for c in focus_industry.columns])
ax.set_title('科创板/北交所/创业板退市公司行业分布', fontsize=16)
ax.set_xlabel('公司数', fontsize=12)
ax.set_ylabel('行业', fontsize=12)
plt.tight_layout()
plt.savefig('plot_industry_dist.png', dpi=150)

# 4.4 公告时间线
delisting_df['公告月份'] = pd.to_datetime(delisting_df['公告日期']).dt.to_period('M').astype(str)
timeline = delisting_df[delisting_df['板块'].isin(focus_boards)].groupby(
    ['板块','公告月份','is_zhaimao']
).size().reset_index(name='count')
timeline['类型'] = timeline['is_zhaimao'].map({True:'摘帽相关', False:'其他退市公告'})

fig, axes = plt.subplots(1, 3, figsize=(18, 5), sharey=True)
for i, board in enumerate(focus_boards):
    sub = timeline[timeline['板块']==board]
    pivot = sub.pivot_table(index='公告月份', columns='类型', values='count', fill_value=0)
    pivot.plot(kind='bar', stacked=True, ax=axes[i],
               color={'摘帽相关':'#27ae60','其他退市公告':'#e74c3c'})
    axes[i].set_title(board, fontsize=14)
    axes[i].set_xlabel('')
    axes[i].tick_params(axis='x', rotation=45)
plt.suptitle('三大板块退市公告月度分布', fontsize=16)
plt.tight_layout()
plt.savefig('plot_timeline.png', dpi=150)

# 5. 导出结果 -------------------------------------------------------------------

# 5.1 导出Excel
with pd.ExcelWriter('退市风险分析结果_Python.xlsx', engine='openpyxl') as writer:
    result_df.to_excel(writer, sheet_name='全市场退市风险', index=False)
    focus_df.to_excel(writer, sheet_name='科创板北交所创业板', index=False)
    zhaimao_df.to_excel(writer, sheet_name='摘帽机会', index=False)
    risk_df.to_excel(writer, sheet_name='退市风险高危', index=False)

# 5.2 导出JSON
export_data = result_df.copy()
for col in export_data.columns:
    export_data[col] = export_data[col].astype(str)
export_data.to_json('delisting_analysis_result.json', orient='records', force_ascii=False, indent=2)

print("\n=== 分析完成！输出文件 ===")
print("1. plot_board_status.png — 各板块风险状态分布")
print("2. plot_zhaimao_rate.png — 摘帽成功率")
print("3. plot_industry_dist.png — 行业分布")
print("4. plot_timeline.png — 月度公告时间线")
print("5. 退市风险分析结果_Python.xlsx — 完整分析Excel")
print("6. delisting_analysis_result.json — JSON格式结果")

# 6. 统计摘要 -------------------------------------------------------------------
summary = {
    '全市场退市风险公司数': len(result_df),
    '退市公告总数': len(delisting_df),
    '三大板块公司数': len(focus_df),
    '摘帽机会公司数': len(zhaimao_df),
    '退市风险高危公司数': len(risk_df),
    '板块分布': result_df['板块'].value_counts().to_dict(),
    '风险状态分布': result_df['风险状态'].value_counts().to_dict(),
}
print("\n=== 统计摘要 ===")
for k, v in summary.items():
    print(f"  {k}: {v}")
