#!/usr/bin/env python3

# -*- coding: utf-8 -*-

"""

A股上市公司数据获取与分析主程序 V2.0

增加市盈率(PE)和行业细分维度


使用Tushare接口获取数据并进行分析

"""


import sys

import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


import pandas as pd

import numpy as np

from datetime import datetime

import json


# 导入分析配置V2

from config_v2 import (

    comprehensive_analysis,

    REGION_DEFINITIONS,

    MARKET_CAP_THRESHOLDS,

    ANALYSIS_RULES_V2,

    INDUSTRY_PE_BENCHMARK,

    classify_pe,

    pe_relative_analysis,

    get_industry_feature

)


# ============================================================

# 行业PE数据（从用户上传的文件加载）

# ============================================================


def load_industry_pe_data(file_path='/root/uploads/csi20260327_1774671043042_5j4va7.xls'):

    """加载行业PE基准数据"""

    try:

        df = pd.read_excel(file_path)

        # 清理列名

        df.columns = ['行业代码', '行业名称', '最新静态PE', '股票家数', '亏损家数', 

                      '月均PE', '季均PE', '半年均PE', '年均PE']

        

        # 提取有效数据

        pe_dict = {}

        for idx, row in df.iterrows():

            name = row['行业名称']

            pe = row['最新静态PE']

            if pe != '-' and pd.notna(pe):

                try:

                    pe_dict[name] = float(pe)

                except:

                    pass

        

        print(f"加载了 {len(pe_dict)} 个行业PE数据")

        return pe_dict

    except Exception as e:

        print(f"加载行业PE数据失败: {e}")

        return INDUSTRY_PE_BENCHMARK


# ============================================================

# 数据获取（增强版）

# ============================================================


def get_tushare_data_enhanced(token=None, industry_pe_data=None):

    """

    使用Tushare获取A股上市公司数据（增强版）

    增加PE、PB、行业等字段

    """

    

    try:

        import tushare as ts

    except ImportError:

        print("请先安装tushare: pip install tushare")

        return None

    

    if token:

        ts.set_token(token)

    

    pro = ts.pro_api()

    

    print("=" * 60)

    print("开始获取A股上市公司数据（增强版）...")

    print("=" * 60)

    

    # 1. 获取股票列表

    print("\n[1/5] 获取股票列表...")

    try:

        stock_basic = pro.stock_basic(exchange='', list_status='L', 

                                       fields='ts_code,symbol,name,area,industry,list_date')

        print(f"  获取到 {len(stock_basic)} 只股票")

    except Exception as e:

        print(f"  获取股票列表失败: {e}")

        return None

    

    # 2. 获取最新日线行情（含PE、PB）

    print("\n[2/5] 获取行情数据（含PE、PB）...")

    try:

        trade_cal = pro.trade_cal(exchange='SSE', is_open='1', 

                                   start_date=datetime.now().strftime('%Y%m%d'),

                                   end_date=(datetime.now() + pd.Timedelta(days=30)).strftime('%Y%m%d'))

        

        if len(trade_cal) > 0:

            latest_date = trade_cal['cal_date'].iloc[0]

        else:

            latest_date = '20241231'

        

        # 分批获取日线数据

        all_daily = []

        for i in range(0, len(stock_basic), 500):

            codes = stock_basic['ts_code'].iloc[i:i+500].tolist()

            try:

                # 获取日线数据（含PE、PB、市值）

                daily = pro.daily_basic(ts_code=','.join(codes[:100]), trade_date=latest_date,

                                        fields='ts_code,close,pe,pb,ps,total_mv,circ_mv,turnover_rate')

                if len(daily) > 0:

                    all_daily.append(daily)

            except:

                pass

        

        if all_daily:

            daily_df = pd.concat(all_daily, ignore_index=True)

            print(f"  获取到 {len(daily_df)} 条行情数据")

        else:

            print("  使用模拟数据...")

            daily_df = create_mock_daily_data_enhanced(stock_basic, industry_pe_data)

            

    except Exception as e:

        print(f"  获取行情数据失败: {e}")

        print("  使用模拟数据...")

        daily_df = create_mock_daily_data_enhanced(stock_basic, industry_pe_data)

    

    # 3. 获取公司基本信息

    print("\n[3/5] 获取公司基本信息...")

    try:

        stock_company = pro.stock_company(exchange='SSE', 

                                          fields='ts_code,province,city,actual_controller,industry')

        if len(stock_company) == 0:

            stock_company = pro.stock_company(exchange='SZSE', 

                                              fields='ts_code,province,city,actual_controller,industry')

        print(f"  获取到 {len(stock_company)} 家公司信息")

    except Exception as e:

        print(f"  获取公司信息失败: {e}")

        stock_company = pd.DataFrame()

    

    # 4. 合并数据

    print("\n[4/5] 合并数据...")

    

    merged_df = stock_basic.merge(

        daily_df[['ts_code', 'close', 'pe', 'pb', 'ps', 'total_mv', 'circ_mv', 'turnover_rate']].drop_duplicates('ts_code'),

        on='ts_code',

        how='left'

    )

    

    if len(stock_company) > 0:

        merged_df = merged_df.merge(

            stock_company[['ts_code', 'province', 'city', 'actual_controller', 'industry']].drop_duplicates('ts_code'),

            on='ts_code',

            how='left',

            suffixes=('', '_company')

        )

        # 优先使用公司表的行业信息

        merged_df['industry'] = merged_df['industry_company'].fillna(merged_df['industry'])

        merged_df = merged_df.drop(columns=['industry_company'], errors='ignore')

    else:

        merged_df['province'] = merged_df['area']

        merged_df['city'] = ''

        merged_df['actual_controller'] = ''

    

    # 判断国企

    merged_df['is_soe'] = merged_df['actual_controller'].apply(is_state_owned)

    

    print(f"  最终数据: {len(merged_df)} 条记录")

    

    return merged_df


def is_state_owned(controller):

    """判断是否为国企"""

    if pd.isna(controller):

        return False

    

    soe_keywords = ['国资委', '国有资产', '国有资本', '人民政府', '财政部', '央企', 

                    '地方国资', '省级', '市级', '县级']

    

    for keyword in soe_keywords:

        if keyword in str(controller):

            return True

    return False


def create_mock_daily_data_enhanced(stock_basic, industry_pe_data=None):

    """创建增强版模拟行情数据"""

    np.random.seed(42)

    n = len(stock_basic)

    

    # 行业PE基准

    if industry_pe_data is None:

        industry_pe_data = INDUSTRY_PE_BENCHMARK

    

    industries = list(industry_pe_data.keys())

    

    mock_data = pd.DataFrame({

        'ts_code': stock_basic['ts_code'],

        'close': np.round(np.random.uniform(3, 50, n), 2),

        'total_mv': np.random.uniform(10, 1000, n) * 10000,

        'circ_mv': np.random.uniform(5, 500, n) * 10000,

        'turnover_rate': np.round(np.random.uniform(0.5, 5, n), 2)

    })

    

    # 模拟PE（根据行业特征）

    pe_list = []

    pb_list = []

    ps_list = []

    

    for i in range(n):

        industry = stock_basic['industry'].iloc[i] if 'industry' in stock_basic.columns else '工业'

        

        # 获取行业基准PE

        base_pe = get_industry_pe(industry) or 25

        

        # 在行业PE基础上波动

        pe = base_pe * np.random.uniform(0.6, 1.8)

        

        # 10%概率亏损

        if np.random.random() < 0.1:

            pe = -abs(np.random.uniform(0.5, 3))

        

        pe_list.append(round(pe, 2))

        

        # PB基于PE推算

        pb = max(0.5, pe / 10 * np.random.uniform(0.6, 1.5))

        pb_list.append(round(pb, 2))

        

        # PS

        ps = np.random.uniform(1, 8)

        ps_list.append(round(ps, 2))

    

    mock_data['pe'] = pe_list

    mock_data['pb'] = pb_list

    mock_data['ps'] = ps_list

    

    return mock_data


def get_industry_pe(industry_name):

    """获取行业PE"""

    if industry_name in INDUSTRY_PE_BENCHMARK:

        return INDUSTRY_PE_BENCHMARK[industry_name]

    

    for key, value in INDUSTRY_PE_BENCHMARK.items():

        if key in str(industry_name) or str(industry_name) in key:

            return value

    

    return None


# ============================================================

# 数据分析

# ============================================================


def analyze_data(df):

    """对数据进行综合分析"""

    

    print("\n" + "=" * 60)

    print("开始数据分析（V2.0增强版）...")

    print("=" * 60)

    

    # 计算相对成交量

    avg_vol = df['turnover_rate'].mean() if 'turnover_rate' in df.columns else 5

    df['volume_ratio'] = df['turnover_rate'] / avg_vol if 'turnover_rate' in df.columns else 1

    

    # 执行综合分析

    result_df = comprehensive_analysis(df)

    

    return result_df


def generate_summary_report_v2(result_df):

    """生成分析报告V2"""

    

    report = []

    report.append("\n" + "=" * 80)

    report.append("A股上市公司市值管理与重组可能性分析报告 V2.0")

    report.append("（增加PE估值与行业细分维度）")

    report.append("=" * 80)

    

    # 总体统计

    report.append("\n【一、总体统计】")

    report.append(f"  分析公司总数: {len(result_df)}")

    report.append(f"  分析日期: {datetime.now().strftime('%Y-%m-%d')}")

    

    # 区域分布

    report.append("\n【二、重点区域公司分布】")

    for region_name in REGION_DEFINITIONS.keys():

        count = len(result_df[result_df['region'] == region_name])

        report.append(f"  {region_name}: {count} 家公司")

    

    # 市值分布

    report.append("\n【三、市值分布】")

    for cap_class in MARKET_CAP_THRESHOLDS.keys():

        count = len(result_df[result_df['market_cap_class'] == cap_class])

        report.append(f"  {cap_class}: {count} 家")

    

    # PE分布

    report.append("\n【四、PE估值分布】")

    pe_classes = ['亏损', '深度低估', '低估值', '合理估值', '高估值', '超高估值', '极度高估']

    for pe_class in pe_classes:

        count = len(result_df[result_df['PE分类'] == pe_class])

        if count > 0:

            report.append(f"  {pe_class}: {count} 家")

    

    # 行业PE基准参考

    report.append("\n【五、行业PE基准参考】")

    for industry, pe in list(INDUSTRY_PE_BENCHMARK.items())[:15]:

        report.append(f"  {industry}: {pe}倍")

    

    # 高概率公司筛选

    report.append("\n【六、高借壳上市概率公司（Top 15）】")

    shell_high = result_df[result_df['借壳上市概率'] == '高'].nlargest(15, '借壳上市得分')

    if len(shell_high) > 0:

        for idx, row in shell_high.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['借壳上市得分']}")

            report.append(f"    PE:{row['pe']}({row['PE分类']}) | 市值:{row['market_cap_bn']:.1f}亿 | {row['借壳上市原因'][:50]}...")

    else:

        report.append("  无高概率公司")

    

    report.append("\n【七、显著低估公司（价值投资标的）】")

    undervalued = result_df[result_df['估值评级'] == '显著低估'].nlargest(15, '市值管理得分')

    if len(undervalued) > 0:

        for idx, row in undervalued.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): PE {row['pe']}倍")

            report.append(f"    行业PE:{row['行业PE']} | {row['相对估值']} | {row['投资建议']}")

    else:

        report.append("  无显著低估公司")

    

    report.append("\n【八、高战略投资者概率公司（Top 15）】")

    strategic_high = result_df[result_df['战略投资者概率'] == '高'].nlargest(15, '战略投资者得分')

    if len(strategic_high) > 0:

        for idx, row in strategic_high.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['战略投资者得分']}")

            report.append(f"    行业:{row['industry']} | PE:{row['pe']} | {row['战略投资者原因'][:50]}...")

    else:

        report.append("  无高概率公司")

    

    report.append("\n【九、市值管理迫切公司（Top 15）】")

    mgmt_urgent = result_df[result_df['市值管理迫切度'] == '迫切'].nlargest(15, '市值管理得分')

    if len(mgmt_urgent) > 0:

        for idx, row in mgmt_urgent.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['市值管理得分']}")

            report.append(f"    PE:{row['pe']}({row['PE分类']}) | 股价:{row['price']:.2f}元 | {row['市值管理原因'][:50]}...")

    else:

        report.append("  无迫切需求公司")

    

    # 综合推荐

    report.append("\n【十、综合推荐名单】")

    report.append("  （借壳+战略+控权+市值管理综合得分最高）")

    top_comprehensive = result_df.nlargest(15, '综合得分')

    for idx, row in top_comprehensive.iterrows():

        report.append(f"\n  ▶ {row['name']}({row['ts_code']}): 综合得分{row['综合得分']}")

        report.append(f"    区域: {row['region']} | 行业: {row['industry']} | 市值: {row['market_cap_bn']:.1f}亿({row['market_cap_class']})")

        report.append(f"    股价: {row['price']:.2f}元 | PE: {row['pe']}倍({row['PE分类']}) | 相对估值: {row['相对估值']}")

        report.append(f"    国企: {'是' if row['is_soe'] else '否'} | ST: {'是' if row['is_st'] else '否'}")

        report.append(f"    借壳: {row['借壳上市概率']} | 战略: {row['战略投资者概率']} | 控权: {row['控股权变更概率']} | 市值管理: {row['市值管理迫切度']}")

        report.append(f"    估值评级: {row['估值评级']} | {row['投资建议']}")

    

    return "\n".join(report)


# ============================================================

# 主程序

# ============================================================


def main():

    """主函数"""

    

    # 加载行业PE数据

    industry_pe_data = load_industry_pe_data()

    

    # 获取Tushare Token

    token = os.environ.get('TUSHARE_TOKEN', '')

    

    if not token:

        print("=" * 60)

        print("警告: 未设置TUSHARE_TOKEN环境变量")

        print("将使用模拟数据进行演示")

        print("设置方法: export TUSHARE_TOKEN=your_token")

        print("=" * 60)

        

        return run_with_mock_data(industry_pe_data)

    

    # 获取真实数据

    df = get_tushare_data_enhanced(token, industry_pe_data)

    

    if df is None or len(df) == 0:

        print("获取数据失败，使用模拟数据...")

        return run_with_mock_data(industry_pe_data)

    

    # 分析数据

    result_df = analyze_data(df)

    

    # 生成报告

    report = generate_summary_report_v2(result_df)

    print(report)

    

    # 保存结果

    output_dir = '/workspace/a_stock_analysis/output'

    os.makedirs(output_dir, exist_ok=True)

    

    result_df.to_excel(f'{output_dir}/分析结果_V2.xlsx', index=False)

    result_df.to_csv(f'{output_dir}/分析结果_V2.csv', index=False, encoding='utf-8-sig')

    

    with open(f'{output_dir}/分析报告_V2.txt', 'w', encoding='utf-8') as f:

        f.write(report)

    

    with open(f'{output_dir}/分析规则说明_V2.md', 'w', encoding='utf-8') as f:

        f.write(ANALYSIS_RULES_V2)

    

    print(f"\n结果已保存到 {output_dir}/")

    

    return result_df


def run_with_mock_data(industry_pe_data):

    """使用模拟数据运行"""

    

    print("\n使用模拟数据进行演示...")

    

    np.random.seed(42)

    

    provinces = ['新疆', '西藏', '甘肃', '青海', '宁夏', '辽宁', '吉林', '黑龙江',

                 '云南', '贵州', '广西', '四川', '北京', '上海', '广东', '浙江', '江苏']

    

    industries = ['半导体', '软件开发', '房地产开发', '医药', '银行', '航空航天与国防',

                  '电力设备', '煤炭', '白酒', '钢铁', '有色金属', '化工', '环保']

    

    n_companies = 300

    mock_data = []

    

    for i in range(n_companies):

        province = np.random.choice(provinces)

        industry = np.random.choice(industries)

        is_soe = np.random.random() < 0.3

        price = np.round(np.random.uniform(2, 60), 2)

        total_mv = np.random.uniform(10, 800) * 10000

        

        # 基于行业生成PE

        base_pe = get_industry_pe(industry) or 25

        pe = round(base_pe * np.random.uniform(0.6, 1.5), 2)

        if np.random.random() < 0.15:

            pe = round(-abs(np.random.uniform(0.5, 5)), 2)

        

        is_st = np.random.random() < 0.08

        

        mock_data.append({

            'ts_code': f'{600000+i:06d}.SH',

            'name': f'{province[:2]}{industry[:2]}{"ST" if is_st else ""}{i+1}号',

            'province': province,

            'industry': industry,

            'close': price,

            'pe': pe,

            'pb': round(max(0.5, pe/10 * np.random.uniform(0.7, 1.3)), 2),

            'ps': round(np.random.uniform(1, 6), 2),

            'total_mv': total_mv,

            'circ_mv': total_mv * 0.7,

            'turnover_rate': round(np.random.uniform(0.5, 5), 2),

            'is_soe': is_soe,

            'actual_controller': '国资委' if is_soe else '个人'

        })

    

    df = pd.DataFrame(mock_data)

    

    # 分析数据

    result_df = analyze_data(df)

    

    # 生成报告

    report = generate_summary_report_v2(result_df)

    print(report)

    

    # 保存结果

    output_dir = '/workspace/a_stock_analysis/output'

    os.makedirs(output_dir, exist_ok=True)

    

    result_df.to_excel(f'{output_dir}/分析结果_V2.xlsx', index=False)

    result_df.to_csv(f'{output_dir}/分析结果_V2.csv', index=False, encoding='utf-8-sig')

    

    with open(f'{output_dir}/分析报告_V2.txt', 'w', encoding='utf-8') as f:

        f.write(report)

    

    with open(f'{output_dir}/分析规则说明_V2.md', 'w', encoding='utf-8') as f:

        f.write(ANALYSIS_RULES_V2)

    

    print(f"\n结果已保存到 {output_dir}/")

    

    return result_df


if __name__ == "__main__":

    main()