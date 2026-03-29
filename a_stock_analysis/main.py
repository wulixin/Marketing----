#!/usr/bin/env python3

# -*- coding: utf-8 -*-

"""

A股上市公司数据获取与分析主程序

使用Tushare接口获取数据并进行分析


注意：运行前需要设置Tushare Token

export TUSHARE_TOKEN=your_token

或直接在代码中设置

"""


import sys

import os

sys.path.insert(0, os.path.dirname(os.path.abspath(__file__)))


import pandas as pd

import numpy as np

from datetime import datetime

import json


# 导入分析配置

from config import (

    comprehensive_analysis,

    REGION_DEFINITIONS,

    MARKET_CAP_THRESHOLDS,

    ANALYSIS_RULES

)


# ============================================================

# Tushare 数据获取

# ============================================================


def get_tushare_data(token=None):

    """

    使用Tushare获取A股上市公司数据

    

    需要的Tushare接口：

    1. stock_basic - 股票列表

    2. daily - 日线行情（用于计算市值、股价）

    3. stock_company - 上市公司基本信息（省份）

    """

    

    try:

        import tushare as ts

    except ImportError:

        print("请先安装tushare: pip install tushare")

        return None

    

    # 设置token

    if token:

        ts.set_token(token)

    

    pro = ts.pro_api()

    

    print("=" * 60)

    print("开始获取A股上市公司数据...")

    print("=" * 60)

    

    # 1. 获取股票列表

    print("\n[1/4] 获取股票列表...")

    try:

        stock_basic = pro.stock_basic(exchange='', list_status='L', fields='ts_code,symbol,name,area,industry,list_date')

        print(f"  获取到 {len(stock_basic)} 只股票")

    except Exception as e:

        print(f"  获取股票列表失败: {e}")

        return None

    

    # 2. 获取最新日线行情（计算市值）

    print("\n[2/4] 获取最新行情数据...")

    try:

        # 获取最近一个交易日的数据

        trade_cal = pro.trade_cal(exchange='SSE', is_open='1', 

                                   start_date=datetime.now().strftime('%Y%m%d'),

                                   end_date=(datetime.now() + pd.Timedelta(days=30)).strftime('%Y%m%d'))

        

        if len(trade_cal) > 0:

            latest_date = trade_cal['cal_date'].iloc[0]

        else:

            # 如果没有找到交易日，使用一个固定日期

            latest_date = '20241231'

        

        # 分批获取日线数据

        all_daily = []

        for i in range(0, len(stock_basic), 1000):

            codes = stock_basic['ts_code'].iloc[i:i+1000].tolist()

            try:

                daily = pro.daily(ts_code=','.join(codes[:100]), trade_date=latest_date)

                if len(daily) > 0:

                    all_daily.append(daily)

            except:

                pass

        

        if all_daily:

            daily_df = pd.concat(all_daily, ignore_index=True)

            print(f"  获取到 {len(daily_df)} 条行情数据")

        else:

            print("  使用模拟数据...")

            daily_df = create_mock_daily_data(stock_basic)

            

    except Exception as e:

        print(f"  获取行情数据失败: {e}")

        print("  使用模拟数据...")

        daily_df = create_mock_daily_data(stock_basic)

    

    # 3. 获取公司基本信息（省份）

    print("\n[3/4] 获取公司基本信息...")

    try:

        stock_company = pro.stock_company(exchange='SSE', fields='ts_code,province,city,actual_controller')

        if len(stock_company) == 0:

            stock_company = pro.stock_company(exchange='SZSE', fields='ts_code,province,city,actual_controller')

        print(f"  获取到 {len(stock_company)} 家公司信息")

    except Exception as e:

        print(f"  获取公司信息失败: {e}")

        stock_company = pd.DataFrame()

    

    # 4. 合并数据

    print("\n[4/4] 合并数据...")

    

    # 合并基本信息和行情

    merged_df = stock_basic.merge(

        daily_df[['ts_code', 'close', 'vol', 'amount', 'total_mv', 'circ_mv']].drop_duplicates('ts_code'),

        on='ts_code',

        how='left'

    )

    

    # 合并省份信息

    if len(stock_company) > 0:

        merged_df = merged_df.merge(

            stock_company[['ts_code', 'province', 'city', 'actual_controller']].drop_duplicates('ts_code'),

            on='ts_code',

            how='left'

        )

    else:

        merged_df['province'] = merged_df['area']  # 使用area作为省份

        merged_df['city'] = ''

        merged_df['actual_controller'] = ''

    

    # 判断是否国企

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


def create_mock_daily_data(stock_basic):

    """创建模拟行情数据（当Tushare不可用时）"""

    np.random.seed(42)

    n = len(stock_basic)

    

    mock_data = pd.DataFrame({

        'ts_code': stock_basic['ts_code'],

        'close': np.round(np.random.uniform(3, 50, n), 2),

        'vol': np.random.randint(100000, 10000000, n),

        'amount': np.random.randint(1000000, 100000000, n),

        'total_mv': np.random.uniform(10, 1000, n) * 10000,  # 万元

        'circ_mv': np.random.uniform(5, 500, n) * 10000

    })

    

    return mock_data


# ============================================================

# 数据分析

# ============================================================


def analyze_data(df):

    """对数据进行综合分析"""

    

    print("\n" + "=" * 60)

    print("开始数据分析...")

    print("=" * 60)

    

    # 计算相对成交量

    avg_vol = df['vol'].mean()

    df['volume_ratio'] = df['vol'] / avg_vol

    

    # 执行综合分析

    result_df = comprehensive_analysis(df)

    

    return result_df


def generate_summary_report(result_df):

    """生成分析报告"""

    

    report = []

    report.append("\n" + "=" * 80)

    report.append("A股上市公司市值管理与重组可能性分析报告")

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

    

    # 高概率公司筛选

    report.append("\n【四、高借壳上市概率公司（Top 20）】")

    shell_high = result_df[result_df['借壳上市概率'] == '高'].nlargest(20, '借壳上市得分')

    if len(shell_high) > 0:

        for idx, row in shell_high.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['借壳上市得分']} - {row['借壳上市原因']}")

    else:

        report.append("  无高概率公司")

    

    report.append("\n【五、高战略投资者概率公司（Top 20）】")

    strategic_high = result_df[result_df['战略投资者概率'] == '高'].nlargest(20, '战略投资者得分')

    if len(strategic_high) > 0:

        for idx, row in strategic_high.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['战略投资者得分']} - {row['战略投资者原因']}")

    else:

        report.append("  无高概率公司")

    

    report.append("\n【六、高控股权变更概率公司（Top 20）】")

    control_high = result_df[result_df['控股权变更概率'] == '高'].nlargest(20, '控股权变更得分')

    if len(control_high) > 0:

        for idx, row in control_high.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['控股权变更得分']} - {row['控股权变更原因']}")

    else:

        report.append("  无高概率公司")

    

    report.append("\n【七、市值管理迫切公司（Top 20）】")

    mgmt_urgent = result_df[result_df['市值管理迫切度'] == '迫切'].nlargest(20, '市值管理得分')

    if len(mgmt_urgent) > 0:

        for idx, row in mgmt_urgent.iterrows():

            report.append(f"  {row['name']}({row['ts_code']}): 得分{row['市值管理得分']} - {row['市值管理原因']}")

    else:

        report.append("  无迫切需求公司")

    

    # 重点推荐

    report.append("\n【八、综合推荐名单】")

    report.append("  （借壳+战略+控权+市值管理综合得分最高）")

    top_comprehensive = result_df.nlargest(20, '综合得分')

    for idx, row in top_comprehensive.iterrows():

        report.append(f"  {row['name']}({row['ts_code']}): 综合得分{row['综合得分']}")

        report.append(f"    区域: {row['region']} | 市值: {row['market_cap_bn']:.1f}亿({row['market_cap_class']}) | 股价: {row['price']:.2f}元")

        report.append(f"    国企: {'是' if row['is_soe'] else '否'} | ST: {'是' if row['is_st'] else '否'}")

        report.append(f"    借壳概率: {row['借壳上市概率']} | 战略投资概率: {row['战略投资者概率']} | 控股权变更概率: {row['控股权变更概率']} | 市值管理迫切度: {row['市值管理迫切度']}")

        report.append("")

    

    return "\n".join(report)


# ============================================================

# 主程序

# ============================================================


def main():

    """主函数"""

    

    # 获取Tushare Token

    token = os.environ.get('TUSHARE_TOKEN', '')

    

    if not token:

        print("=" * 60)

        print("警告: 未设置TUSHARE_TOKEN环境变量")

        print("将使用模拟数据进行演示")

        print("设置方法: export TUSHARE_TOKEN=your_token")

        print("=" * 60)

        

        # 使用模拟数据

        return run_with_mock_data()

    

    # 获取真实数据

    df = get_tushare_data(token)

    

    if df is None or len(df) == 0:

        print("获取数据失败，使用模拟数据...")

        return run_with_mock_data()

    

    # 分析数据

    result_df = analyze_data(df)

    

    # 生成报告

    report = generate_summary_report(result_df)

    print(report)

    

    # 保存结果

    output_dir = '/workspace/a_stock_analysis/output'

    os.makedirs(output_dir, exist_ok=True)

    

    result_df.to_excel(f'{output_dir}/分析结果.xlsx', index=False)

    result_df.to_csv(f'{output_dir}/分析结果.csv', index=False, encoding='utf-8-sig')

    

    with open(f'{output_dir}/分析报告.txt', 'w', encoding='utf-8') as f:

        f.write(report)

    

    print(f"\n结果已保存到 {output_dir}/")

    

    return result_df


def run_with_mock_data():

    """使用模拟数据运行"""

    

    print("\n使用模拟数据进行演示...")

    

    # 创建模拟股票数据

    np.random.seed(42)

    

    provinces = ['新疆', '西藏', '甘肃', '青海', '宁夏', '辽宁', '吉林', '黑龙江',

                 '云南', '贵州', '广西', '四川', '北京', '上海', '广东', '浙江', '江苏']

    

    industries = ['制造业', '房地产', '金融', '能源', '科技', '医药', '消费', '基建']

    

    n_companies = 200

    mock_data = []

    

    for i in range(n_companies):

        province = np.random.choice(provinces)

        is_soe = np.random.random() < 0.3  # 30%国企

        price = np.round(np.random.uniform(2, 60), 2)

        total_mv = np.random.uniform(10, 800) * 10000  # 万元

        

        # ST股概率

        is_st = np.random.random() < 0.1  # 10%概率是ST

        name_suffix = 'ST' if is_st else ''

        

        mock_data.append({

            'ts_code': f'{600000+i:06d}.SH',

            'name': f'{province[:2]}{np.random.choice(industries)}{name_suffix}{i+1}号',

            'province': province,

            'industry': np.random.choice(industries),

            'close': price,

            'vol': np.random.randint(100000, 5000000),

            'amount': np.random.randint(1000000, 50000000),

            'total_mv': total_mv,

            'circ_mv': total_mv * 0.7,

            'is_soe': is_soe,

            'actual_controller': '国资委' if is_soe else '个人'

        })

    

    df = pd.DataFrame(mock_data)

    

    # 分析数据

    result_df = analyze_data(df)

    

    # 生成报告

    report = generate_summary_report(result_df)

    print(report)

    

    # 保存结果

    output_dir = '/workspace/a_stock_analysis/output'

    os.makedirs(output_dir, exist_ok=True)

    

    result_df.to_excel(f'{output_dir}/分析结果.xlsx', index=False)

    result_df.to_csv(f'{output_dir}/分析结果.csv', index=False, encoding='utf-8-sig')

    

    with open(f'{output_dir}/分析报告.txt', 'w', encoding='utf-8') as f:

        f.write(report)

    

    # 同时保存规则说明

    with open(f'{output_dir}/分析规则说明.md', 'w', encoding='utf-8') as f:

        f.write(ANALYSIS_RULES)

    

    print(f"\n结果已保存到 {output_dir}/")

    

    return result_df


if __name__ == "__main__":

    main()