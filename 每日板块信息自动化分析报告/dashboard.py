"""

每日板块信息可视化面板

基于 Streamlit 构建，提供稳定的交互式数据展示

"""


import streamlit as st

import pandas as pd

import plotly.express as px

import plotly.graph_objects as go

from plotly.subplots import make_subplots

import os


# 页面配置

st.set_page_config(

    page_title="每日板块信息分析面板",

    page_icon="📊",

    layout="wide",

    initial_sidebar_state="expanded"

)


# 自定义样式

st.markdown("""

<style>

    .main-header {

        font-size: 2.5rem;

        font-weight: 700;

        color: #1f77b4;

        text-align: center;

        margin-bottom: 1rem;

    }

    .metric-card {

        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);

        padding: 1rem;

        border-radius: 10px;

        color: white;

    }

    .stMetric > div {

        background-color: #f0f2f6;

        padding: 10px;

        border-radius: 10px;

    }

</style>

""", unsafe_allow_html=True)


# 数据加载函数

@st.cache_data

def load_data():

    """加载Excel数据"""

    current_dir = os.path.dirname(os.path.abspath(__file__))

    excel_path = os.path.join(current_dir, '每日板块信息.xlsx')

    xlsx = pd.ExcelFile(excel_path)


    data = {

        '指数表现': pd.read_excel(xlsx, sheet_name='指数表现'),

        '指数估值': pd.read_excel(xlsx, sheet_name='指数估值'),

        '指数贡献': pd.read_excel(xlsx, sheet_name='指数贡献'),

        '每日板块信息': pd.read_excel(xlsx, sheet_name='每日板块信息')

    }

    return data


# 加载数据

try:

    data = load_data()

except Exception as e:

    st.error(f"数据加载失败: {e}")

    st.stop()


# 标题

st.markdown('<p class="main-header">📊 每日板块信息分析面板</p>', unsafe_allow_html=True)

st.markdown("---")


# ==================== 侧边栏导航 ====================

st.sidebar.title("📋 导航")

page = st.sidebar.radio(

    "选择视图",

    ["📈 概览总览", "💹 指数表现", "💰 指数估值", "🏆 板块排行", "🎯 指数贡献"],

    index=0

)


# ==================== 概览总览页 ====================

if page == "📈 概览总览":

    st.header("市场概览")


    # 顶部指标卡片

    col1, col2, col3, col4 = st.columns(4)


    df_index = data['指数表现']

    df_valuation = data['指数估值']


    with col1:

        sh_index = df_index[df_index['指数名称'] == '上证指数'].iloc[0]

        st.metric(

            "上证指数",

            f"{sh_index['收盘']:.2f}",

            f"{sh_index['日涨跌幅（%）']:.2f}%"

        )


    with col2:

        hs300 = df_index[df_index['指数名称'] == '沪深300'].iloc[0]

        st.metric(

            "沪深300",

            f"{hs300['收盘']:.2f}",

            f"{hs300['日涨跌幅（%）']:.2f}%"

        )


    with col3:

        zz500 = df_index[df_index['指数名称'] == '中证500'].iloc[0]

        st.metric(

            "中证500",

            f"{zz500['收盘']:.2f}",

            f"{zz500['日涨跌幅（%）']:.2f}%"

        )


    with col4:

        avg_pe = df_valuation['滚动市盈率'].mean()

        st.metric(

            "平均滚动市盈率",

            f"{avg_pe:.2f}"

        )


    st.markdown("---")


    # 第一行：指数涨跌幅和估值对比

    col_left, col_right = st.columns(2)


    with col_left:

        st.subheader("指数日涨跌幅")

        fig_bar = px.bar(

            df_index,

            x='指数名称',

            y='日涨跌幅（%）',

            color='日涨跌幅（%）',

            color_continuous_scale='RdYlGn',

            title="各指数当日涨跌幅对比",

            text_auto=True

        )

        fig_bar.update_layout(

            xaxis_tickangle=-45,

            height=400,

            showlegend=False

        )

        st.plotly_chart(fig_bar, use_container_width=True)


    with col_right:

        st.subheader("估值指标对比")

        df_val = df_valuation.rename(columns={'指数名称）': '指数名称'})

        fig_val = go.Figure(data=[

            go.Bar(name='滚动市盈率', x=df_val['指数名称'], y=df_val['滚动市盈率'], marker_color='#1f77b4'),

            go.Bar(name='市净率', x=df_val['指数名称'], y=df_val['市静率'], marker_color='#ff7f0e'),

            go.Bar(name='股息率(%)', x=df_val['指数名称'], y=df_val['股息率'], marker_color='#2ca02c')

        ])

        fig_val.update_layout(

            barmode='group',

            title="核心估值指标对比",

            xaxis_tickangle=-45,

            height=400,

            legend=dict(orientation="h", yanchor="bottom", y=1.02)

        )

        st.plotly_chart(fig_val, use_container_width=True)


    # 第二行：板块表现

    st.subheader("板块表现排行")


    df_sector = data['每日板块信息']

    df_best = df_sector[df_sector['指数名称'] == '沪市最强表现'].head(10)

    df_worst = df_sector[df_sector['指数名称'] == '沪市最弱表现'].head(10)


    col1, col2 = st.columns(2)


    with col1:

        fig_best = px.bar(

            df_best,

            x='日涨跌幅（%）',

            y='板块名称',

            orientation='h',

            color='日涨跌幅（%）',

            color_continuous_scale='Greens',

            title="🚀 最强板块 TOP10"

        )

        fig_best.update_layout(height=400, yaxis={'categoryorder': 'total ascending'})

        st.plotly_chart(fig_best, use_container_width=True)


    with col2:

        fig_worst = px.bar(

            df_worst,

            x='日涨跌幅（%）',

            y='板块名称',

            orientation='h',

            color='日涨跌幅（%）',

            color_continuous_scale='Reds',

            title="📉 最弱板块 TOP10"

        )

        fig_worst.update_layout(height=400, yaxis={'categoryorder': 'total ascending'})

        st.plotly_chart(fig_worst, use_container_width=True)


# ==================== 指数表现页 ====================

elif page == "💹 指数表现":

    st.header("指数表现详情")


    df_index = data['指数表现']


    # 数据表格

    st.subheader("指数表现数据")

    st.dataframe(

        df_index.style.format({

            '收盘': '{:.2f}',

            '日涨跌': '{:.2f}',

            '日涨跌幅（%）': '{:.2f}',

            '今年以来涨跌': '{:.2f}',

            '今年以来涨跌幅（%）': '{:.2f}',

            '成交额较昨日增减（亿元）': '{:.2f}',

            '成交额较昨日增减（%）': '{:.2f}'

        }).background_gradient(subset=['日涨跌幅（%）'], cmap='RdYlGn'),

        use_container_width=True

    )


    # 可视化

    col1, col2 = st.columns(2)


    with col1:

        # 日涨跌幅 vs 今年以来涨跌幅

        fig = go.Figure()

        fig.add_trace(go.Bar(

            name='日涨跌幅(%)',

            x=df_index['指数名称'],

            y=df_index['日涨跌幅（%）'],

            marker_color='#3498db'

        ))

        fig.add_trace(go.Bar(

            name='今年以来涨跌幅(%)',

            x=df_index['指数名称'],

            y=df_index['今年以来涨跌幅（%）'],

            marker_color='#e74c3c'

        ))

        fig.update_layout(

            barmode='group',

            title="日涨跌幅 vs 今年以来涨跌幅",

            xaxis_tickangle=-45,

            height=450

        )

        st.plotly_chart(fig, use_container_width=True)


    with col2:

        # 成交额变化

        fig_vol = px.bar(

            df_index,

            x='指数名称',

            y='成交额较昨日增减（亿元）',

            color='成交额较昨日增减（亿元）',

            color_continuous_scale='RdYlGn',

            title="成交额较昨日变化（亿元）",

            text_auto=True

        )

        fig_vol.update_layout(xaxis_tickangle=-45, height=450)

        st.plotly_chart(fig_vol, use_container_width=True)


# ==================== 指数估值页 ====================

elif page == "💰 指数估值":

    st.header("指数估值分析")


    df_val = data['指数估值'].rename(columns={'指数名称）': '指数名称'})


    # 数据表格

    st.subheader("估值数据")

    st.dataframe(

        df_val.style.format({

            '静态市盈率': '{:.2f}',

            '滚动市盈率': '{:.2f}',

            '市静率': '{:.2f}',

            '股息率': '{:.2f}',

            '去年静态市盈率': '{:.2f}',

            '去年滚动市盈率': '{:.2f}',

            '去年市静率': '{:.2f}'

        }).background_gradient(subset=['滚动市盈率'], cmap='YlOrRd'),

        use_container_width=True

    )


    # 可视化

    col1, col2 = st.columns(2)


    with col1:

        # 市盈率对比

        fig_pe = go.Figure()

        fig_pe.add_trace(go.Bar(

            name='当前滚动市盈率',

            x=df_val['指数名称'],

            y=df_val['滚动市盈率'],

            marker_color='#3498db'

        ))

        fig_pe.add_trace(go.Bar(

            name='去年滚动市盈率',

            x=df_val['指数名称'],

            y=df_val['去年滚动市盈率'],

            marker_color='#95a5a6'

        ))

        fig_pe.update_layout(

            barmode='group',

            title="滚动市盈率：当前 vs 去年同期",

            xaxis_tickangle=-45,

            height=450

        )

        st.plotly_chart(fig_pe, use_container_width=True)


    with col2:

        # 股息率 vs 市净率散点图

        fig_scatter = px.scatter(

            df_val,

            x='市静率',

            y='股息率',

            text='指数名称',

            size='滚动市盈率',

            color='滚动市盈率',

            color_continuous_scale='Viridis',

            title="估值矩阵：市净率 vs 股息率（气泡大小=市盈率）"

        )

        fig_scatter.update_traces(textposition='top center')

        fig_scatter.update_layout(height=450)

        st.plotly_chart(fig_scatter, use_container_width=True)


    # 估值热力图

    st.subheader("估值指标热力图")

    heatmap_data = df_val.set_index('指数名称')[['静态市盈率', '滚动市盈率', '市静率', '股息率']]

    fig_heat = px.imshow(

        heatmap_data.T,

        labels=dict(x="指数", y="指标", color="数值"),

        x=heatmap_data.index,

        y=heatmap_data.columns,

        color_continuous_scale='RdYlGn_r',

        aspect="auto"

    )

    fig_heat.update_layout(height=300)

    st.plotly_chart(fig_heat, use_container_width=True)


# ==================== 板块排行页 ====================

elif page == "🏆 板块排行":

    st.header("板块表现排行")


    df_sector = data['每日板块信息']


    # 分类数据

    df_best = df_sector[df_sector['指数名称'] == '沪市最强表现']

    df_worst = df_sector[df_sector['指数名称'] == '沪市最弱表现']


    # 显示数量选择

    top_n = st.slider("显示数量", min_value=5, max_value=30, value=10)


    col1, col2 = st.columns(2)


    with col1:

        st.subheader("🚀 最强板块")

        df_best_top = df_best.head(top_n)

        fig_best = px.bar(

            df_best_top,

            x='日涨跌幅（%）',

            y='板块名称',

            orientation='h',

            color='日涨跌幅（%）',

            color_continuous_scale='Greens',

            text_auto='.2f'

        )

        fig_best.update_layout(

            height=500,

            yaxis={'categoryorder': 'total ascending'}

        )

        st.plotly_chart(fig_best, use_container_width=True)


        st.dataframe(

            df_best_top.style.format({'日涨跌幅（%）': '{:.2f}'}).background_gradient(

                subset=['日涨跌幅（%）'], cmap='Greens'

            ),

            use_container_width=True,

            height=300

        )


    with col2:

        st.subheader("📉 最弱板块")

        df_worst_top = df_worst.head(top_n)

        fig_worst = px.bar(

            df_worst_top,

            x='日涨跌幅（%）',

            y='板块名称',

            orientation='h',

            color='日涨跌幅（%）',

            color_continuous_scale='Reds',

            text_auto='.2f'

        )

        fig_worst.update_layout(

            height=500,

            yaxis={'categoryorder': 'total ascending'}

        )

        st.plotly_chart(fig_worst, use_container_width=True)


        st.dataframe(

            df_worst_top.style.format({'日涨跌幅（%）': '{:.2f}'}).background_gradient(

                subset=['日涨跌幅（%）'], cmap='Reds_r'

            ),

            use_container_width=True,

            height=300

        )


    # 板块涨跌分布

    st.subheader("板块涨跌分布")

    fig_dist = px.histogram(

        df_sector,

        x='日涨跌幅（%）',

        nbins=20,

        color='指数名称',

        title="板块涨跌幅分布",

        marginal='box'

    )

    fig_dist.update_layout(height=400)

    st.plotly_chart(fig_dist, use_container_width=True)


# ==================== 指数贡献页 ====================

elif page == "🎯 指数贡献":

    st.header("指数成分股贡献分析")


    df_contrib = data['指数贡献']


    # 选择指数

    unique_indices = df_contrib['贡献指数名称'].unique()

    selected_index = st.selectbox("选择指数", unique_indices)


    df_selected = df_contrib[df_contrib['贡献指数名称'] == selected_index]


    # 数据表格

    st.subheader(f"{selected_index} 成分股贡献")

    st.dataframe(

        df_selected.style.format({

            '收盘价': '{:.2f}',

            '日涨跌幅(%)': '{:.2f}',

            '贡献点数': '{:.2f}'

        }).background_gradient(subset=['贡献点数'], cmap='RdYlGn'),

        use_container_width=True

    )


    col1, col2 = st.columns(2)


    with col1:

        # 贡献点数排行

        fig_contrib = px.bar(

            df_selected.sort_values('贡献点数', ascending=True),

            x='贡献点数',

            y='股票名称',

            orientation='h',

            color='贡献点数',

            color_continuous_scale='RdYlGn',

            title="贡献点数排行"

        )

        fig_contrib.update_layout(height=600)

        st.plotly_chart(fig_contrib, use_container_width=True)


    with col2:

        # 涨跌幅 vs 贡献点数

        fig_scatter = px.scatter(

            df_selected,

            x='日涨跌幅(%)',

            y='贡献点数',

            text='股票名称',

            color='贡献点数',

            color_continuous_scale='RdYlGn',

            title="涨跌幅 vs 贡献点数"

        )

        fig_scatter.update_traces(textposition='top center')

        fig_scatter.update_layout(height=600)

        st.plotly_chart(fig_scatter, use_container_width=True)


    # 正负贡献对比

    st.subheader("正负贡献统计")

    positive = df_selected[df_selected['贡献点数'] > 0]['贡献点数'].sum()

    negative = df_selected[df_selected['贡献点数'] < 0]['贡献点数'].sum()


    col1, col2, col3 = st.columns(3)

    with col1:

        st.metric("正贡献合计", f"{positive:.2f}点")

    with col2:

        st.metric("负贡献合计", f"{negative:.2f}点")

    with col3:

        st.metric("净贡献", f"{positive + negative:.2f}点")


# 页脚

st.markdown("---")

st.markdown(

    "<div style='text-align: center; color: #888;'>"

    "📊 每日板块信息可视化面板 | 数据来源：每日板块信息.xlsx"

    "</div>",

    unsafe_allow_html=True

)