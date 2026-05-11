# ChpSTREM2AD

## 2026-04-14 Update

- Formal analysis entry: `statistics/run_all.R`
- Methods manuscript draft for writing is available in:
  `document/论文方法_统计分析方案.md`
- Each run now writes all outputs into a versioned folder under:
  `result/YYYYMMDD_HHMMSS/`
- The version folder includes cleaned data, summaries, tables, figures, report, and run metadata.
- The integrated report is generated automatically in:
  `result/YYYYMMDD_HHMMSS/report/ChpSTREM2AD_analysis_report.md`
- Report language and display labels are controlled by `statistics/local.json`.
- Overall descriptive statistics and narrative result paragraphs are generated automatically.
- The workflow now also includes reviewer-oriented extended models:
  `tau/Aβ`-adjusted regression, `sTREM2 × diagnosis`, nonlinearity checks,
  and the `sTREM2 -> PTAU -> cognition` SEM path.
- SEM outputs record the adjusted covariates:
  `S_PTGENDER + S_AGE + PTEDUCAT + APOE401`
- In the Markdown report, significant p-values are shown as bold text with:
  `*` for `p < 0.05`, `**` for `p < 0.001`
- `result/LATEST.txt` points to the most recent run.

本项目用于重新分析：

- `sTREM2`
- 脉络丛体积 / ICV (`ChPICV`)
- AD 相关 CSF 生物标志物
- 认知功能复合指标

## 当前数据源

- 原始数据：`data/raw/Data.csv`
- 研究材料：`document/`
- 统计入口：`statistics/project_config.R`
- 一键运行：`statistics/run_all.R`
- 自动报告：`result/report/ChpSTREM2AD_analysis_report.md`

## 当前分析主线

1. 项目内清洗数据，只保留本课题使用变量
2. 判断连续变量正态性并自动生成 `L_变量名`
3. 完成诊断组组间比较
4. 检查年龄和性别是否失衡，必要时做 pairwise matching
5. 完成 `ChPICV` 和 `sTREM2` 的线性回归与偏相关
6. 完成 `PTAU` 与 `ChPICV` / `sTREM2` 的关联分析
7. 以三套认知功能构成方式完成 SEM 风格中介分析并生成关系图
8. 自动汇总所有结果并输出 Markdown 总报告
