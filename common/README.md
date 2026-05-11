# common

## 2026-04-14 Update

- `common/R/utils_io.R` now creates versioned result folders for every run.
- Default output pattern:
  `project/<project_name>/result/YYYYMMDD_HHMMSS/`
- The common layer writes `result/LATEST.txt` and supports per-version analysis logs.
- Shared Markdown report helpers remain in `common/R/reporting_utils.R`.
- Shared JSON defaults live in `common/config/defaults.json`.
- Shared multilingual text strings live in `common/config/locale.json`.

`common/` 用于存放跨项目共享的 R 统计方法。新的设计目标是：

- 尽量不依赖外部扩展包
- 变量、路径、图表和结果输出可复用
- 结果优先返回 `data.frame`
- 公共方法不写死任何项目名

## 文件说明

- `R/utils_io.R`：读写、目录、日志、HTML 表格导出
- `R/data_processing.R`：数值清洗、脏值处理、正态性筛查、`log/log1p` 变换、认知复合指标
- `R/descriptive_stats.R`：描述性统计、t 检验、卡方检验
- `R/group_comparison.R`：ANOVA、Kruskal-Wallis、组间分类变量比较、调整模型
- `R/association_models.R`：线性回归、分组线性回归、偏相关
- `R/matching_sem.R`：贪心匹配、回归式中介分析、关系图绘制
- `R/regression_linear.R`：基础线性回归接口
- `R/regression_logistic.R`：基础 Logistic 回归接口
- `R/plot_utils.R`：散点图、箱线图、基础图形保存
- `R/table_utils.R`：结果表格式化与三线表导出

## 使用原则

1. 新方法若可跨项目复用，优先放入 `common/R/`
2. 每次新增方法后，同步更新本文件和 `help/使用说明.md`
3. 若缺少扩展包，优先提供 base R 回退方案

## 调用方式

```r
source(file.path(project_root, "..", "..", "common", "R", "utils_io.R"))
load_common_functions(file.path(project_root, "..", "..", "common", "R"))
```
