# dualdag

dualdag 是给统计程序员使用的本地网页应用。它根据 ADaM spec 和 SDTM 或外部 ADaM 的 XPT 数据建立依赖图，为每个变量生成两份相互独立的 R 程序，运行后比较结果。你需要安装 R，并配置一个大模型服务，或登录受支持的编程命令行工具。

请自行检查生成的代码和结果。dualdag 尚未通过监管用途验证。

## 下载

从 GitHub Releases 下载 ZIP，或选择 **Code > Download ZIP** 后解压。也可以克隆仓库。

## 快速启动

需要 R 4.2 或更高版本。不需要 Node.js 或 Python。首次启动时，dualdag 会检查所需的 R 包；安装依赖时需要联网，或配置公司内部 CRAN 镜像。

在 RStudio 中打开解压后的文件夹并运行：

```r
source("run.R")
```

也可以在终端运行：

```sh
Rscript run.R
Rscript run.R --no-browser
Rscript run.R --install-deps
Rscript run.R --project=~/studies/study-01 --port=18790
Rscript run.R --restore-admin-snapshot --project=~/studies/study-01
```

`--restore-admin-snapshot` 会恢复该项目最近一次 App Admin 快照；恢复后再正常启动应用。应用只监听 `127.0.0.1`，并打印带 token 的网址。请在本次会话中保留该网址；按 Ctrl+C 停止应用。其他选项包括 `--port=N`、`--project=DIR` 和 `--no-browser`；对应环境变量为 `DUALDAG_PORT`、`DUALDAG_PROJECT` 和 `DUALDAG_NO_BROWSER=1`。没有图形界面的 Linux 不会自动打开浏览器。设置 `DUALDAG_URL_FILE` 后，网址会写入指定文件。

默认项目目录是 `~/dualdag-projects/default`。配置保存在 `~/.dualdag/`；设置 `DUALDAG_HOME` 可以更换配置目录。

## 试用示例

在应用目录中运行任一示例：

```sh
Rscript run_demo_adsl.R
Rscript run_demo_advs.R
```

ADSL 和 ADVS 示例 spec 已包含在下载包中。首次运行时，启动脚本会从 `pharmaversesdtm` 获取示例源数据，并生成 ADVS 示例数据。此步骤需要联网，以及 `pharmaversesdtm` 和 `haven` R 包（ADVS 还需要 `dplyr`）。该数据包使用 Apache 2.0 许可证，详见 [demos/README.txt](demos/README.txt)。

也可以手动获取或更新数据：运行 `Rscript tools/get_demo_data.R adsl`、`Rscript tools/get_demo_data.R advs` 或 `Rscript tools/get_demo_data.R all`。

## 应用功能

- 上传 `variable.csv`、可选的 `parameter.csv`，以及按域命名的 SDTM 或外部 ADaM `.xpt` 文件（例如 `dm.xpt`）。可在 **Spec & checks** 下载 spec 模板。Dataset、Variable 和 PARAMCD 会转为大写；Key 编号必须从 1 开始连续递增。
- 生成变量依赖 DAG。对于 BDS spec，先生成 Parameter DAG，再按参数逐个生成变量 DAG。参数会分为 carry-over（延续）或 derived（衍生）；所有 carry-over 参数作为一组生成。
- 使用 **DAG agent** 讨论图的修改；在支持的页面（Settings、Spec & checks、R code & runs、Data 和 Mismatches）使用 **Super agent**。**App Admin** 通过已登录的 Claude Code 或 Codex 命令行工具修改应用和当前项目，并创建可在应用中撤销或通过命令行恢复的快照。
- 在 **R code & runs** 中生成并运行 Primary 和 Validation 程序。两者使用独立的 R 进程，随后比较输出。Primary 只能使用该变量声明的 lineage 来源；Validation 可以访问所有源数据，并以 spec 为参考。双方无法读取对方的代码、日志、笔记和结果。
- 查看不一致的行，在 DAG 页面下载网络 JSON。使用 **Reset** 清除项目工作内容：DAG、程序、中间数据、会话、日志和导出文件都会删除，spec 与原始数据会保留。
- 在 coder 窗口开启 **SU mode**，即可让助手在查看全部 spec 和数据的情况下检查当前一侧；它只能编辑这一侧。

## 项目文件

```text
<project>/
  spec/variable.csv
  spec/parameter.csv
  data/<domain>.xpt
  .dualdag/state/                 保存状态、DAG、布局和网络 JSON
  .dualdag/runs/<DATASET>/<VAR>/  生成的代码、日志、结果和比较记录
  .dualdag/app-admin/snapshots/   App Admin 恢复快照
  output/                         导出的数据集
```

下载包还包含 `demos/`（示例 spec 和启动脚本）以及 `tools/get_demo_data.R`（获取并构建示例数据的脚本）。

## 安全与隐私

- 应用绑定到 `127.0.0.1`。每次启动都会生成会话 token，API 请求必须携带该 token；应用也会检查 `Host` 和 `Origin`。HTML 响应包含 Content-Security-Policy。
- 生成的程序是在本机以你的账户权限运行的 R 代码，并在独立进程中执行。运行前请检查代码。运行超时默认为 600 秒，可在 Settings 中修改。
- API key 由 R 发送给你选择的大模型服务，不会经过浏览器。key 保存在权限为 600 的 `~/.dualdag/api_key`，或从 `DUALDAG_LLM_API_KEY` 读取；不会写入 `config.json`。
- **你的数据可能会发送给大模型服务商。** 依赖抽取、代码生成和 self-review 会发送 spec 文本。代码生成和 self-review 还会发送 lineage 中各源列的类型、缺失值数量和最多 20 个样本值。self-review 会收到该侧生成的代码及运行结果。你主动发起 mismatch 调查时，最多会发送 200 行不一致数据。使用编程命令行工具作为模型服务时，这些信息会经过该工具。
- dualdag 不收集遥测数据。它会连接你选择的大模型服务；安装 R 包或获取示例数据时会连接 CRAN。
- 设置 `DUALDAG_FAKE_LLM=1` 后会使用确定性的模拟回复，不会连接大模型服务。

## 联系方式

如有问题、错误报告或反馈，请联系 [wayne.ding26@gmail.com](<mailto:wayne.ding26@gmail.com>)。仓库公开后，也可以使用 GitHub Issues。

## 许可证

本应用采用 MIT 许可证，详见 [LICENSE](LICENSE)。`DESCRIPTION` 中声明为 `MIT + file LICENSE`。
