
# Squall Line Simulation Visualization Tools

## 概要 / Overview

このプロジェクトは、数値シミュレーション（SCALE-RMなど）によって出力されたスコールラインのNetCDF形式データを可視化するためのRスクリプト集です。
主な目的は、複数の異なる実験設定（例: Tomita08）の結果を、同じグラフ上に重ねて描画し、比較・分析することです。

This project is a collection of R scripts designed to visualize NetCDF output data from numerical simulations of squall lines (e.g., using the SCALE-RM model). The primary goal is to create comparative plots from multiple experimental runs, overlaying them on a single graph for analysis.

---
## 主な機能 / Features

* **複数実験の比較**: ほとんどのスクリプトで、複数のシミュレーション結果を読み込み、1つの図にまとめてプロットできます。
* **多様な可視化**: 以下のようないくつかの物理量について、様々な形式のプロットを作成します。
    * 時系列グラフ（領域平均、領域最大値など）
    * 時間で区切った棒グラフ（エラーバー付き）
* **設定の一元管理**: `config.R`ファイルで、データパスや比較対象の実験、プロットの見た目などを集中的に管理できます。

---
* **Compare Multiple Experiments**: Most scripts are capable of reading and plotting results from multiple simulations on a single figure.
* **Diverse Visualizations**: Create various plot types for several physical quantities:
    * Time-series plots (domain average, domain maximum, etc.)
    * Time-binned bar charts with error bars
    * Frequency distribution plots (histograms, frequency polygons)
* **Centralized Configuration**: A single `config.R` file allows for easy management of paths, experiments to compare, and plot aesthetics.

---
## ディレクトリ構成 / Directory Structure

```
.
├── config.R          # 設定ファイル (Central configuration file)
├── data/             # 生データ格納用 (For raw simulation data)
├── output/           # プロット出力用 (For generated plots)
├── scripts/          # 可視化スクリプト群 (R scripts for visualization)
├── util/             # 共通関数用 (For utility functions)
├── .gitignore        # Git管理対象外ファイルを指定 (Specifies files to be ignored by Git)
└── squall-line.Rproj # RStudioプロジェクトファイル (RStudio project file)
```

---
## 必要なもの / Requirements

* R (4.0以降を推奨 / version 4.0 or later recommended)
* R パッケージ (R Packages):
    * `ncdf4`
    * `fields`

以下のコマンドで必要なパッケージをインストールできます。/ You can install the required packages with the following command:
```R
install.packages(c("ncdf4", "fields"))
```

---
## 使い方 / Usage

1.  **データ配置 (Place Data Files)**:
    シミュレーションで出力されたNetCDFファイルを`data/`ディレクトリ以下に配置します。実験ごとにサブディレクトリを作成することを推奨します。（例: `data/tomita08/`）
    
    Place your NetCDF output files into the `data/` directory. It's recommended to create a subdirectory for each experiment (e.g., `data/SDM/`, `data/SN14/`).

2.  **設定ファイルの編集 (Edit Configuration)**:
    `config.R`を開き、プロジェクトのパスや、比較したい実験のパス、グラフの色などを設定します。
    
    Open `config.R` and edit the settings. This includes setting the project path and specifying which experiment files to analyze and what colors to use for plotting.

3.  **スクリプトの実行 (Run a Script)**:
    `scripts/`の中から実行したい可視化スクリプトを実行します。

    Run the desired visualization script from the `scripts/` directory.

4.  **結果の確認 (Check the Output)**:
    `output/`ディレクトリに、指定したファイル名でPDF形式のプロットが生成されます。
    
    Find the generated PDF plots in the `output/` directory.

---
## 作成者 / Author

* g-yamano