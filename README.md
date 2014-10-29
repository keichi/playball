# Playball

[![Build Status](https://api.shippable.com/projects/542b717a80088cee586d195c/badge?branchName=master)](https://app.shippable.com/projects/542b717a80088cee586d195c/builds/latest)

## Kickoffの使い方
1. activator/sbtを起動
2. `project kickoff`
3. `run 生成するクラスの完全修飾名 テンプレートのパス 出力ディレクトリ`
	- 例: `run models.BeerBrand kickoff/template/table.mustache output`
	- 注: 出力ディレクトリは事前に作成しておいてください。
