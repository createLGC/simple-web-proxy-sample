**※注 Windowsではそのままだとビルドも実行もできません。Windowsパソコンの中にLinux環境をご用意いただき、その中でビルド・実行してください。**

## ビルド
以下のコマンドでビルドできます
```
make
```
**※** ビルドにはC++17以降に対応したclangが必要です。clangが使えない場合、Makefileをご自身で書き換えてください。

## 実行
ビルドした後、以下のコマンドで実行できます
```
make run
```
**※** Mac mini M1 macOS Sonoma 14.4.1でしか動作確認しておりませんが、OS依存のコードはないと思うのでUnix系OSであれば動くはずです
