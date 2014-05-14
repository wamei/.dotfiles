# Mac、Linux用設定ファイル群
ユーザのホームディレクトリにクローンし

makeSymbolicLink.shを実行する

## 内容
1. .inputrc
 - bash入力時に大文字小文字を区別しない
1. .bashrc
 - bash用設定ファイル
 - ロケールを東京に
 - 文字コードをUTF-8に
 - alias: ls='ls -a --color=auto'
 - alias: ec='emacsclient'
 - alias: screen='screen -U'
 - 起動時に環境変数をemacs用に書き出し
1. .tmux.conf
 - tmux用設定ファイル
 - 詳しくは中身を
1. .screenrc
 - screen用設定ファイル
 - 256色モードにする
 - プレフィックスをC-tに
1. .emacs.d/
 - Emacs用設定フォルダ
 - Anything用のファイル一覧作成スクリプトも付属
   - sudo ruby /path/to/make-filelist.rb > /tmp/all.filelist
