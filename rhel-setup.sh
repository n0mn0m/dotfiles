sudo yum install rh-git29-git
scl enable rh-git29 bash

wget http://mirrors.ibiblio.org/gnu/ftp/gnu/emacs/emacs-25.1.tar.xz
tar xJvfp emacs-25.1.tar.xz
cd emacs-25.1/
sudo yum -y install libXpm-devel gnutls-devel libxml2-devel GConf2-devel dbus-devel wxGTK-devel gtk3-devel gpm-devel ncurses-devel
./configure
make
sudo make install

git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
~/.bash_it/install.sh

curl -o source-font.sh https://gist.githubusercontent.com/AlexHagerman/0a83d66236998ae7648740b674c1b59b/raw/436c2b78d1bda662054a8d404e419250e1ca998f/source-font.sh
chmod +x source-font.sh
./source-font.sh

curl -o .profile https://gist.githubusercontent.com/AlexHagerman/a029ce475ec749ddf76a1729fd49436c/raw/9e51e813028c60a39c40dac83e8f265d130e08d6/.profile
curl -o .tmux.conf https://gist.githubusercontent.com/AlexHagerman/bb2470b076c544b291cdc3ac866474a9/raw/30acb98ac4b1ca3cbd9e4f838596ddf5bf18c549/.tmux.conf
curl -o .bashrc https://gist.githubusercontent.com/AlexHagerman/dd91e3222569357ffeac9087a89bd0fa/raw/cc3da699124255ba03cdad15eddfa47af07e1fe2/.bashrc
curl -o .vimrc https://gist.githubusercontent.com/AlexHagerman/3964d62ecd43a3c0e46082c2330e1a3c/raw/750cc07b403c1d08ed1ea850124714afa9601bb2/.vimrc

vim +PluginInstall +qall
