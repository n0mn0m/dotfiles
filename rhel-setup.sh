sudo yum install rh-git29-git
scl enable rh-git29 bash

./helpers/source-font.sh

wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
bash ~/miniconda.sh -b -p $HOME/miniconda

sudo curl https://packages.microsoft.com/config/rhel/7/prod.repo > /etc/yum.repos.d/mssql-release.repo
sudo yum remove unixODBC-utf16 unixODBC-utf16-devel
sudo ACCEPT_EULA=Y yum install msodbcsql17
sudo ACCEPT_EULA=Y yum install mssql-tools
source ~/.bashrc
sudo yum install unixODBC-devel 

wget https://go.microsoft.com/fwlink/?LinkID=760867 -O code.rpm 
sudo yum install code.rpm
./helpers/vs-extensions.sh
mv ./settings/settings.json ~/.config/Code/User
mv ./settings/keybindings.json ~/.config/Code/User
mv ./settings/vsicons.settings.json ~/.config/Code/User

wget http://mirrors.ibiblio.org/gnu/ftp/gnu/emacs/emacs-25.1.tar.xz
tar xJvfp emacs-25.1.tar.xz
cd emacs-25.1/
sudo yum -y install libXpm-devel gnutls-devel libxml2-devel GConf2-devel dbus-devel wxGTK-devel gtk3-devel gpm-devel ncurses-devel
./configure
make
sudo make install

git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
mv ./settings/.spacemacs ~/.spacemacs

git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

mv ./settings/.vimrc ~/
vim +PluginInstall +qall

mv ./settings/.profile ~/
mv ./settings/.bash_profile ~/
mv ./settings/.bashrc ~/

git clone https://github.com/tmux-plugins/tmux-resurrect ~/
mv ./settings/.tmux.conf ~/

git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
~/.bash_it/install.sh
