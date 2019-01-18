# Extra Packages for Enterprise Linux
sudo yum install https://dl.fedoraproject.org/pub/epel/epel-release-latest-$(rpm -E '%{rhel}').noarch.rpm
sudo yum install https://$(rpm -E '%{?centos:centos}%{!?centos:rhel}%{rhel}').iuscommunity.org/ius-release.rpm
sudo yum install yum-plugin-replace
sudo yum install cmake gcc-c++ make python3-devel

# General packages
sudo yum upgrade
sudo yum install rh-git29-git tmux
scl enable rh-git29 bash

# Adobe source code pro front
./helpers/source-font.sh

# Docker
sudo yum install -y yum-utils \
                    device-mapper-persistent-data \
                    lvm2
sudo yum-config-manager \
                --add-repo \
                https://download.docker.com/linux/centos/docker-ce.repo
sudo yum install docker-ce
sudo systemctl start docker

# docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.23.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Add user to docker for dev machines and VS Code
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker

# Rust
curl https://sh.rustup.rs -sSf | sh
source $HOME/.cargo/env
rustup toolchain add nightly
rustup component add rust-src
cargo +nightly install racer

# Miniconda
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
bash ~/miniconda.sh -b -p $HOME/miniconda

# MSSQL Drivers and tools
sudo curl https://packages.microsoft.com/config/rhel/7/prod.repo > /etc/yum.repos.d/mssql-release.repo
sudo yum remove unixODBC-utf16 unixODBC-utf16-devel
sudo ACCEPT_EULA=Y yum install msodbcsql17
sudo ACCEPT_EULA=Y yum install mssql-tools
source ~/.bashrc
sudo yum install unixODBC-devel 

# VS Code
wget https://go.microsoft.com/fwlink/?LinkID=760867 -O code.rpm 
sudo yum install code.rpm
./helpers/vs-extensions.sh
mv ./settings/settings.json ~/.config/Code/User
mv ./settings/keybindings.json ~/.config/Code/User
mv ./settings/vsicons.settings.json ~/.config/Code/User

# Emacs/Spacemacs
# wget http://mirrors.ibiblio.org/gnu/ftp/gnu/emacs/emacs-25.1.tar.xz
# tar xJvfp emacs-25.1.tar.xz
# cd emacs-25.1/
# sudo yum -y install libXpm-devel gnutls-devel libxml2-devel GConf2-devel dbus-devel wxGTK-devel gtk3-devel gpm-devel ncurses-devel
# ./configure
# make
# sudo make install
# 
# git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
# mv ./settings/.spacemacs ~/.spacemacs

# Vim
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

mv ./settings/.vimrc ~/
vim +PluginInstall +qall

pushd ~/.vim/bundle/youcompleteme
./install.py --clang-completer --rust-completer
popd

# tmux
git clone https://github.com/tmux-plugins/tmux-resurrect ~/
mv ./settings/.tmux.conf ~/

# bash customization via bash-it
git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
~/.bash_it/install.sh

# General user settings
mv ./settings/.profile ~/
mv ./settings/.bash_profile ~/
mv ./settings/.bashrc ~/
mv ./settings/.dev-tmux ~/

# Cleanup
sudo yum upgrade
sudo yum autoremove

# Some changes like the docker-group may require a reboot to take affect.
# sudo reboot
