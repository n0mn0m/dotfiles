# General Dependencies
sudo apt-get install -yqq --no-install-recommends \
        build-essential \
        apt-utils \
        rsync \
        netcat \
        locales \
        gnupg2 \
        apt-transport-https \
        tmux \
        ca-certificates \
        curl \
        software-properties-common \
	cmake \
	python3-devel

# Adobe Source Code Pro Fonts
./helpers/source-font.sh

# Docker
curl -fsSL https://download.docker.com/linux/ubuntu/gpg | sudo apt-key add -
sudo add-apt-repository \
   "deb [arch=amd64] https://download.docker.com/linux/ubuntu \
   $(lsb_release -cs) \
   stable"
sudo apt-get update
sudo apt-get install docker-ce   

# docker-compose
sudo curl -L "https://github.com/docker/compose/releases/download/1.23.1/docker-compose-$(uname -s)-$(uname -m)" -o /usr/local/bin/docker-compose
sudo chmod +x /usr/local/bin/docker-compose

# Add user to docker for dev machines and VS Code
sudo groupadd docker
sudo usermod -aG docker $USER
sudo systemctl enable docker

# Miniconda
wget https://repo.anaconda.com/miniconda/Miniconda3-latest-Linux-x86_64.sh -O ~/miniconda.sh
bash ~/miniconda.sh -b -p $HOME/miniconda

# MSSQL Drivers and tools
sudo curl https://packages.microsoft.com/config/ubuntu/18.04/prod.list > /etc/apt/sources.list.d/mssql-release.list 
sudo apt-get update
sudo ACCEPT_EULA=Y apt-get install msodbcsql17
sudo ACCEPT_EULA=Y apt-get install mssql-tools
sudo apt-get install unixodbc-dev

# VS Code
wget https://go.microsoft.com/fwlink/?LinkID=760868 -O code.deb
sudo apt install code.deb
./helpers/vs-extensions.sh
mv ./settings/settings.json ~/.config/Code/User
mv ./settings/keybindings.json ~/.config/Code/User
mv ./settings/vsicons.settings.json ~/.config/Code/User

# Vim
git clone https://github.com/gmarik/Vundle.vim.git ~/.vim/bundle/Vundle.vim
vim +PluginInstall +qall

mv ./settings/.vimrc ~/
vim +PluginInstall +qall

pushd ~/.vim/bundle/youcompleteme
python3 install.py --clang-completer
popd

# Spacemacs
#git clone https://github.com/syl20bnr/spacemacs ~/.emacs.d
#mv ./settings/.spacemacs ~/.spacemacs

# tmux
git clone https://github.com/tmux-plugins/tmux-resurrect ~/
mv ./settings/.tmux.conf ~/

# bash-it
git clone --depth=1 https://github.com/Bash-it/bash-it.git ~/.bash_it
~/.bash_it/install.sh

# General user settings
mv ./settings/.profile ~/
mv ./settings/.bash_profile ~/
mv ./settings/.bashrc ~/
mv ./settings/.dev-tmux ~/

# Cleanup
sudo apt-get upgrade
sudo apt-get autoremove

# For some settings to take hold such as the docker-group you will need to reboot
# sudo reboot
