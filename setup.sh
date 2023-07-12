autoload -Uz bracketed-paste-magic
zle -N bracketed-paste bracketed-paste-magic

autoload -Uz url-quote-magic
zle -N self-insert url-quote-magic

https://github.com/hjoshi123/M1DevSetup

zsh syntax highlighting
zsh you should use
zsh command not found

https://gist.github.com/LukeSmithxyz/e62f26e55ea8b0ed41a65912fbebbe52

https://github.com/zsh-users/antigen

https://github.com/michaelschwobe/mac-dev-setup
https://github.com/RamiKrispin/awesome-ds-setting

# https://apple.stackexchange.com/questions/101090/list-of-all-packages-installed-using-homebrew
# `brew bundle dump` to get the Brewfile
brew bundle 

## python3 is installed, this upgrades the pip setup:
pip install --upgrade setuptools
pip install --upgrade pip


## config files
~/.emacs.d/init.el
~/.config/starship.toml 



## battery charging
# turn off "smart" battery charging (how)?
# then install this: https://github.com/actuallymentor/battery
sudo curl https://raw.githubusercontent.com/actuallymentor/battery/main/setup.sh | sudo bash
# has ^M characters on all lines: 
sudo dos2unix /usr/local/bin/battery


## set up emacs-mac with launchpad/app support
# https://gist.github.com/jasonm23/89ad29748f49ddbafab1
brew tap railwaycat/emacsmacport
#brew update
brew install --cask emacs-mac

#if [[ -d /usr/local/Cellar/emacs-mac ]]; then
 # brew upgrade emacs-mac
 #else
  #brew install emacs-mac
  #fi  

# brew install emacs-mac --with-emacs-sexy-icon --with-natural-title-bar --with-starter

# Locate the Emacs.app from brew installed emacs-mac
#mv $( brew info emacs-mac | grep -A1 "was installed to:" | tail -1 )/Emacs.app /Applications/
#touch /Applications/Emacs.app


## we can probably automate settings?
## https://apple.stackexchange.com/questions/198257/how-can-i-script-the-setup-of-preferences-in-mac-os-x
## http://www.defaults-write.com/
## https://www.shell-tips.com/mac/defaults/#gsc.tab=0
## also check this: https://github.com/mathiasbynens/dotfiles/blob/master/.macos



# disable screensaver at login to avoid bug (or turn off fast user switching)
# killall ScreenSaverEngine if you're stuck on the screensaver
sudo defaults write /Library/Preferences/com.apple.screensaver loginWindowIdleTime 0

# To speed up SMB file browsing, you can prevent macOS from reading .DS_Store files on SMB share
# https://support.apple.com/en-us/HT208209
sudo defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool TRUE

# show all hidden files by default
# shortcut to toogle: CMD-SHIFT-.
sudo defaults write com.apple.finder AppleShowAllFiles TRUE && killall Finder







#!/bin/zsh

# Script to install dependencies for a developer on Apple Silicon M1. 
# Pre-requisites: Git, Zsh (Mac uses zsh by default)
# Note: that all dependencies installed through homebrew by default will be done through Rosetta 2 since M1 doesnt't have native support for all formulas.
# Note: Homebrew in rosetta is installed in /usr/local/bin. When M1 support arrives it will be installed in /opt
# Node v15 works on Mac M1 for now. 

# Author: Hemant Joshi

# TODO: Add deps_part1 to PATH so that it can be called without zsh

source ~/.zshrc

bold=$(tput bold)

check_install_result() {
    if [ $? -eq 0 ]; then
        printf "\n${bold}$1 installed successfully\n"
    fi

    sleep 5
}

printf "${bold}\n--------------------- Welcome to dev setup Part I ---------------------\n\n"
printf "${bold}This includes setting up of homebrew and installing basic dependencies...\n"

arch -x86_64 /usr/local/bin/brew -v >> /dev/null
if [ $? -eq 0 ]; then
    printf "${bold}\nHomebrew is already installed under Rosetta..\n"
else
    printf "${bold}\nHomebrew is not installed... Installing homebrew under Rosetta for now...\n"
    arch -x86_64 /bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/master/install.sh)"
    echo 'alias ibrew="arch -x86_64 /usr/local/bin/brew"' >> ~/.zshrc
    source ~/.zshrc
    ibrew --help
    check_install_result "ibrew"
fi

/opt/homebrew/bin/brew -v >> /dev/null
if [ $? -eq 0 ]; then
    printf "${bold}\nM1 Homebrew already Installed\n"
else
    printf "${bold}\nInstalling M1 Homebrew\n..."
    sudo mkdir -p /opt/homebrew
    sudo chown -R $(whoami):staff /opt/homebrew
    cd /opt
    curl -L https://github.com/Homebrew/brew/tarball/master | tar xz --strip 1 -C homebrew
    echo 'export PATH=/opt/homebrew/bin:/usr/local/bin:$PATH' >> ~/.zshrc
fi

source ~/.zshrc

commands_install=(
    zsh-syntax-highlighting
    go
    android-platform-tools
    htop
    openjdk
    redis
    pyenv
    postgresql
    sqlc
    php
    mysql
)

printf "\n"

for j in $commands_install
do
    printf "${bold}Installing $j\n"
    brew list $j > /dev/null
    if [ $? -eq 0 ]; then
        printf "$j already installed successfully\n\n"
    else
        brew install $j
        check_install_result "$j"
    fi
done

java --version
if [ $? -eq 0 ]; then
    printf "\n${bold} Java setup successfull"
else
    `sudo ln -sfn /usr/local/opt/openjdk/libexec/openjdk.jdk /Library/Java/JavaVirtualMachines/openjdk.jdk`
fi

# Installing heroku-cli
printf "\n${bold} Heroku CLI\n"
brew list heroku > /dev/null
if [ $? -eq 0 ]; then
    printf "heroku installed successfully\n"
else
    brew tap heroku/brew > /dev/null
    brew install heroku
    check_install_result "heroku"
fi

zsh node_deps.sh
yarn --version
if [ $? -eq 0 ]; then
    printf "${bold}\nYarn installed successfully\n"
else
    printf "${bold}\nYarn not installed\n"
fi

echo 'export PATH=$(pyenv root)/shims:$PATH' >> ~/.zshrc

zsh py_deps.sh
python --version
if [ $? -eq 0 ]; then
    printf "${bold}\nPython Installed...\n"
else
    printf "${bold}\nReinstall python\n"
fi
