. "$HOME/.cargo/env"

# make sure Finder/Launchpad can find Homebrew stuff
if [[ -x /opt/homebrew/bin/brew ]]; then
  export PATH=/opt/homebrew/bin:${PATH}
fi
