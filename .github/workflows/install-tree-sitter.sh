#!/usr/bin/env sh

version=$1
archive=tree-sitter-linux-x64.gz

wget "https://github.com/tree-sitter/tree-sitter/releases/download/${version}/${archive}"
gunzip "${archive}"
chmod +x tree-sitter-linux-x64

mkdir -p "$HOME/.local/bin"
mv tree-sitter-linux-x64 "$HOME/.local/bin/tree-sitter"

tree-sitter init-config
