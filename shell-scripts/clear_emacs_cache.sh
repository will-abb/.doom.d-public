#!/bin/bash

# This script clears Emacs and Doom Emacs caches and syncs configurations

# Specify the Emacs directory
EMACS_DIR="$HOME/.emacs.d"

# Clear eln-cache
echo "Clearing eln-cache..."
rm -rf $EMACS_DIR/eln-cache/*

# Clear Doom Emacs cache
echo "Clearing Doom Emacs cache..."
rm -rf $EMACS_DIR/.local/cache/*

# Clear ELPA packages
echo "Clearing ELPA packages..."
rm -rf $EMACS_DIR/elpa

# Clear var directory
echo "Clearing var directory..."
rm -rf $EMACS_DIR/var

# Clear auto-save-list
echo "Clearing auto-save-list..."
rm -rf $EMACS_DIR/auto-save-list

# Clear TRAMP cache
echo "Clearing TRAMP cache..."
rm -rf $EMACS_DIR/tramp

# Clear additional Emacs cache in ~/.cache
echo "Clearing additional Emacs cache in ~/.cache..."
rm -rf ~/.cache/emacs

# Run Doom clean
echo "Running doom clean..."
doom clean

# Run Doom sync
echo "Running doom sync..."
doom sync

echo "Cache cleared and Doom Emacs synced."
