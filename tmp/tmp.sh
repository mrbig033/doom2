#!/usr/bin/env bash

rofi -modi "run,window" -font "Monospace 15" -sidebar-mode -auto-select -width 15 -lines 10 -show window
rofi -show combi -combi-modi "window,run" -modi combi -font "Monospace 15" -sidebar-mode -auto-select -width 25 -lines 10 -show window
