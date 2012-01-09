#!/usr/bin/env zsh

./try editor1.asm || exit 1
stty -echo raw </dev/tty
exec ./a.out \
	<basics.fth \
	<data.fth \
	<keymaps.fth \
	<term.fth \
	<colorconsole.fth \
	<coloreditor.fth \
	</dev/tty \
	3<>blocks.dat # fd 3 is mmaped by data.fth
