# Eboy - Emacs Gameboy Emulator

A Nintendo Gameboy emulator for Emacs.
This is a work in progress!
Just got Tetris starting up, not tested with anything else.

![](gif/tetris.gif)

It currently runs a constant loop without delay.
To get out of this loop, use the common `C-g` command.

The display is drawn using unicode, by default it is set for dark theme.
Use the unicode list below when you have a light theme installed in Emacs.

``` emacs-lisp
(setq eboy-display-unicode-list eboy-display-unicode-list-light-theme)
```

It has frameskip enabled, by default set to 20.

## Why?
I always wanted to learn how to a make an emulator and want learn how to write packages for Emacs.
So here my first emulator and first emacs package.

## Prerequisites

Emacs :)

## Installing

For now just `M-x eval-buffer` or `M-x byte-compile-file` the following files: `eboy-macros.el`, `eboy-cpu.el` and `eboy.el`.

## Usage

Load a rom using the `M-x eboy-load-rom` command.

  | Gameboy     | Eboy     |
  |------------:|---------:|
  | Start       | Enter    |
  | Select      | Space    |
  | B           | D        |
  | A           | S        |
  | down        | k        |
  | up          | i        |
  | left        | j        |
  | right       | l        |

To continue a game after you pressed `C-g`, use the `M-x eboy-run` command.

Eboy related messages are written to the buffer `*Eboy Messages*`.

## Blargg's test roms status
1. special: Seems to hang, without giving an error
2. interrupts: Passed
3. op hl,sp: F8 F8 Failed
4. op r,imm: C6 DE Failed
5. op rp: Passed
6. ld r,r: Passed
7. jr,jp,call,ret,rst: Seems to hang, without giving an error
8. misc instrs: Passed
9. op r,r: List of failing opcodes
  3F, 07, 17, 0F, CB 00, CB 01, CB 02, CB 03, CB 05, CB 07, Cb 08, CB 09, CB 0A, CB 0B, CB 0C, CB 0D, CB 0F, CB 10, CB 11, CB 12, CB 13, CB 14, CB 15, CB 17, CB 20, CB 21, CB 22, CB 23, CB 24, CB 25, CB 27,
10. bit ops: Passed
11. op a,(hl):  List of failing opcodes
 CB 0E,CB 2E,CB 3E,CB 46,CB 4E,CB 56,CB 5E,CB 66,CB 6E,CB 76,CB 7E,CB 86,CB 8E,CB 96,CB 9E,CB A6,CB AE,CB B6,CB BE,CB C6,CB CE,CB D6,CB DE,CB E6,CB EE,CB F6,CB FE

The test roms are hosted at: https://github.com/retrio/gb-test-roms
