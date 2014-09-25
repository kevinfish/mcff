;;; ffwin.asm  FreeForth kernel, Windows specific port
;;; $Id: ffwin.asm,v 1.1 2006/05/29 01:55:28 lavarenn Exp $

;;; -------------------------------------------------------
;;; Typical compile command (with http://flatassembler.net):
;;;   fasm.exe ffwin.asm ff.exe

;;; ------------------------------------------------------------------------
;;; OSFORMAT specifies the Operating-System dependent executable file format

macro OSFORMAT {
format PE console
stack 16000
entry start
section ".code" code readable writeable executable
}

;;; -----------------------------------------------------
;;; OSINCLUDE defines OS-specific ASM source to "include"

macro OSINCLUDE { include "ffwinio.asm" }

;;; -----------------------------------------------------
;;; OSFILE defines OS-specific FF source to "file"

macro OSFILE { file "ffwin.boot" }

;;; -----------------------------------------------------
;;; all macros ready: compile all:

include "ff.asm"

;;; That's all folks!!
