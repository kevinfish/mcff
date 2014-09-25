;;; ffwinio.asm  FreeForth Windows I/O
;;; $Id: ffwinio.asm,v 1.8 2009-09-03 20:49:22 lavarec Exp $

;;; --------------------------------------------------
;;; FreeForth interface to Windows file-I/O

macro GENWORDS {                ; redefines GENWORDS
stdin   dd 0                    ; code or data address, or literal
        db 1                    ; 0:code, 1:data/lit, etc?
        CDB "stdin"             ; name counted string
stdout  dd 0                    ; code or data address, or literal
        db 1                    ; 0:code, 1:data/lit, etc?
        CDB "stdout"            ; name counted string
        GENWORDS                ; calls GENWORDS _previous_ definition
}

iobuf   dd 0

CODE "exit",_exit               ; n --
        push ebx                ; process-return value
        call [ExitProcess]      ; never returns

CODE "close",_close             ; fh -- ?
        xchg eax,esp
        push edx
        push eax                ; save eax, edx
        push ebx                ; HANDLE hObject
        call [CloseHandle]      ; returns BOOL, 1:ok, 0:failed
        dec eax                 ; -- ? ; 0:ok, -1:failed
        jmp nipeax


DATA "open'",_openbuf,0         ; path to FreeForth root directory /-terminated
        rb 60                   ; counted string, for append with zt-string

;;; openr opens existing file for read-only
CODE "openr",_openr             ; @ # -- fh
        xor ecx,ecx
        inc ecx                 ; ecx=1
        jmp _open

;;; openw0 opens (existing if: truncates else: creates) file for read-write
CODE "openw0",_openw0           ; @ # -- fh
        xor ecx,ecx
        inc ecx
        inc ecx                 ; ecx=2
        jmp _open

;;; openw opens (and creates if non-existing) file for read-write
CODE "openw",_openw             ; @ # -- fh ; -1:failed
        xor ecx,ecx             ; ecx=0
_open:  mov byte[edx+ebx],0     ; append zero-terminator (another whitespace)
        xchg eax,esp
        push eax                ; save callSP
        pushd 0                 ; HANDLE hTemplateFile
        pushd $80               ; DWORD FLAG_ATTRIBUTE_NORMAL
        pushd 4                 ; DWORD OPEN_ALWAYS=4 OPEN_EXISTING=3(openr)
        sub [esp],ecx           ;       CREATE_ALWAYS=2(openw0)
        pushd 0                 ; LPSECURITY_ATTRIBUTES
        and cl,1                ; ecx=1:openr, 0:openw*
        pushd ecx               ; DWORD FILE_SHARE_NONE=0 FILE_SHARE_READ=1
        pushd $C0000000         ; DWORD GENERIC_READ=8<<28 GENERIC_WRITE=4<<28
        shl ecx,30
        sub [esp],ecx
        cmp byte[edx],"'"       ; quote-prefix?
        jne @f                  ;   append @,# to _openbuf:
        lea esi,[edx+1]         ;   esi points after prefix
        mov edx,_openbuf+1      ;   edx points after _openbuf count
        movzx edi,byte[edx-1]   ;   edi = _openbuf count
        lea edi,[edx+edi]       ;   edi points after last _openbuf char
        mov ecx,ebx             ;   one less for prefix, one more for zt
        rep movsb               ;   append filename to root directory
@@:     push edx                ; LPCTSTR lpFileName
        call [CreateFile]       ; returns HANDLE, INVALID_HANDLE_VALUE=-1
        jmp nipeax              ; -- eax
        
VECT "type",_type               ; @ # -- n ; stdout write drop ;
        DUP1
.patch: mov ebx,1               ; BB immediate, patched at startup
        call _write
        jmp drop1

CODE "write",_write             ; @ # fh -- n
        mov ecx,[WriteFile]     ; (hFile, lpBuffer, nNumberOfBytesToWrite,
        jmp _rw                 ;  lpNumberOfBytesWritten, lpOverlapped)

VECT "accept",_accept           ; @ # -- n ; stdin read ;
        DUP1
.patch: mov ebx,0               ; BB immediate, patched at startup
;;;     jmp _read
CODE "read",_read               ; @ # fh -- n ; -1:failed
        mov ecx,[ReadFile]      ; (hFile, lpBuffer, nNumberOfBytesToRead,
_rw:    xchg eax,esp            ;  lpNumberOfBytesRead, lpOverlapped)
        pop esi                 ; esi=@
        push eax                ; save callSP
        pushd 0                 ; lpOverlapped
        pushd iobuf             ; lpNumberOfBytesRead/Written
        push edx                ; nNumberOfBytesToRead/Write
        push esi                ; lpBuffer
        push ebx                ; hFile
        call ecx                ; returns BOOL, 1:ok, 0:failed
        mov ebx,[iobuf]
        dec eax                 ; 0:ok, -1:failed
        jz @f
nipeax: mov ebx,eax             ; -- eax
@@:     pop eax                 ; restore callSP
        pop edx
        xchg eax,esp
        ret

;;; -----------------------------------------------------
;;; FreeForth interface to Windows dynamic-link libraries

CODE "#lib",_dllib              ; @ # -- libh
        xchg eax,esp
        push eax                ; save callSP
        mov byte[edx+ebx],0     ; append zero-terminator
        push edx                ; LPCTSTR lpLibFileName
        call [LoadLibrary]      ; returns HINSTANCE; eax=0 on error
        jmp dlret               ; -- libh

CODE "#fun",_dlfun              ; @ # libh -- funh
        xchg eax,esp
        pop ecx                 ; ecx = @
        push eax                ; save callSP
        mov byte[ecx+edx],0     ; append zero-terminator
        pushd ecx               ; LPCSTR lpProcName
        pushd ebx               ; HMODULE libhandle
        call [GetProcAddress]   ; returns FARPROC; eax=0 on error
dlret:  or eax,eax
        jz _checkior.fail
        jmp nipeax

DATA "ior",_ior,0               ; stores last I/O error
CODE "?ior",_checkior           ; ? -- ; 0>=:ok 0<:failed
        xchg eax,esp
        mov [_ior],ebx
        or ebx,ebx
        jl @f
        mov ebx,edx
        pop edx
        xchg eax,esp
        ret
@@:     push eax                ; save callSP
.fail:  call [GetLastError]     ; eax = errorCode
        mov [_ior],eax
        pushd 0                 ; lpArglist
        pushd 128               ; maxMessageSize
        pushd eob               ; lpBuffer
        pushd 0                 ; dwLangageId
        push eax                ; dwMessageId
        pushd 0                 ; lpSource
        pushd $1000             ; dwFlags = FORMAT_MESSAGE_FROM_SYSTEM
        call [FormatMessage]    ; eax = message length
        mov ebx,eax
        mov edx,eob
        pop eax                 ; restore callSP
        xchg eax,esp
        call _type
        call _error
        CDB "system call failed"

CODE "#call",_dlcall            ; #args funh -- funresult
        ;; Windows DLLs use two different calling conventions,
        ;; either "STDCALL" (callee pops caller arguments before returning),
        ;; or "CDECL" (caller pops arguments after callee returns);
        ;; adjusting the stack pointer works in both cases.
        ;; Note: WindowsXP requires CreateProcessA arguments to be pushed
        ;; on the CALLstack initially allocated by WindowsXP loader
        ;; (whereas Windows2000 doesn't care; if anybody knows why: tell me!)
        ;; this is why DATAstack is allocated from loader-allocated CALLstack.
        lea edx,[eax+4*edx]     ; dataSP after funcall
        push edx                ; save it
        xchg eax,esp
        mov [bssend-4],eax      ; save callSP
        call ebx                ; eax = funresult
        mov esp,[bssend-4]      ; restore callSP
        mov ebx,eax
        pop eax
        xchg eax,esp
        pop edx
        xchg eax,esp
        ret

;;; -------------------------------------------------------
;;; startup code:

start:
        pushd -10               ; STD_INPUT_HANDLE
        call [GetStdHandle]
        mov [stdin],eax
        mov [_accept.patch+1],eax ; patch "mov ebx,imm" at patchi
        pushd -11               ; STD_OUTPUT_HANDLE
        call [GetStdHandle]
        mov [stdout],eax
        mov [_type.patch+1],eax ; patch "mov ebx,imm" at patcho
        call [GetCommandLine]   ; eax points on command-line
        mov edx,eax
        mov ebx,edx
@@:     inc ebx
        cmp byte[ebx],0         ; command-line zero-terminated
        jnz @b
        sub ebx,edx             ; -- args argslen
        jmp ffboot

;;; -------------------------------------------------------
;;; windows-specific linkage stuff:

;include "INCLUDE/MACRO/IMPORT32.INC"
;;; inlined here after simplification:
; ------------------------------------
macro library [name,string] {
  forward
    local _label
    dd RVA name#.lookup,0,0,RVA _label,RVA name#.address
  common
    dd 0,0,0,0,0
  forward
    _label db string,0
    rb RVA $ and 1
}
macro import name,[label,string] {
  common
    name#.lookup:
  forward
    local _label
    dd RVA _label
  common
    dd 0
    name#.address:
  forward
    label dd RVA _label
  common
    dd 0
  forward
    _label dw 0
    db string,0
    rb RVA $ and 1
}
; ------------------------------------

data import
  library kernel,'KERNEL32.DLL'
  import kernel,\
    GetStdHandle,'GetStdHandle',\
    GetCommandLine,'GetCommandLineA',\
    CreateFile,'CreateFileA',\
    ReadFile,'ReadFile',\
    WriteFile,'WriteFile',\
    CloseHandle,'CloseHandle',\
    LoadLibrary,'LoadLibraryA',\
    GetProcAddress,'GetProcAddress',\
    GetLastError,'GetLastError',\
    FormatMessage,'FormatMessageA',\
    ExitProcess,'ExitProcess'
end data

;;; That's all folks!!
