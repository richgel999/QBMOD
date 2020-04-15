;QBMPASM.ASM (Misc. ASM & PC-Speaker routines for QBMP15.BAS)
;Written by Rich Geldreich (C) Copyright November 27, 1992
;Assembled with TASM v2.0 (This module hasn't been documented yet.)
IDEAL
MODEL SMALL
CODESEG
PUBLIC  CheckDoneFlag, SetBuffers, GetCS, GetOF, UnsignedComp
EVEN
ASSUME cs:@CODE, ds:@CODE, es:NOTHING, ss:@DATA
;=====================================================================
Buffer1  dw 0
Buffer2  dw 0
DoneFlag db 0
XLATTable: ;PC-Speaker LOG table
db 32,31,30,29,28,27,26,25,24,24,23,23,22,22,21,21,21,20,20,20,19,19,19
db 18,18,18,18,17,17,17,17,16,16,16,16,15,15,15,15,14,14,14,14,14,13,13
db 13,13,13,12,12,12,12,12,11,11,11,11,11,11,10,10,10,10,10,10,9,9,9,9
db 9,9,8,8,8,8,8,8,7,7,7,7,7,7,7,6,6,6,6,6,6,6,5,5,5,5,5,5,5,4,4,4,4,4
db 4,4,3,3,3,3,3,3,3,2,2,2,2,2,2,2,1,1,1,1,1,1,1,1,65,65,65,65,65,65,65
db 64,64,64,64,64,64,64,63,63,63,63,63,63,63,62,62,62,62,62,62,62,61,61
db 61,61,61,61,61,60,60,60,60,60,60,60,59,59,59,59,59,59,59,58,58,58,58
db 58,58,57,57,57,57,57,57,56,56,56,56,56,56,55,55,55,55,55,55,54,54,54
db 54,54,53,53,53,53,53,52,52,52,52,52,51,51,51,51,50,50,50,50,49,49,49
db 49,48,48,48,48,47,47,47,46,46,46,45,45,45,44,44,43,43,42,42,41,40,39
db 38,37,36,35,34,33
;=====================================================================
EVEN
PROC    NewInt8
        Push    ds ax bx 
BDS     = $+1
        Mov     ax, 09999h
        Mov     ds, ax
Offset8 = $+1
        Mov     bx, 09999h
        Mov     al, [ds:bx]
        Inc     bx
        Inc     bx
        Mov     [word cs:Offset8], bx
End8    = $+2
        Cmp     bx, 09999h
        Je      @@10
@@Back: 
        Mov     bx, offset XLATTable
        Xlat    [cs:bx]
        Out     042h, al
        
;        Mov     bx, dx ;for DAC on printer port
;        Mov     dx, 0378h
;        Add     al, 128
;        Out     dx, al
;        Mov     dx, bx
        
        Mov     al, 020h
        Out     020h, al
        Pop     bx ax ds
        Iret
EVEN
@@10:   Mov     [byte cs:DoneFlag], -1
        Mov     bx, [cs:Buffer1]
        Xchg    bx, [cs:Buffer2]
        Mov     [cs:Buffer1], bx
        Mov     [word cs:Offset8], bx
BufferSize = $+2
        Add     bx, 09999h
        Mov     [word cs:End8], bx
        Jmp     @@Back
ENDP    NewInt8
;=====================================================================
EVEN
PROC    CheckDoneFlag
        Xor     al, al
        Xchg    [cs:DoneFlag], al
        Cbw
        Retf    0
ENDP    CheckDoneFlag
;=====================================================================
PROC    SetBuffers  ;BDS Buffer1 Buffer2 BufferSize
;                     12   10       8        6 
        Push    bp 
        Mov     bp, sp
        Push    ds cs
        Pop     ds
        Mov     ax, [ss:bp+8]
        Mov     [ds:Buffer2], ax
        Mov     ax, [ss:bp+12]
        Mov     [word ds:BDS], ax
        Mov     ax, [ss:bp+10]
        Mov     [ds:Buffer1], ax
        Mov     [word ds:Offset8], ax
        Mov     bx, [ss:bp+6]
        Mov     [word ds:BufferSize], bx
        Add     ax, bx
        Mov     [word ds:End8], ax
        Pop     ds bp
        Retf    8
ENDP    SetBuffers
;=====================================================================
PROC    GetCS
        Mov     ax, cs
        Retf    0
ENDP    GetCS
;=====================================================================
PROC    GetOF
        Mov     ax, offset NewInt8
        Retf    0
ENDP    GetOF
;=====================================================================
PROC    UnsignedComp
        Push    bp
        Mov     bp, sp
        Mov     ax, [ss:bp+08]
        Cmp     ax, [ss:bp+06]
        Jae     @@AboveOrEqual
        Xor     ax, ax
@@Back: Pop     bp
        Retf    4
@@AboveOrEqual:
        Mov     ax, 1
        Jmp     @@Back
ENDP    UnsignedComp
;=====================================================================
END
