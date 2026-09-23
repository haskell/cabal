    # macOS/AArch64 (Mach-O)
    .macro  func name
    .globl _\name
_\name:
    .endm

    .text
    func meaning_of_life_asm

    mov     w0, #MEANING_OF_LIFE_VAL
    ret