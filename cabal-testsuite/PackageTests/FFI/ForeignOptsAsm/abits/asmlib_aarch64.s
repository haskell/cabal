        .text
        .globl meaning_of_life_asm
        .type meaning_of_life_asm, @function
        // meaning_of_life_val must be defined via --defsym from asm-options.
        // If asm-options are not passed, the assembler will fail here.
meaning_of_life_asm:
        mov     w0, #meaning_of_life_val
        ret
        .size meaning_of_life_asm, .-meaning_of_life_asm
