        .text
        .globl meaning_of_life_asm
        .type meaning_of_life_asm, @function
        # meaning_of_life_val must be defined via --defsym from asm-options.
        # If asm-options are not passed, the assembler will fail here.
meaning_of_life_asm:
        movl    $meaning_of_life_val, %eax
        ret
        .size meaning_of_life_asm, .-meaning_of_life_asm
