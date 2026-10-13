        .text
        .globl meaning_of_life_asm
        .type meaning_of_life_asm, @function
meaning_of_life_asm:
        movl    $MEANING_OF_LIFE_VAL, %eax
        ret
        .size meaning_of_life_asm, .-meaning_of_life_asm