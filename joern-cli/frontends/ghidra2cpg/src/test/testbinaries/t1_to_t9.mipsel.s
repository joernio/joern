.section .text

.global __start

#---------------------------------------- 
# compile and run on non mipsel platforms:
#
# $ mipsel-linux-gnu-as -O0 t1_to_t9.mipsel.s -o t1_to_t9.o && mipsel-linux-gnu-gcc-9 -O0 t1_to_t9.o -o t1_to_t9 -static -nostdlib && qemu-mipsel ./t1_to_t9
# $ echo $?
# 42
#---------------------------------------- 

__start:
# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x0
	li $t1, 0x2a # 42

	add $t2, $t0, $t1
	addu $t3, $t2, $t0
	addu $t4, $t3, $t0
	addi $t5, $t4, 0x1
	addiu $t6, $t5, 0x1

	or $t9, $t6, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0xffff
	or $t1, $t9, $zero

	and $t2, $t1, $t0
	andi $t3, $t2, 0xffff

	or $t9, $t3, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero

	div $t1, $t0
	mflo $t2
	divu $t2, $t0
	mflo $t3

	or $t9, $t3, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero

	mult $t1, $t0
	mflo $t2
	multu $t2, $t0
	mflo $t3
	mul $t4, $t3, $t0

	or $t9, $t4, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero
	li $t2, 0b11101011

	nor $t3, $t1, $t2
	or $t4, $t3, 0b00110000

	or $t9, $t4, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero

	sll $t2, $t1, 0x1
	sllv $t3, $t2, $t0
	sra $t4, $t3, 0x1
	srav $t5, $t4, $t0
	srl $t6, $t5, 0x0
	srlv $t7, $t6, $zero

	or $t9, $t6, $zero
	nop

# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero

	sub $t2, $t1, $t0
	subu $t3, $t2, $t0

	or $t9, $t3, $zero
	nop


# nopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnopnop
	li $t0, 0x1
	or $t1, $t9, $zero

	xor $t2, $t1, $zero
	xori $t3, $t2, 0b00000100

	or $t9, $t3, $zero
	nop

	li $v0, 0xfa1 # 4001
	or $a0, $t9, $zero
	syscall


.section .data
