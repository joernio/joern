.section .text

.global __start

#---------------------------------------- 
# compile and run on non mipsel platforms:
#
# $ mipsel-linux-gnu-as -O0 84_div_2.mipsel.s -o 84_div_2.o && mipsel-linux-gnu-gcc-9 -O0 84_div_2.o -o 84_div_2 -static -nostdlib && qemu-mipsel ./84_div_2
# $ echo $?
# 42
#---------------------------------------- 

__start:
	li $t0, 0x2
	li $t1, 0x54 # 84

	div $t1, $t0
	nop # the following three `li`s are here to ensure that data flows
	nop # are not tripped up by irrelevant instructions.
	li $t5, 0x7ed
	li $t6, 0xdead
	li $t7, 0xbabe
	nop
	nop
	mflo $t2
	or $t9, $t2, $zero
	nop

	li $v0, 0xfa1 # 4001
	or $a0, $t9, $zero
	syscall


.section .data
