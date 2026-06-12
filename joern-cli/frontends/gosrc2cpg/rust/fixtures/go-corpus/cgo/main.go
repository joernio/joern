package cgo

/*
#include <stdlib.h>
*/
import "C"
import "unsafe"

func Allocate(size C.size_t) unsafe.Pointer {
	return C.malloc(size)
}
