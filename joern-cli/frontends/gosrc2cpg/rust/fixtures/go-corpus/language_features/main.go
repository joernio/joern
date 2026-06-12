package main

import "fmt"

type Reader interface {
	Read(p []byte) (int, error)
}

type Pair[K comparable, V any] struct {
	Key   K
	Value V
}

type Thing[T int64 | float32] struct {
	path string
	next *Thing[T]
}

func (t *Thing[T]) Read(p []byte) (int, error) {
	defer fmt.Println("read")
	go fmt.Println(<-make(chan string))
	return len(p), nil
}

func mapValues[T any](items []T, fn func(T) T) []T {
	out := make([]T, 0, len(items))
	for _, item := range items {
		out = append(out, fn(item))
	}
	return out
}

func main() {
	ch := make(chan string)
	select {
	case ch <- "x":
		fallthrough
	default:
		fmt.Println("default")
	}

	pair := Pair[string, int]{Key: "count", Value: 1}
	_ = pair.Value
	_ = any(&Thing[int64]{}).(*Thing[int64])
	_ = mapValues([]int{1, 2}, func(v int) int { return v + 1 })
	servers := []string{"api"}
	_ = servers[:0]
	for range servers {
		fmt.Println("server")
	}
}
