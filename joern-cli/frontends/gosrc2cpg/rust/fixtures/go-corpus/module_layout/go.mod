module example.com/root

go 1.22

require (
	github.com/acme/lib v1.2.3
	github.com/acme/indirect v0.5.0 // indirect
)

replace github.com/acme/lib => ../lib
