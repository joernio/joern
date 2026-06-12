package service

type Server struct {
	name string
}

func Name() string {
	server := Server{name: "api"}
	return server.name
}
