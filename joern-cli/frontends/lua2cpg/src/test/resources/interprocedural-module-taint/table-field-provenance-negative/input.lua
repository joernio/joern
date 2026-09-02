local box = {}
box.metadata = luci.http.formvalue("metadata")

return os.execute(box.command)
