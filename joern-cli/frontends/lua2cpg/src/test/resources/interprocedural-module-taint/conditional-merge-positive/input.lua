local value = luci.http.formvalue("command")

if use_default then
  value = "fixed"
end

return os.execute(value)
