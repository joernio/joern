local value = luci.http.formvalue("command")

if enabled then
  value = "fixed"
  return os.execute(value)
end
