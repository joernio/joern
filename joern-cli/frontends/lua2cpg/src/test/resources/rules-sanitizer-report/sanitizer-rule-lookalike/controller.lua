local formatter = unknown_provider()
local command = luci.http.formvalue("cmd")
local unchanged = formatter._cmdformat(command)

return os.execute(unchanged)
