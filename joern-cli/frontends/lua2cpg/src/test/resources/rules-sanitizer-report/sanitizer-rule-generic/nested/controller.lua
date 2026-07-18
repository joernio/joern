local formatter = require("formatter")
local command = luci.http.formvalue("cmd")
local safe_command = formatter._cmdformat(command)

return os.execute(safe_command)
