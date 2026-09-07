local bridge = require("bridge")
local command = luci.http.formvalue("cmd")
local transformed = bridge.forward(command)

return os.execute(transformed)
