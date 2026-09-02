local function via_local()
  local lib = require("returnlib")
  local cmd = luci.http.formvalue("cmd")
  return lib.run(cmd)
end

local function via_direct()
  local cmd = luci.http.formvalue("cmd")
  return require("returnlib").run(cmd)
end

return {
  via_local = via_local,
  via_direct = via_direct,
}
