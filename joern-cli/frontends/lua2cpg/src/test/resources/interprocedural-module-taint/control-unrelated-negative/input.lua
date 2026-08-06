local selector = luci.http.formvalue("selector")
local ignored = "fixed"

if selector == "chosen" then
  ignored = "ignored"
end

local selected = "fixed"
return os.execute(selected)
