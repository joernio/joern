local selector = luci.http.formvalue("selector")
local candidate = "selected-command"
local selected = "fixed"

if selector == "chosen" then
  selected = candidate
end

selected = "fixed"
return os.execute(selected)
