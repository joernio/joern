luci = { http = {} }

function luci.http.formvalue(name)
  return name
end

os = {
  execute = function(cmd)
    return cmd
  end
}

local tainted = luci.http.formvalue("cmd")
local clean = "fixed"
tonumber(clean)
os.execute(tainted)
