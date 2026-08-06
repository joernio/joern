local function identity(value)
  return value
end

function source_branch()
  local tainted = luci.http.formvalue("command")
  return identity(tainted)
end

function safe_sink_branch()
  local safe = identity("fixed")
  return os.execute(safe)
end
