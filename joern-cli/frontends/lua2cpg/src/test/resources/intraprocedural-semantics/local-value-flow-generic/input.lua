local function alias_flow(input)
  local box = {}
  box.value = input
  local alias = box
  return os.execute(alias.value)
end

local function overwrite_flow(input)
  local box = {}
  box.value = input
  box.value = "safe"
  return os.execute(box.value)
end

local function conditional_parameter(input, use_default)
  if use_default then
    input = "safe"
  end
  return os.execute(input)
end

return {
  alias_flow = alias_flow,
  overwrite_flow = overwrite_flow,
  conditional_parameter = conditional_parameter,
}
