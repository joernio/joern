module("shared.module")

function exec(cmd)
  return io.popen(cmd)
end
