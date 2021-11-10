def addrAndCode(y : CfgNode) =
    y.lineNumber.map(_.toLong.toHexString).get + ": " + y.code 
def instrBefore(x : CfgNode, n: Int) = x.cfgPrev(n).l.reverse.map(addrAndCode)
def rets = cpg.call.code(".*jr.*")

rets.map(x => instrBefore(x, 5).mkString("|\t") + "\n").l |> "/tmp/gadgets.txt"
