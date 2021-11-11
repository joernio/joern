def addrAndCode(y : CfgNode) =
    y.address.get + ": " + y.code 
def instrBefore(x : CfgNode, n: Int) = x.cfgPrev(n).l.reverse.map(addrAndCode)

cpg.ret.map(x => instrBefore(x, 5).mkString("|\t") + "\n").l |> "/tmp/gadgets.txt"
