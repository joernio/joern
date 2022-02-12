def instrBefore(x: CfgNode, n: Int) =
  x.cfgPrev(n).l.reverse.map(y => y.address.get + ": " + y.code)

cpg.ret.map(x => instrBefore(x, 5).mkString("|\t") + "\n").l |> "/tmp/gadgets.txt"
