package graph

class Graph(val nodes: List[Node], val edges: List[String]) {
    def print(): Unit = {
        println("digraph G {")
        edges.reverse.foreach(println)
        nodes.map(n ⇒ n.id → n.transaction.name).toMap.foreach { case (k, v) ⇒ println(s"$k [label=$v]") }
        println("}")
    }
}

object Graph {
    def apply(nodes: List[Node], edges: List[String]): Graph = new Graph(nodes, edges)
}
