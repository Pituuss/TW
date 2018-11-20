package graph

class Graph(val nodes: List[Node], val edges: List[String])

object Graph {
    def apply(nodes: List[Node], edges: List[String]): Graph = new Graph(nodes, edges)
}
