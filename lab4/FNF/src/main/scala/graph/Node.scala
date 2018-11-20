package graph

import transaction.Transaction

case class Node(id: Int, transaction: Transaction, children: List[Node]) {
    override def toString: String = transaction.name
}
object Node {
    def DFS(node: Node) = {
    
    }
}
