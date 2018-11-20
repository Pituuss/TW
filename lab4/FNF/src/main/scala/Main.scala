import graph.{Graph, Node}
import transaction.{Alphabet, Transaction, Word}

import scala.language.postfixOps

object Main extends App {
    val a = Transaction("a", "x", List("x", "y"))
    val b = Transaction("b", "y", List("y", "z"))
    val c = Transaction("c", "x", List("x", "z"))
    val d = Transaction("d", "z", List("y", "z"))
    //    val e = Transaction("e", "z", List("y", "z"))
    
    val alphabet = Alphabet(List(a, b, c, d))
    val word = Word(List(b, a, a, d, c, b))
    val dependent = dependencyRelation(alphabet.transactions)
    val independent = independencyRelation(alphabet.transactions)
    
    def isDependent(transactions: List[Transaction], element: Transaction): (String, List[Transaction]) = {
        (element.name, transactions.filter(t ⇒
            element.params.contains(t.result) ||
                t.params.contains(element.result)
        ))
    }
    
    def isNotDependent(transactions: List[Transaction], element: Transaction): (String, List[Transaction]) = {
        (element.name, transactions.filter(t ⇒
            !(element.params.contains(t.result) ||
                t.params.contains(element.result))
        ))
    }
    
    def dependencyRelation(transactions: List[Transaction]): Map[String, List[Transaction]] = {
        transactions.map(t ⇒ isDependent(transactions, t)).map {
            case (k: String, v: List[Transaction]) ⇒ k → v
        }.toMap
    }
    
    def independencyRelation(transactions: List[Transaction]): Map[String, List[Transaction]] = {
        transactions.map(t ⇒ isNotDependent(transactions, t)).map {
            case (k: String, v: List[Transaction]) ⇒ k → v
        }.toMap
    }
    
    def calculateFNF(word: Word): List[List[Transaction]] = {
        FNF(word.transactions, List.empty)
    }
    
    def FNF(transactions: List[Transaction], possible: List[List[Transaction]]): List[List[Transaction]] = {
        transactions match {
            case Nil ⇒ possible
            case ::(head, tail) ⇒
                val possibleToVerify = independent(head.name).toSet
                val verified: List[Transaction] = verify(possibleToVerify, tail)
                FNF(verified.foldLeft(tail)((acc, e) ⇒ dropFirstMatch(acc, e)), List(List(head) ++ verified) ++ possible)
        }
    }
    
    def dropFirstMatch[A](ls: List[A], value: A): List[A] = {
        val index = ls.indexOf(value)
        if (index < 0) {
            ls
        } else if (index == 0) {
            ls.tail
        } else {
            val (a, b) = ls.splitAt(index)
            a ++ b.tail
        }
    }
    
    
    def verify(possibleToVerify: Set[Transaction], list: List[Transaction]): List[Transaction] = {
        list.foldLeft((possibleToVerify, List.empty[Transaction]))(
            (mergedAcc, head) ⇒ {
                if (possibleToVerify(head))
                    (mergedAcc._1 -- dependent(head.name), List(head) ++ mergedAcc._2)
                else
                    (mergedAcc._1 -- dependent(head.name), mergedAcc._2)
            })._2
    }
    
//    def createGraph(word: Word): List[Node] = {
//        val nodes: List[Node] = word.transactions.zipWithIndex.map { case (t, id) ⇒ Node(id, t, List.empty[Node]) }
//        nodes.map(
//            rootNode ⇒ {
//                val dep = dependent(rootNode.transaction.name).toSet
//                val possibleChildren = nodes.dropWhile(node ⇒ node != rootNode) match {
//                    case Nil ⇒ Nil
//                    case _ :: tail ⇒ tail
//                }
//                val children = possibleChildren.filter(c ⇒ dep(c.transaction)).foldLeft(List.empty[Node])((acc: List[Node], c: Node) ⇒ acc.::(c))
//                Node(rootNode.id, rootNode.transaction, children)
//            })
//    }
    
    def createGraphV2(word: Word): Graph = {
        val nodes: List[Node] = word.transactions.zipWithIndex.map { case (t, id) ⇒ Node(id, t, List.empty[Node]) }
        nodes.reverse.foldLeft(
            Set.empty[Node]
            , Graph(List.empty[Node], List.empty[String])
        )(
            (setAndGraph: (Set[Node], Graph), head: Node) ⇒ {
                val dependentNodes = setAndGraph._1.toList.filter(node ⇒ dependent(head.transaction.name).toSet(node.transaction))
                val independentNodes = setAndGraph._1.toList.filter(node ⇒ independent(head.transaction.name).toSet(node.transaction))
                val passDep =
                    dependentNodes match {
                        case Nil ⇒ independentNodes.flatMap(d ⇒ d.children).filter(node ⇒ dependent(head.transaction.name).toSet(node.transaction))
                        case _ ⇒ dependentNodes
                    }
                val nNode = Node(head.id, head.transaction, passDep)
                
                (setAndGraph._1 -- dependentNodes + nNode
                    , Graph(setAndGraph._2.nodes.::(nNode), setAndGraph._2.edges ++ passDep.map(d ⇒ s"${head.id} -> ${d.id}"))
                )
            })._2
    }
    
    def independentNodes(nodes: List[Node]): List[Node] = {
        nodes.foldLeft(Set.empty[Node], List.empty[Node])((acc, n: Node) ⇒ if (acc._1(n)) (acc._1 ++ n.children, acc._2) else (acc._1 ++ n.children, acc._2.::(n)))._2
    }
    
    def graphToFnF(graph: Graph): List[List[Transaction]] = {
        graph.nodes.foldLeft(graph.nodes, List.empty[List[Transaction]])((nodesAndClasses, _) ⇒ {
            val c = independentNodes(nodesAndClasses._1)
            nodesAndClasses._1 match {
                case Nil ⇒ (Nil, nodesAndClasses._2)
                case ::(_, tl) ⇒ (c.foldLeft(tl)((acc, e) ⇒ dropFirstMatch(acc, e)), nodesAndClasses._2.::(c.map(_.transaction)))
            }
        })._2.reverse
    }
    
    
    println(dependent)
    println(independent)
    println(calculateFNF(word))
    val graph = createGraphV2(word)
    println(graphToFnF(graph))
}
