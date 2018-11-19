import transaction.{Alphabet, FNFOps, Transaction, Word}

object Main extends App {
    val a = Transaction("a", "x", List("x", "y"))
    val b = Transaction("b", "y", List("y", "z"))
    val c = Transaction("c", "x", List("x", "z"))
    val d = Transaction("d", "z", List("y", "z"))
    //    val e = Transaction("e", "z", List("y", "z"))
    
    val alphabet = Alphabet(List(a, b, c, d))
    val word = Word(List(b, a, a, d, c, b))
    val dependent = FNFOps.dependencyRelation(alphabet.transactions)
    val independent = FNFOps.independencyRelation(alphabet.transactions)
    
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
    
    
    println(dependent)
    println(independent)
    println(FNFOps.FNF(word.transactions, List.empty))
    
}
